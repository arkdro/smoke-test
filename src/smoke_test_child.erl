%%%
%%% smoke_test_child: dynamically added worker
%%% 
%%% Copyright (c) 2011 Megaplan Ltd. (Russia)
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"),
%%% to deal in the Software without restriction, including without limitation
%%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%%% and/or sell copies of the Software, and to permit persons to whom
%%% the Software is furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included
%%% in all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
%%% IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
%%% CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
%%% TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
%%% SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
%%%
%%% @author arkdro <arkdro@gmail.com>
%%% @since 2012-02-07 14:42
%%% @license MIT
%%% @doc dynamically added worker that does the real thing.
%%%

-module(smoke_test_child).
-behaviour(gen_server).

%%%----------------------------------------------------------------------------
%%% Exports
%%%----------------------------------------------------------------------------
-export([start/0, start_link/0, start_link/1, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([terminate/2, code_change/3]).

%%%----------------------------------------------------------------------------
%%% Includes
%%%----------------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("child.hrl").
-include("smoke_test.hrl").

-define(TC, 1).

%%%----------------------------------------------------------------------------
%%% gen_server callbacks
%%%----------------------------------------------------------------------------
init(Params) ->
    C = prepare_all(Params),
    process_flag(trap_exit, true), % to send stats
    mpln_p_debug:pr({?MODULE, 'init done', ?LINE, C#child.id, self()},
        C#child.debug, run, 2),
    {ok, C}.

%%-----------------------------------------------------------------------------
%%
%% Handling call messages
%% @since 2012-02-07 14:42
%%
-spec handle_call(any(), any(), #child{}) ->
                         {stop, normal, ok, #child{}}
                             | {reply, any(), #child{}}.

handle_call(stop, _From, St) ->
    {stop, normal, ok, St};

handle_call(status, _From, St) ->
    {reply, St, St};

handle_call(_N, _From, St) ->
    {reply, {error, unknown_request}, St}.

%%-----------------------------------------------------------------------------
%%
%% Handling cast messages
%% @since 2012-02-07 14:42
%%
-spec handle_cast(any(), #child{}) -> {stop, normal, #child{}}
                                          | {noreply, #child{}}.

handle_cast(stop, St) ->
    {stop, normal, St};

handle_cast(_, St) ->
    {noreply, St}.

%%-----------------------------------------------------------------------------
terminate(_, #child{id=Id} = State) ->
    send_stat(State),
    mpln_p_debug:pr({?MODULE, terminate, ?LINE, Id, self()},
        State#child.debug, run, 2),
    ok.

%%-----------------------------------------------------------------------------
%%
%% Handling all non call/cast messages
%%
-spec handle_info(any(), #child{}) -> {noreply, #child{}}.

handle_info(last_job_timeout, St) ->
    mpln_p_debug:pr({?MODULE, 'info_last_job_timeout', ?LINE},
                    St#child.debug, run, 3),
    % we waited 1000/Hz + Timeout. All the jobs must be terminated by now.
    {stop, normal, St};

handle_info({job_timeout, Id}, State) ->
    mpln_p_debug:pr({?MODULE, 'info_job_timeout', ?LINE},
                    State#child.debug, run, 6),
    New = stop_job(State, Id),
    {noreply, New};

handle_info(periodic_check, State) ->
    mpln_p_debug:pr({?MODULE, 'info_periodic_check', ?LINE},
                    State#child.debug, run, 6),
    New = periodic_check(State),
    {noreply, New};

handle_info({'DOWN', Mref, _, _, _}, #child{id=Id} = St) ->
    mpln_p_debug:pr({?MODULE, info_down, ?LINE, Id, self(), Mref},
                    St#child.debug, run, 2),
    New = job_done(St, Mref),
    {noreply, New};

handle_info(_Req, State) ->
    mpln_p_debug:pr({?MODULE, other, ?LINE, _Req, State#child.id, self()},
        State#child.debug, run, 2),
    {noreply, State}.

%%-----------------------------------------------------------------------------
code_change(_Old_vsn, State, _Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------------
start() ->
    start_link().

%%-----------------------------------------------------------------------------
start_link() ->
    start_link([]).

start_link(Params) ->
    gen_server:start_link(?MODULE, Params, []).

%%-----------------------------------------------------------------------------
stop() ->
    gen_server:call(?MODULE, stop).

%%%----------------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------------
%%
%% @doc prepares necessary things
%%
-spec prepare_all(list()) -> #child{}.

prepare_all(L) ->
    Hz = proplists:get_value(hz, L),
    Seconds = proplists:get_value(seconds, L),
    Cnt = Seconds * Hz,
    #child{
          id = proplists:get_value(id, L),
          debug = proplists:get_value(debug, L, []),
          timeout = proplists:get_value(timeout, L),
          url = proplists:get_value(url, L),
          method = proplists:get_value(method, L),
          hz = Hz,
          seconds = Seconds,
          cnt = Cnt,
          timer = erlang:send_after(trunc(1 + 1000/Hz), self(), periodic_check)
        }.

%%-----------------------------------------------------------------------------
%%
%% @doc 
%%
-spec periodic_check(#child{}) -> #child{}.

periodic_check(#child{cnt=0, timeout=Timeout} = State) ->
    erlang:send_after(Timeout, self(), last_job_timeout),
    State;

periodic_check(#child{cnt=Cnt, hz=Hz, timer=Ref} = State) ->
    mpln_misc_run:cancel_timer(Ref),
    New = add_job(State),
    Nref = erlang:send_after(trunc(1 + 1000/Hz), self(), periodic_check),
    New#child{cnt=Cnt-1, timer=Nref}.

%%-----------------------------------------------------------------------------
%%
%% @doc terminates job if it still works
%%
stop_job(#child{jobs=Jobs} = St, Id) ->
    F = fun(#chi{id=X}) ->
                X == Id
        end,
    {Found, Rest} = lists:partition(F, Jobs),
    case Found of
        [] ->
            ok;
        [C] ->
            smoke_test_misc:stop_child(St#child.debug,
                                       smoke_test_request_supervisor,
                                       C#chi.pid)
    end,
    St#child{jobs=Rest}.

%%-----------------------------------------------------------------------------
%%
%% @doc starts new job and stores job info into state
%%
-spec add_job(#child{}) -> #child{}.

add_job(#child{jobs=Jobs, timeout=T} = St) ->
    Ref = make_ref(),
    case prepare_one_job(St, Ref, T) of
        [C] ->
            erlang:send_after(T, self(), {job_timeout, C#chi.id}),
            St#child{jobs=[C|Jobs]};
        _ -> % error
            St
        end.

%%-----------------------------------------------------------------------------
%%
%% @doc creates parameters and starts job process with the parameters
%%
-spec prepare_one_job(#child{}, reference(), non_neg_integer()) -> [#chi{}].

prepare_one_job(St, Ref, Time) ->
    Params = [
              {id, Ref},
              {debug, St#child.debug},
              {url, St#child.url},
              {method, St#child.method},
              {params, make_params(St)},
              {timeout, Time}
             ],
    smoke_test_misc:do_one_child(St#child.debug,
                                 smoke_test_request_supervisor,
                                 [], Params).

%%-----------------------------------------------------------------------------
%%
%% @doc sends job statistic to the handler
%%
send_stat(#child{stat=Stat}) ->
    #stat{count=Count, sum=Sum, sum_sq=Sq} = Stat,
    smoke_test_handler:send_stat(Count, Sum, Sq).

%%-----------------------------------------------------------------------------
make_params(St) ->
    []
    .

%%-----------------------------------------------------------------------------
%%
%% @doc updates stat for the job and removes it from the list of jobs
%%
job_done(#child{jobs=Jobs} = St, Mref) ->
    F = fun(#chi{mon=X}) ->
                X == Mref
        end,
    {Found, Rest} = lists:partition(F, Jobs),
    Stu = update_stat(St, Found),
    Stu#child{jobs=Rest}.

%%-----------------------------------------------------------------------------
%%
%% @doc updates stat for the job
%%
update_stat(St, []) ->
    St;

update_stat(#child{stat=Stat} = St, [J]) ->
    Now = now(),
    Dur = timer:now_diff(Now, J#chi.start) / 1000.0,
    Nstat = smoke_test_misc:update_stat(Stat, Dur),
    St#child{stat=Nstat}.

%%-----------------------------------------------------------------------------
