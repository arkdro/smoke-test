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

handle_info(periodic_check, State) ->
    mpln_p_debug:pr({?MODULE, 'info_periodic_check', ?LINE},
                    State#child.debug, run, 6),
    New = periodic_check(State),
    {noreply, New};

handle_info({'DOWN', Mref, _, _, _}, #child{job=#chi{mon=Mref} = J, stat=Stat}
            = St) ->
    mpln_p_debug:pr({?MODULE, info_down, ?LINE, St#child.id, self()},
        St#child.debug, run, 2),
    Now = now(),
    Dur = timer:now_diff(Now, J#chi.start) / 1000.0,
    Nstat = smoke_test_misc:update_stat(Stat, Dur),
    New = St#child{stat=Nstat},
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
          url = proplists:get_value(url, L),
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

periodic_check(#child{cnt=C} = State) when C =< 0 ->
    gen_server:cast(self(), stop),
    State;

periodic_check(#child{cnt=Cnt, hz=Hz, timer=Ref} = State) ->
    mpln_misc_run:cancel_timer(Ref),
    Stp = stop_job(State),
    New = add_job(Stp),
    Nref = erlang:send_after(trunc(1 + 1000/Hz), self(), periodic_check),
    New#child{cnt=Cnt-1, timer=Nref}.

%%-----------------------------------------------------------------------------
%%
%% @doc terminates job if it still works
%%
stop_job(#child{job=#chi{pid=P}} = State) when is_pid(P) ->
    exit(P, kill),
    State;

stop_job(State) ->
    State.

%%-----------------------------------------------------------------------------
%%
%% @doc starts new job and stores job info into state
%%
-spec add_job(#child{}) -> #child{}.

add_job(#child{hz=Hz} = St) ->
    Ref = make_ref(),
    Time = trunc(1 + 1000 / Hz),
    case prepare_one_job(St, Ref, Time) of
        [C] ->
            St#child{job=C};
        _ -> % error
            St#child{job=undefined}
        end.

%%-----------------------------------------------------------------------------
%%
%% @doc creates parameters and starts job process with the parameters
%%
-spec prepare_one_job(#child{}, reference(), non_neg_integer()) -> [#chi{}].

prepare_one_job(St, Ref, Time) ->
    Params = [
              {id, Ref},
              {parent, self()},
              {url, St#child.url},
              {time, Time}
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
