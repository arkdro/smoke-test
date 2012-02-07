%%%
%%% smoke_test_handler: gen_server that just starts a bunch of tests
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
%%% @since 2012-02-06 18:30
%%% @license MIT
%%% @doc a gen_server that starts tests upon a request
%%%

-module(smoke_test_handler).
-behaviour(gen_server).

%%%----------------------------------------------------------------------------
%%% Exports
%%%----------------------------------------------------------------------------

-export([start/0, start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([terminate/2, code_change/3]).

-export([stat_q/0, st/0]).

%%%----------------------------------------------------------------------------
%%% Includes
%%%----------------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("smoke_test.hrl").

%%%----------------------------------------------------------------------------
%%% gen_server callbacks
%%%----------------------------------------------------------------------------
init(_) ->
    St = prepare_all(),
    mpln_p_debug:pr({?MODULE, 'init done', ?LINE}, St#sth.debug, run, 1),
    {ok, St}.
%%-----------------------------------------------------------------------------
%%
%% Handling call messages
%% @since 2012-02-06 18:30
%%
-spec handle_call(any(), any(), #sth{}) ->
    {reply , any(), #sth{}}
    | {stop, normal, ok, #sth{}}.

handle_call(run_smoke_test, _From, St) ->
    mpln_p_debug:pr({?MODULE, 'run_smoke_test', ?LINE}, St#sth.debug, run, 2),
    Res = run_smoke_test(St),
    {reply, Res, St};

handle_call(stop, _From, St) ->
    {stop, normal, ok, St};

handle_call(status, _From, St) ->
    {reply, St, St};

handle_call(_N, _From, St) ->
    mpln_p_debug:pr({?MODULE, 'other', ?LINE, _N}, St#sth.debug, run, 2),
    {reply, {error, unknown_request}, St}.

%%-----------------------------------------------------------------------------
%%
%% Handling cast messages
%% @since 2012-02-06 18:30
%%
-spec handle_cast(any(), #sth{}) -> any().

handle_cast(stop, St) ->
    {stop, normal, St};

handle_cast(_N, St) ->
    mpln_p_debug:pr({?MODULE, 'cast other', ?LINE, _N}, St#sth.debug, run, 2),
    {noreply, St}.

%%-----------------------------------------------------------------------------
%%
%% @doc Note: it won't be called unless trap_exit is set
%%
terminate(_, State) ->
    mpln_p_debug:pr({?MODULE, 'terminate', ?LINE}, State#sth.debug, run, 1),
    ok.

%%-----------------------------------------------------------------------------
%%
%% Handling all non call/cast messages
%% @since 2012-02-06 18:30
%%
-spec handle_info(any(), #sth{}) -> {noreply, #sth{}}.

handle_info({'DOWN', Mref, _, _Oid, _Info} = Msg, St) ->
    mpln_p_debug:pr({?MODULE, 'info_down', ?LINE, Msg}, St#sth.debug, run, 2),
    New = clean_child(St, Mref),
    {noreply, New};

handle_info(_Req, State) ->
    mpln_p_debug:pr({?MODULE, 'info_other', ?LINE, _Req},
                    State#sth.debug, run, 2),
    {noreply, State}.

%%-----------------------------------------------------------------------------
code_change(_Old_vsn, State, _Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------------
-spec start() -> any().
%%
%% @doc starts handler gen_server
%% @since 2012-02-06 18:30
%%
start() ->
    start_link().

%%-----------------------------------------------------------------------------
-spec start_link() -> any().
%%
%% @doc starts handler gen_server with given config
%% @since 2012-02-06 18:30
%%
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%-----------------------------------------------------------------------------
-spec stop() -> any().
%%
%% @doc stops handler gen_server
%% @since 2012-02-06 18:30
%%
stop() ->
    gen_server:call(?MODULE, stop).

%%-----------------------------------------------------------------------------
%%
%% @doc asks smoke_test_handler for state of queues
%%
-spec stat_q() -> string().

stat_q() ->
    gen_server:call(?MODULE, stat_q).

%%-----------------------------------------------------------------------------
%%
%% @doc runs smoke_test
%%
-spec st() -> string().

st() ->
    gen_server:call(?MODULE, run_smoke_test).

%%%----------------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------------
%%
%% @doc prepares necessary things
%%
-spec prepare_all() -> #sth{}.

prepare_all() ->
    L = application:get_all_env('smoke_test'),
    #sth{
          debug = proplists:get_value(debug, L, []),
          url = proplists:get_value(url, L, "http://localhost:8086/echo"),
          hz = proplists:get_value(hz, L, 1),
          count = proplists:get_value(count, L, 500),
          seconds = proplists:get_value(seconds, L, 20)
        }.

%%-----------------------------------------------------------------------------
%%
%% @doc spawns children to do test
%%
-spec run_smoke_test(#sth{}) -> #sth{}.

run_smoke_test(#sth{children = Ch} = St) ->
    F = fun(_, Acc) ->
                do_one_child(St, Acc)
        end,
    Res = lists:foldl(F, [], lists:duplicate(St#sth.count, true)),
    St#sth{children = Res ++ Ch}.

%%-----------------------------------------------------------------------------
%%
%% @doc spawns one child
%%
-spec prepare_one_child(#sth{}, [pid()]) -> [pid()].

prepare_one_child(St, Ch) ->
    Ref = make_ref(),
    Params = [
              {id, Ref},
              {url, St#sth.url},
              {hz, St#sth.hz},
              {seconds, St#sth.seconds}
             ],
    smoke_test_misc:do_one_child(smoke_test_child_supervisor, Ch, Params).

%%-----------------------------------------------------------------------------
%%
%% @doc cleans terminated child info away from a list of children
%%
-spec clean_child(#sth{}, reference()) -> #sth{}.

clean_child(#sth{children = Ch} = St, Mref) ->
    F = fun(#chi{mon=X}) ->
                X == Mref
        end,
    {_Done, Cont} = lists:partition(F, Ch),
    mpln_p_debug:pr({?MODULE, 'clean_child', ?LINE, _Done, Cont},
                    St#sth.debug, run, 4),
    St#sth{children=Cont}.

%%-----------------------------------------------------------------------------