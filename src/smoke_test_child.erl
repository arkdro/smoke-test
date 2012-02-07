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

-include("smoke_test.hrl").

-define(TC, 1).

%%%----------------------------------------------------------------------------
%%% gen_server callbacks
%%%----------------------------------------------------------------------------
init(Params) ->
    C = prepare_all(Params),
    mpln_p_debug:pr({?MODULE, 'init', ?LINE, C#child.id, self(), Params, C},
        C#child.debug, config, 4),
    mpln_p_debug:pr({?MODULE, 'init done', ?LINE, C#child.id, self()},
        C#child.debug, run, 2),
    {ok, C, ?TC}. % to immediate fall into timeout

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
    mpln_p_debug:pr({?MODULE, 'other', ?LINE, _N, St#child.id, self()},
        St#child.debug, run, 2),
    New = main_action(St),
    {reply, {error, unknown_request}, New}.

%%-----------------------------------------------------------------------------
%%
%% Handling cast messages
%% @since 2012-02-07 14:42
%%
-spec handle_cast(any(), #child{}) -> any().

handle_cast(stop, St) ->
    {stop, normal, St};

handle_cast(_, St) ->
    New = main_action(St),
    {noreply, New, ?TC}.

%%-----------------------------------------------------------------------------
terminate(_, #child{id=Id} = State) ->
    mpln_p_debug:pr({?MODULE, terminate, ?LINE, Id, self()},
        State#child.debug, run, 2),
    ok.

%%-----------------------------------------------------------------------------
%%
%% Handling all non call/cast messages
%%
-spec handle_info(any(), #child{}) -> any().

handle_info(timeout, State) ->
    mpln_p_debug:pr({?MODULE, info_timeout, ?LINE, State#child.id, self()},
        State#child.debug, run, 6),
    New = main_action(State),
    {noreply, New, ?TC};

handle_info(_Req, State) ->
    mpln_p_debug:pr({?MODULE, other, ?LINE, _Req, State#child.id, self()},
        State#child.debug, run, 2),
    New = main_action(State),
    {noreply, New, ?TC}.

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
%% @doc processes command, then sends stop message to itself
%% @since 2012-02-07 14:42
%%
-spec main_action(#child{}) -> #child{}.

main_action(State) ->
    State.

%%-----------------------------------------------------------------------------
%%
%% @doc prepares necessary things
%%
-spec prepare_all(list()) -> #child{}.

prepare_all(L) ->
    #child{
          id = proplists:get_value(id, L),
          debug = proplists:get_value(debug, L, []),
          url = proplists:get_value(url, L),
          hz = proplists:get_value(hz, L),
          seconds = proplists:get_value(seconds, L)
        }.

%%-----------------------------------------------------------------------------
