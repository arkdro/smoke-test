%%%
%%% smoke_test_req: async request created by a dynamically added worker
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
%%% @since 2012-02-07 18:24
%%% @license MIT
%%% @doc async request that is created by a dynamically added worker
%%%

-module(smoke_test_req).
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

-include("smoke_test.hrl").

-define(TC, 1).

%%%----------------------------------------------------------------------------
%%% gen_server callbacks
%%%----------------------------------------------------------------------------
init(Params) ->
    C = prepare_all(Params),
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
%% @since 2012-02-07 14:42
%%
-spec periodic_check(#child{}) -> #child{}.

periodic_check(#child{cnt=C} = State) when C =< 0 ->
    gen_server:cast(self(), stop),
    State;

periodic_check(#child{cnt=Cnt, hz=Hz, timer=Ref} = State) ->
    mpln_misc_run:cancel_timer(Ref),
    New = add_job(State),
    Nref = erlang:send_after(trunc(1 + 1000/Hz), self(), periodic_check),
    New#child{cnt=Cnt-1, timer=Nref}.

%%-----------------------------------------------------------------------------
add_job(St) ->
    St
    .

%%-----------------------------------------------------------------------------
