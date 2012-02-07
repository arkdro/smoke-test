%%%
%%% smoke_test_misc: common functions for spawning children
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
%%% @since 2012-02-07 17:53
%%% @license MIT
%%% @doc common functions for spawning children
%%%

-module(smoke_test_misc).

%%%----------------------------------------------------------------------------
%%% Exports
%%%----------------------------------------------------------------------------

-export([do_one_child/1]).

%%%----------------------------------------------------------------------------
%%% Includes
%%%----------------------------------------------------------------------------

-include("child.hrl").

%%%----------------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------------
%%
%% @doc spawns a child for given supervisor
%%
-spec do_one_child(list(), atom(), [pid()], list()) -> [pid()].

do_one_child(Debug, Sup, Ch, Params) ->
    Res = supervisor:start_child(Sup, [Params]),
    mpln_p_debug:pr({?MODULE, 'do_one_child res', ?LINE, Res},
                    Debug, run, 5),
    case Res of
        {ok, Pid} ->
            add_child(Ch, Pid, Ref);
        {ok, Pid, _Info} ->
            add_child(Ch, Pid, Ref);
        _ ->
            mpln_p_debug:pr({?MODULE, 'do_one_child res', ?LINE, 'error',
                             Params, Res}, Debug, run, 1),
            Ch
    end.

%%%----------------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------------
%%
%% @doc stores new process data in a list
%%
-spec add_child([pid()], pid(), reference()) -> [pid()].

add_child(Children, Pid, Id) ->
    Mref = erlang:monitor(process, Pid),
    Ch = #chi{pid = Pid, id = Id, start = now(), mon=Mref},
    [Ch | Children].

%%-----------------------------------------------------------------------------
