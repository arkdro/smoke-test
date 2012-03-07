%%%
%%% smoke_test_job: runs http request
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
%%% @since 2012-02-08 14:55
%%% @license MIT
%%% @doc does real job, e.g. http request
%%% @todo change all httpc operations from returning values to sending messages

-module(smoke_test_job).

%%%----------------------------------------------------------------------------
%%% Exports
%%%----------------------------------------------------------------------------

-export([add_job/1]).

%%%----------------------------------------------------------------------------
%%% Includes
%%%----------------------------------------------------------------------------

-include("req.hrl").

-define(CTYPE, "application/x-www-form-urlencoded").
-define(SERVER_ID, "000").

%%%----------------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------------

-spec add_job(#req{}) -> ok.

add_job(#req{id=Id} = St) ->
    case open_session(St) of
        {ok, Data} ->
            proceed_session(St, Data);
        {{error, Reason}, _} ->
            mpln_p_debug:pr({?MODULE, add_job, ?LINE, Id, self(), Reason},
                            St#req.debug, run, 0)
    end.

%%%----------------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------------
make_req({head, Url, Hdr, _Params}) ->
    {Url, Hdr};
make_req({get, Url, Hdr, _Params}) ->
    {Url, Hdr};
make_req({post, Url, Hdr, Params}) ->
    Ctype = ?CTYPE,
    Body = make_body(Params),
    {Url, Hdr, Ctype, Body}.

make_req_encode({post, Url, Hdr, Params}) ->
    Ctype = ?CTYPE,
    Body = make_body(Params),
    Encoded = mochijson2:encode([Body]),
    Bin = unicode:characters_to_binary(Encoded),
    {Url, Hdr, Ctype, Bin}.

%%-----------------------------------------------------------------------------
make_body(Pars) ->
    unicode:characters_to_binary(Pars).

%%-----------------------------------------------------------------------------
clean_method(Src) ->
    Str = mpln_misc_web:make_string(Src),
    clean_method_aux(string:to_lower(Str)).

clean_method_aux("head") -> head;
clean_method_aux("get") ->  get;
clean_method_aux(_) ->      post.

%%-----------------------------------------------------------------------------
clean_url([$/ | Rest]) ->
    Rest;
clean_url(Url) ->
    Url.

%%-----------------------------------------------------------------------------
%%
%% @doc create full url based on host, port, uri, service tag, server id,
%% session id
%%
-spec make_full_url(string(), string(), string(), string(),
                    non_neg_integer()) -> string().

make_full_url(Host, Url, Tag, Sbase, Sn) ->
    Snstr = integer_to_list(Sn),
    Server = ?SERVER_ID,
    Cu = clean_url(Url),
    Session = string:join([Sbase, "_", Snstr], ""),
    lists:flatten(string:join([Host, Tag, Server, Session, Cu], "/")).

%%-----------------------------------------------------------------------------
%%
%% @doc open new session
%%
open_session(#req{method=Msrc, url=Url, timeout=Time, id=Id,
            host=Host, serv_tag=Tag, ses_sn=Sn, ses_base=Sbase} = St) ->
    Full_url = make_full_url(Host, Url, Tag, Sbase, Sn),
    Method = clean_method(Msrc),
    Data = {Method, Full_url, [], []},
    Req = make_req(Data),
    mpln_p_debug:pr({?MODULE, open_session, ?LINE, Req, Id, self()},
                    St#req.debug, run, 2),
    Res = httpc:request(Method, Req,
        [{timeout, Time}, {connect_timeout, Time}],
        [{body_format, binary}]),
    mpln_p_debug:pr({?MODULE, open_session, ?LINE, Res, Id, self()},
                    St#req.debug, run, 3),
    {check_open(Res), Data}.

%%-----------------------------------------------------------------------------
%%
%% @doc check for open session result
%%
check_open(Res) ->
    case extract_info(Res) of
        {ok, <<"o\n">>} ->
            ok;
        {ok, _} ->
            {error, other_than_open};
        {error, Reason} ->
            {error, Reason}
    end.

%%-----------------------------------------------------------------------------
%%
%% @doc extract info from http response
%%
extract_info({ok, {_Stline, _Hdr, Info}}) ->
    {ok, Info};

extract_info({ok, {_Stcode, Info}}) ->
    {ok, Info};

extract_info({error, Reason}) ->
    {error, Reason}.

%%-----------------------------------------------------------------------------
%%
%% @doc create a message to send/receive
%%
create_message(#req{id=Id}) ->
    mpln_misc_web:make_term_binary(Id).

%%-----------------------------------------------------------------------------
create_params(Msg) ->
    mochijson2:encode(Msg).

%%-----------------------------------------------------------------------------
%%
%% @doc send a message and go to the waiting loop
%%
proceed_session(#req{timeout=Time, id=Id} = St,
                {Method, Full_url, Hdr, _Params}) ->
    Msg = create_message(St),
    %Params = create_params(Msg),
    Params = Msg,
    Req = make_req_encode({Method, Full_url ++ "_send", Hdr, Params}),
    mpln_p_debug:pr({?MODULE, proceed_session, ?LINE, Id, self(), Req},
                    St#req.debug, run, 2),
    Res = httpc:request(Method, Req,
                        %% version needed here. Otherwise we get
                        %% {error,socket_closed_remotely}
        [{timeout, Time}, {connect_timeout, Time}, {version, "HTTP/1.0"}],
        [{body_format, binary}]),
    mpln_p_debug:pr({?MODULE, proceed_session, ?LINE, Id, self(), Res},
                    St#req.debug, run, 3),
    waiting_response(St, {Method, Full_url, Hdr, []}, Params).

%%-----------------------------------------------------------------------------
waiting_response(#req{id=Id, heartbeat_timeout=Htime, timeout=Time} = St,
                 {Method, _Full_url, _Hdr, Params} = Data, In_data) ->
    Req = make_req(Data),
    mpln_p_debug:pr({?MODULE, waiting_response, ?LINE, Req, Id, self()},
                    St#req.debug, run, 2),
    Res = httpc:request(Method, Req,
        [{timeout, Htime}, {connect_timeout, Time}],
        [{body_format, binary}]),
    mpln_p_debug:pr({?MODULE, waiting_response, ?LINE, Res, Id, self()},
                    St#req.debug, run, 3),
    case extract_info(Res) of
        {ok, <<"h\n">>} ->
            % heartbeat
            mpln_p_debug:pr({?MODULE, 'waiting_response heartbeat', ?LINE,
                             Id, self()},
                            St#req.debug, run, 3),
            waiting_response(St, Data, In_data);
        {ok, Info} ->
            Payload = extract_payload(Info),
            Res_f = find_source(Payload, Params),
            mpln_p_debug:pr({?MODULE, 'waiting_response ok', ?LINE,
                             Id, self(), Res_f, Info, Payload},
                            St#req.debug, run, 4);
        {error, Reason} ->
            mpln_p_debug:pr({?MODULE, 'waiting_response error', ?LINE,
                             Id, self(), Reason}, St#req.debug, run, 0)
    end.

%%-----------------------------------------------------------------------------
extract_payload(<<"a", Rest/binary>>) ->
    mochijson2:decode(Rest);

extract_payload(Data) ->
    Data.

%%-----------------------------------------------------------------------------
find_source(Payload, Params) ->
    false
    .

%%-----------------------------------------------------------------------------
