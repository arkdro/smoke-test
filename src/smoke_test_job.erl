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
%%%

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

%%%----------------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------------

add_job(#req{method=Msrc, params=Params, url=Url, timeout=Time, id=Id,
            host=Host, serv_tag=Tag, ses_sn=Sn, ses_base=Sbase} = St) ->
    Hdr = [],
    Full_url = make_full_url(Host, Url, Tag, Sbase, Sn),
    Method = clean_method(Msrc),
    Req = make_req(Method, Full_url, Hdr, Params),
    mpln_p_debug:pr({?MODULE, add_job, ?LINE, Req, Id, self()},
                    St#req.debug, run, 2),
    Res = httpc:request(Method, Req,
        [{timeout, Time}, {connect_timeout, Time}],
        [{body_format, binary}]),
    mpln_p_debug:pr({?MODULE, add_job, ?LINE, Res, Id, self()},
                    St#req.debug, run, 3),
    Res
    .


%%%----------------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------------
make_req(head, Url, Hdr, _Params) ->
    {Url, Hdr};
make_req(get, Url, Hdr, _Params) ->
    {Url, Hdr};
make_req(post, Url, Hdr, Params) ->
    Ctype = ?CTYPE,
    Body = make_body(Params),
    {Url, Hdr, Ctype, Body}.

%%-----------------------------------------------------------------------------
make_body(Pars) ->
    mpln_misc_web:query_string(Pars).

%%-----------------------------------------------------------------------------
clean_method(Src) ->
    Str = mpln_misc_web:make_string(Src),
    clean_method_aux(string:to_lower(Str)).

clean_method_aux("head") -> head;
clean_method_aux("post") -> post;
clean_method_aux(_) ->      get.

%%-----------------------------------------------------------------------------
clean_url([$/ | Rest]) ->
    Rest;
clean_url(Url) ->
    Url.

%%-----------------------------------------------------------------------------
-spec make_full_url(string(), string(), string(), string(),
                    non_neg_integer()) -> string().

make_full_url(Host, Url, Tag, Sbase, Sn) ->
    Snstr = integer_to_list(Sn),
    Server = "000",
    Cu = clean_url(Url),
    Session = string:join([Sbase, "_", Snstr], ""),
    lists:flatten(string:join([Host, Tag, Server, Session, Cu], "/")).

%%-----------------------------------------------------------------------------
