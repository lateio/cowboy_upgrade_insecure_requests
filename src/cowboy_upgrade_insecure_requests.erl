-module(cowboy_upgrade_insecure_requests).

-vsn(0).

%% API exports
-behavior(cowboy_middleware).
-export([execute/2]).


% ------------------------------------------------------------------------------
%
% Copyright (c) 2018, Lauri Moisio <l@arv.io>
%
% The MIT License
%
% Permission is hereby granted, free of charge, to any person obtaining a copy
% of this software and associated documentation files (the "Software"), to deal
% in the Software without restriction, including without limitation the rights
% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
% copies of the Software, and to permit persons to whom the Software is
% furnished to do so, subject to the following conditions:
%
% The above copyright notice and this permission notice shall be included in
% all copies or substantial portions of the Software.
%
% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
% THE SOFTWARE.
%
% ------------------------------------------------------------------------------

-type mfargs() :: {Module :: module(), Function :: atom(), Args :: [term()]}.
-type upgrade_insecure_requests_opts() ::
    #{
        'host'     => iodata()  | mfargs(),
        'port'     => undefined | inet:port_number() | mfargs(),
        'path'     => iodata()  | mfargs(),
        'qs'       => iodata()  | mfargs(),
        'fragment' => iodata()  | mfargs(),
        'reply_status' => 301 | 302 | 303 | 307 | 308 | mfargs()
    }.


execute(Req = #{scheme := <<"http">>}, Env) ->
    case cowboy_req:header(<<"upgrade-insecure-requests">>, Req) of
        undefined -> {ok, Req, Env};
        <<$1>> -> redirect(Req, Env)
    end;
execute(Req = #{scheme := <<"https">>}, Env) ->
    {ok, Req, Env}.


redirect(Req, Env) ->
    Uri = cowboy_req:uri(Req, to_uri_opts(Req, Env)),
    Headers = #{
        <<"vary">> => <<"upgrade-insecure-requests">>,
        <<"location">> => Uri
    },
    {stop, cowboy_req:reply(reply_status(Req, Env), Headers, Req)}.


to_uri_opts(Req, Env = #{'Upgrade-Insecure-Requests' := EnvOpts}) ->
    Opts = lists:foldl(
        fun (Key, Map) ->
            case maps:get(Key, EnvOpts, undefined) of
                {M,F,A} ->
                    case apply(M,F,[Req, Env|A]) of
                        undefined -> Map;
                        Value -> Map#{Key => Value}
                    end;
                undefined -> Map;
                Value -> Map#{Key => Value}
            end
        end, #{port => undefined}, [host, port, path, qs, fragment]),
    Opts#{scheme => <<"https">>};
to_uri_opts(_, #{}) ->
    #{scheme => <<"https">>, port => undefined}.


reply_status(Req, Env = #{'Upgrade-Insecure-Requests' := EnvOpts}) ->
    case maps:get(reply_status, EnvOpts, 307) of
        {M,F,A} ->
            case apply(M,F,[Req, Env|A]) of
                Tuple = {_, _, _} ->
                    logger:error("upgrade_insecure_requests applying {M,F,A} in reply_status returned another tuple (~p). Infinite loop?", [Tuple]),
                    307;
                Status -> reply_status(Req, Env#{'Upgrade-Insecure-Requests' => EnvOpts#{reply_status => Status}})
            end;
        301 -> 301;
        302 -> 302;
        303 -> 303;
        307 -> 307;
        308 -> 308;
        Code ->
            logger:error("upgrade_insecure_requests middleware can only return a subset of 300-status codes. ~p is not one of them", [Code]),
            307
    end;
reply_status(_, _) ->
    307.
