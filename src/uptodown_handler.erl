%% Copyright (c) 2012, Wes James <comptekki@gmail.com>
%% All rights reserve.
%% 
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%% 
%%     * Redistributions of source code must retain the above copyright
%%       notice, this list of conditions and the following disclaimer.
%%     * Redistributions in binary form must reproduce the above copyright
%%       notice, this list of conditions and the following disclaimer in the
%%       documentation and/or other materials provided with the distribution.
%%     * Neither the name of "ESysMan" nor the names of its contributors may be
%%       used to endorse or promote products derived from this software without
%%       specific prior written permission.
%% 
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
%% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
%% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.
%% 
%%

-module(uptodown_handler).

-export([init/2]).

-include("esysman.hrl").

init(Req, Opts) ->
    {{Ipprt1,Ipprt2,Ipprt3,Ipprt4}, _} = maps:get(peer, Req, {}),
    [Host|_] = ?SERVERS,
    Peer = list_to_atom(?NODENAME ++ "@" ++ integer_to_list(Ipprt1) ++ "." ++ integer_to_list(Ipprt2) ++ "." ++ integer_to_list(Ipprt3) ++ "." ++ integer_to_list(Ipprt4)),
    Req4 =
	case Peer of
	    Host ->
		{ok, Headers, Req2} = cowboy_req:read_part(Req),
		{file, <<"inputfile">>, Filename, ContentType}
		    = cow_multipart:form_data(Headers),
		case file:delete(<<(?DOWNLOADS)/binary,Filename/binary>>) of
		    ok -> "";
		    {error, _} -> ""
		end,
		{ok, Req3} = body_to_console(Req2, Filename), 
		{{Year, Month, Day}, {Hour, Minute, Second}} = calendar:local_time(),
		Date = lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",[Year,Month,Day,Hour,Minute,Second])),
		io:format("date: ~p -> uptodown -> Received file ~p of content-type ~p~n~n", [Date, Filename, ContentType]),
		Req3;
	    _ ->
		io:format("~n~nBlocked uptodown IP: ~p~n", [Peer]),
		Req 
	end,
    {ok, Req4, Opts}.

body_to_console(Req, Filename) ->
    case cowboy_req:read_part_body(Req) of
        {ok, Data, Req2} ->
			case file:write_file(<<(?DOWNLOADS)/binary,Filename/binary>>, Data, [append]) of
				ok ->
					Res = <<"ok">>,
					Res;
				{error, Res} ->
					Res
			end,
			{ok, Req2};
		{more, Data, Req2} ->
			case file:write_file(<<(?DOWNLOADS)/binary,Filename/binary>>, Data, [append]) of
				ok ->
					body_to_console(Req2, Filename),
					Res = <<"ok">>,
					Res;
				{error, Res} ->
					Res
			end,
            {ok, Req2}
	end.
