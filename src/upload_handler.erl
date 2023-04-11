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

-module(upload_handler).

-export([init/2]).

-include("esysman.hrl").

init(Req, Opts) ->
    Req4 =
	case fire_wall(Req) of
	    allow ->
		{ok, Headers, Req2} = cowboy_req:read_part(Req),
		{file, <<"inputfile">>, Filename, ContentType}
		    = cow_multipart:form_data(Headers),
		case file:delete(<<(?UPLOADS)/binary,Filename/binary>>) of
		    ok -> "";
		    {error, _} -> ""
		end,
		{ok, Req3} = body_to_console(Req2, Filename), 
		{{Year, Month, Day}, {Hour, Minute, Second}} = calendar:local_time(),
		Date = lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",[Year,Month,Day,Hour,Minute,Second])),
		io:format("~ndate: ~p -> Received file ~p of content-type ~p~n", [Date, Filename, ContentType]),
		Req3;
	    deny ->
		{ok, Req3, _} = fwDenyMessage(Req, Opts),
		Req3
	end,
    {ok, Req4, Opts}.

body_to_console(Req, Filename) ->
    case cowboy_req:read_part_body(Req) of
        {ok, Data, Req2} ->
			case file:write_file(<<(?UPLOADS)/binary,Filename/binary>>, Data, [append]) of
				ok ->
					Res = <<"ok">>,
					Res;
				{error, Res} ->
					Res
			end,
			{ok, Req2};
		{more, Data, Req2} ->
			case file:write_file(<<(?UPLOADS)/binary,Filename/binary>>, Data, [append]) of
				ok ->
					body_to_console(Req2, Filename),
					Res = <<"ok">>,
					Res;
				{error, Res} ->
					Res
			end,
            {ok, Req2}
	end.


fire_wall(Req) ->	
    {PeerAddress, _Port} = cowboy_req:peer(Req),
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:local_time(),
    Date = lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",[Year,Month,Day,Hour,Minute,Second])),
    {ok, [_,{FireWallOnOff,IPAddresses},_,_,_,_]}=file:consult(?CONF),
    case FireWallOnOff of
	on ->
	    case lists:member(PeerAddress,IPAddresses) of
		true ->
		    io:format("~ndate: ~p -> upload - firewall allow -> ~p",[Date, PeerAddress]),
		    allow;
		false ->
		    io:format("~ndate: ~p -> upload - firewall denied -> ~p",[Date, PeerAddress]),
		    deny
	    end;
	off ->
	    allow
    end.

%%

fwDenyMessage(Req, Opts) ->
    Req2 = cowboy_req:reply(
	     200,
	     #{ <<"content-type">> => <<"text/html">> },

<<"<html lang='en'>
<head>
<meta charset='utf-8'>
<title>", ?TITLE, "</title>

<meta Http-Equiv='Cache-Control' Content='no-cache'>
<meta Http-Equiv='Pragma' Content='no-cache'>
<meta Http-Equiv='Expires' Content='0'>
<META HTTP-EQUIV='EXPIRES' CONTENT='Mon, 30 Apr 2012 00:00:01 GMT'>

<link rel='icon' href='/static/favicon.ico' type='image/x-icon' />
<style>
body {background-color:black; color:yellow}
</style>
</head>
<body>
Access Denied!
</body>
</html>">>, Req),
    {ok, Req2, Opts}.
