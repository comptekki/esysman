%% @doc Upload handler.
%% https://github.com/ninenines/cowboy/ 1.1.2 /examples/upload/src/upload_handler.erl
-module(upload_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("esysman.hrl").

init(_, Req, _Opts) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{ok, Headers, Req2} = cowboy_req:part(Req),
	{file, <<"inputfile">>, Filename, ContentType, _TE}
		= cow_multipart:form_data(Headers),
	case file:delete(<<(?UPLOADS)/binary,Filename/binary>>) of
		ok -> "";
		{error, _} -> ""
	end,
	{ok, Req3} = body_to_console(Req2, Filename), 
	io:format("Received file ~p of content-type ~p~n~n", [Filename, ContentType]),
	{ok, Req3, State}.

body_to_console(Req, Filename) ->
    case cowboy_req:part_body(Req) of
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
					Res = <<"ok">>,
					Res;
				{error, Res} ->
					Res
			end,
            body_to_console(Req2, Filename),
            {ok, Req2}
    end.

terminate(_Reason, _Req, _State) ->
	ok.


