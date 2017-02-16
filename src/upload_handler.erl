%% @doc Upload handler.
%% https://github.com/ninenines/cowboy/blob/master/examples/upload/src/upload_handler.erl
-module(upload_handler).

-export([init/2]).

init(Req, Opts) ->
	
	{ok, Headers, Req2} = cowboy_req:read_part(Req),
	{ok, Data, Req3} = cowboy_req:read_part_body(Req2),
	{file, <<"inputfile">>, Filename, ContentType, _TE}
		= cow_multipart:form_data(Headers),
%	io:format("Received file ~p of content-type ~p as follow:~n~p~n~n", [Filename, ContentType, Data]),
	io:format("Received file ~p of content-type ~p~n", [Filename, ContentType]),
	{ok, Req3, Opts}.
