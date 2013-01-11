%% Copyright (c) 2012, Wes James <comptekki@gmail.com>
%% All rights reserved.
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

-module(esysman_app).
-behaviour(application).

%% API.

-export([start/2, stop/1]).

%% API.

start(_Type, Args) ->
	[HTTP_Port,HTTPS_Port]=
		case Args of
			[] -> [8080,8443];
			Any -> Any
		end,
	Dispatch = [
		{'_', [
			{[<<"esysman">>], websocket_handler, []},
			{[<<"esysman">>,<<"logout">>], redirect_handler, []},
			{[<<"static">>,'...'], cowboy_static,[{directory, "static/"}]},
			{[<<"/">>,'...'], cowboy_static,[{directory, "slash/"}]},
			{'_', default_handler, []}
		]}
	],
	{ok, _} = 
		cowboy:start_http(
		  http,
		  100,
		  [{port, HTTP_Port}],
		  [{env, [{dispatch, Dispatch}]}]
		 ),
	{ok, _} = 
		cowboy:start_https(
		  https,
		  100,
		  [{port, HTTPS_Port},
		   {certfile, "priv/ssl/cert.pem"},
		   {keyfile, "priv/ssl/key.pem"},
		   {password, ""}],
		  [{env, [{dispatch, Dispatch}]}]
		 ),
	esysman_sup:start_link().

stop(_State) ->
	ok.
