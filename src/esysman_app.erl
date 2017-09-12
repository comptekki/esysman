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

start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
      {'_', [
			 {"/esysman", main_handler, []},
			 {"/websocket", websocket_handler, []},
			 {"/esysman/logout", redirect_handler, []},
			 {"/static/[...]", cowboy_static, {priv_dir, esysman, "static"}},
			 {"/upload", upload_handler, []}
%			 {'_', cowboy_static, {priv_file, esysman, "index.html"}}
		]}
	]),
	PrivDir = code:priv_dir(esysman),
	{ok, _} = 
		cowboy:start_tls(
		  https,
		  [{port, 8443},
%		   {cacertfile, PrivDir ++ "/ssl/cowboy-ca.crt"},
		   {certfile, PrivDir ++ "/ssl/server.crt"},
		   {keyfile, PrivDir ++ "/ssl/server.key"},
		   {password, ""}],
		  #{env => #{dispatch => Dispatch}
	   }),
	esysman_sup:start_link().

stop(_State) ->
	ok.
