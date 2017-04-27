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

-module(ecom).

-export([start/0, rec_com/0]).

-include("ecom.hrl").

start() ->
    register(rec_com, spawn(ecom, rec_com, [])).

rec_com() ->
    receive
        finished ->
            io:format("finished~n", []);
        {Box, Com, Args} ->
            process_msg(Box, Com, Args),
            rec_com()
		after ?MSG_TIMER ->
%				rpc:multi_server_call(?SERVERS, hanwebs, {comp_name()++?DOMAIN++"/pong",self()}),
%				rpc:multi_server_call(?SERVERS, hanwebs, {comp_name()++?DOMAIN++"/loggedon/"++logged_on(),self()}),
				send_msg(?SERVERS),
				rec_com()
    end.

send_msg([Server|Rest]) ->
	msg_to_consoles(Server, ?CONSOLES),
	send_msg(Rest);
send_msg([]) ->
	[].

msg_to_consoles(Server, [Console|Rest]) ->
	{Console, Server} ! {comp_name()++?DOMAIN++"/pong",self()},
	{Console, Server} ! {comp_name()++?DOMAIN++"/loggedon/"++logged_on(),self()},
	msg_to_consoles(Server, Rest);
msg_to_consoles(_Server, []) ->
	[].

send_msg([Server|Rest], Msg) ->
%	rpc:multi_server_call(?SERVERS, hanwebs, Msg).
	msg_to_consoles(Server, ?CONSOLES, Msg),
	send_msg(Rest, Msg);
send_msg([], _Msg) ->
	[].

msg_to_consoles(Server, [Console|Rest], Msg) ->
	{Console, Server} ! Msg,
	msg_to_consoles(Server, Rest, Msg);
msg_to_consoles(_Server, [], _Msg) ->
	[].

process_msg(Box, Com, Args) ->
	io:format("~nBox: ~p -> Com: ~p -> args: ~p -> pid: ~p~n",[Box, Com, Args, self()]),
	case Com of
		<<"com">> ->
			send_msg(?SERVERS, <<Box/binary,":com <- ",Args/binary>>),
			case Args of
				<<"mkuploads">> ->
					os:cmd("mkdir "++?UPLOADS_DIR),
					case ?PLATFORM of
						_ ->
							os:cmd("chmod 700 "++?UPLOADS_DIR);
						"w" -> ok
					end,
					send_msg(?SERVERS, <<Box/binary,(list_to_binary(":mkdir "++?UPLOADS_DIR))/binary>>);
				<<"anycmd">> ->
					Res = 
						case ?PLATFORM of
							"w" -> 
								list_to_binary(os:cmd(?UPLOADS_DIR++"any.cmd"));
							_ ->
								list_to_binary(os:cmd("sh "++?UPLOADS_DIR++"any.cmd"))
						end,
					send_msg(?SERVERS, <<Box/binary,":anycmd - Results -> ", Res/binary>>);
				<<"listupfls">> ->
					send_msg(?SERVERS, <<Box/binary, (list_to_binary(":listupfls:<br>"++list_up_fls()))/binary>>);
				<<"ninitecmd">> ->
					case ?PLATFORM of
						"w" ->
							os:cmd(?UPLOADS_DIR++"ninite.cmd"),
							send_msg(?SERVERS, <<Box/binary,":ninitecmd">>);
						_ ->
							send_msg(?SERVERS,  <<Box/binary,":error - no function on this platform...">>)
					end;
				<<"ninite">> ->
					case ?PLATFORM of
						"w" ->
							Date=get_date(),
							os:cmd(?UPLOADS_DIR++"NiniteOne.exe /updateonly /exclude Python  /disableshortcuts /silent "++?UPLOADS_DIR++"ninite_"++Date++"_log.txt"),
							send_msg(?SERVERS, <<Box/binary, (list_to_binary(":ninite date -> "++Date))/binary>>);
						_ ->
							send_msg(?SERVERS, <<Box/binary,":error - no function on this platform...">>)
					end;
				<<"adobe-update">> ->
					case ?PLATFORM of
						"w" ->
							Date=get_date(),
							os:cmd(?UPLOADS_DIR++"remoteupdatemanager.exe"),
							send_msg(?SERVERS, <<Box/binary, (list_to_binary(":adobe-update date -> "++Date))/binary>>);
						_ ->
							send_msg(?SERVERS, <<Box/binary,":error - no function on this platform...">>)
					end;
				<<"ninitelog">> ->
					case ?PLATFORM of
						"w" ->
							{ok,Files}=file:list_dir(?UPLOADS_DIR),
							Log=get_files(Files,n),
							case size(Log) of
								0 ->
									send_msg(?SERVERS, <<Box/binary,":no ninite logs">>);
								_ ->
									send_msg(?SERVERS, <<Box/binary,":ninitemlog -> ",Log/binary>>)
							end;
						_ ->
							send_msg(?SERVERS, <<Box/binary,":error - no function on this platform...">>)
					end;
				<<"unamea">> ->
					case ?PLATFORM of
						"x" ->
							Res = os:cmd("/bin/uname -a"),
							send_msg(?SERVERS, <<Box/binary,(list_to_binary(":unamea -> done..."))/binary, (fix_log(Res))/binary>>);
						_ ->
							send_msg(?SERVERS, <<Box/binary,":error - no function on this platform...">>)
					end;
				<<"lsbra">> ->
					case ?PLATFORM of
						"x" ->
							Res = os:cmd("/usr/bin/lsb_release -a"),
							send_msg(?SERVERS, <<Box/binary,":ubuntuver -> done...", (fix_log(Res))/binary>>);
						_ ->
							send_msg(?SERVERS, <<Box/binary,":error - no function on this platform...">>)
					end;
				<<"aptcheck">> ->
					case ?PLATFORM of
						"x" ->
							Res = os:cmd("/usr/bin/apt update; /usr/lib/update-notifier/apt-check --human-readable"),
							send_msg(?SERVERS, <<Box/binary, ":aptcheck -> done...", (fix_log(Res))/binary>>);
						_ ->
							send_msg(?SERVERS, <<Box/binary, ":error - no function on this platform...">>)
					end;
				<<"aptlistupgradable">> ->
					case ?PLATFORM of
						"x" ->
							Res = os:cmd("/usr/bin/apt update; /usr/bin/apt list --upgradable"),
							send_msg(?SERVERS, <<Box/binary, ":aptlistupgradable -> done...", (fix_log(Res))/binary>>);
						_ ->
							send_msg(?SERVERS, <<Box/binary, ":error - no function on this platform...">>)
					end;
				<<"aptupgrade">> ->
					case ?PLATFORM of
						"x" ->
							os:cmd("/usr/bin/apt-get update >"++?UPLOADS_DIR++"aptu_log.txt;/usr/bin/apt-get -y upgrade >>"++?UPLOADS_DIR++"aptu_log.txt"),
							send_msg(?SERVERS, <<Box/binary,(list_to_binary(":aptupgrade -> done..."))/binary>>);
						_ ->
							send_msg(?SERVERS, <<Box/binary,":error - no function on this platform...">>)
					end;
				<<"aptdistupgrade">> ->
					case ?PLATFORM of
						"x" ->
							os:cmd("/usr/bin/apt-get update >"++?UPLOADS_DIR++"aptu_log.txt;/usr/bin/apt-get -y dist-upgrade >>"++?UPLOADS_DIR++"aptu_log.txt"),
							send_msg(?SERVERS, <<Box/binary,(list_to_binary(":aptdistupgrade -> done..."))/binary>>);
						_ ->
							send_msg(?SERVERS, <<Box/binary,":error - no function on this platform...">>)
					end;
				<<"aptupgrade-list">> ->
					case ?PLATFORM of
						"x" ->
							os:cmd("/usr/bin/apt-get update >"++?UPLOADS_DIR++"aptu_log.txt;/usr/bin/apt-get -s upgrade >>"++?UPLOADS_DIR++"aptu_log.txt"),
							send_msg(?SERVERS, <<Box/binary,(list_to_binary(":aptupgrade-list -> done..."))/binary>>);
						_ ->
							send_msg(?SERVERS, <<Box/binary,":error - no function on this platform...">>)
					end;
				<<"aptdistupgrade-list">> ->
					case ?PLATFORM of
						"x" ->
							os:cmd("/usr/bin/apt-get update >"++?UPLOADS_DIR++"aptu_log.txt;/usr/bin/apt-get -s dist-upgrade >>"++?UPLOADS_DIR++"aptu_log.txt"),
							send_msg(?SERVERS, <<Box/binary,(list_to_binary(":aptdistupgrade-list -> done..."))/binary>>);
						_ ->
							send_msg(?SERVERS, <<Box/binary,":error - no function on this platform...">>)
					end;
				<<"aptulog">> ->
					case ?PLATFORM of
						"x" ->
							{ok,Files}=file:list_dir(?UPLOADS_DIR),
							Log=get_files(Files,a),
							case size(Log) of
								0 ->
									send_msg(?SERVERS, <<Box/binary,":no apt update log ">>);
								_ ->
									send_msg(?SERVERS, <<Box/binary,":apt-update-log -> ",Log/binary>>)
							end;
						_ ->
							send_msg(?SERVERS, <<Box/binary,":error - no function on this platform...">>)
					end;
				<<"yumcheck">> ->
					case ?PLATFORM of
						"x" ->
							Res = os:cmd("/usr/bin/yum check-update"),
							send_msg(?SERVERS, <<Box/binary,":yumcheck -> done...", (fix_log(Res))/binary>>);
						_ ->
							send_msg(?SERVERS, <<Box/binary,":error - no function on this platform...">>)
					end;
				<<"yumupdate">> ->
					case ?PLATFORM of
						"x" ->
							Res = os:cmd("/usr/bin/yum -y update"),
							send_msg(?SERVERS, <<Box/binary,":yumupdate -> done...", (fix_log(Res))/binary>>);
						_ ->
							send_msg(?SERVERS, <<Box/binary,":error - no function on this platform...">>)
					end;
				<<"dnfcheck">> ->
					case ?PLATFORM of
						"x" ->
							Res = os:cmd("/usr/bin/dnf -y update"),
							send_msg(?SERVERS, <<Box/binary,":dnfupdate -> done...", (fix_log(Res))/binary>>);
						_ ->
							send_msg(?SERVERS, <<Box/binary,":error - no function on this platform...">>)
					end;
				<<"dnfupdate">> ->
					case ?PLATFORM of
						"x" ->
							Res = os:cmd("/usr/bin/yum -y update"),
							send_msg(?SERVERS, <<Box/binary,":dnfupdate -> done...", (fix_log(Res))/binary>>);
						_ ->
							send_msg(?SERVERS, <<Box/binary,":error - no function on this platform...">>)
					end;
				<<"osxsupdate">> ->
					case ?PLATFORM of
						"m" ->
							os:cmd("/usr/sbin/softwareupdate -ia >/tmp/uploads/osxsu_log.txt"),
							send_msg(?SERVERS, <<Box/binary,(list_to_binary(":osxupdate -> "))/binary>>);
						_ ->
							send_msg(?SERVERS, <<Box/binary,":error - no function on this platform...">>)
					end;
				<<"osxsulog">> ->
					case ?PLATFORM of
						"m" ->
							{ok,Files}=file:list_dir(?UPLOADS_DIR),
							Log=get_files(Files,m),
							case size(Log) of
								0 ->
									send_msg(?SERVERS, <<Box/binary,":no osx softwareupdate log ">>);
								_ ->
									send_msg(?SERVERS, <<Box/binary,":osx-softwareupdate-log -> ",Log/binary>>)
							end;
						_ ->
							send_msg(?SERVERS, <<Box/binary,":error - no function on this platform...">>)
					end;
				<<"wuinstall">> ->
					case ?PLATFORM of
						"w" ->
							Date=get_date(),
							os:cmd(?UPLOADS_DIR++"wuinstall.exe /install /criteria \"IsInstalled=0 and Type='Software'\" >"++?UPLOADS_DIR++"wui_"++Date++"_log.txt"),
							send_msg(?SERVERS, <<Box/binary,(list_to_binary(":wuinstall date -> "++Date))/binary>>);
						_ ->
							send_msg(?SERVERS, <<Box/binary,":error - no function on this platform...">>)
					end;
				<<"wuilog">> ->
					case ?PLATFORM of
						"w" ->
							{ok,Files}=file:list_dir(?UPLOADS_DIR),
							Log=get_files(Files,w),
							case size(Log) of
								0 ->
									send_msg(?SERVERS, <<Box/binary,":no wui logs">>);
								_ ->
									send_msg(?SERVERS, <<Box/binary,":wuilog -> ",Log/binary>>)
							end;
						_ ->
							send_msg(?SERVERS, <<Box/binary,":error - no function on this platform...">>)
					end;
				<<?WOLNAME>> ->
					case ?PLATFORM of
						"x" ->
												% list of mac addresses in different subnet
							wol(?WOLLIST),
							io:format("~n done wol - ~p ~n",[Box]),
							send_msg(?SERVERS, <<Box/binary,":wol -> ">>);
						_ ->
							send_msg(?SERVERS, <<Box/binary,":error - no function on this platform...">>)
					end;
				Unsupported -> Unsupported
			end;
		<<"loggedon">> ->
			send_msg(?SERVERS, <<Box/binary,(list_to_binary(":loggedon:"++logged_on()))/binary>>);
		<<"copy">> ->
			{FileName, Data} = Args,
			{ok, File} =
				case FileName of
					<<"ecom.beam">> ->
						file:open(<<(list_to_binary(?ERL_DIR))/binary,FileName/binary>>, [write]); 
					_ ->
						file:open(<<(list_to_binary(?UPLOADS_DIR))/binary,FileName/binary>>, [write])
				end,
			file:write(File,Data), 
			file:close(File),
			send_msg(?SERVERS, <<Box/binary,":copied ",FileName/binary>>);
		<<"dffreeze">> ->
			case ?PLATFORM of
				"w" ->
					send_msg(?SERVERS, <<Box/binary,":dffreeze">>),
					os:cmd(?DFC_DIR++" "++?DFC_PASSWD++" /BOOTFROZEN");
				_ ->
					send_msg(?SERVERS, <<Box/binary,":error - no function on this platform...">>)
			end;
		<<"dfthaw">> ->
			case ?PLATFORM of
				"w" ->
					send_msg(?SERVERS, <<Box/binary,":dfthaw">>),
					os:cmd(?DFC_DIR++" "++?DFC_PASSWD++" /BOOTTHAWED");
				_ ->
					send_msg(?SERVERS, <<Box/binary,":error - no function on this platform...">>)
			end;
		<<"dfstatus">> ->
			case ?PLATFORM of
				"w" ->
					Output=os:cmd(?ERL_DIR++"df-status.cmd"),
					send_msg(?SERVERS, <<Box/binary,(list_to_binary(":dfstatus:"++string:left(Output,length(Output)-2)))/binary>>);
				_ ->
					send_msg(?SERVERS, <<Box/binary,":error - no function on this platform...">>)
			end;
		<<"ping">> ->
			send_msg(?SERVERS, <<Box/binary,":pong">>);
		<<"net_stop">> ->
			init:stop(),
			send_msg(?SERVERS, <<Box/binary,":net_stop">>);
		<<"net_restart">> ->
			init:restart(),
			send_msg(?SERVERS, <<Box/binary,":net_restart">>);
		<<"reboot">> ->
			send_msg(?SERVERS, <<Box/binary, ":reboot">>),
			case ?PLATFORM of
				"w" ->
					os:cmd("shutdown -r -t 0");
				_ -> 
					os:cmd("shutdown -r now")
			end;
		<<"shutdown">> ->
			send_msg(?SERVERS, <<Box/binary,":shutdown">>),
			case ?PLATFORM of
				"w" ->
					os:cmd("shutdown -s -t 0");
				_ ->
					os:cmd("shutdown -h now")
			end;
		_ ->
			send_msg(?SERVERS, <<"Unknown command: '",Com/binary,"'">>)
	end.

wol([MacAddr|Macs]) ->
	MacAddrBin= <<<<(list_to_integer(X, 16))>> || X <- string:tokens(MacAddr,"-")>>,
	MagicPacket= << (dup(<<16#FF>>, 6))/binary, (dup(MacAddrBin, 16))/binary >>,
	{ok,S} = gen_udp:open(0, [{broadcast, true}]),
	gen_udp:send(S, ?BROADCAST_ADDR, 9, MagicPacket),
	gen_udp:close(S),
	wol(Macs);
wol([]) ->
	[].

dup(B,Acc) when Acc > 1 ->	
    B2=dup(B, Acc-1),
	<< B/binary,  B2/binary >>;
dup(B,1) ->
    B.

get_files([File|Rest],T) ->
	case string:str(File,"log") of
		0 ->
			get_files(Rest,T);		
		_ ->
			case T of
				a ->
					case string:str(File,"apt") of
						0 ->
							get_files(Rest,T);
						_ ->
							{ok,Log}=file:read_file(?UPLOADS_DIR++File),
							fix_log(Log)
					end;
				n ->
					case string:str(File,"ninite") of
						0 ->
							get_files(Rest,T);
						_ ->
							{ok,Log}=file:read_file(?UPLOADS_DIR++File),
							fix_log(Log)
					end;
				m ->
					case string:str(File,"osxsu") of
						0 ->
							get_files(Rest,T);
						_ ->
							{ok,Log}=file:read_file(?UPLOADS_DIR++File),
							fix_log(Log)
					end;
				w ->
					case string:str(File,"wui") of
						0 ->
							get_files(Rest,T);
						_ ->
							{ok,Log}=file:read_file(?UPLOADS_DIR++File),
							fix_log(Log)
					end
			end
	end;
get_files([],_T) ->
	<<>>.

fix_log(PreLog) ->
	PostLog = case is_binary(PreLog) of
			  false ->
				  list_to_binary(PreLog);
			  _ ->
				  PreLog
		  end,
	<<"<br><br>----------------------------------------<br>",
	  (binary:replace(binary:replace(binary:replace(PostLog, <<":">>, <<"-">>, [global]), <<"\n">>, <<"<br>">>, [global]), <<"\r">>, <<"">>, [global]))/binary,
	  "<br>----------------------------------------<br>">>.

get_date() ->
	{Year,Month,Day}=date(),
	{Hour,Min,Sec}=time(),
	lists:flatten(io_lib:format("~p~2..0B~2..0B_~2..0B~2..0B~2..0B",[Year,Month,Day,Hour,Min,Sec])).

logged_on() ->
	case ?PLATFORM of
		"w" ->
			case file:list_dir(?USERS_DIR) of
				{ok, UserDirs} -> get_user(UserDirs);
				{error, Reason} -> atom_to_list(Reason)
			end;
		_ ->
			get_user(string:tokens(os:cmd("who"),"\n"))
	end.

get_user([UserInfo|Rest]) ->
	case ?PLATFORM of
		"w" ->
			case lists:member(UserInfo,?USERS) of
				true ->
					case Rest of
						[] ->
							[];
						_ ->
							get_user(Rest)
					end;
				_ ->
					case Rest of
						[] ->
							UserInfo;
						_ ->
							UserInfo++
								case get_user(Rest) of
									[] ->
										get_user(Rest);
									_ ->
										"|"++get_user(Rest)
								end
					end
			end;
		_ ->
			User =
				case string:tokens(UserInfo, " ")  of
					[Usert,_,_,_] ->
						Usert;
					[Usert,_,_,_,_] ->
						Usert;
					[Usert,_,_,_,_,_] ->
						Usert
				end,
			case Rest of
				[] ->
					User;
				_ ->
					User++
					case get_user(Rest) of
						[] ->
							get_user(Rest);
						_ ->
							"|"++get_user(Rest)
					end

			end
	end;
get_user([]) ->
	"".

comp_name() ->
	case ?PLATFORM of
		"w" ->
			[NBName, _] = string:tokens(os:cmd("echo %computername%"), "\r"),
			?NODE_NAME ++ string:to_lower(NBName);
		_ ->
			[Hostname]=string:tokens(os:cmd("hostname -s"), "\n"),
			?NODE_NAME ++ Hostname
	end.

list_up_fls() ->
	{ok, Files}=file:list_dir(?UPLOADS_DIR),
	[ X++"<br>" || X <- Files].


