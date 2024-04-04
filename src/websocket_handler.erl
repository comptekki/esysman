%% Copyright (c) 2012, Wes James <comtekki@gmail.com>
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

-module(websocket_handler).

-export([init/2]).

-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([send_msg/2]).

-export([terminate/3]).

-include("esysman.hrl").
-include("db.hrl").

%%

init(Req, State) ->
    Res =
	case fire_wall(Req) of
	    allow ->
		io:format("~n~nhost ws connect ok....~n"),
%	Opts = #{ compress => true, idle_timeout => 36000000 },
		Opts = #{ idle_timeout => 31200000 },
		{cowboy_websocket, Req, State, Opts};
	    deny ->
		fwDenyMessage(Req, {})
	end,
    Res.

%%

websocket_init(State) ->
    case lists:member(hanwebs, registered()) of
	true -> 
	    case lists:member(hanwebs2, registered()) of
		true -> 
		    case lists:member(hanwebs3, registered()) of
			true -> 
			    register(hanwebs4, self()),
			    ok;
			false ->
			    register(hanwebs3, self())
		    end,
		    ok;
		false ->
		    register(hanwebs2, self())
	    end,
	    ok;
	false ->
	    register(hanwebs, self())
	end,
    update_refresh_timer(1),
    {ok, State, hibernate}.

%%

update_refresh_timer(Arg) ->
    case Arg > 1 of
        true ->
	    {ok, [FTRef]} = file:consult(?TIMERREFFILE),
	    FTRefToTuple = binary_to_term(FTRef),
            timer:cancel(FTRefToTuple);
	_ ->
	    []
    end,
    {ok, TRef} = timer:apply_interval(?REFRESHTIME, websocket_handler, send_msg, [?SERVERS, <<"com - resetrefreshtimer - from ", (pid())/binary>>]),
    file:write_file(?TIMERREFFILE, io_lib:fwrite("~p.", [term_to_binary(TRef)])).


%%

websocket_handle({text, <<"close">>}, State) ->
    {stop, State};
websocket_handle({text, <<"client-connected">>}, State) ->
    {reply, {text, <<"client-connected">> }, State, hibernate};
websocket_handle({text, Msg}, State) ->

    Ldatacrt = binary:split(Msg,<<"^">>,[global]),

    Ldata = 
	case erlang:length(Ldatacrt) > 1 of
	    true ->
		[C1, C2, C3] = Ldatacrt,
		[B1, B2, B3] = binary:split(C1,<<":">>,[global]),
		[B1,B2,<<B3/binary,"^",C2/binary,"^",C3/binary>>];
	    _  ->
		binary:split(Msg,<<":">>,[global])
	end,

    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:local_time(),
    Date = lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",[Year,Month,Day,Hour,Minute,Second])),

    [Box,Com,Args]=Ldata,
%    io:format("~ndate: ~p -> done - sent com ~p - data2: ~p ~n",[Box, Com, Args]),
    Rec_Node=binary_to_atom(<<Box/binary>>,latin1),
    Data3 =
	case Com of
	    <<"com">> ->
	        send_msg(?SERVERS, <<"com - ",Args/binary," - from ", (pid())/binary>>),
	    	%io:format("ldata: ~p~n",[Ldata]),
		Data2=case binary:match(Args,[<<"sqllastentrycmd">>]) of
		  nomatch ->
		    {rec_com, Rec_Node} ! {Box,Com,Args},
		    <<"done - com -> ",Args/binary,"  <- sent to: ",Box/binary>>;
		  _ ->
		    {ok, Db} = pgsql:connect(?DBHOST, ?USERNAME, ?PASSWORD, [{database, ?DB}, {port, ?PORT}]),
		    S = <<"select * from esysman order by atimestamp desc limit 1">>,
		    {ok, _, [{Timestampp, Boxp, Userp, Idp}]} = pgsql:squery(Db, S),
		    <<"done - com -> ",
		    Args/binary,"  <- sent to: ",Box/binary,
		    " -- <br><br>Last record:<br>atimestamp: ",Timestampp/binary,
		    "<br>box: ",Boxp/binary,
		    "<br>user: ",Userp/binary,
		    "<br>id: ",Idp/binary,"<br>">>
                end,
		%Data2= <<"done - com -> ",Args/binary,"  <- sent to: ",Box/binary>>,
		io:format("~ndate: ~p -> done - sent com ~p - data2: ~p ~n",[Date, Box, Data2]),
		Data2;
            <<"loggedon">> ->
		{rec_com, Rec_Node} ! {Box,Com,<<"">>},
		Data2= <<"done - loggedon sent to: ",Box/binary>>,
		io:format("~ndate: ~p -> done - loggedon ~p - data2: ~p ~n",[Date, Box, Data2]),
		Data2;
	    <<"copy">> ->
		send_msg(?SERVERS, <<"copy from ", (pid())/binary>>),
		case file:read_file(<<?UPLOADS/binary,Args/binary>>) of
		    {ok, DataBin} ->
			{rec_com, Rec_Node} ! {Box,Com,{Args,DataBin}},
			io:format("~ndate: ~p -> done - copy - ~p ~n",[Date, Box]),
			<<"done - copy sent to: ",Box/binary>>;
		    {error, Reason} ->
			io:format("~ndate: ~p -> done - copy - ~p - error: ~p~n",[Date, Box, Reason]),
			<<Box/binary,":copy error-",(atom_to_binary(Reason,latin1))/binary>>
		end;
	    <<"dffreeze">> ->
		send_msg(?SERVERS, <<"dffreeze from ", (pid())/binary>>),
		{rec_com, Rec_Node} ! {Box,Com,<<"">>},
		Data2= <<Box/binary,":dffreeze">>,
		io:format("~ndate: ~p -> done dffreeze ~p~n",[Date, Box]),
		Data2;
	    <<"dfthaw">> ->
		send_msg(?SERVERS, <<"dfthaw from ", (pid())/binary>>),
		{rec_com, Rec_Node} ! {Box,Com,<<"">>},
		Data2= <<Box/binary,":dfthaw">>,
		io:format("~ndate: ~p -> done - dfthaw ~p~n",[Date, Box]),
		Data2;
	    <<"dfstatus">> ->
		send_msg(?SERVERS, <<"dfstatus from ", (pid())/binary>>),
		{rec_com, Rec_Node} ! {Box,Com,<<"">>},
		Data2= <<"done - dfstatus sent to: ",Box/binary>>,
		io:format("~ndate: ~p -> done - dfstatus ~p~n",[Date, Box]),
		Data2;
	    <<"net_restart">> ->
		send_msg(?SERVERS, <<"net_restart from ", (pid())/binary>>),
		{rec_com, Rec_Node} ! {Box,Com,<<"">>},
		Data2= <<"done - net_restart sent to: ",Box/binary>>,
		io:format("~ndate: ~p -> done - net_restart ~p~n",[Date, Box]),
		Data2;
	    <<"net_stop">> ->
		send_msg(?SERVERS, <<"net_stop from ", (pid())/binary>>),
		{rec_com, Rec_Node} ! {Box,Com,<<"">>},
		Data2= <<"done - net_stop sent to: ",Box/binary>>,
		io:format("~ndate: ~p -> done - net_stop ~p~n",[Date, Box]),
		Data2;
	    <<"reboot">> ->
		send_msg(?SERVERS, <<"reboot from ", (pid())/binary>>),
		{rec_com, Rec_Node} ! {Box,Com,<<"">>},
		Data2= <<"done - reboot sent to: ",Box/binary>>,
		io:format("~ndate: ~p -> done - reboot ~p~n",[Date, Box]),
		Data2;
	    <<"shutdown">> ->
		send_msg(?SERVERS, <<"shutdown from ", (pid())/binary>>),
		{rec_com, Rec_Node} ! {Box,Com,<<"">>},
		Data2= <<"done - shutdown sent to: ",Box/binary>>,
		io:format("~ndate: ~p -> done - shutdown ~p~n",[Date, Box]),
		Data2;
	    <<"wol">> ->
		MacAddr=binary_to_list(Args),
		MacAddrBin= <<<<(list_to_integer(X, 16))>> || X <- string:tokens(MacAddr,"-")>>,
		MagicPacket= << (dup(<<16#FF>>, 6))/binary, (dup(MacAddrBin, 16))/binary >>,
		{ok,S} = gen_udp:open(0, [{broadcast, true}]),
		gen_udp:send(S, ?BROADCAST_ADDR, 9, MagicPacket),
		gen_udp:close(S),
		send_msg(?SERVERS, <<"wol from ", (pid())/binary>>),
		Data2= <<"done - wol: ",Box/binary,"....!">>,
		io:format("~ndate: ~p -> done - wol - ~p ~n",[Date, Box]),
		Data2;
	    <<"ping">> ->
		send_msg(?SERVERS, <<"ping from ", (pid())/binary>>),
		{rec_com, Rec_Node} ! {Box,Com,<<"">>},
		Data2= <<"done - ping sent to: ",Box/binary>>,
		io:format("~ndate: ~p -> done - ping ~p~n",[Date, Box]),
		Data2;
	    <<"list_dwnlds_dir">> ->
		send_msg(?SERVERS, <<"list_dwnlds_dir from ", (pid())/binary>>),
		Data2= <<Box/binary,":list_dwnlds_dir:",(list_dwnld_fls())/binary>>,
		io:format("~ndate: ~p -> done - list_dwnlds_dir ~p ~n",[Date, Box]),
		Data2;
	    <<"deldwnldsfile">> ->
		_ = file:delete(<<(?DOWNLOADS)/binary,Args/binary>>),	
		io:format("~ndate: ~p ->  done - deleting file/downloads file: ~p ~n",[Date, Args]),
		Data2= <<"done - deleting file/downloads file: ", Args/binary, "....!">>,
		Data2;
	    <<"list_ups_dir">> ->
		send_msg(?SERVERS, <<"list_ups_dir from ", (pid())/binary>>),
		Data2= <<Box/binary,":list_ups_dir:",(list_up_fls(Args))/binary>>,
		io:format("~ndate: ~p -> done - list_ups_dir ~p ~n",[Date, Box]),
		Data2;
	    <<"delscrfile">> ->
		_ = file:delete(<<(?UPLOADS)/binary,Args/binary>>),
		_ = file:delete(<<(?UPLOADS)/binary, "info/", Args/binary, ".info">>),
		io:format("~ndate: ~p ->  done - deleting file/script file: ~p ~n",[Date, Args]),
		Data2= <<"done - deleting file/script file: ", Args/binary, "....!">>,
		Data2;
	    <<"lnscrfile">> ->
		send_msg(?SERVERS, <<"lnscrfile from ", (pid())/binary>>),
		[F1,F2] = binary:split(Args, <<"+">>, [global]),
		case file:make_symlink(<<(?UPLOADS)/binary,F1/binary>>, <<(?UPLOADS)/binary,F2/binary>>) of
		    ok ->
			"";
		    {error, eexist} ->
			case binary:split(F1, <<".">>, [global]) of
			    [_, <<"cmd">>] ->
				file:delete(<<(?UPLOADS)/binary, "any.cmd">>);
			    [_, <<"exe">>] ->
				file:delete(<<(?UPLOADS)/binary, "any.exe">>);
			    [_, <<"msi">>] ->
				file:delete(<<(?UPLOADS)/binary, "any.msi">>);
			    [_, <<"msp">>] ->
				file:delete(<<(?UPLOADS)/binary, "any.msp">>)
			end,
			file:make_symlink(<<(?UPLOADS)/binary,F1/binary>>, <<(?UPLOADS)/binary, F2/binary>>)
		end,
		io:format("~ndate: ~p -> done - linking file/script file: ~p -> ~p~n",[Date, F1, F2]),
		Data2= <<"done - linking file/script file: ", F1/binary, "->", F2/binary, "....!">>,
		Data2;
	    <<"renscrfile">> ->
		send_msg(?SERVERS, <<"renscrfile from ", (pid())/binary>>),
		[F1,F2] = binary:split(Args, <<"+">>, [global]),
		file:rename(<<(?UPLOADS)/binary,F1/binary>>, <<(?UPLOADS)/binary, F2/binary>>),
		file:rename(<<(?UPLOADS)/binary,"info/",F1/binary,".info">>, <<(?UPLOADS)/binary,"info/",F2/binary,".info">>),
		io:format("~ndate: ~p -> done - renaming file/script file: ~p -> ~p~n",[Date, F1, F2]),
		Data4= <<"done - renaming file/script file: ", F1/binary, "->", F2/binary, "....!">>,
		Data4;
	    <<"rendwnldsfile">> ->
		send_msg(?SERVERS, <<"rendwnldsfile from ", (pid())/binary>>),
		[F1,F2] = binary:split(Args, <<"+">>, [global]),
		file:rename(<<(?DOWNLOADS)/binary,F1/binary>>, <<(?DOWNLOADS)/binary, F2/binary>>),
		io:format("~ndate: ~p -> done - renaming file/downloads file: ~p -> ~p~n",[Date, F1, F2]),
		Data4= <<"done - renaming file/downloads file: ", F1/binary, "->", F2/binary, "....!">>,
		Data4;
	    <<"editscrfile">> ->
		send_msg(?SERVERS, <<"editscrfile from ", (pid())/binary>>),
		Dataf = 
		    case lists:last(binary:split(Args, <<".">>, [global])) of
			<<"cmd">> ->
			    {ok, Dataf2} = 
				file:read_file(<<(?UPLOADS)/binary,Args/binary>>),
			    Dataf2;
			_ ->
			    Dataf2 = 
				<<"">>,
			    Dataf2
		    end,
		Datafi = 
		    case file:read_file(<<(?UPLOADS)/binary,"info/",Args/binary,".info">>) of
			{ok, Data} ->
			    Data;
			{reason, _} ->
			    <<"">>; %erlang:atom_to_binary(Res,latin1);
			{error, _} ->
			    <<"">>
		    end,
		io:format("~ndate: ~p -> done - edit script file: ...~n",[Date]),
		Data2= <<"done - edit script file...:editscrfile:^",Dataf/binary,"^:",Datafi/binary>>,
		Data2;
	    <<"savescrfile">> ->
		send_msg(?SERVERS, <<"savescrfile from ", (pid())/binary>>),
		[Fname,Dataf,Datafi] = binary:split(Args, <<"^">>, [global]),
		[_,T] = binary:split(Fname, <<".">>, [global]),
		Fnres = 
		    case T of
			<<"cmd">> ->
			    case file:write_file(<<(?UPLOADS)/binary,Fname/binary>>, Dataf) of
				ok ->
				    Res = <<"ok">>,
				    Res;
				{error, Res} ->
				    Res
			    end;
			_ ->
			    <<"">>
		    end,
		Finres = 
		    case file:write_file(<<(?UPLOADS)/binary,"info/",Fname/binary,".info">>, <<"\"",Datafi/binary,"\".">>) of
			ok ->
			    Res2 = <<"ok">>,
			    Res2;
			{error, Res2} ->
			    Res2
		    end,
		io:format("~ndate: ~p -> done - save script file: ~p...~n",[Date, Fname]),
		Data2= <<"done - save script file...: ",Fname/binary, " - fnres -> ",Fnres/binary, " - finres -> ",Finres/binary >>,
		Data2;
	    <<"clearcmsg">> ->
		send_msg(?SERVERS, <<"clearcmsg from ", (pid())/binary>>),
		io:format("~ndate: ~p -> done - clearing client message panel~n",[Date]),
		Data2= <<"done - clearing client message panel:">>,
		Data2;
	    <<"clearsmsg">> ->
		send_msg(?SERVERS, <<"clearsmsg from ", (pid())/binary>>),
		io:format("~ndate: ~p -> done - clearing *server message panel~n",[Date]),
		Data2= <<"done - clearing *server message panel:">>,
		Data2;
	    <<"cleardmsg">> ->
		send_msg(?SERVERS, <<"cleardmsg from ", (pid())/binary>>),
		io:format("~ndate: ~p -> done - clearing duplicates message panel~n",[Date]),
		Data2= <<"done - clearing duplicates message panel:">>,
		Data2;
	    <<"resetall">> ->
		send_msg(?SERVERS, <<"resetall from ", (pid())/binary>>),
		io:format("~ndate: ~p -> done - reset all~n",[Date]),
		Data2= <<"done - reset all:">>,
		Data2;
	    <<"lockactivate">> ->
		send_msg(?SERVERS, <<"lockactivate from ", (pid())/binary>>),
		io:format("~ndate: ~p -> done - lock activate~n",[Date]),
		Data2= <<"done - lock activate:">>,
		Data2;
	    <<"lockloginok">> ->
		send_msg(?SERVERS, <<"lockloginok from ", (pid())/binary>>),
		io:format("~ndate: ~p -> done - login from lock ok~n",[Date]),
		Data2= <<"done - login from lock ok:">>,
		Data2;
	    <<"lockloginfailed">> ->
		send_msg(?SERVERS, <<"lockloginfailed from ", (pid())/binary>>),
		io:format("~ndate: ~p -> done - login from lock failed~n",[Date]),
		Data2= <<"done - login from lock failed:">>,
		Data2;
	    <<"toggleawsts">> ->
		send_msg(?SERVERS, <<"toggleawsts (",Args/binary,") from ", (pid())/binary>>),
		{ok, [{ShutdownStartTime,ShutdownStopTime,_OnorOff}]} = file:consult(?AUTOSHUTDOWNCONF),
		case Args of
		    <<"On">> ->
			file:write_file(?AUTOSHUTDOWNCONF, 
				"{<<\"" ++ binary_to_list(ShutdownStartTime) ++ "\">>,<<\""
				++ binary_to_list(ShutdownStopTime) ++ "\">>,<<\"Off\">>}.");
		    <<"Off">> ->
			file:write_file(?AUTOSHUTDOWNCONF, 
				"{<<\"" ++ binary_to_list(ShutdownStartTime) ++ "\">>,<<\""
				++ binary_to_list(ShutdownStopTime) ++ "\">>,<<\"On\">>}.")
		end,
		io:format("~ndate: ~p -> done - toggleawsts/~p~n",[Date,Args]),
		<<"done - server@localhost/toggleawsts/(",Args/binary,")">>;
	    <<"chkpasswd">> ->
		send_msg(?SERVERS, <<"chkpasswd from ", (pid())/binary>>),
		io:format("~ndate: ~p -> done - chkpasswd~n",[Date]),
		{ok, [{Passwd}]}=file:consult(?PASSWDCONF),
		Data2 =
		    case Passwd of
			Args -> <<"done - 0/chkpasswd/pass">>;
			_ -> <<"done - 0/chkpasswd/fail">>
		    end,
		Data2;
	    <<"sdtchng">> ->
		send_msg(?SERVERS, <<"sdtchng (",Args/binary,") from ", (pid())/binary>>),
		{ok, [{_ShutdownStartTime,_ShutdownStopTime,OnorOff}]} = file:consult(?AUTOSHUTDOWNCONF),
		[ShutdownStartTime,ShutdownStopTime]=binary:split(Args,<<"-">>),
		file:write_file(?AUTOSHUTDOWNCONF, 
				"{<<\"" ++ binary_to_list(ShutdownStartTime) ++ "\">>,<<\"" 
				++ binary_to_list(ShutdownStopTime) ++ "\">>,<<\"" ++ binary_to_list(OnorOff) ++ "\">>}."),	      
		io:format("~ndate: ~p -> done - sdtchng/~p~n",[Date,Args]),
		<<"done - server@localhost/sdtchng/(",Args/binary,")">>;
	    <<"wkautoshutdown">> ->
		send_msg(?SERVERS, <<"wkautoshutdown (-",Args/binary,"-",Box/binary,"-) from ", (pid())/binary>>),
		case Args of
		    <<"on">> ->
			file:write_file(?WKSCONF ++ binary_to_list(Box) ++ ".conf", "{<<\"on\">>}.");
		    _ ->
			file:write_file(?WKSCONF ++ binary_to_list(Box) ++ ".conf", "{<<\"off\">>}.")
		end,
		io:format("~ndate: ~p -> done - wkautoshutdown/~p~p",[Date,Args,Box]),
		<<"done - server@localhost/wkautoshutdown/(-",Args/binary,"-",Box/binary,"-)">>;
	    <<"update_timers">> ->
		send_msg(?SERVERS, <<"update_timers from ", (pid())/binary>>),
		file:write_file(?TIMERSCONF, Args),
		io:format("~ndate: ~p -> done - update_timers~n",[Date]),
		<<"done - server@localhost/updates_timers">>;
	    <<"getmem">> ->
		{ok, [_,_,_,_,_,{OS}]} = file:consult(?CONF),
  		Memo = get_mem(OS),
		Data2 = <<"done - 0/getmem/",Memo/binary>>,
		Data2;
	    <<"dbinfo">> ->
		send_msg(?SERVERS, <<"dbinfo from ", (pid())/binary>>),
		Data2= <<Box/binary,":dbinfo:",(dbinfo(Args))/binary>>,
		io:format("~ndate: ~p -> done - dbinfo ~p ~n",[Date, Box]),
		Data2;
	    <<"update_refresh_timer">> ->
		update_refresh_timer(2),
		send_msg(?SERVERS, <<"update refresh timer ", (pid())/binary>>),
		Data2= <<Box/binary,":update_refresh_timer:0">>,
		io:format("~ndate: ~p -> done - update refresh timer ~p ~n",[Date, Box]),
		Data2;
	    _ ->					
		send_msg(?SERVERS, <<"unsupported command from ", (pid())/binary>>),
		<<"unsupported command">>
	end,
    {reply, {text, Data3}, State, hibernate};
websocket_handle(_Data, State) ->
    {ok, State}.

%%

dup(B,Acc) when Acc > 1 ->	
    B2=dup(B, Acc-1),
    << B/binary,  B2/binary >>;
dup(B,1) ->
    B.

%%

send_msg([Server|Rest], Msg) ->
    msg_to_consoles(Server, ?CONSOLES, Msg),
    send_msg(Rest, Msg);
send_msg([], _Msg) ->
    [].

%%

msg_to_consoles(Server, [Console|Rest], Msg) ->
    {Console, Server} ! Msg,
    msg_to_consoles(Server, Rest, Msg);
msg_to_consoles(_Server, [], _Msg) ->
    [].

%%

pid() ->
    Apid = self(),
    case whereis(hanwebs) =:= Apid of
	true -> 
	    <<"cons1">>;
	_ ->
	    case whereis(hanwebs2) =:= Apid of
		true ->
		    <<"cons2">>; 
		_ ->
		    case whereis(hanwebs3) =:= Apid of
			true -> 
			    <<"cons3">>; 
			_ ->
			    <<"cons4">> 
		    end
	    end
    end.	

%%

websocket_info(PreMsg, State) ->
    Msg=
	case PreMsg of
	    {Msg2,_PID}-> Msg2;
	    _ -> PreMsg
	end,
    Msg3 = 
	case is_binary(Msg) of
	    true ->
		Msg;
	    false -> 
		case Msg of
		    {'EXIT', _, normal} ->
			<<>>;
		    _ ->
			list_to_binary(Msg)
		end				 
	end,
    chk_insert(binary:split(Msg3, <<"/">>, [global])),
    {reply, {text, Msg3}, State, hibernate}.

%%

chk_insert([_]) -> ok;
chk_insert([_, <<"pong">>]) -> ok;
chk_insert([_, _, <<>>]) ->	ok;
chk_insert([B1, _, B2]) ->
    case binary:split(B2, <<" ">>) of
	[_, _] ->
	    [];
	_ ->		
            case binary:match(B2,binary:split(?IGNOREUSERS2,<<":">>,[global]), []) of
		nomatch ->
		    {{Year, Month, Day}, {Hour, Min, _}} = calendar:local_time(),
		    TimeStamp = list_to_binary(io_lib:format("~p-~2..0B-~2..0B ~2..0B:~2..0B", [Year, Month, Day, Hour, Min])),
		    [_,H1]=binary:split(B1,<<"@">>,[global]),
		    [H2|_]=binary:split(H1,<<".">>,[global]),
		    case binary:match(H2,[<<?IGNORESHOWUSERS>>], []) of
			nomatch ->
			    do_insert(TimeStamp, B1, B2);
			_ ->
			    do_insert(TimeStamp, B1, <<"">>)
		    end;
		_  -> 
		    ok
	    end
    end;
chk_insert(_Data) when length(_Data) >= 2 -> ok.

%%

do_insert(TimeStamp, Box, User) ->
    S = <<"insert into esysman (atimestamp, abox, auser) values ('", TimeStamp/binary, "', '", Box/binary, "', '", User/binary, "')">>,
    case pgsql:connect(?DBHOST, ?USERNAME, ?PASSWORD, [{database, ?DB}, {port, ?PORT}]) of
	{error,_} ->
	    {S, error};
	{ok, Db} -> 
	    case pgsql:squery(Db, S) of
		{error,Error} ->
		    io:format("insert error: ~p~n", [Error]),
		    {S, error};
		{_,Res} ->
		    pgsql:close(Db),
		    {S, Res}
	    end
    end.

%%

list_dwnld_fls() ->
    {ok, Files0}=file:list_dir(?DOWNLOADS),
    Files=lists:sort(Files0),
    Head = <<"<div id='dwnldslist'>[ Manage Downloads ]<br><br><button id='closedwnldslist' class='ui-button ui-widget ui-corner-all'>Close</button></div><div id='dprog'></div><form id='mypostd' method='post' enctype='multipart/form-data' action='/uptodown'><br><input id='fdwnload' type='submit' value='Upload'/><input id='selfiled' type='file' name='inputfile' value='No File Selected yet!' class='isize' /></form><table><tr><th></th><th>File Name</th><th>File Size</th><th>File Date</th></tr>">>,
    Mid = <<(list_to_binary([ mng_dfile(File) || File <- Files]))/binary>>,
    Tail = <<"</table><div class='brk'></div><button id='closedwnldslist' class='ui-button ui-widget ui-corner-all'>Close</button></div>">>,
    <<Head/binary,Mid/binary,Tail/binary>>.

%%

mng_dfile(File) ->
    case file:read_file_info(binary_to_list(?DOWNLOADS) ++ "/" ++ File) of
	{ok, {_,Fsize1,Ftype,_,_,Ftime1,_,_,_,_,_,_,_,_}} ->
	    case Ftype of
		directory -> 
		    "";
		_ ->
		    Fsize = file_size(Fsize1),
		    Ftime = file_time(Ftime1),
		    tr(File, Fsize, Ftime, 2)
	    end;
	_ ->
	    tr(File, "", "", 2)
    end.

%%

dbinfo(Args) ->
    {ok, Db} = pgsql:connect(?DBHOST, ?USERNAME, ?PASSWORD, [{database, ?DB}, {port, ?PORT}]),
    Res1 = case Args of
    	 <<"0">> ->
	         S = <<"select * from esysman order by atimestamp desc limit 1">>,
		 {ok, _, [{Timestampp, Boxp, Userp, Idp}]} = pgsql:squery(Db, S),
		 <<"-- <br><br>Last record:<br>atimestamp: ",Timestampp/binary,
		 "<br>box: ",Boxp/binary,
		 "<br>user: ",Userp/binary,
		 "<br>id: ",Idp/binary,"<br><br>--<br><br>">>;
	 _ -> 
	         S = <<Args/binary>>,
		 S2=binary:replace(S, <<"~">>, <<":">>, [global]),
%		 {ok, Cols, Rows} = pgsql:squery(Db, S2),

                 try pgsql:squery(Db, S2) of
                   {ok, Cols, Rows} ->
			 process_query(Cols, Rows);
                   {error, _Reason} ->
			 <<"--<br><br>Query error<br><br>--">>
                 catch
                   _:_ ->
			 <<"--<br><br>Query error<br><br>--">>
                 end
     end,

     Res=binary:replace(Res1, <<":">>, <<"~">>, [global]),
     Mid = Res, %<<"query-data">>,
     <<"<table>", Mid/binary, "</table><br>">>.

%%
    
process_row([ColName|ColNames], [Val|Vals]) ->
    {_, Col, _, _, _, _} = ColName,
    <<"<tr><td>",Col/binary,"</td><td>", Val/binary, "</td></tr>", (process_row(ColNames, Vals))/binary>>;
process_row([], []) ->
    <<"<tr><td colspan=100>-</td></tr>">>.

%%

process_query(Cols, [Row|Rows]) ->
    <<(process_query(Cols, Rows))/binary, (process_row(Cols, tuple_to_list(Row)))/binary>>;
process_query(_Cols,[]) ->
    <<"">>.

%%

list_up_fls(Filter) ->
    {ok, Files0}=file:list_dir(?UPLOADS),
    Files=lists:sort(Files0),
    Head= <<"<script>$('#scrfilter').focus(); var tmp=$('#scrfilter').val(); $('#scrfilter').val(''); $('#scrfilter').val(tmp);</script><div id='scrslist'>[ Manage Scripts ]<br><br><button id='closescrslist' class='ui-button ui-widget ui-corner-all'>Close</button><button id='addscrf' class='ui-button ui-widget ui-corner-all'>Add Script</button> <div id='scrcount' class='fr'>[">>,
    Head2= <<"]-Items</div> <div class='brk'></div><div id='upprog'></div><form id='mypost' method='post' enctype='multipart/form-data' action='/upload'><br><input id='fupload' type='submit' value='Upload'/><input id='selfile' type='file' name='inputfile' value='No File Selected yet!' class='isize' /></form>Filter -> <input id='scrfilter' type='text' class='ui-widget' maxlength=20 value='", (Filter)/binary, "' /><br><br><table id='mngscripts'><tr><th class='comw'>Commands</th><th>File Name</th><th>File Size</th><th>File Date</th><th>Linked File Name</th><th>Description</th></tr>">>,
    Mid = [mng_file(File, Filter) || File <- Files],
    Tail = <<"</table><div class='brk'></div><button id='closescrslist' class='ui-button ui-widget ui-corner-all'>Close</button></div><div id='editscr'><div>Editing -> <span id='scrname'></span></div><div><div id='scrtxtbox'>Script text<br><textarea id='scripttext' rows='10' cols='60'></textarea><br><br></div>Script Description<br><input id='scrdesc' type='text' maxlength='69'><br><br><input type='button' id='scredcancel' value='Cancel'><input type='button' id='scrsave' value='Save'></div></div>">>,
    <<Head/binary,(list_to_binary(integer_to_list(file_count(Mid))))/binary,Head2/binary,(list_to_binary([any_mng_file(File, Filter) || File <- Files]))/binary,(list_to_binary(Mid))/binary,Tail/binary>>.

%%

file_count([Item|Rest]) ->
    case length(Item) of
	0 -> file_count(Rest);
	_ -> 1 + file_count(Rest)
    end;
file_count([]) ->
    0;
file_count(<<>>) ->
    0.


%%

any_mng_file(File, _Filter) ->
    {Res, LnFile} = file:read_link(binary_to_list(?UPLOADS) ++ "/" ++ File),
    case file:read_file_info(binary_to_list(?UPLOADS) ++ "/" ++ File) of
	{ok, {_,Fsize1,Ftype,_,_,Ftime1,_,_,_,_,_,_,_,_}} ->
	    case Ftype of
		directory -> 
		    "";
		_ ->
		    Fsize = file_size(Fsize1),
		    Ftime = file_time(Ftime1),
		    case File of
			"any.cmd" -> 
			    ShortLnf = erlang:binary_to_list(lists:last(binary:split(erlang:list_to_binary(LnFile),<<"/">>, [global]))),
			    tr1(File, Fsize, Ftime, Res, "cmddiv", "lncmddiv", ShortLnf);
			"any.exe" -> 
			    ShortLnf = erlang:binary_to_list(lists:last(binary:split(erlang:list_to_binary(LnFile),<<"/">>, [global]))),
			    tr1(File, Fsize, Ftime, Res, "exediv", "lnexediv", ShortLnf);
			"any.msi" -> 
			    ShortLnf = erlang:binary_to_list(lists:last(binary:split(erlang:list_to_binary(LnFile),<<"/">>, [global]))),
			    tr1(File, Fsize, Ftime, Res, "msidiv", "lnmsidiv", ShortLnf);
			"any.msp" -> 
			    ShortLnf = erlang:binary_to_list(lists:last(binary:split(erlang:list_to_binary(LnFile),<<"/">>, [global]))),
			    tr1(File, Fsize, Ftime, Res, "mspdiv", "lnmspdiv", ShortLnf);
			_ -> 
				<<>>
		    end
	    end;
	_ ->
	    case File of
		"any.cmd" ->
		    tr1(File, "", "", Res, "cmddiv", "lncmddiv", "");
		"any.exe" ->
		    tr1(File, "", "", Res, "exediv", "lnexediv", "");
		"any.msi" ->
		    tr1(File, "", "", Res, "msidiv", "lnmsidiv", "");
		"any.msp" ->
		    tr1(File, "", "", Res, "mspdiv", "lnmspdiv", "");
		_ -> 
		    <<>>			
	    end
    end.

%%

mng_file(File, _Filter) ->
    case file:read_file_info(binary_to_list(?UPLOADS) ++ "/" ++ File) of
	{ok, {_,Fsize1,Ftype,_,_,Ftime1,_,_,_,_,_,_,_,_}} ->
	    case Ftype of
		directory -> 
		    "";
		_ ->
		    Fsize = file_size(Fsize1),
		    Ftime = file_time(Ftime1),
		    case File of
			"any.cmd" -> 
			    "";			
			"any.exe" -> 
			    "";
			"any.msi" -> 
			    "";
			"any.msp" -> 
			    "";
			_ -> 
			    Filter2 = "",
			%	case Filter of
			%	    <<>> -> "";
			%	    _ -> binary:bin_to_list(Filter)
			%	end,
			    FileInfo = mng_file_info(File),
			    case Filter2 of
				"" -> 
				    tr(File, Fsize, Ftime, 0, FileInfo);
				_ ->
				    FF = string:rstr(File, Filter2),
				    FIF = string:rstr(FileInfo, Filter2),
				    case (FF > 0) or (FIF > 0) of
					true -> tr(File, Fsize, Ftime, 0, FileInfo);
					_ -> ""
				    end
			    end
		    end
	    end;
	_ ->
	    case File of
		"any.cmd" ->
		    "";
		"any.exe" ->
		    "";			
		"any.msi" ->
		    "";			
		"any.msp" ->
		    "";			
		_ -> 
		    tr(File, "", "", 0, mng_file_info(File))	
	    end
    end.

%%


file_size(Size) -> 
    file_size(Size, ["B","KB","MB","GB","TB","PB"]).

file_size(S, [_|[_|_] = L]) when S >= 1024 -> 
    file_size(S/1024, L);
file_size(S, [M|_]) ->
    io_lib:format("~.2f ~s", [float(S), M]).    

file_time(Ftime) ->
    {{Year, Month, Day}, {Hour, Min, _Sec}} = Ftime,
    io_lib:format("&nbsp;&nbsp;~w/~2..0w/~2..0w ~2..0w~2..0w&nbsp;&nbsp;",[Year, Month, Day, Hour, Min]).
%%

tr1(File, Fsize, Ftime, Res, Fdiv, Ldiv, LnFile) ->
    case Res of
	ok ->
	    "<tr class='r'><td></td><td><div id='" ++ Fdiv ++"'>" ++ File ++ "</div></td><td align=right>"++Fsize++"</td><td>"++Ftime++"</td><td><div id='" ++ Ldiv ++ "'>" ++ LnFile ++ "</div></td><td>"++mng_file_info(LnFile)++" </td></tr>";
	_ ->
	    "<tr class='r'><td></td><td>"++File++"</td><td>"++Ftime++"</td><td></td><td></td><td>"++mng_file_info(File)++" </td></tr>"
    end.

%%

tr(File, Fsize, Ftime, 0, FileInfo) ->
    "<tr class='r'><td><button id='dbut' class='ui-button ui-widget ui-corner-all' title='Delete File'>Del</button><button id='rbut' class='ui-button ui-widget ui-corner-all' title='Rename File'>Ren</button><button id='lbut' class='ui-button ui-widget ui-corner-all' 'Link file to any(.cmd/.exe/.msi/.msp)>ln</button><button id='ebut' class='ui-button ui-widget ui-corner-all' title='Edit Script'>Edit</button><button id='fncbut' class='ui-button ui-widget ui-corner-all' title='Copy file name to clipboard'>Copy</button></td><td>"++File++"</td><td align=right>"++Fsize++"</td><td>"++Ftime++"</td><td></td><td>"++FileInfo++"</td></tr>".

tr(File, Fsize, Ftime, 2) ->
    "<tr class='r'><td><button id='dbutd' class='ui-button ui-widget ui-corner-all'>Del</button><button id='rbutd' class='ui-button ui-widget ui-corner-all'>Ren</button></td><td>"++File++"</td><td align=right>"++Fsize++"</td><td>"++Ftime++"</td><td></td></tr>".

%%

mng_file_info(File) ->
    Info = 
	case file:consult(<<(?UPLOADS)/binary,"info/",(erlang:list_to_binary(File))/binary,".info">>) of
	    {ok, [Terms]} ->
		Terms;
	    {error, Reason} ->
		io_lib:format("~p",[Reason])
        end,
    case Info of
	["enoent"] -> "";
	_ -> Info
    end.

%%

terminate(Reason, _Opts, _State) ->
    io:format("~nTerminate Reason: ~p~n", [Reason]),
    ok.

%%

fire_wall(Req) ->	
    {PeerAddress, _Port} = cowboy_req:peer(Req),
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:local_time(),
    Date = lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",[Year,Month,Day,Hour,Minute,Second])),
    {ok, [_,{FireWallOnOff,IPAddresses},_,_,_,_]}=file:consult(?CONF),
    case FireWallOnOff of
	on ->
	    case lists:member(PeerAddress,IPAddresses) of
		true ->
		    io:format("~ndate: ~p -> websocket - firewall allow -> ~p~n",[Date, PeerAddress]),
		    allow;
		false ->
		    io:format("~ndate: ~p -> websocket - firewall denied -> ~p~n",[Date, PeerAddress]),
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

get_mem(OS) ->
  % Rel = remove empty list
  Rel = fun Rel(_,[]) -> []; Rel(X,[X|R]) -> Rel(X,R); Rel(X,[Y|R]) -> [Y] ++ Rel(X,R) end,
  case OS of
    <<"bsd">> ->
        Mem=string:split(os:cmd("freecolor -m -o|grep Mem:"), " ", all),
        {Memt, _}=string:to_integer(lists:nth(2,Rel([],Mem))),
        {Memu, _}=string:to_integer(lists:nth(4,Rel([],Mem))),
        Memo=list_to_binary(io_lib:format(" T ~.2fGB | U ~.2fGB ", [Memt/1000,Memu/1000])),
        Memo;
    <<"linux">> ->
        Mem=string:split(os:cmd("free --giga|grep Mem:"), " ", all),
        Memt=lists:nth(2,Rel([],Mem)),
        Memu=lists:nth(4,Rel([],Mem)),
        Memo=list_to_binary(io_lib:format(" T ~sGB | U ~sGB ", [Memt,Memu])),
        Memo;
    _ -> <<>>
  end.
