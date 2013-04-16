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

-module(websocket_handler).
-export([
		 init/3,
		 handle/2,
		 terminate/3
		]).
-export([
		 websocket_init/3,
		 websocket_handle/3,
		 websocket_info/3,
		 websocket_terminate/3
		]).

%im().
%ii(websocket_handler).
%iaa([init]).

-include("esysman.hrl").

init(_Transport, Req, []) ->
	case cowboy_req:header(<<"upgrade">>, Req) of
		{undefined, Req2} ->
			{ok, Req2, undefined};
		{<<"websocket">>, _Req2} ->
			{upgrade, protocol, cowboy_websocket};
		{<<"WebSocket">>, _Req2} ->
			{upgrade, protocol, cowboy_websocket}
	end.

terminate(_Reason, _Req, _State) ->
	ok.

websocket_init(_Any, Req, []) ->
	case lists:member(hanwebs, registered()) of
		true -> ok;
		false ->
			register(hanwebs, self())
	end,
	Req2 = cowboy_req:compact(Req),
	{ok, Req2, undefined, hibernate}.

websocket_handle({text, <<"close">>}, Req, State) ->
			{shutdown, Req, State};
websocket_handle({text, <<"client-connected">>}, Req, State) ->
			{reply, {text, <<"client-connected">> }, Req, State, hibernate};
websocket_handle({text, Msg}, Req, State) ->
	Ldata=binary:split(Msg,<<":">>,[global]),
	io:format("~nLdata: ~p~n",[Ldata]),
	[Box,Com,Args]=Ldata,
	Rec_Node=binary_to_atom(<<Box/binary>>,latin1),
	case Com of
		<<"com">> ->
			{rec_com, Rec_Node} ! {Box,Com,Args},
			Data2= <<"com -> ",Args/binary,"  <- sent to: ",Box/binary>>,
			io:format("~n done com: ~p - args: ~p~n",[Box,Args]);
		<<"loggedon">> ->
			{rec_com, Rec_Node} ! {Box,Com,<<"">>},
			Data2= <<"loggedon sent to: ",Box/binary>>,
			io:format("~n done loggedon ~p - data2: ~p ~n",[Box, Data2]);
		<<"copy">> ->
			case file:read_file(<<?UPLOADS/binary,Args/binary>>) of
				{ok, DataBin} ->
					{rec_com, Rec_Node} ! {Box,Com,{Args,DataBin}},
					Data2= <<"copy sent to: ",Box/binary>>,
					io:format("~n done copy - ~p ~n",[Box]);
				{error, Reason} ->
					Data2= <<Box/binary,":copy error-",(atom_to_binary(Reason,latin1))/binary>>,
					io:format("~n done copy - ~p - error: ~p~n",[Box, Reason])
			end;
		<<"dffreeze">> ->
			{rec_com, Rec_Node} ! {Box,Com,<<"">>},
			Data2= <<Box/binary,":dffreeze">>,
			io:format("~n done dffreeze ~p - data2: ~p ~n",[Box, Data2]);
		<<"dfthaw">> ->
			{rec_com, Rec_Node} ! {Box,Com,<<"">>},
			Data2= <<Box/binary,":dfthaw">>,
			io:format("~n done dfthaw ~p - data2: ~p ~n",[Box, Data2]);
		<<"dfstatus">> ->
			{rec_com, Rec_Node} ! {Box,Com,<<"">>},
			Data2= <<"dfstatus sent to: ",Box/binary>>,
			io:format("~n done dfstatus ~p - data2: ~p ~n",[Box, Data2]);
		<<"net_restart">> ->
			{rec_com, Rec_Node} ! {Box,Com,<<"">>},
			Data2= <<"net_restart sent to: ",Box/binary>>,
			io:format("~n done net_restart ~p - data2: ~p ~n",[Box, Data2]);
		<<"net_stop">> ->
			{rec_com, Rec_Node} ! {Box,Com,<<"">>},
			Data2= <<"net_stop sent to: ",Box/binary>>,
			io:format("~n done net_stop ~p - data2: ~p ~n",[Box, Data2]);
		<<"reboot">> ->
			{rec_com, Rec_Node} ! {Box,Com,<<"">>},
			Data2= <<"reboot sent to: ",Box/binary>>,
			io:format("~n done reboot ~p - data2: ~p ~n",[Box, Data2]);
		<<"shutdown">> ->
			{rec_com, Rec_Node} ! {Box,Com,<<"">>},
			Data2= <<"shutdown sent to: ",Box/binary>>,
			io:format("~n done shutdown ~p - data2: ~p ~n",[Box, Data2]);
		<<"wol">> ->
			MacAddr=binary_to_list(Args),
			MacAddrBin= <<<<(list_to_integer(X, 16))>> || X <- string:tokens(MacAddr,"-")>>,
			MagicPacket= << (dup(<<16#FF>>, 6))/binary, (dup(MacAddrBin, 16))/binary >>,
			{ok,S} = gen_udp:open(0, [{broadcast, true}]),
			gen_udp:send(S, ?BROADCAST_ADDR, 9, MagicPacket),
			gen_udp:close(S),
			Data2= <<"done wol: ",Box/binary,"....!">>,
			io:format("~n done wol - ~p ~n",[Box]);
		  <<"ping">> ->
			{rec_com, Rec_Node} ! {Box,Com,<<"">>},
			Data2= <<"ping sent to: ",Box/binary>>,
			io:format("~n done ping ~p - data2: ~p ~n",[Box, Data2]);
		_ ->
			Data2= <<"unsupported command">>
	end,
	{reply, {text, Data2}, Req, State, hibernate};
websocket_handle(_Any, Req, State) ->
	{ok, Req, State}.

dup(B,Acc) when Acc > 1 ->	
    B2=dup(B, Acc-1),
	<< B/binary,  B2/binary >>;
dup(B,1) ->
    B.

websocket_info(PreMsg, Req, State) ->
	Msg=
		case PreMsg of
			{Msg2,_PID}-> Msg2;
			_ -> PreMsg
		end,
	Msg3 = 
		case is_binary(Msg) of
			true -> Msg;
			false -> list_to_binary(Msg)
		end,
	{reply, {text, Msg3}, Req, State, hibernate}.

websocket_terminate(_Reason, _Req, _State) ->
	ok.

fire_wall(Req) ->	
	{PeerAddress, _Req}=cowboy_req:peer_addr(Req),
	{ok, [_,{FireWallOnOff,IPAddresses},_,_]}=file:consult(?CONF),
	case FireWallOnOff of
		on ->
			case lists:member(PeerAddress,IPAddresses) of
				true ->
					allow;
				false ->
					deny
			end;
		off -> allow
	end.

%

fwDenyMessage(Req, State) ->
	{ok, Req2} =
		cowboy_req:reply(200, [{<<"Content-Type">>, <<"text/html">>}],
			  <<"<html>
<head> 
<title>", ?TITLE, "</title>
<link rel='icon' href='/static/favicon.ico' type='image/x-icon' />
<style>
body {background-color:black; color:yellow}
</style>
</head>
<body>
Access Denied!
</body>
</html>">>, Req),
    {ok, Req2, State}.


%%

login_is() ->
	{ok, [_,_,{UPOnOff,UnamePasswds},_]}=file:consult(?CONF),
	case UPOnOff of
		on -> UnamePasswds;
		off -> off
	end.
	
%%

checkCreds(UnamePasswds, Req, _State) ->
	[{Uname,_}] = UnamePasswds,
	{C, Req1} = cowboy_req:cookie(Uname, Req),
    case (C == undefined) or (C == <<>>) of
		true ->
			checkPost(UnamePasswds, Req1);
		false  ->
			CookieVal = get_cookie_val(), 
			Req2 = cowboy_req:set_resp_cookie(Uname, CookieVal, [{max_age, ?MAXAGE}, {path, "/"}, {secure, true}, {http_only, true}], Req1),
			{pass, Req2}
	end.

%

checkCreds([{Uname,Passwd}|UnamePasswds], Uarg, Parg, Req) ->
    case Uname of
		Uarg ->
			case Passwd of
				Parg ->
					CookieVal = get_cookie_val(), 
					Req0 = cowboy_req:set_resp_cookie(Uname, CookieVal, [{max_age, ?MAXAGE}, {path, "/"}, {secure, true}, {http_only, true}], Req),
					{pass, Req0};
				_ ->
					checkCreds(UnamePasswds,Uarg,Parg,Req)
			end;
		_ ->
			checkCreds(UnamePasswds, Uarg, Parg, Req)
	end;
checkCreds([], _Uarg, _Parg, Req) ->
	{fail, Req}.

%

checkPost(UnamePasswds,Req) ->
	case cowboy_req:method(Req) of
		{<<"POST">>, Req0} ->
			{ok, FormData, Req1} = cowboy_req:body_qs(Req0),
			case FormData of
				[{_UnameVar,UnameVal},{_PasswdVar,PasswdVal},_Login] ->
					checkCreds(UnamePasswds,UnameVal,PasswdVal,Req1);
				_ ->
					{fail,Req}
			end;
		_ ->
			{fail,Req}
	end.

%

get_cookie_val() ->
	list_to_binary(
	  integer_to_list(
		calendar:datetime_to_gregorian_seconds({date(), time()})
	   )).

%

app_login(Req, State) ->
	case fire_wall(Req) of
		allow ->
			case is_list(login_is()) of
				true ->
					{ok, Req2} = cowboy_req:reply(200, [{<<"Content-Type">>, <<"text/html">>}],
<<"<html>
<head> 
<title>", ?TITLE, "</title>

<meta Http-Equiv='Cache-Control' Content='no-cache'>
<meta Http-Equiv='Pragma' Content='no-cache'>
<meta Http-Equiv='Expires' Content='0'>
<META HTTP-EQUIV='EXPIRES' CONTENT='Mon, 30 Apr 2012 00:00:01 GMT'>

<link rel='icon' href='/static/favicon.ico' type='image/x-icon' />
<link rel=\"stylesheet\" href=\"", ?CSS, "?", (now_bin())/binary, "\" type=\"text/css\" media=\"screen\" />
<script type='text/javascript' src='", ?JQUERY, "'></script>
<script>
$(document).ready(function(){

$('#uname').focus();

});
</script>
</head>
<body>
<form action='/esysman' method='post'>
<div>
  <h3>", ?TITLE, " Login</h3>
</div>
<div class='unamed'>
  <div class='unamed-t'>Username: </div><div><input id='uname' type='text' name='uname'></div>
</div>
<div class='passwdd'>
  <div class='passwdd-t'>Password: </div><div><input id='passwd' type='password' name='passwd'></div>
</div>
<div class='logind'>
  <div class='fl'><input type='submit' name='login' value='Login'></div>
</div>
</form>
</body>
</html>">>, Req);
                false ->
					{ok, Req2} = cowboy_req:reply(200, [{<<"Content-Type">>, <<"text/html">>}],
<<"<html>
<head> 
<title>", ?TITLE, " Login</title>
<link rel='icon' href='/static/favicon.ico' type='image/x-icon' />
</head>
<body>
hi
</body>
</html>">>, Req)
            end,
    {ok, Req2, State};
        deny ->
            fwDenyMessage(Req, State)
    end.

%

handle(Req, State) ->
case fire_wall(Req) of
		allow ->
			Creds=login_is(),
			case is_list(Creds) of
				true ->
					{Cred, Req0} = checkCreds(Creds, Req, State),
					case Cred of
						fail ->
							app_login(Req0, State);
						pass ->
							app_front_end(Req0, State)
					end;
				false -> 
					case Creds of
						off ->
							app_front_end(Req, State);
						_  ->
							app_login(Req, State)
					end
			end;
		deny ->
			fwDenyMessage(Req, State)
	end.

%

app_front_end(Req, State) ->
	Is_SSL=
		case cowboy_req:get([socket, transport], Req) of
			[_Socket, ranch_ssl] ->
				<<"true">>;
			[_Socket, _] ->
				<<"false">>
		end,
	{Host, Req2} = cowboy_req:host(Req),

	{PortInt, Req3} = cowboy_req:port(Req2),
	Port = list_to_binary(integer_to_list(PortInt)),
	io:format("~n host: ~p port: ~p~n", [Host, Port]),
	Get_rms = get_rms_keys(?ROOMS, 49),
	{ok, [_, _, _, {Ref_cons_time}]} = file:consult(?CONF),
	{ok, Req4} = cowboy_req:reply(200, [{<<"Content-Type">>, <<"text/html">>}],
<<"<html>
<head> 
<title>", ?TITLE, "</title>

<meta Http-Equiv='Cache-Control' Content='no-cache'>
<meta Http-Equiv='Pragma' Content='no-cache'>
<meta Http-Equiv='Expires' Content='0'>
<META HTTP-EQUIV='EXPIRES' CONTENT='Mon, 30 Apr 2012 00:00:01 GMT'>

<link rel='icon' href='/static/favicon.ico' type='image/x-icon' />
<link rel=\"stylesheet\" href=\"", ?CSS, "?", (now_bin())/binary, "\" type=\"text/css\" media=\"screen\" />
<script type='text/javascript' src='", ?JQUERY, "'></script>

<script>

$(document).ready(function(){
	if ('MozWebSocket' in window) {
		WebSocket = MozWebSocket;
	}

if (!window.WebSocket){
	alert('WebSocket not supported by this browser')
} else {  //The user has WebSockets

// websocket code from: http://net.tutsplus.com/tutorials/javascript-ajax/start-using-html5-websockets-today/

		var host=
'",
Host/binary,
"';
        var port='",
Port/binary,
"';
        var is_ssl=",
Is_SSL/binary,
";
    if(is_ssl)
        var ws_str='wss://'+host+':'+port+'/esysman';
    else 
        var ws_str='ws://'+host+':'+port+'/esysman';

	var r=false;
	var rall=false;
	var first=true;
    var tot_cnt=0;

	try{
        var socket = new WebSocket(ws_str);

		message(true, socket.readyState);

		socket.onopen = function(){
			console.log('onopen called');
			send('client-connected');
			message(true, socket.readyState);

",
(init_open(?ROOMS))/binary,
(init2(?ROOMS,Ref_cons_time))/binary,
"
		}

		socket.onmessage = function(m){
			console.log('onmessage called');
			if (m.data)
				if(m.data.indexOf(':'>0) || m.data.indexOf('/')>0){
					if(m.data.indexOf(':')>0) {
					   boxCom=m.data.split(':');
					   sepcol=true;
					}
					else {
					   boxCom=m.data.split('/');
					   sepcol=false;
					}
					box=boxCom[0].substr(0,boxCom[0].indexOf('.'));
					if (box.indexOf('@')>0)
					   box=box.substr(box.indexOf('@')+1, box.length-1);
					switch(boxCom[1]) {
						case 'loggedon':
							message(sepcol,boxCom[0] + ': ' + boxCom[2]);
							if (boxCom[2].indexOf('command not')<0) {
								 if(boxCom[2].length)
								     $('#'+box+'status').html(boxCom[2]);
							     else
							         $('#'+box+'status').html('Up');
                            }
                            else {
                                $('#'+box+'status').html('.');
							    $('#'+box+'status').css('color','red');
							    $('#'+box+'status').css('background-color','#550000');
                            }
							break;
						case 'pong':
							$('#'+box+'status').css('color','green');
							$('#'+box+'status').css('background-color','#005500');
							$('#'+box+'_hltd').css('background-color','#005555');
							$('#'+box+'_ltd').css('background-color','#005555');
							message(sepcol,boxCom[0] + ': ' + 'pong');
							break;
					    case 'pang':
							$('#'+box+'status').css('color','red');
							$('#'+box+'status').css('background-color','#550000');
							message(sepcol,boxCom[0] + ': ' + 'pang');
							break;
						case 'reboot':
							$('#'+box+'status').css('color','red');
							$('#'+box+'status').css('background-color','#550000');
                            $('#'+box+'status').html('.');
							$('#'+box+'_hltd').css('background-color','#000000');
							$('#'+box+'_ltd').css('background-color','#000000');
							break;
					    case 'shutdown':
							$('#'+box+'status').css('color','red');
							$('#'+box+'status').css('background-color','#550000');
                            $('#'+box+'status').html('.');
							$('#'+box+'_hltd').css('background-color','#000000');
							$('#'+box+'_ltd').css('background-color','#000000');
							break;
					    case 'dffreeze':
							$('#'+box+'dfstatus').css('color','cyan');
							$('#'+box+'dfstatus').css('background-color','#006666');
							$('#'+box+'status').css('color','red');
							$('#'+box+'status').css('background-color','#550000');
                            $('#'+box+'status').html('.');
							$('#'+box+'_hltd').css('background-color','#000000');
							$('#'+box+'_ltd').css('background-color','#000000');
							break;
					    case 'dfthaw':
							$('#'+box+'dfstatus').css('color','green');
							$('#'+box+'dfstatus').css('background-color','#006600');
							$('#'+box+'status').css('color','red');
							$('#'+box+'status').css('background-color','#550000');
                            $('#'+box+'status').html('.');
							$('#'+box+'_hltd').css('background-color','#000000');
							$('#'+box+'_ltd').css('background-color','#000000');
							break;
					    case 'dfstatus':
							if(!(boxCom[2].indexOf('thawed'))){
								$('#'+box+'dfstatus').html('DF');
								$('#'+box+'dfstatus').css('color','green');
								$('#'+box+'dfstatus').css('background-color','#006600');
							}
							else {
								$('#'+box+'dfstatus').html('DF');
								$('#'+box+'dfstatus').css('color','cyan');
								$('#'+box+'dfstatus').css('background-color','#006666');
							}
							break;
					    case 'copy':
							$('#'+box+'status').css('color','#00cc00');
							$('#'+box+'status').css('background-color','#006600');
							message(sepcol,boxCom[0] + ': ' + 'copy');
							break;
					    case 'com':
							$('#'+box+'status').css('color','#00cc00');
							$('#'+box+'status').css('background-color','#006600');
							message(sepcol,boxCom[0] + ': ' + 'com');
							break;
					    default:
                            if(boxCom[2])
							    message(sepcol,boxCom[0] + ': ' + boxCom[1] + ' ' + boxCom[2])
                            else
                                if(boxCom[1] == undefined)
							        message(sepcol,boxCom[0])
                                else
							        message(sepcol,boxCom[0] + ': ' + boxCom[1])
					}
				}
				else message(true,m.data)
		}

		socket.onclose = function(){
			console.log('onclose called')
		    message(true,'Socket status: 3 (Closed)');
		}

		socket.onerror = function(e){
			message(true,'Socket Status: '+e.data)
		}

	} catch(exception){
	   message(true,'Error'+exception)
	}

	function send(msg){
		console.log('send called');
		if(msg == null || msg.length == 0){
			message(true,'No data....');
			return
		}
		try{
			socket.send(msg)
		} catch(exception){
			message(true,'Error'+exception)
		}
	}

	function message(sepcol,msg){
        var jsnow = new Date();
        var month=jsnow.getMonth()+1;
        var day=jsnow.getDate();
        var hour=jsnow.getHours();
        var mins=jsnow.getMinutes();
        var seconds=jsnow.getSeconds();

        (month<10)?month='0'+month:month;
        (day<10)?day='0'+day:day;
        (hour<10)?hour='0'+hour:hour;
        (mins<10)?mins='0'+mins:mins;
        (seconds<10)?seconds='0'+seconds:seconds;

        now = month+'/'+day+'/'+jsnow.getFullYear()+'-'+hour+':'+mins+':'+seconds;
        
		if (isNaN(msg)) {
            if(sepcol)
			    $('#msg').html(now+':'+msg+'<br>'+$('#msg').html())
            else {
			    $('#msgcl').html(now+':'+msg+'<br>'+$('#msgcl').html());
                mcnt = $('#msgcl').html().length;
                kb = 1024;
                mb = 1048576;
                lines=$('#msgcl br').length;
                mcnt = (mcnt > mb ? (mcnt / mb).toFixed(2) +'MB': mcnt > kb ? (mcnt / kb).toFixed(2) + 'KB' : mcnt + 'B') + '/' + lines +'L';
                $('#cnt').html(mcnt)
            }
        }
		else
			$('#msg').html(now+':'+socket_status(msg)+'<br>'+$('#msg').html())
	}

	function socket_status(readyState){
		if (readyState == 0)
			return 'Socket status: ' + socket.readyState +' (Connecting)'
		else if (readyState == 1)
			return 'Socket status: ' + socket.readyState + ' (Open)'
		else if (readyState == 2)
			return 'Socket status: ' + socket.readyState + ' (Closing)'
		else if (readyState == 3)
			return 'Socket status: ' + socket.readyState +' (Closed)'
	}

	$('#disconnect').click(function(){
        send('close')
	});

",
(jsAll(?ROOMS,<<"ping">>))/binary,
(jsAllConfirm(?ROOMS,<<"reboot">>))/binary,
(jsAllConfirm(?ROOMS,<<"shutdown">>))/binary,
(jsAllConfirm(?ROOMS,<<"dfthaw">>))/binary,
(jsAllConfirm(?ROOMS,<<"dffreeze">>))/binary,
(jsAll(?ROOMS,<<"wake">>))/binary,
(jsAll(?ROOMS,<<"dfstatus">>))/binary,
(jsAll(?ROOMS,<<"net_restart">>))/binary,
(jsAll(?ROOMS,<<"net_stop">>))/binary,
(jsAll(?ROOMS,<<"loggedon">>))/binary,
(jsAll(?ROOMS,<<"copy">>))/binary,
(jsAll(?ROOMS,<<"com">>))/binary,
(mkjsAllSelect_copy(?ROOMS))/binary,
(mkjsSelect_copy(?ROOMS))/binary,
(mkjsAllSelect_com(?ROOMS))/binary,
(mkjsSelect_com(?ROOMS))/binary,
(mkjsSelectAllChk(?ROOMS))/binary,
(mkjsUnSelectAllChk(?ROOMS))/binary,
(mkjsToggleAllChk(?ROOMS))/binary,
(mkcomButtons(?ROOMS))/binary,
(mkjsComAll(?ROOMS,<<"ping">>))/binary,
(mkjsComAll(?ROOMS,<<"reboot">>))/binary,
(mkjsComAll(?ROOMS,<<"shutdown">>))/binary,
(mkjsComAll(?ROOMS,<<"wake">>))/binary,
(mkjsComAll(?ROOMS,<<"dfthaw">>))/binary,
(mkjsComAll(?ROOMS,<<"dffreeze">>))/binary,
(mkjsComAll(?ROOMS,<<"dfstatus">>))/binary,
(mkjsComAll(?ROOMS,<<"net_restart">>))/binary,
(mkjsComAll(?ROOMS,<<"net_stop">>))/binary,
(mkjsComAll(?ROOMS,<<"loggedon">>))/binary,
(mkjsComAll(?ROOMS,<<"copy">>))/binary,
(mkjsComAll(?ROOMS,<<"com">>))/binary,
(chk_dupe_usersa(?ROOMS))/binary,
(chk_dupe_users(?ROOMS))/binary,
(refresh_cons(?ROOMS))/binary,
(toggles(?ROOMS))/binary,
(rms_keys(Get_rms,Get_rms))/binary,
"

    interval_chk_dupes=setInterval(chk_dupe_users,60000);

}//End else - has websockets

	$('#smbig').click(function(){
        $('#big_msg').html($('#msg').html());
        $('#big_msg').toggle();
	});

    $('#sacs').click(function(){
        if($(':checkbox:lt(1)').is(':checked')){
//            $(':checkbox:lt(10)').removeAttr('checked');
//            $('#tinputs:checkbox:lt(2)').removeAttr('checked');
          $(':checkbox').removeAttr('checked');
        }
        else {
//            $(':checkbox:lt(10)').attr('checked', 'checked');
//            $('#tinputs:checkbox:lt(2)').attr('checked', 'checked');
          $(':checkbox').attr('checked', 'checked');  
      }
  
    });
});

</script>
 
</head>

<body>
<div id='wrapper'>

<div id='menu' class='fl'>

<div id='rooms_title' class='fl'>
[0]-Rooms 
</div>

<div id='switcher'>
",
(switcher(?ROOMS))/binary,
"
</div>

</div>

 <div class='brk'></div>

 <div id='commands'>

 <div id='com_title'>
 Commands
 </div>

 <div id='tcoms'>",
(case is_list(login_is()) of
	 true -> <<"<a href='esysman/logout' id='logout' class='button' />Logout</a><br>">>;
	 false -> <<"">>
 end)/binary,
"
<a href=# id='disconnect' class='button' />Disconnect</a>
<a href=# id='sacs' class='button lbar' />S/UnS All Coms</a>
<div class='brk'></div>
",
( mkAllRoomsComs([
				 {<<"ping">>,<<"Ping All">>},
				 {<<"reboot">>,<<"Reboot All">>},
				 {<<"shutdown">>,<<"Shutdown All">>},
				 {<<"wake">>,<<"Wake All">>},
				 {<<"dfthaw">>,<<"DeepFreeze Thaw All">>},
				 {<<"dffreeze">>,<<"DeepFreeze Freeze All">>},
				 {<<"dfstatus">>,<<"DeepFreeze Status All">>},
				 {<<"net_restart">>,<<"Restart Service All">>},
				 {<<"net_stop">>,<<"Stop Service All">>},
				 {<<"loggedon">>,<<"Logged On All">>}
				]))/binary,
"
 </div>

 <div id='tinputs'>
",
(mkAllRoomsComsInput({<<"copy">>,<<"Copy All">>}))/binary,
(mkAllRoomsComsInput({<<"com">>,<<"Com All">>}))/binary,
(mkAllRoomsSelectUnselectToggleAll(?ROOMS))/binary,
"
 </div>

 <div id='tmsgs' class='tmsgsc'>
   <div id='mtop' class='mtopc'> <a href='#' id='smbig' class='mbig'/>+</a> Server Messages (most recent at top):</div>
	 <div id='msg-div'>
	 <div id='msg' class='msgc'></div>
   </div>
 </div>

 <div id='tmsgscl' class='tmsgsc'>
   <div id='mtopcl' class='mtopc'>Client Messages (most recent at top): <div id='cnt'>0KB</div></div>
	 <div id='msg-divcl'>
	   <div id='msgcl' class='msgc'></div>
     </div>
 </div>

 <div id='tmsgsdup' class='tmsgsc'>
   <div id='mtopdup' class='mtopcd'>Duplicate Users (most recent at top):</div>
	 <div id='msg-div-dup'>
	 <div id='msgdup' class='msgcd'></div>
   </div>
 </div>

 </div>

 <div class='brk'></div>

 <div id='workstations'>

",
(mkRooms(?ROOMS))/binary,
"
 </div>
 </div>


<div id='big_msg'>
</div>

</body> 
</html>">>, Req3),
	{ok, Req4, State}. % main_page()

 %%

 init_open([Room|_]) ->
	 [Rm|_]=Room,
<<"
					  $('#",Rm/binary,"').show();
					  $('#",Rm/binary,"_coms').show();
					  $('#",Rm/binary,"_comsInputcopy').show();
					  $('#",Rm/binary,"_comsInputcom').show();

					  $('#",Rm/binary,"_selunseltogall').show();

                      $('#",Rm/binary,"toggle').click();
					  $('#",Rm/binary,"toggle').focus();

">>.

 %%

toggles([Room|Rooms]) ->
	<<(toggles_rm(Room))/binary,(toggles(Rooms))/binary>>;
toggles([]) ->
	<<>>.

toggles_rm([Rm|_]) ->
	<<"
	 $('#",Rm/binary,"toggle').click(function(){
",
	  (toggle_items(?ROOMS,Rm))/binary,
"
	 });
">>.

toggle_items([Room|Rooms],Rm) ->
	<<(toggle_item(Room,Rm))/binary,(toggle_items(Rooms,Rm))/binary>>;
toggle_items([],_) ->
	<<>>.

toggle_item([Room|_],Rm) ->
	case Room of
		Rm ->
			<< "
		 $('#",Rm/binary,"').show();
		 $('#",Rm/binary,"_coms').show();
		 $('#",Rm/binary,"_comsInputcopy').show();
		 $('#",Rm/binary,"_comsInputcom').show();
 	     $('#",Rm/binary,"_selunseltogall').show();
		 $('#",Rm/binary,"toggle').removeClass('rm_selected');
		 $('#",Rm/binary,"toggle').removeClass('rm_not_selected');
		 $('#",Rm/binary,"toggle').addClass('rm_selected');
">>;
		_ -> 
			<<"
		 $('#",Room/binary,"').hide();
		 $('#",Room/binary,"_coms').hide();
		 $('#",Room/binary,"_comsInputcopy').hide();
		 $('#",Room/binary,"_comsInputcom').hide();
	     $('#",Room/binary,"_selunseltogall').hide();
		 $('#",Room/binary,"toggle').removeClass('rm_selected');
		 $('#",Room/binary,"toggle').removeClass('rm_not_selected');
		 $('#",Room/binary,"toggle').addClass('rm_not_selected')

">>
	end;
toggle_item([],_) ->
	<<>>.

 %%

jsAll([Room|Rooms],Com) ->
	[Rm|_]=Room,
	<<(case Com of
		<<"com">>  -> ifcomcopy(Rm,Com);
		<<"copy">> -> ifcomcopy(Rm,Com);
		_ ->
			<<"

	 $('#",Com/binary,"All",Rm/binary,"').click(function(){
",Com/binary,"All",Rm/binary,"();
			 message(true,'",Com/binary," All ",Rm/binary,"...')
	 });">>
	end)/binary,(jsAll(Rooms,Com))/binary>>;
jsAll([],_) ->
	<<>>.

 %%

ifcomcopy(Rm,Com) ->
<<"
	 $('#",Com/binary,"All",Rm/binary,"').click(function(){
		 if($('#",Com/binary,"AllInput",Rm/binary,"').val().length){
			 ",Com/binary,"All",Rm/binary,"();
			 message(true,'",Com/binary," All ",Rm/binary,"...')
		 } else {
			 $('#",Com/binary,"AllInput",Rm/binary,"').val('!');
			 message(true,'",Com/binary," All ",Rm/binary," is blank!')
		 }
	 });

">>.

 %%

jsAllConfirm([Room|Rooms],Com) ->
	[Rm|_]=Room,
	<<"

	 $('#",Com/binary,"All",Rm/binary,"').click(function(){
		 rall=confirm('",Com/binary," All Systems ",Rm/binary,"?');
		 if (rall==true)
			 ",Com/binary,"All",Rm/binary,"()
		 else
			 message(true,'",Com/binary," All in ",Rm/binary," aborted...')
	 });

",(jsAllConfirm(Rooms,Com))/binary>>;
jsAllConfirm([],_) ->
	<<>>.

 %%

mkjsAllSelect_copy([Room|Rooms]) ->
	<<(mkjsAllSelectRm_copy(Room))/binary,(mkjsAllSelect_copy(Rooms))/binary>>;
mkjsAllSelect_copy([]) ->
	<<>>.

mkjsAllSelectRm_copy([Room|Rows]) ->
	<<"

 $('#copyAllSelect",Room/binary,"').change(function(){

	 $('#copyAllInput",Room/binary,"').val($('#copyAllSelect",Room/binary," option:selected').text());
	 ",(jsAllSelectRows_copy(Room,Rows))/binary,"
 });

 ">>.

jsAllSelectRows_copy(Room,[Row|Rows]) ->
	<<(jsAllSelect_copy(Room,Row))/binary,(jsAllSelectRows_copy(Room,Rows))/binary>>;
jsAllSelectRows_copy(_Room,[]) ->
	<<>>.

jsAllSelect_copy(Rm,[{Wk,_FQDN,_MacAddr,_Os}|Wks]) ->
	case Wk of
		<<".">> ->	jsAllSelect_copy(Rm,Wks);
		_ ->
			<<"
	 if(
		 ($('#copyAll",Rm/binary,"check').prop('checked') && $('#",Wk/binary,"check').prop('checked')) ||
		 (!$('#copyAll",Rm/binary,"check').prop('checked') && 
			 (!$('#",Wk/binary,"check').prop('checked') || $('#",Wk/binary,"check').prop('checked')))
	   )
		 $('#copyfn_",Wk/binary,"').val($('#copyAllInput",Rm/binary,"').val());
 ",(jsAllSelect_copy(Rm,Wks))/binary>>
	end;
jsAllSelect_copy(_Room,[]) ->
	<<>>.

 %%

mkjsSelect_copy([Room|Rooms]) ->
	<<(mkjsSelectRm_copy(Room))/binary,(mkjsSelect_copy(Rooms))/binary>>;
mkjsSelect_copy([]) ->
	<<>>.

mkjsSelectRm_copy([_Room|Rows]) ->
	jsSelectRows_copy(Rows).

jsSelectRows_copy([Row|Rows]) ->
	<<(jsSelect_copy(Row))/binary,(jsSelectRows_copy(Rows))/binary>>;
jsSelectRows_copy([]) ->
	<<>>.

jsSelect_copy([{Wk,_FQDN,_MacAddr,_Os}|Wks]) ->
	case Wk of
		<<".">> ->	jsSelect_copy(Wks);
		_ ->
			<<"

 $('#copyselect",Wk/binary,"').change(function(){
	 $('#copyfn_",Wk/binary,"').val($('#copyselect",Wk/binary," option:selected').text());
 });

 ",(jsSelect_copy(Wks))/binary>>
	end;
jsSelect_copy([]) ->
	<<>>.

 %%

mkjsAllSelect_com([Room|Rooms]) ->
	<<(mkjsAllSelectRm_com(Room))/binary,(mkjsAllSelect_com(Rooms))/binary>>;
mkjsAllSelect_com([]) ->
	<<>>.

mkjsAllSelectRm_com([Room|Rows]) ->
	<<"

 $('#comAllSelect",Room/binary,"').change(function(){

	 $('#comAllInput",Room/binary,"').val($('#comAllSelect",Room/binary," option:selected').text());
	 ",(jsAllSelectRows_com(Room,Rows))/binary,"
 });

">>.

jsAllSelectRows_com(Room,[Row|Rows]) ->
	<<(jsAllSelect_com(Room,Row))/binary,(jsAllSelectRows_com(Room,Rows))/binary>>;
jsAllSelectRows_com(_Room,[]) ->
	<<>>.

jsAllSelect_com(Rm,[{Wk,_FQDN,_MacAddr,_Os}|Wks]) ->
	case Wk of
		<<".">> ->	jsAllSelect_com(Rm,Wks);
		_ ->
<<"
	 if(
		 ($('#comAll",Rm/binary,"check').prop('checked') && $('#",Wk/binary,"check').prop('checked')) ||
		 (!$('#comAll",Rm/binary,"check').prop('checked') && 
			 (!$('#",Wk/binary,"check').prop('checked') || $('#",Wk/binary,"check').prop('checked')))
	   )
		 $('#comstr_",Wk/binary,"').val($('#comAllInput",Rm/binary,"').val());
 ",(jsAllSelect_com(Rm,Wks))/binary>>
	end;
jsAllSelect_com(_Room,[]) ->
	<<>>.
 
%%

mkjsSelect_com([Room|Rooms]) ->
	<<(mkjsSelectRm_com(Room))/binary,(mkjsSelect_com(Rooms))/binary>>;
mkjsSelect_com([]) ->
	<<>>.

mkjsSelectRm_com([_Room|Rows]) ->
	jsSelectRows_com(Rows).

jsSelectRows_com([Row|Rows]) ->
	<<(jsSelect_com(Row))/binary,(jsSelectRows_com(Rows))/binary>>;
jsSelectRows_com([]) ->
	<<>>.

jsSelect_com([{Wk,_FQDN,_MacAddr,_Os}|Wks]) ->
	case Wk of
		<<".">> ->	jsSelect_com(Wks);
		_ ->
<<"

 $('#comselect",Wk/binary,"').change(function(){
	 $('#comstr_",Wk/binary,"').val($('#comselect",Wk/binary," option:selected').text());
 });

 ",(jsSelect_com(Wks))/binary>>
	end;
jsSelect_com([]) ->
	<<>>.

%%

mkjsSelectAllChk([Room|Rooms]) ->
	[Rm|_]=Room,
	<<"
 $('#selectAll",Rm/binary,"').click(function(){
     $('#",Rm/binary," input:checkbox').each(function() {
         $(this).attr('checked','checked');
     });
 });

",(mkjsSelectAllChk(Rooms))/binary>>;
mkjsSelectAllChk([]) ->
	<<>>.

%%

mkjsUnSelectAllChk([Room|Rooms]) ->
	[Rm|_]=Room,
	<<"
 $('#unselectAll",Rm/binary,"').click(function(){
     $('#",Rm/binary," input:checkbox').each(function() {
         $(this).removeAttr('checked');
     });
 });

",(mkjsUnSelectAllChk(Rooms))/binary>>;
mkjsUnSelectAllChk([]) ->
	<<>>.

%%

mkjsToggleAllChk([Room|Rooms]) ->
	[Rm|_]=Room,
	<<"
 $('#toggleAll",Rm/binary,"').click(function(){
     $('#",Rm/binary," input:checkbox').each(function() {
         $(this).attr('checked',!$(this).attr('checked'));
     });
 });

",(mkjsToggleAllChk(Rooms))/binary>>;
mkjsToggleAllChk([]) ->
	<<>>.

%%

mkAllRoomsComs(Coms) ->
	mkARComs(?ROOMS,Coms).

mkARComs([Room|Rooms],Coms) ->
	[Rm|_]=Room,
	<<"<div id='",Rm/binary,"_coms' class='room'>",(mkARComsComs(Rm,Coms))/binary,"</div>",(mkARComs(Rooms,Coms))/binary>>;
mkARComs([],_Coms) ->
	<<>>.

mkARComsComs(Rm,[{Com,ComText}|Coms]) ->
	<<"

 <div class='fl'>
 <input id='",Com/binary,"All",Rm/binary,"check' type='checkbox' class='checkbox' /></a>
  <a href='#' id='",Com/binary,"All",Rm/binary,"' class='button'/>",ComText/binary,"</a>
 </div>
 <div class='brk'></div>

",(mkARComsComs(Rm,Coms))/binary>>;
mkARComsComs(_Rm,[]) ->
	<<>>.

%%

 mkAllRoomsComsInput(Com) ->
	 mkARComsInput(?ROOMS,Com).

 mkARComsInput([Room|Rooms],ComT) ->
	 {Com,ComText}=ComT,
	 [Rm|_]=Room,
<<"

 <div id='",Rm/binary,"_comsInput",Com/binary,"' class='room'>
	 ",(mkARComsComsInput(Rm,ComT))/binary,"
 </div>

",(mkARComsInput(Rooms,{Com,ComText}))/binary>>;
mkARComsInput([],_Com) ->
	<<>>.

mkARComsComsInput(Rm,{Com,ComText}) ->
	<<"

 <div class='fl'>
 <input id='",Com/binary,"All",Rm/binary,"check' type='checkbox' class='checkbox' /></a>
  <a href='#' id='",Com/binary,"All",Rm/binary,"' class='button' />",ComText/binary,"</a>
 <div class='brk'></div>

 <select id='",Com/binary,"AllSelect",Rm/binary,"' class='fl'>
	 ",

	  (case Com of
		   <<"copy">> ->
			   selections(?APPS);
		   <<"com">> ->
			   selections(?COMS)
	   end)/binary,
"
 </select>
<br>
  <input id='",Com/binary,"AllInput",Rm/binary,"' type='text', name='",Com/binary,"AllInput' class='fl'/>

 </div>
 ">>.

%%

mkAllRoomsSelectUnselectToggleAll([Room|Rooms]) ->
	 [Rm|_]=Room,
	 <<"
 <div class='brk'></div>

 <div id='",Rm/binary,"_selunseltogall' class='room'>
<br>
	 ",(mkselunseltogAll(Rm))/binary,"
 </div>

 ",(mkAllRoomsSelectUnselectToggleAll(Rooms))/binary>>;
 mkAllRoomsSelectUnselectToggleAll([]) ->
	<<>>.

mkselunseltogAll(Rm) ->
	<<"
  <a href='#' id='selectAll",Rm/binary,"' class='button' />Select All</a><br>
  <a href='#' id='unselectAll",Rm/binary,"' class='button' />UnSelect All</a><br>
  <a href='#' id='toggleAll",Rm/binary,"' class='button' />Toggle All</a><br>
">>.

 %%

mkRooms([Room|Rooms]) ->
	<<(mkRoom(Room))/binary,(mkRooms(Rooms))/binary>>;
mkRooms([]) ->
	<<>>.

mkRoom([Room|Rows]) ->
	<<"

 <div id='",Room/binary,"' class='room'>
 ",(mkRoomRows(Rows,Room,1))/binary,"

 </div>

 ">>.

mkRoomRows([Row|Rows],Rm,RowCnt) ->
	<<"
 <div id='",Rm/binary,"_row_",(list_to_binary(integer_to_list(RowCnt)))/binary,"'>",
	  (divhc(Rm,Row,1))/binary,
"
 </div>
 <div class='brk'></div>
 <div>",
	  << <<(divc(Wks))/binary>> || Wks <- Row >>/binary,
"
 </div>
 <div class='brk'></div>"
	 ,(mkRoomRows(Rows,Rm,RowCnt+1))/binary>>;
 mkRoomRows([],_Rm,_RowCnt) ->
	 <<>>.

divhc(Rm,[{Wk,FQDN,MacAddr,_Os}|Wks],ColCnt) ->
	<<(case Wk of
		 <<".">> ->	<<"<div class='hltd'>.</div>">>;
			_ ->
			   <<"

<div id='",Wk/binary,"_hltd' class='hltd ",Rm/binary,"_col_",(list_to_binary(integer_to_list(ColCnt)))/binary,"'>

<div id='",Wk/binary,"status' class='status'>.</div>

<div class='wkchk'><input id='",Wk/binary,"check' type='checkbox' class='checkbox' /></div></a><div class='wk'>",FQDN/binary,"</div>

<div class='brk'></div>

<div id='",Wk/binary,"macaddr' class='macaddr'>",MacAddr/binary,"</div> <div id='",Wk/binary,"dfstatus' class='dfstatus'>DF?</div>

</div>

">>
	  end)/binary,(divhc(Rm,Wks,ColCnt+1))/binary>>;
divhc(_Rm,[],_ColCnt) ->
	<<>>.

divc({Wk,_FQDN,_MacAddr,_Os}) ->
%	<<"<div class='ltd'>.</div>">>;
%divc({0,_FQDN,_MacAddr,_Os}) ->
%Wk= <<".">>,
	case Wk of
		<<".">> ->	<<"<div class='ltd'>.</div>">>;
		   _ ->
<<"
<div id='",Wk/binary,"_ltd' class=\"ltd\">
<div id='",Wk/binary,"_ccell'>

<div class=\"lc\">
 <a href='#' id='ping_",Wk/binary,"' class='button' />P</a>
 <a href='#' id='reboot_",Wk/binary,"' class='button' />R</a>
 <a href='#' id='shutdown_",Wk/binary,"' class='button' />S</a>
 <a href='#' id='wake_",Wk/binary,"' class='button' />WOL</a>
 <a href='#' id='dffreeze_",Wk/binary,"' class='button' />DFF</a>
 <a href='#' id='dfthaw_",Wk/binary,"' class='button' />DFT</a>
 <a href='#' id='dfstatus_",Wk/binary,"' class='button' />DFS</a>
 <a href='#' id='net_restart_",Wk/binary,"' class='button' />ReS</a>
 <a href='#' id='net_stop_",Wk/binary,"' class='button' />StS</a>
 <a href='#' id='loggedon_",Wk/binary,"' class='button' />L</a>
 <a href='#' id='",Wk/binary,"_col' class='cols'>C</a>
</div>
<div class='brk'></div>
<div>
 <a href='#' id='copy_",Wk/binary,"' class='button' />Copy</a><br>

 <input id='copyfn_",Wk/binary,"' type='text'/>

<select id='copyselect",Wk/binary,"'>
",
       (selections(?APPS))/binary,
"
</select>

</div>

<div>

<a href='#' id='com_",Wk/binary,"' class='button' />Com</a><br>

<input id='comstr_",Wk/binary,"' type='text'/>

<select id='comselect",Wk/binary,"'>
",
        (selections(?COMS))/binary,
"
</select>
</div>
</div>
</div>
">>
	end.

%

selections([Com|Coms]) ->
<<"
<option value='",Com/binary,"'>",Com/binary,"</option>
",(selections(Coms))/binary>>;
selections([]) ->
<<>>.
	
%

mkcomButtons([Room|Rooms]) ->
	<<(comButtonsRm(Room))/binary,(mkcomButtons(Rooms))/binary>>;
mkcomButtons([]) ->
	<<>>.

comButtonsRm([Room|Rows]) ->
    comButtonsRows(Rows,Room,1).

comButtonsRows([Row|Rows],Rm,RowCnt) ->
	<<(comButtons(Row,Rm,RowCnt,1))/binary,(comButtonsRows(Rows,Rm,RowCnt+1))/binary>>;
comButtonsRows([],_Rm,_RowCnt) ->
	<<>>.

comButtons([{Wk,FQDN,MacAddr,_Os}|Wks],Rm,RowCnt,ColCnt) ->
	case Wk of
		<<".">> -> << (comButtons(Wks,Rm,RowCnt,ColCnt+1))/binary >>;
		_ ->
	<<"

    $('#",Wk/binary,"_col').click(function(){
        $('.",Rm/binary,"_col_",(list_to_binary(integer_to_list(ColCnt)))/binary," input:checkbox').each(function() {
           $(this).attr('checked',!$(this).attr('checked'));
       });
	});

    $('#",Wk/binary,"status').click(function(){
        $('#",Rm/binary,"_row_",(list_to_binary(integer_to_list(RowCnt)))/binary," input:checkbox').each(function() {
           $(this).attr('checked',!$(this).attr('checked'));
       });
	});

	$('#reboot_",Wk/binary,"').click(function(){
        r=false;
        if (rall==false)
            r=confirm('Reboot ",Wk/binary,"?');
        if (r==true || rall==true){
   		    send('",FQDN/binary,":reboot:0');
		    message(true,'Rebooting ",Wk/binary,"...')
        } else
		    message(true,'Reboot of ",Wk/binary," aborted...')
	});

	$('#shutdown_",Wk/binary,"').click(function(){
        r=false;
        if (rall==false)
            r=confirm('Shutdown ",Wk/binary,"?');
        if (r==true || rall==true){
		    send('",FQDN/binary,":shutdown:0');
		    message(true,'Shutting down ",Wk/binary,"...');
        } else
		    message(true,'Shutdown of ",Wk/binary," aborted...')
	});

	$('#wake_",Wk/binary,"').click(function(){
		send('",FQDN/binary,":wol:",MacAddr/binary,"');
		message(true,'Waking ",Wk/binary,"...')
	});

	$('#ping_",Wk/binary,"').click(function(){
		send('",FQDN/binary,":ping:0');
		message(true,'Pinging ",Wk/binary,"...');
	});

	$('#net_restart_",Wk/binary,"').click(function(){
		send('",FQDN/binary,":net_restart:0');
		message(true,'Restarting service on ",Wk/binary,"...')
	});

	$('#net_stop_",Wk/binary,"').click(function(){
		send('",FQDN/binary,":net_stop:0');
		message(true,'Stopping service on ",Wk/binary,"...')
	});


	$('#dffreeze_",Wk/binary,"').click(function(){
        r=false;
        if (rall==false)
            r=confirm('Freeze ",Wk/binary,"?');
        if (r==true || rall==true){
   		    send('",FQDN/binary,":dffreeze:0');
		    message(true,'Freezing ",Wk/binary,"...')
            $('#",Wk/binary,"status').html('.');
        } else
		    message(true,'Freeze of ",Wk/binary," aborted...')
	});

	$('#dfthaw_",Wk/binary,"').click(function(){
        r=false;
        if (rall==false)
            r=confirm('Thaw ",Wk/binary,"?');
        if (r==true || rall==true){
   		    send('",FQDN/binary,":dfthaw:0');
		    message(true,'Thawing ",Wk/binary,"...')
            $('#",Wk/binary,"status').html('.');
        } else
		    message(true,'Thaw of ",Wk/binary," aborted...')
	});

	$('#dfstatus_",Wk/binary,"').click(function(){
		send('",FQDN/binary,":dfstatus:0');
		message(true,'DF Status sent ",Wk/binary,"...')
	});

	$('#loggedon_",Wk/binary,"').click(function(){
		send('",FQDN/binary,":loggedon:0');
		message(true,'loggedon sent ",Wk/binary,"...')
	});

	$('#copy_",Wk/binary,"').click(function(){
        if($('#copyfn_",Wk/binary,"').val().length){
		    send('",FQDN/binary,":copy:' + $('#copyfn_",Wk/binary,"').val());
		    message(true,'Copy sent ",Wk/binary,"...')
        } else {
            $('#copyfn_",Wk/binary,"').val('!');
		    message(true,'Copy file name blank! ",Wk/binary,"...')
        }
	});

	$('#com_",Wk/binary,"').click(function(){
        if($('#comstr_",Wk/binary,"').val().length){
		    send('",FQDN/binary,":com:' + $('#comstr_",Wk/binary,"').val());
		    message(true,'Command sent ",Wk/binary,"...')
        } else {
            $('#comstr_",Wk/binary,"').val('!');
		    message(true,'Command is blank! ",Wk/binary,"...')
        }
	});

",(comButtons(Wks,Rm,RowCnt,ColCnt+1))/binary>>
	end;

comButtons([],_Rm,_RowCnt,_ColCnt) ->
	<<>>.

%%

mkjsComAll([Room|Rooms],Com) ->
   <<(mkjsComAllRm(Room,Com))/binary,(mkjsComAll(Rooms,Com))/binary>>;
mkjsComAll([],_Com) ->
	<<>>.

mkjsComAllRm([Rm|Rows],Com) ->
<<"

function ",Com/binary,"All",Rm/binary,"(){
",(mkjsComAllRows(Rows,Rm,Com))/binary,"
    rall=false;
}

">>.

mkjsComAllRows([Row|Rows],Rm,Com) ->
	<<(mkjsComAllRow(Row,Rm,Com))/binary,(mkjsComAllRows(Rows,Rm,Com))/binary>>;
mkjsComAllRows([],_Rm,_Com) ->
    <<>>.

mkjsComAllRow([{Wk,_FQDN,_MacAddr,_Os}|Wks],Rm,Com) ->
	case Wk of
		<<".">> ->
			mkjsComAllRow(Wks,Rm,Com);
		_ ->
			<<(case Com of
				   <<"copy">> ->
<<"
    if(
        ($('#",Com/binary,"All",Rm/binary,"check').prop('checked') && $('#",Wk/binary,"check').prop('checked')) ||
        (!$('#",Com/binary,"All",Rm/binary,"check').prop('checked') && 
            (!$('#",Wk/binary,"check').prop('checked') || $('#",Wk/binary,"check').prop('checked')))
      ){
	    $('#copyfn_",Wk/binary,"').val($('#copyAllInput",Rm/binary,"').val());
        $('#copy_",Wk/binary,"').click();
    }
">>;
				   _  ->
<<"
    if(
        ($('#",Com/binary,"All",Rm/binary,"check').prop('checked') && $('#",Wk/binary,"check').prop('checked')) ||
        (!$('#",Com/binary,"All",Rm/binary,"check').prop('checked') && 
            (!$('#",Wk/binary,"check').prop('checked') || $('#",Wk/binary,"check').prop('checked')))
      )
        $('#",Com/binary,"_",Wk/binary,"').click();
">>
			   end)/binary,(mkjsComAllRow(Wks,Rm,Com))/binary>>
	end;
mkjsComAllRow([],_Rm,_Com) ->
	<<>>.

%%

init2([Room|Rooms],Ref_cons_time) ->	
	<<(init2_rm(Room,Ref_cons_time))/binary,(init2(Rooms,Ref_cons_time))/binary>>;
init2([],_) ->
    <<>>.

init2_rm([Rm|_],Ref_cons_time) ->
<<"
                     interval_",Rm/binary,"_ref_cons=setInterval(refresh_cons_",Rm/binary,",",(list_to_binary(integer_to_list(Ref_cons_time)))/binary,");

">>.

%%

get_rms_keys([Room|Rooms],Key) ->
	[Rm|_]=Room,
	[{Rm,Key}|get_rms_keys(Rooms,Key+1)];
get_rms_keys([],_) ->
	[].

rms_keys([{Rm,_}|Rms],Rms_ks) ->
	<<"
    $('#",Rm/binary,"toggle').keydown(function(event) {
",
(loop_rms_keys(Rms_ks))/binary,
"
    });

",(rms_keys(Rms,Rms_ks))/binary>>;
rms_keys([],_) ->
	<<>>.

%

loop_rms_keys([Rm|Rms]) ->
	<<(loop_rm_keys(Rm))/binary,(loop_rms_keys(Rms))/binary>>;
loop_rms_keys([]) ->
	<<>>.

loop_rm_keys({Rm,Key}) ->
<<"
        if (event.which == ",(list_to_binary(integer_to_list(Key)))/binary,"){
            event.preventDefault();
            $('#",Rm/binary,"toggle').click();
        }
">>.

%

chk_dupe_usersa(Rooms) ->
<<"
function  chk_dupe_users(){
        tot_cnt=0;
",
(chk_dupe_users_rms(Rooms))/binary,
"
}
">>.

chk_dupe_users_rms([Room|Rooms]) ->
	<<(jschkduRma(Room))/binary,(chk_dupe_users_rms(Rooms))/binary>>;
chk_dupe_users_rms([]) ->
	<<>>.

jschkduRma([Rm|_Rows]) ->
	<<"
    chk_dupe_users_",Rm/binary,"();

">>.

%

chk_dupe_users([Room|Rooms]) ->
	<<(jschkduRm(Room))/binary,(chk_dupe_users(Rooms))/binary>>;
chk_dupe_users([]) ->
	<<>>.

jschkduRm([Rm|Rows]) ->
	<<"

function chk_dupe_users_",Rm/binary,"(){
    var dupe_",Rm/binary,"=[];

    var hash_",Rm/binary," = [];

	var ",Rm/binary,"cnt=0;
    

",
(jschkduRows(Rows,Rm))/binary,
"

    for (var key in hash_",Rm/binary,"){
        if (hash_",Rm/binary,".hasOwnProperty(key) && hash_",Rm/binary,"[key].length > 1)
            $('#msgdup').html(key+':['+hash_",Rm/binary,"[key]+']<br>'+$('#msgdup').html())
    }

    $('#",Rm/binary,"toggle').html('['+((",Rm/binary,"cnt>0)?",Rm/binary,"cnt:0).toString()+']-",Rm/binary,"');

}

">>.

jschkduRows([Row|Rows],Rm) ->
	<<(jschkduRow(Row,Rm))/binary,(jschkduRows(Rows,Rm))/binary>>;
jschkduRows([],_Rm) ->
    <<>>.

jschkduRow([{Wk,_FQDN,_MacAddr,_Os}|Wks],Rm) ->
	case Wk of
		<<".">> ->	jschkduRow(Wks,Rm);
		   _ ->
<<"

    if ($('#",Wk/binary,"status').html()!='.'){
        dupe_",Rm/binary,".push($('#",Wk/binary,"status').html().toLowerCase());
        if (typeof hash_",Rm/binary,"[dupe_",Rm/binary,"[dupe_",Rm/binary,".length-1]] === 'undefined')
            hash_",Rm/binary,"[dupe_",Rm/binary,"[dupe_",Rm/binary,".length-1]] = [];
        hash_",Rm/binary,"[dupe_",Rm/binary,"[dupe_",Rm/binary,".length-1]].push('",Wk/binary,"');
        ",Rm/binary,"cnt++;
        tot_cnt++;
        $('#rooms_title').html('['+tot_cnt.toString()+']-'+'Rooms:');
    }
",(jschkduRow(Wks,Rm))/binary>>
	end;
jschkduRow([],_Rm) ->
	<<>>.

%

switcher([Room|Rooms]) ->
	<<(switcher_rm(Room))/binary,(switcher(Rooms))/binary>>;
switcher([]) ->
	<<>>.

switcher_rm([Rm|_Rows]) ->
	<<"
<a href='#' id='",Rm/binary,"toggle' class='button1' />[0]-",Rm/binary,"</a>
">>.

%

refresh_cons([Room|Rooms]) ->
	<<(jsrefcons_rm(Room))/binary,(refresh_cons(Rooms))/binary>>;
refresh_cons([]) ->
	<<>>.

jsrefcons_rm([Rm|Rows]) ->
	<<"

function refresh_cons_",Rm/binary,"(){
",
(jsrefcons_rows(Rows,Rm))/binary,
"
}
">>.

jsrefcons_rows([Row|Rows],Rm) ->
	<<(jsrefcons_row(Row,Rm))/binary,(jsrefcons_rows(Rows,Rm))/binary>>;
jsrefcons_rows([],_Rm) ->
    <<>>.

jsrefcons_row([{Wk,_FQDN,_MacAddr,_Os}|Wks],Rm) ->
	case Wk of
		<<".">> ->	jsrefcons_row(Wks,Rm);
		   _ ->
<<"

		$('#",Wk/binary,"_hltd').css('background-color','#000');
		$('#",Wk/binary,"_ltd').css('background-color','#000');
		$('#",Wk/binary,"dfstatus').css('color','cyan');
		$('#",Wk/binary,"dfstatus').css('background-color','#006666');
		$('#",Wk/binary,"status').css('color','red');
		$('#",Wk/binary,"status').css('background-color','#550000');
        $('#",Wk/binary,"status').html('.');

",(jsrefcons_row(Wks,Rm))/binary>>
	end;
jsrefcons_row([],_Rm) ->
	<<>>.

%

now_bin() ->
	{N1,N2,N3}=now(),
	list_to_binary(integer_to_list(N1)++integer_to_list(N2)++integer_to_list(N3)).
