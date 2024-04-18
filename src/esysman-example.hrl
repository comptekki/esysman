-define(BROADCAST_ADDR, {192,168,0,255}).
-define(NODENAME,"ecom").
-define(SERVERS,['ecom@hostanme']).
-define(CONSOLES, [hanwebs, hanwebs2, hanwebs3, hanwebs4]).
-define(APPS, [<<"">>,<<"any.cmd">>,<<"any.exe">>,<<"any.msi">>,<<"any.msp">>,<<"any2.exe">>,<<"any2.msi">>,<<"any2.msp">>,<<"any.reg">>,<<"ecom.beam">>,<<"ecom.conf">>,<<"NiniteOne.exe">>,<<"ninite.cmd">>]).
-define(COMS, [<<"">>,<<"anycmd">>,<<"viewanycmd">>,<<"sqllastentrycmd">>,<<"listupfls">>,<<"mkuploads">>,<<"ninite">>,<<"ninitecmd">>,<<"ninitelog">>,<<"unamea">>,<<"lsbver">>,<<"yumcheck">>,<<"yumupdate">>,<<"ubuntuver">>,<<"aptcheck">>,<<"aptupgrade-list">>,<<"aptupgrade">>,<<"aptdistupgrade-list">>,<<"aptdistupgrade">>,<<"aptulog">>,<<"yumcheck">>,<<"yumupdate">>,<<"dnfcheck">>,<<"dnfupdate">>,<<"osxsupdate">>,<<"osxsulog">>]).
-define(CONF,"/home/user/erl/esysman/src/esysman.conf").
-define(PASSWDCONF,"/home/user/erl/esysman/src/passwd.conf").
-define(AUTOSHUTDOWNCONF,"/home/user/erl/esysman/src/autoshutdown.conf").
-define(WKSCONF,"/home/user/erl/esysman/src/wksconf/").
-define(TIMERSCONF,"/home/user/erl/esysman/src/timers.conf").
-define(TIMERREFFILE,"/home/user/erl/esysman/src/timerref.conf").

-define(ROOMS,"/path/to/rooms.conf").
-define(UPLOADS,<<"/home/user/erl/uploads/">>).
-define(DOWNLOADS,<<"/home/user/erl/esysman/_rel/esysman/lib/esysman-version/priv/downloads/">>).

-define(IGNOREREBOOT,"box1").
-define(IGNORESHOWUSERS,"box1").
-define(IGNOREUSERS,"u1:Default2:default2").
-define(IGNOREUSERS2,<<"u1:Default2:default2">>).
-define(IGNOREU1,"u1").
-define(IGNOREU2,"u2").
-define(IGNOREU3,"u3").
-define(IGNOREU4,"u4").

-define(REFRESHTIME,300000). % 300000 = 5 minutes
-define(CHKDUPETIME,60000).

-define(MAXAGE, 500).
-define(TITLE, "ESysMan").
-define(CSS, "/static/esysman.css").
-define(JQUERY, "/static/jquery.min.js").
-define(JQUERYUI, "/static/jquery-ui-1.12.1/jquery-ui.min.js").
-define(JQUERYUICSS, "/static/jquery-ui-1.12.1/jquery-ui.min.css").
-define(LINES, "10000").

