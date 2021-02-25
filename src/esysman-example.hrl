-define(BROADCAST_ADDR, {192,168,0,255}).
-define(SERVERS,['ecom@hostanme']).
-define(CONSOLES, [hanwebs, hanwebs2, hanwebs3, hanwebs4]).
-define(APPS, [<<"">>,<<"any.cmd">>,<<"any.exe">>,<<"any.msi">>,<<"any.msp">>,<<"any.reg">>,<<"ecom.beam">>,<<"NiniteOne.exe">>,<<"ninite.cmd">>,<<"WUInstall.exe">>]).
-define(COMS, [<<"">>,<<"anycmd">>,<<"listupfls">>,<<"mkuploads">>,<<"ninite">>,<<"ninitecmd">>,<<"ninitelog">>,<<"wuinstall">>,<<"wuilog">>,<<"unamea">>,<<"lsbver">>,<<"yumcheck">>,<<"yumupdate">>,<<"ubuntuver">>,<<"aptcheck">>,<<"aptupgrade-list">>,<<"aptupgrade">>,<<"aptdistupgrade-list">>,<<"aptdistupgrade">>,<<"aptulog">>,<<"yumcheck">>,<<"yumupdate">>,<<"dnfcheck">>,<<"dnfupdate">>,<<"osxsupdate">>,<<"osxsulog">>]).
-define(CONF,"/usr/local/src/esysman/src/esysman.conf").
-define(PASSWDCONF,"/usr/local/src/esysman/src/passwd.conf").
-define(AUTOSHUTDOWNCONF,"/home/user/erl/esysman/src/autoshutdown.conf").

-define(ROOMS,"/path/to/rooms.conf").
-define(UPLOADS,<<"/usr/local/src/uploads/">>).
-define(DOWNLOADS,<<"/home/user/erl/esysman/_rel/esysman/lib/esysman-version/priv/downloads/">>).

-define(IGNOREDUPES,"box1").
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
-define(JQUERY, "/static/jquery-3.5.1.min.js").
-define(JQUERYUI, "/static/jquery-ui-1.12.1/jquery-ui.min.js").
-define(JQUERYUICSS, "/static/jquery-ui-1.12.1/jquery-ui.min.css").
-define(LINES, "10000").

