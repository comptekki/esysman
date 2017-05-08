@echo off

@rem
@rem sample erlang install service script
@rem - add then start ecom erlang service
@rem - if it already exists, stop then remove first before adding and starting
@rem 

@set erl_service=0ecom
@erlsrv list %erl_service% | @findstr /i "%erl_service%"
@if errorlevel 1 goto next
@echo.
@echo Removing erl service...
@erlsrv remove "%erl_service%"

:next

@FOR /F %%s IN ('powershell -command "(get-item env:'computername').Value.ToLower()"') DO @set comp=%%s

@set module=-s ecom
@set onfail=-onfail restart
@set node=-name ecom@%comp%.nr.usu.edu
@rem echo %node%
@set root=-w "C:/windows/erl"
@set srvc_name=-i 0ecom
@set boot=-boot c:/windows/erl/start_ssl_ecom -proto_dist inet_tls -ssl_dist_opt server_certfile c:/windows/erl/ssl/cert.pem -ssl_dist_opt server_keyfile c:/windows/erl/ssl/key.pem -ssl_dist_opt server_secure_renegotiate true client_secure_renegotiate true
@rem set args=-args "%boot% %module%"
@set args=-args "%boot% %module%"

@echo.
@echo Adding erl service...
@erlsrv add "%erl_service%" %node% %root% %onfail% %args%
@echo.
@rem echo Starting erl service...
@erlsrv start %erl_service%
@echo.


@echo set service auto-delay
@FOR /F "delims=" %%i IN ('sc getkeyname 0ecom') DO set z=%%i
@for /f "tokens=3" %%i IN ("%z%") DO set erl_servicedispname=%%i

@echo.

@sc config %erl_servicedispname% start= delayed-auto

@echo.
@echo setting auto delay to 1 second...

@reg add HKLM\SYSTEM\CurrentControlSet\Control /v AutoStartDelay /t REG_DWORD /d 1

@echo.
@pause


