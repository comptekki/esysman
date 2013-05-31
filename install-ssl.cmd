@rem
@rem sample erlang install service script
@rem - add then start ecom erlang service
@rem - if it already exists, stop then remove first before adding and starting
@rem 

@set erl_service=0ecom
@erlsrv list %erl_service% | @findstr /i "%erl_service%"
@if errorlevel 1 goto install
@echo.
@rem echo Removing erl service...
@erlsrv remove "%erl_service%"

:install
@set module=-s ecom
@set onfail=-onfail restart
@set node=-name ecom@host.domain
@set root=-w "C:/windows/erl"
@set srvc_name=-i 0ecom
@set boot=-boot c:/windows/erl/start_ssl_ecom -proto_dist inet_tls -ssl_dist_opt server_certfile c:/windows/erl/ssl/cert.pem -ssl_dist_opt server_keyfile c:/windows/erl/ssl/key.pem -ssl_dist_opt server_secure_renegotiate true client_secure_renegotiate true
@set args=-args "%boot% %module%"

@echo.
@rem echo Adding erl service...
@erlsrv add "%erl_service%" %node% %root% %onfail% %args% %srvc_name%
@echo.
@rem echo Starting erl service...
@erlsrv start %erl_service%
@echo.
@pause