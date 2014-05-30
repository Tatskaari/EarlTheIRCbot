{application, earl,
 [{description, "Erlang based IRC Bot"},
  {vsn, "1"},
  {modules, [earl, earl_sup, earlConnection, ircParser, ircSetup, ircTime, logger, 
  	    messageRouter, ning, optimusPrime, reminder, settingsServer, telnet]},
  {registered, [sendPid, bufferPid, connectPid, parserPid, mainPid, settings, channel_info]},
  %%{registered, []},
  {applications, [
  		 kernel, 
		 stdlib
		 ]},
  {mod, {earl, []}}
 ]}.