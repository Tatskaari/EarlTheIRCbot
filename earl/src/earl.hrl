-define(HOSTNAME, "irc.freenode.net").
-define(PORT, 6667).

-define(NICKS, ["Earl", "Earl2"]).
-define(USER, "Sir_Earl Sir_Earl Sir_Earl Sir_Earl").
%-define(COLORS, true).
-define(AUTOJN, ["#bottesting_earl"]).

-define(PLUGINS, ["logger", "ircSetup", "earlAdminPlugin", "optimusPrime", "telnet", "reminder", "ircTime"]).


-record(getVal, {name, return_chan}).
-record(noVal, {name}).
-record(retVal, {name, value}).
-record(setVal, {name, value}).
