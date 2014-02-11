-define(HOSTNAME, "irc.cs.kent.ac.uk").
-define(PORT, 6667).

-define(NICK, "MexEarl").
-define(USER, "Tatskaari Sir_Earl Sir_Earl Sir_Earl").
-define(COLORS, true).
-define(AUTOJN, ["#bottesting"]).

-record(getVal, {name, return_chan}).
-record(noVal, {name}).
-record(retVal, {name, value}).
-record(setVal, {name, value}).
