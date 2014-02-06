-define(HOSTNAME, "irc.cs.kent.ac.uk").
-define(PORT, 6667).

-define(NICK, "GrayEarl").
-define(USER, "graymalkin Sir_Earl Sir_Earl Sir_Earl").
-define(COLORS, true).

-record(getVal, {name, return_chan}).
-record(noVal, {name}).
-record(retVal, {name, value}).
-record(setVal, {name, value}).
