-define(HOSTNAME, "localhost").
-define(PORT, 6667).

-define(NICKS, ["MexEarl", "MexEarl2"]).
-define(USER, "Tatskaari Sir_Earl Sir_Earl Sir_Earl").
-define(COLORS, true).
-define(AUTOJN, ["#bottesting"]).

-record(getVal, {name, return_chan}).
-record(noVal, {name}).
-record(retVal, {name, value}).
-record(setVal, {name, value}).
