% Records for the various types of message that can be sent
-record(privmsg, {target, from, admin, message}).
-record(command, {command, data}).
-record(raw, {data}).
-record(ping, {nonce}).
-record(pong, {nonce}).
-record(user, {user}).
-record(nick, {nick}).
-record(join, {channel}).
-record(part, {channel}).
-record(quit, {reason}).
-record(mode, {modes}).

-record(notice, {target, message}).

-record(registerPlugin, {chan}).

% Data types
%-record(user, {nick, username, host}).

