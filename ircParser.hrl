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
-record(topic, {channel, old_topic, new_topic, setby}).

-record(notice, {target, message}).

-record(registerPlugin, {name}).
-record(deregisterPlugin, {name}).

-record(registerAdmin, {admin}).
-record(deregisterAdmin, {admin}).

% Data types
%-record(user, {nick, username, host}).

