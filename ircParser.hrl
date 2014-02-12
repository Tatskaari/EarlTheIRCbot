% Records for the various types of message that can be sent
-record(privmsg, {target, from, admin, message}).
-record(command, {command, data}).
-record(raw, {data, numbercode, trail}).
-record(ping, {nonce}).
-record(pong, {nonce}).
-record(user, {user}).
-record(nick, {nick}).
-record(join, {channel, nick}).
-record(part, {channel, nick}).
-record(quit, {reason}).
-record(mode, {modes}).
-record(topic, {channel, old_topic, new_topic, setby}).
-record(motd, {message}).
-record(rpl_myinfo,{server_name, server_version, user_modes, chan_modes}).
-record(rpl_topic, {channel, topic}).
-record(rpl_topicwhotime, {channel, date, nick}).

-record(notice, {target, message}).

-record(registerPlugin, {name}).
-record(deregisterPlugin, {name}).

-record(registerAdmin, {admin}).
-record(deregisterAdmin, {admin}).

% Data types
%-record(user, {nick, username, host}).

