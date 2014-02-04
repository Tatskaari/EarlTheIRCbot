% Records for the various types of message that can be sent
-record(privmsg, {target, from, admin, message}).
-record(ping, {nonce}).
-record(user, {nick, username, host}).
-record(mode, {modes}).
-record(notice, {target, message}).

-record(registerPlugin, {chan}).
