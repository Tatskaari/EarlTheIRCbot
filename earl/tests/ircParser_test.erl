
getCommand_test() ->
	?assertEqual({"PRIVMSG", ["#bottesting"]}, getCommand("PRIVMSG #bottesting")).

lineParse_test_() ->
	[
		?_assertEqual(
	   		#privmsg{message="Hello everyone!", target="#mychannel"},
			lineParse(":CalebDelnay!calebd@localhost PRIVMSG #mychannel :Hello everyone!")
		),
		?_assertEqual(
			#privmsg{message=":", target="#bottesting"},
			lineParse(":Mex!~a@a.kent.ac.uk PRIVMSG #bottesting ::")),
		?_assertEqual(
			#ping{nonce="irc.localhost.localdomain"},
			lineParse("PING :irc.localhost.localdomain")
		),	
		?_assertEqual(
			#quit{reason="Bye bye!"},
			lineParse(":CalebDelnay!calebd@localhost QUIT :Bye bye!")
		)
	].

getTrail_test_() ->
	[
		?_assertEqual( {true, " :", "PRIVMSG #bottesting"}, getTrail("PRIVMSG #bottesting : :"))
	].


getPrefix_test_() ->
	[
		?_assertEqual({true, "a", "b"}, getPrefix(":a b")),
		?_assertEqual({true, "a", "b"}, getPrefix(":a     b")),
		?_assertEqual({false, "", "b"}, getPrefix("b"))
	].


% Tests isAdmin funciton
isAdmin_test() ->
	[
		?_assertEqual(false, isAdmin("graymalkin", [])),
		?_assertEqual(false, isAdmin("graymalkin", ["Tatskaari", "Mex"])),
		?_assertEqual(true, isAdmin("graymalkin", ["Tatskaari", "Mex", "graymalkin"]))
	].

