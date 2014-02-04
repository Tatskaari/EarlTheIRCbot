setting_server_test_() ->
	[
	 	?_test(fun() ->
			Val = 100,
			Pid = spawn(fun() -> setting_server(dict:store(foo, Val, dict:new())) end),
			Pid ! #getVal{name=foo, return_chan=self()},
			receive
				X -> X
			end,
			?assertEqual(X, Val),
			Pid ! die
		       end()),
		?_test(fun() ->
			Pid = spawn(fun() -> setting_server() end),
			Pid ! #setVal{name=bar, value="badger"},
			Pid ! #getVal{name=bar, return_chan=self()},
			receive
				X -> X
			end,
			?assertEqual("badger", X)
		end())
	].
