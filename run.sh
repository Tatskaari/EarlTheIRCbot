#!/bin/bash
erlc earl.erl settingsServer.erl messageRouter.erl ircParser.erl \
optimusPrime.erl ircTime.erl telnet.erl earlConnection.erl earlAdminPlugin.erl && \
erl -s earl main -s init stop
