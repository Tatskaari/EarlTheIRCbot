#!/bin/bash
erlc earl.erl ircParser.erl optimusPrime.erl ircTime.erl telnet.erl earlConnection.erl && \
erl -s earl main -s init stop
