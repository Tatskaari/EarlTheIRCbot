#!/bin/bash
erlc earl.erl ircParser.erl optimusPrime.erl timer.erl telnet.erl
erl -s earl main -s init stop