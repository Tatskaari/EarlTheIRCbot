#!/bin/bash
erlc earl.erl ircParser.erl optimusPrime.erl time.erl telnet.erl
erl -s earl main -s init stop