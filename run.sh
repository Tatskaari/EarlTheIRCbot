#!/bin/bash
erlc earl.erl ircParser.erl optimusPrime.erl time.erl
erl -noshell -s earl main -s init stop