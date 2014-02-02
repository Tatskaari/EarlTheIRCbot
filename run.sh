#!/bin/bash
erlc earl.erl ircParser.erl optimusPrime.erl
erl -noshell -s earl main -s init stop