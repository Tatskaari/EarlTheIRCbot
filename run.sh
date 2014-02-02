#!/bin/bash
erlc earl.erl ircParser.erl
erl -noshell -s earl main -s init stop