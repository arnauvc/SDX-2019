#!/bin/bash
erlc *.erl
konsole -e erl -name "p2@127.0.0.1" -setcookie secret &
konsole -e erl -name "p3@127.0.0.1" -setcookie secret &
konsole -e erl -name "p4@127.0.0.1" -setcookie secret &
konsole -e erl -name "p5@127.0.0.1" -setcookie secret &
konsole -e erl -name "p1@127.0.0.1" -setcookie secret -eval "groupy1:start(gms1,1000)" &
