#!/bin/bash
erlc *.erl
konsole -e 'cd /home2/users/alumnes/1203204/SDX-2019/Groupy/; erl -name "p2@127.0.0.1" -setcookie secret &'
konsole -e erl -name "p3@127.0.0.1" -setcookie secret &
konsole -e erl -name "p4@127.0.0.1" -setcookie secret &
konsole -e erl -name "p5@127.0.0.1" -setcookie secret &
konsole -e erl -name "p1@127.0.0.1" -setcookie secret & #-eval "groupy1:start(gms1,1000)" &
