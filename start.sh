. /usr/local/erlang_r15b_riak/activate
erl -sname ws_loadtest -cookie ws_loadtest -pa ebin -boot start_sasl -s ws_loadtest -config ./start.config -args_file start.args
