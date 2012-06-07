%%%-------------------------------------------------------------------
%%% @author Ben Halsted <bhalsted@gmail.com>
%%% @copyright (C) 2012, Ben Halsted
%%% @doc
%%%
%%% @end
%%% Created :  5 Jun 2012 by Ben Halsted <bhalsted@gmail.com>
%%%-------------------------------------------------------------------
-module(ws_loadtest_runner).

-behaviour(gen_server).

%% API
-export([start_link/0]).

-export([connect/1]).
-export([make_client_socket/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).


-define(SERVER, ?MODULE). 

-record(state, {host, port, path, connections}).

%%%===================================================================
%%% API
%%%===================================================================



%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    Host = config(host, unde),
    Port = config(port, unde),
    Path = config(path, unde),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [{Host, Port, Path}], []).


connect(Count) ->
    gen_server:cast(?SERVER, {connect, Count}).


config(Name, Default) ->
    case application:get_env(ws_loadtest, Name) of
	{ok, Value} -> Value;
	undefined -> Default
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([WS]) ->
    erlang:display(WS),
    {Host, Port, Path} = WS,
    {ok, #state{host=Host, port=Port, path=Path}}.

make_hello(Host, Port, Path) ->
    "GET "++ Path ++" HTTP/1.1\r\n" ++ 
    "Upgrade: WebSocket\r\nConnection: Upgrade\r\n" ++ 
    "Host: " ++ Host ++ ":" ++ erlang:integer_to_list(Port) ++ "\r\n" ++
    "Origin: " ++ Host ++ ":" ++ erlang:integer_to_list(Port) ++ "\r\n" ++
    "Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n" ++
    %"Accept: */*\r\n" ++
    %"Referer: http://halzy.sportvision.com/debug/cup/html/raceview.html\r\n" ++
    %"Connection: keep-alive\r\n" ++ 
    "Sec-WebSocket-Version: 13\r\n" ++
    "\r\n".

make_client_socket(Host, Port, Hello, Owner) ->
    {ok, Sock} = gen_tcp:connect(Host, Port, [binary, {packet, 0}]),
    ok = gen_tcp:send(Sock, Hello),
    ok = gen_tcp:controlling_process(Sock, Owner).
    

make_client(Host, Port, Hello) ->
    spawn(?MODULE, make_client_socket, [Host, Port, Hello, self()]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({connect, Count}, State) ->
    Hello = make_hello(State#state.host, State#state.port, State#state.path),
    [ make_client(State#state.host, State#state.port, Hello) || _ <- lists:seq(1, Count)  ],
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
