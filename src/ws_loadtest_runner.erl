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

-export([max_connections/1]).
-export([connpersec/1]).
-export([make_client_socket/5]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {
    host, 
    port, 
    path, 
    ssl, 
    connmax,
    connpersec,
    conncount,
    connpending,
    connfailed,
    timer_ref
}).

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
    Host = config(host, undefined),
    Port = config(port, undefined),
    Path = config(path, undefined),
    SSL  = config(ssl, undefined),
    ConnPerSec = config(connpersec, undefined),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [{Host, Port, Path, SSL, ConnPerSec}], []).


max_connections(ConnMax) ->
    gen_server:cast(?SERVER, {connmax, ConnMax}).

connpersec(ConnPerSec) ->
    gen_server:cast(?SERVER, {connpersec, ConnPerSec}).

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
    io:format("Settings: ~p~n", [WS]),
    {ok, TimerRef} = timer:send_interval(1000, update),
    {Host, Port, Path, SSL, ConnPerSec} = WS,
    {ok, #state{host=Host, port=Port, path=Path, ssl=SSL, conncount=0, connmax=0, connpending=0, connfailed=0, connpersec=ConnPerSec, timer_ref=TimerRef}}.

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
handle_cast({connmax, ConnMax}, State) ->
    {noreply, State#state{connmax=ConnMax}};
handle_cast({connpersec, ConnPerSec}, State) ->
    {noreply, State#state{connpersec=ConnPerSec}};
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
handle_info({tcp, _Socket, _Data}, State) ->
    {noreply, State};
handle_info({ssl, _Socket, _Data}, State) ->
    {noreply, State};
handle_info(update, State) ->
    {noreply, update(State)};
handle_info(connected, State=#state{conncount=ConnCount,connpending=ConnPending}) ->
    {noreply, State#state{conncount=(ConnCount+1),connpending=(ConnPending-1)}};
handle_info(connfailed, State=#state{connpending=ConnPending,connfailed=ConnFailed}) ->
    {noreply, State#state{connpending=(ConnPending-1),connfailed=(ConnFailed+1)}};
handle_info({ssl_closed, Socket}, State=#state{conncount=ConnCount}) ->
    io:format("SSL Socket Closed: ~p~n", [Socket]),
    {noreply, State#state{conncount=(ConnCount-1)}};
handle_info({ssl_error, Socket, Reason}, State=#state{conncount=ConnCount}) ->
    io:format("SSL Socket Error: (~p) ~p~n", [Socket, Reason]),
    gen_tcp:close(Socket),
    {noreply, State#state{conncount=(ConnCount-1)}};
handle_info({tcp_closed, Socket}, State=#state{conncount=ConnCount}) ->
    io:format("Socket Closed: ~p~n", [Socket]),
    {noreply, State#state{conncount=(ConnCount-1)}};
handle_info({tcp_error, Socket, Reason}, State=#state{conncount=ConnCount}) ->
    io:format("Socket Error: (~p) ~p~n", [Socket, Reason]),
    gen_tcp:close(Socket),
    {noreply, State#state{conncount=(ConnCount-1)}};
handle_info(Info, State) ->
    io:format("Info: ~p~n", [Info]),
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
terminate(_Reason, #state{timer_ref=TRef}) ->
    {ok, cancel} = timer:cancel(TRef),
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

update(State) ->
    ConnCount = State#state.conncount,
    ConnPending = State#state.connpending,
    ConnFailed = State#state.connfailed,
    io:format("Connection Count: ~p Pending: ~p Failed: ~p ~n", [ConnCount, ConnPending, ConnFailed]),
    start_connections(State).

start_connections(State=#state{connpending=ConnPending,connpersec=ConnPerSec}) ->
    ConnLeft = State#state.connmax - State#state.conncount - ConnPending,
    CanConn = case ConnLeft of
        ConnLeft when ConnLeft < ConnPerSec ->
            ConnLeft;
        _ -> ConnPerSec
    end,
    case CanConn of
        DoConnect when DoConnect > 0 ->
            {ok, IpList} = inet:getaddrs(State#state.host, inet),
            io:format("Connecting to: ~p~n", [IpList]),
            Hello = make_hello(State#state.host, State#state.port, State#state.path),
            [ make_client(IpList, State#state.port, Hello, Index, State#state.ssl) || Index <- lists:seq(1, DoConnect) ];
        _ -> ok
    end,
    State#state{connpending=(ConnPending+CanConn)}.

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

make_client_socket(Ip, Port, Hello, Owner, SSL) ->
    case SSL of
        true ->
            case ssl:connect(Ip, Port, [binary, {packet, 0}]) of
                {ok, SSLSock} -> 
                    Owner ! connected,
                    ok = ssl:send(SSLSock, Hello),
                    ok = ssl:controlling_process(SSLSock, Owner);
                _ -> Owner ! connfailed
            end;
        _ ->
            case gen_tcp:connect(Ip, Port, [binary, {packet, 0}]) of
                {ok, TCPSock} -> 
                    Owner ! connected,
                    ok = gen_tcp:send(TCPSock, Hello),
                    ok = gen_tcp:controlling_process(TCPSock, Owner);
                _ -> Owner ! connfailed
            end
    end.


make_client(Ips, Port, Hello, ClientID, SSL) ->
    WhichIp = ClientID rem erlang:length(Ips),
    Ip = lists:nth( (WhichIp+1) , Ips),
    spawn(?MODULE, make_client_socket, [Ip, Port, Hello, self(), SSL]).
