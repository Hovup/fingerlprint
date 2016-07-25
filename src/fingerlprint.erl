%%%----------------------------------------------------------------------
%%% Copyright: (c) 2011-2013 MoreloSoft - Hova Networks S.A.P.I. de C.V.  
%%% All rights reserved.
%%%
%%% Redistribution and use in any form, with or without modification, 
%%% is strictly prohibited.
%%%
%%% Created by : Jorge Garrido <jorge.garrido@morelosoft.com> [zgb]
%%%----------------------------------------------------------------------
-module(fingerlprint).

-behaviour(gen_server).

%% API
-export([start_link/0, compare/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {lib=[]}).

%%%===================================================================
%%% API
%%%===================================================================
compare([FMDx, FMDy]) ->
    gen_server:call(?MODULE, {compare, FMDx, FMDy}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    {ok, Lib} = application:get_env(fingerlprint, lib),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Lib], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Lib]) ->
    process_flag(trap_exit, true),
    ok = case erl_ddll:load_driver(code:priv_dir(fingerlprint), Lib) of
	     ok -> ok;
	     {error, already_loaded} -> ok;
	     {error, Error}          -> exit(erl_ddll:format_error(Error))
         end,
    Port = open_port({spawn, Lib}, []),
    {ok, #state{lib=Port}}.

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
handle_call({compare, FMDxun, FMDyun}, _From, State=#state{lib=Port}) ->
    FMDx = list_to_binary(FMDxun),
    FMDy = list_to_binary(FMDyun), 
    SizeFmdx= fingerlprint_lib:carry(erlang:byte_size(FMDx)),
    SizeFmdy = fingerlprint_lib:carry(erlang:byte_size(FMDy)),
    Msg = <<SizeFmdx/binary, SizeFmdy/binary, FMDx/binary, FMDy/binary>>,
    {reply, {ok, fingerlprint_lib:rpcoin(Port, self(), Msg)}, State};
handle_call(stop, _From, State=#state{lib=Port}) ->
    Port ! {self(), close},
    {stop, normal, ok, State}.

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
handle_info({'EXIT', _Port, _Reason}, _State) ->
    exit(port_terminated),
    {noreply, #state{lib=[]}};
handle_info({_Port, closed}, _State) ->
    {noreply, #state{lib=[]}}.

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
