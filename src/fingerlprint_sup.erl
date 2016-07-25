%%%----------------------------------------------------------------------
%%% Copyright: (c) 2011-2013 MoreloSoft - Hova Networks S.A.P.I. de C.V.  
%%% All rights reserved.
%%%
%%% Redistribution and use in any form, with or without modification, 
%%% is strictly prohibited.
%%%
%%% Created by : Jorge Garrido <jorge.garrido@morelosoft.com> [zgb]
%%%----------------------------------------------------------------------
-module(fingerlprint_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Childs = [?CHILD(fingerlprint, worker)],
    {ok, { {one_for_one, 1000, 3600}, Childs} }.
