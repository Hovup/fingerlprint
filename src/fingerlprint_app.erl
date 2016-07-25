%%%----------------------------------------------------------------------
%%% Copyright: (c) 2011-2013 MoreloSoft - Hova Networks S.A.P.I. de C.V.  
%%% All rights reserved.
%%%
%%% Redistribution and use in any form, with or without modification, 
%%% is strictly prohibited.
%%%
%%% Created by : Jorge Garrido <jorge.garrido@morelosoft.com> [zgb]
%%%----------------------------------------------------------------------
-module(fingerlprint_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    fingerlprint_sup:start_link().

stop(_State) ->
    ok.
