% Copyright 2013 and onwards Roman Gafiyatullin
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%     http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
% 
% See the NOTICE file distributed with this work for additional information regarding copyright ownership.
% 

-module (hooks_hook_mgr_sup).
-behaviour (supervisor).
-export([init/1]).
-export([
		start_link/0,
		get_hook_mgr/1
	]).

-spec start_link() -> {ok, pid()}.
start_link() -> supervisor:start_link( {local, ?MODULE}, ?MODULE, {} ).

-spec get_hook_mgr( term() ) -> {ok, pid()}.
get_hook_mgr( HookID ) ->
	case [ Pid || {ID, Pid, _, _} <- supervisor:which_children( ?MODULE ), ID == HookID ] of
		[] -> get_hook_mgr_maybe_start_it( HookID );
		[ HookMgr ] -> {ok, HookMgr}
	end.

%%% %%%%%%%%%% %%%
%%% supervisor %%%
%%% %%%%%%%%%% %%%

init({}) ->
	hooks_ets = hooks_ets:create(),
	{ok, { {one_for_one, 5, 10}, [] }}.

%%% %%%%%%%% %%%
%%% Internal %%%
%%% %%%%%%%% %%%

hook_mgr_child_spec( HookID ) ->
	{ HookID, 
		{hooks_hook_mgr, start_link, [ HookID ]},
		transient, 1000, worker, [ hooks_hook_mgr ] }.

get_hook_mgr_maybe_start_it( HookID ) ->
	StartChildRet = supervisor:start_child( ?MODULE, hook_mgr_child_spec( HookID ) ),
	% ?log_debug([ ?MODULE, get_hook_mgr_maybe_start_it, {ret, StartChildRet} ]),
	case StartChildRet of
		{ok, HookMgr} -> {ok, HookMgr};
		{error, {already_started, HookMgr}} -> {ok, HookMgr}
	end.


