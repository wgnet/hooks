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

-module(hooks).
-export([
		add_handler/3,
		remove_handler/3,
		run/3,
		run_pipeline/3
	]).
-export_type([ret/1]).
-include("hooks.hrl").

-type ret( Acc_t ) :: {continue, Acc_t} | {stop, Acc_t} | stop.

-spec add_handler( HookID :: term(), Handler :: hook_handler(), Priority :: integer() ) -> ok.
add_handler( HookID, Handler, Priority ) -> hooks_hook_mgr:add_handler(hooks_hook_mgr:get( HookID ), Handler, Priority ).

-spec remove_handler( HookID :: term(), Handler :: hook_handler(), Priority :: integer() ) -> ok.
remove_handler( _HookID, _Handler, _Priority ) -> error({not_implemented, ?MODULE, remove_handler}).

-spec run( HookID :: term(), AccIn :: term(), Args :: [term()] ) -> AccOut :: term().
run( HookID, AccIn, Args ) -> hooks_hook_runner_beam:run( HookID, AccIn, Args ).

run_pipeline( HookIDs, AccIn, Args ) -> hooks_hook_runner_ets:run_multiple( HookIDs, AccIn, Args ).

