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

-module (hooks_ets).
-export([
		create/0,
		handlers_set/2,
		handlers_get/1,
		handlers_all/0
	]).

-type hook_handler_with_prio() :: {{mfa, atom(), atom(), [term()]}, integer()}.

-spec create() -> ?MODULE.
create() -> ets:new( ?MODULE, [named_table, set, public, {write_concurrency, false}, {read_concurrency, true}] ).

-spec handlers_set( term(), [hook_handler_with_prio()] ) -> ok.
handlers_set( HookID, Handlers ) -> true = ets:insert( ?MODULE, {HookID, Handlers} ), ok.

-spec handlers_get( term() ) -> [ hook_handler_with_prio() ].
handlers_get( HookID ) -> case ets:lookup( ?MODULE, HookID ) of [] -> []; [ {HookID, Some} ] -> Some end.

-spec handlers_all() -> [ {term(), [{mfa, atom(), atom(), [term()]}]} ].
handlers_all() -> [ {HookID, [ HF || {HF, _} <- HookFuncs]} || {HookID, HookFuncs} <- ets:tab2list( ?MODULE ) ].
