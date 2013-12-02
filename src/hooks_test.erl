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

-module(hooks_test).
-compile(export_all).

hook_ids() -> {{the, first, hook}, {hook, <<"number">>, 2}}.

t1_install_hooks() -> 
	{H1, H2} = hook_ids(),
	_H1M = hooks_hook_mgr:get( H1 ),
	H2M = hooks_hook_mgr:get( H2 ),
	lists:foreach(
		fun( P ) -> hooks_hook_mgr:add_handler( H2M, {mfa, ?MODULE, hook_handler, [ H2, P ]}, P ) end,
		lists:seq( 15,20 ) ).

t1_run_ets() ->
	{H1, H2} = hook_ids(),
	Acc1 = hooks_hook_runner_ets:run( H1, [], [ a1, a2 ] ),
	Acc2 = hooks_hook_runner_ets:run( H2, [], [ a1, a2 ] ),
	{Acc1, Acc2}.

t1_run_forms() ->
	{H1, H2} = hook_ids(),
	Acc1 = hooks_hook_runner_beam:run( H1, [], [ a1, a2 ] ),
	Acc2 = hooks_hook_runner_beam:run( H2, [], [ a1, a2 ] ),
	{Acc1, Acc2}.



hook_handler( HookID, Prio, Acc, Arg1, Arg2 ) ->
	io:format( "~p[~p]: ~p, ~p, ~p~n", [ HookID, Prio, Acc, Arg1, Arg2 ] ),
	{continue, [Prio | Acc]}.

