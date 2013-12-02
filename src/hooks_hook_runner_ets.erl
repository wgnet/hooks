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

-module (hooks_hook_runner_ets).
-export([ run/3, run_multiple/3 ]).

run( HookID, Acc, Args ) ->
	Handlers = hooks_ets:handlers_get( HookID ),
	run_handlers( Acc, Args, Handlers ).

run_multiple( HookIDs, Acc, Args ) ->
	Handlers = lists:flatten([ hooks_ets:handlers_get(HookID) || HookID <- HookIDs ]),
	run_handlers( Acc, Args, Handlers ).

run_handlers( AccIn, _Args, [] ) -> AccIn;
run_handlers( AccIn, Args, [ {HandlerFunc, _Prio} | Handlers ] ) ->
	case run_single_handler( HandlerFunc, AccIn, Args ) of
		{stop, AccOut} -> AccOut;
		stop -> stopped;
		{continue, AccOut} ->
			run_handlers( AccOut, Args, Handlers )
	end.

run_single_handler_no_catch( {mfa, M, F, A}, AccIn, Args ) -> {ok, erlang:apply( M, F, A ++ [ AccIn | Args ] )};
run_single_handler_no_catch( {function, F}, AccIn, Args ) -> {ok, erlang:apply( F, [ AccIn | Args ] )}.

run_single_handler( H, AccIn, Args ) ->
	{ok, Return} = run_single_handler_no_catch( H, AccIn, Args ),
	case Return of
		{stop, _} -> Return;
		{continue, _} -> Return;
		stop -> stop;
		_ ->
			error_logger:error_report([
				"Wrong hook return value",
				{ret_value, Return}
				]),
			error({bad_hook_ret_val, Return})
	end.

