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

-module (hooks_forms).
-export ([
		hook_dispatcher_function/1,
		hook_function_name/1,
		hook_function_form/2
	]).

-spec hook_dispatcher_function( HookIDs :: [term()] ) -> term().
hook_dispatcher_function( HookIDs ) ->
	form_function( run, 3, [
			form_clause( 
				[
					form_literal(HookID),
					form_var( 'Acc' ),
					form_var( 'Args' )
				], [], [
					{call, 0, form_literal( hook_function_name(HookID) ), [
							form_var( 'Acc' ),
							form_var( 'Args' )
						]}
				] )
			|| HookID <- HookIDs
		] ++ [
			form_clause(
				[
					form_var( '_' ),
					form_var( 'Acc' ),
					form_var( '_' )
				], [], [
					form_var( 'Acc' )
				] )
		] ).


-spec hook_function_name( term() ) -> atom().
% hook_function_name( HookID ) -> list_to_atom([ $h | integer_to_list( erlang:phash2( HookID ) ) ]).
hook_function_name( HookID ) -> list_to_atom(lists:flatten(io_lib:format( "#[~p]", [ HookID ] ))).

-spec hook_function_form( term(), [ {mfa, atom(), atom(), [ term() ]} ] ) -> term().
hook_function_form( HookID, Handlers ) ->
	HookFunctionName = hook_function_name( HookID ),
	form_function( HookFunctionName, 2, [
			form_clause( [form_var( hook_acc_var( 0 ) ), form_var( hook_args_var() )], [], [
					hook_long_case( 0, Handlers )
				] )
		] ).
hook_acc_var( Idx ) when is_integer( Idx ) -> list_to_atom([ $A, $c, $c | integer_to_list( Idx ) ]).
hook_args_var() -> 'Args'.

hook_long_case( AccIdx, [] ) -> form_var(hook_acc_var( AccIdx ));
hook_long_case( AccIdx, [ {mfa, HM, HF, HA} | Handlers] ) ->
	VarAcc = form_var( hook_acc_var(AccIdx) ),
	VarAccNext = form_var( hook_acc_var(AccIdx + 1) ),
	{'case', 0,
		{call, 0, {remote, 0, form_literal(erlang), form_literal(apply)}, [
				form_literal( HM ),
				form_literal( HF ),
				hook_handler_args( VarAcc, [ form_literal(HArg) || HArg <- HA] )
			]},
		[
			form_clause( [form_literal(stop)], [], [ form_literal(stopped) ] ),
			form_clause( [form_tuple( [ form_literal(stop), VarAccNext ] )], [], [ VarAccNext ] ),
			form_clause( [form_tuple( [ form_literal(continue), VarAccNext ] )], [], [
				case Handlers of
					[] -> VarAccNext;
					[ _ | _ ] -> hook_long_case( AccIdx + 1, Handlers )
				end
			] )
		] }.

hook_handler_args( AccVar, HArgs ) ->
	lists:foldr(
		fun form_cons/2,
		form_cons( 
				AccVar, form_var(hook_args_var()) 
			), 
		HArgs ).

% form_list_to_cons( L ) when is_list( L ) -> lists:foldr( fun form_cons/2, form_nil(), L ).
% form_nil() -> {nil, 0}.
form_literal( Value ) -> erl_parse:abstract( Value ).
form_tuple( Elements ) -> {tuple, 0, Elements}.
form_cons( H, T ) -> {cons, 0, H, T}.
form_function( FName, FArity, FClauses ) -> {function, 0, FName, FArity, FClauses}.
form_clause( Heads, Guards, Bodies ) -> {clause, 0, Heads, Guards, Bodies}.
form_var( VarName ) -> {var, 0, VarName}.