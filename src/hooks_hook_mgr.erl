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

-module (hooks_hook_mgr).
-behaviour (gen_server).
-export([
	start_link/1
]).
-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

-export([
	get/1,
	add_handler/3,
	dump/1
]).

-spec start_link( HookID :: term() ) -> {ok, pid()}.
start_link( HookID ) ->
	gen_server:start_link( ?MODULE, { HookID }, [] ).

%%% %%% %%%
%%% API %%%
%%% %%% %%%
-spec get( term() ) -> pid().
get( HookID ) -> {ok, HookMgr} = hooks_hook_mgr_sup:get_hook_mgr( HookID ), HookMgr.

-spec add_handler( pid(), {mfa, atom(), atom(), [term()]}, integer() ) -> ok.
add_handler( HookMgr, {mfa, M, F, A}, Priority ) -> gen_server:call( HookMgr, {add_handler, {mfa, M, F, A}, Priority} ).

-spec dump( pid() ) -> term().
dump( HookMgr ) -> gen_server:call( HookMgr, dump ).

%%% %%%%%%%%%% %%%
%%% gen_server %%%
%%% %%%%%%%%%% %%%
-record(handler, {
		func :: undefined | {mfa, atom(), atom(), [term()]},
		priority :: integer()
	}).
-record(s, {
		hook_id = throw({not_nil, ?MODULE, '#s.hook_id'}) :: term(),
		handlers = [] :: [ #handler{} ]
	}).

init({ HookID }) -> {ok, #s{ hook_id = HookID, handlers = [ #handler{ func = F, priority = P } || {F, P} <- hooks_ets:handlers_get( HookID ) ] }}.

handle_call( dump, _From, State ) -> {reply, State, State};
handle_call( {add_handler, {mfa, M, F, A}, Priority}, _From, State ) -> do_add_handler( {mfa, M, F, A}, Priority, State );
handle_call(Request, _From, State = #s{}) -> {stop, {bad_arg, Request}, State}.

handle_cast(Request, State = #s{}) -> {stop, {bad_arg, Request}, State}.

handle_info(Message, State = #s{}) -> {stop, {bad_arg, Message}, State}.

terminate(_Reason, _State) -> ignore.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%% %%%%%%%% %%%
%%% Internal %%%
%%% %%%%%%%% %%%
-spec do_add_handler( {mfa, atom(), atom(), [term()]}, integer(), #s{} ) -> {reply, ok, #s{}}.
do_add_handler( {mfa, M, F, A}, Priority, State = #s{ hook_id = HookID, handlers = Handlers } ) ->
	Handler = #handler{ func = {mfa, M, F, A}, priority = Priority },
	NewHandlers = add_handler_loop( Handler, Handlers, queue:new() ),
	hooks_ets:handlers_set( HookID, [ {Func, Prio} || #handler{ func = Func, priority = Prio } <- NewHandlers] ),
	hooks_compiler:notify_hook_handlers_updated( HookID ),
	{reply, ok, State #s{ handlers = NewHandlers }}.

-spec add_handler_loop( #handler{}, [#handler{}], queue() ) -> [#handler{}].
add_handler_loop( H = #handler{}, [], HeadQ ) -> queue:to_list( queue:in( H, HeadQ ) );
add_handler_loop( H = #handler{ priority = P }, [ H1 = #handler{ priority = P1 } | Hs ], HeadQ ) when P >= P1 -> add_handler_loop( H, Hs, queue:in( H1, HeadQ ) );
add_handler_loop( H = #handler{ priority = P }, Hs = [ #handler{ priority = P1 } | _ ], HeadQ ) when P < P1 -> queue:to_list( queue:in( H, HeadQ ) ) ++ Hs.



