-module (ex01).
-export ([
		init/0,
		emit_event/0,
		handle_one_1/4,
		handle_one_2/4
	]).

init() ->
	application:start( hooks ),
	ok = hooks:add_handler( {event, one}, {mfa, ?MODULE, handle_one_1, []}, 50 ),
	ok = hooks:add_handler( {event, one}, {mfa, ?MODULE, handle_one_2, []}, 50 ).

emit_event() ->
	hooks:run( {event, one}, [], [ arg1, arg2, arg3 ] ).

handle_one_1( Acc0, A1, A2, A3 ) ->
	{continue, [ {handle_one_1, [ A1, A2, A3 ]} | Acc0 ]}.

handle_one_2( Acc0, A1, A2, A3 ) ->
	{continue, [ {handle_one_2, [ A1, A2, A3 ]} | Acc0 ]}.

