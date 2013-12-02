-ifndef(hooks_include_hooks_hrl).
-define(hooks_include_hooks_hrl, true).

-type hook_scope() :: local | global.
-type hook_handler() :: {mfa, M :: atom(), F :: atom(), A :: [term()]}.

-endif. % hooks_include_hooks_hrl
