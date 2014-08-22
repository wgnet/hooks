hooks
=====

Generic events dispatcher.

The components of a modular system shold interact with each other.

Yet it would be way better if they do not know anything about each other.

Hooks - is one of the ways to satisfy both of the latter critera.

API
===


`-spec hooks:add_handler(HookID, {mfa, M, F, A}, Priority) -> ok.`
`-spec hooks:remove_handler(HookID, {mfa, M, F, A}, Priority) -> ok.`

`-spec hooks:run(HookID, AccIn, ConstArgs :: [ term() ] ) -> AccOut.`

This call runs the the pipeline of hook-handlers identified by `HookID` one by one according to their priorities passing `[Acc | ConstArgs]` as the arguments list.
