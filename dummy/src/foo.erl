-module(foo).

-compile(export_all).

start() ->
    ok = application:start(dummy).

foo() ->
    bar().

bar() ->
    rebar.

xxx() ->
    {foo(), 43}.

