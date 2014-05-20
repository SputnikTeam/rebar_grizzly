-module(foo).

-compile(export_all).

start() ->
    ok = application:start(dummy).

foo() ->
    bar.
