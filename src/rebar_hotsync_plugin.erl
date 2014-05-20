-module(rebar_hotsync_plugin).
-compile(export_all).

hotsync(Config, AppFile) ->
    case grizzly_utils:read_config(Config, AppFile) of
        no_grizzly ->
            rebar_log:log(info, "No grizzly", []);
        {ok, GrizzlyConfig} ->
            AppName = grizzly_utils:grizzly_app(GrizzlyConfig),
            Nodes   = grizzly_utils:grizzly_nodes(GrizzlyConfig),
            Modules = grizzly_utils:grizzly_modules(GrizzlyConfig),

            rebar_log:log(info, "grizzly config available~n\tapplication: ~p~n\tnodes: ~p~n\tmodules: ~p~n",
                          [AppName, Nodes, Modules]),
            grizzly_utils:start_net_kernel('grizzly-node'),
            lists:foreach(fun grizzly_utils:deploy/1, Nodes),
            rebar_log:log(info, "xxx hotsync!~n", [])
    end.
