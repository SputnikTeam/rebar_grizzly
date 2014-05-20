-module(rebar_hotsync_plugin).
-compile(export_all).

hotsync(Config, AppFile) ->
    case grizzly_utils:read_config(Config, AppFile) of
        no_grizzly ->
            rebar_log:log(info, "No work for grizzly bear~n", []);
        {ok, GrizzlyConfig} ->
            AppName = grizzly_utils:grizzly_app(GrizzlyConfig),
            Nodes   = grizzly_utils:grizzly_nodes(GrizzlyConfig),
            Modules = grizzly_utils:grizzly_modules(GrizzlyConfig),

            rebar_log:log(info, "grizzly config available~n\tapplication: ~p~n\tnodes: ~p~n\tmodules: ~p~n",
                          [AppName, Nodes, Modules]),
            grizzly_utils:start_net_kernel('grizzly-node'),
            lists:foreach(fun grizzly_utils:deploy/1, Nodes),

            sync_modules(GrizzlyConfig, Nodes, Modules)
    end.

sync_modules(_GrizzlyConfig, [], _Modules) ->
    ok;
sync_modules(GrizzlyConfig, [Node | NodesTail], Modules) ->
    rebar_log:log(info, "start modules sync on '~s'~n", [Node]),

    RemoteModulesInfo = grizzly_utils:get_remote_modules_info(Node, Modules),
    true = code:add_path(filename:absname("ebin")), %% add default rebar output path
    LocalModulesInfo = grizzly_utils:get_local_modules_info(Modules),
    ModulesForUpload = [element(1, Task) || Task <- LocalModulesInfo -- RemoteModulesInfo],

    io:format("upload task on '~s':~n\t~p ... ", [Node, ModulesForUpload]),

    ok = grizzly_utils:sync_application_modules(GrizzlyConfig, Node, ModulesForUpload),

    io:format("ok~n", []),

    sync_modules(GrizzlyConfig, NodesTail, Modules).

