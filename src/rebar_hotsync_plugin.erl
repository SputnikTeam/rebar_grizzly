-module(rebar_hotsync_plugin).
-compile(export_all).

hotsync(Config, AppFile) ->
    case grizzly_utils:read_config(Config, AppFile) of
        no_grizzly ->
            rebar_log:log(info, "No work for grizzly bear~n", []);
        {ok, GrizzlyConfig} ->
            AppName = proplists:get_value(app_name, GrizzlyConfig),
            Nodes   = proplists:get_value(nodes, GrizzlyConfig),
            Modules = proplists:get_value(modules, GrizzlyConfig),

            rebar_log:log(info, "grizzly config available~n\tapplication: ~p~n\tnodes: ~p~n\tmodules: ~p~n",
                          [AppName, Nodes, Modules]),
            grizzly_utils:start_net_kernel('grizzly-node'),
            lists:foreach(fun grizzly_utils:deploy/1, Nodes),

            sync_modules(AppName, Nodes, Modules)
    end.

sync_modules(_AppName, [], _Modules) ->
    ok;
sync_modules(AppName, [Node | NodesTail], Modules) ->
    rebar_log:log(info, "start modules sync on '~s'~n", [Node]),

    RemoteModulesInfo = grizzly_utils:get_remote_modules_info(Node, Modules),
    true = code:add_path(filename:absname("ebin")), %% add default rebar output path
    LocalModulesInfo = grizzly_utils:get_local_modules_info(Modules),
    ModulesForUpdate = get_modules_for_update(LocalModulesInfo, RemoteModulesInfo),
    ModulesForDelete = get_modules_for_delete(
                         Modules,
                         Node,
                         AppName
                        ),

    rebar_log:log(info, "upload task on '~s':~n\t~p ... ~n",
                  [
                   Node,
                   ModulesForUpdate
                  ]),

    io:format("sync modules on '~s'~n\tupdate modules: ~p~n\tdelete modules: ~p~n",
              [node(), ModulesForUpdate, ModulesForDelete]),
    
    ok = grizzly_utils:sync_application_modules(
           AppName,
           Node,
           ModulesForUpdate,
           ModulesForDelete
          ),

    io:format("sync successfull~n"),

    rebar_log:log(info, "sync application modules finished~n", []),

    sync_modules(AppName, NodesTail, Modules).

get_modules_for_update(LocalModulesInfo, RemoteModulesInfo) ->
    [element(1, Task) || Task <- LocalModulesInfo -- RemoteModulesInfo].

get_modules_for_delete(LocalModules, Node, AppName) ->
    RemoteModules = grizzly_utils:get_beams_list(Node, AppName),
    io:format("remote modules: ~p, local: ~p~n", [RemoteModules, LocalModules]),
    RemoteModules -- LocalModules.

modules_list(ModulesInfo) ->
    [element(1, Entry) || Entry <- ModulesInfo].

