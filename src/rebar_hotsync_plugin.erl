-module(rebar_hotsync_plugin).
-compile(export_all).

hotsync(ConfigOriginal, AppFile) ->
    {Config, AppName} = rebar_app_utils:app_name(ConfigOriginal, AppFile),
    case rebar_config:get(Config, grizzly, no_grizzly) of
        no_grizzly ->
            rebar_log:log(info, "No grizzly", []);
        GrizzlyOptions ->
            rebar_log:log(info, "GrizzlyOptions: ~p~n", [GrizzlyOptions]),
            {ok, GrizzlyConfig} = grizzly_utils:read_config(GrizzlyOptions),
            Nodes = grizzly_utils:grizzly_nodes(GrizzlyConfig),
            rebar_log:log(info, "grizzly config available~n\tapplication: ~p~n\tnodes: ~p~n",
                          [AppName, Nodes]),
            grizzly_utils:start_net_kernel('grizzly-node'),
            lists:foreach(fun grizzly_utils:deploy/1, Nodes),
            rebar_log:log(info, "xxx hotsync!~n", [])
    end.
