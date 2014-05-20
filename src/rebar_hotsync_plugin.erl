-module(rebar_hotsync_plugin).
-compile(export_all).

hotsync(Config, _AppFile) ->
    {config, _, RebarConfig, _ENV} = Config,
    case grizzly_utils:read_config(RebarConfig) of
        no_config ->
            ok;
        {ok, GrizzlyConfig} ->
            AppName = grizzly_utils:grizzly_app(GrizzlyConfig),
            Nodes = grizzly_utils:grizzly_nodes(GrizzlyConfig),
            rebar_log:log(info, "grizzly config available~n\tapplication: ~p~n\tnodes: ~p~n",
                          [AppName, Nodes]),
            
            grizzly_utils:start_net_kernel('grizzly-node'),

            lists:foreach(fun grizzly_utils:deploy/1, Nodes),

            rebar_log:log(info, "xxx hotsync!~n", [])
    end.

