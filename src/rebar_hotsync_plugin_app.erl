-module(rebar_hotsync_plugin_app).
-compile(export_all).

hotsync(_Config, _AppFile) ->
    rebar_log:log(info, "hotsync!~n", []).
