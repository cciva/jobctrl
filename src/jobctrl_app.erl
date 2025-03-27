-module(jobctrl_app).

-behaviour(application).

-export([start/2, stop/1]).

%% API
start(_StartType, _StartArgs) ->
  lager:start(),
  lager:info("[jobctrl] starting..."),

  {ok, Conf} = utils:read_conf(all, []),
  case serve_http(Conf) of
    ok ->
      jobctrl_sup:start_link();
    {error, Reason} ->
      {error, Reason}
  end.

serve_http(undefined = _Conf) ->
  {error, bad_conf};
serve_http([{web, Config}|_] = _Conf) ->
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/jobs", job_api, []},
      {"/jobs/:job_id", job_api, [query]},
      {"/jobs/:job_id/dump", job_api, [dump]},
      {"/jobs/:job_id/schedule", job_api, [schedule]}
    ]}
  ]),

  lager:info("-> starting http server on http://localhost/~p...", [proplists:get_value(port, Config)]),

  Env = #{env => #{dispatch => Dispatch}},
  case cowboy:start_clear(web_server, Config, Env) of
    {ok, _} -> ok;
    {error, Reason} -> {error, Reason}
  end;
serve_http(_Conf) ->
  {error, bad_conf}.

stop(_State) ->
  cowboy:stop_listener(web_server).
