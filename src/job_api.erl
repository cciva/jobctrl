-module(job_api).

-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).
-export([content_types_provided/2]).
-export([jobs_router/2]).

-record(state, {op :: binary()}).

-define(GET, <<"GET">>).
-define(POST, <<"POST">>).
-define(DELETE, <<"DELETE">>).

init(Req, [Op|_] = _State) ->
  Context = #state{op=Op},
  {cowboy_rest, Req, Context};
init(Req, _State) ->
  {cowboy_rest, Req, #state{op=any}}.

allowed_methods(Req, State) ->
  {[?GET, ?POST, ?DELETE], Req, State}.

content_types_provided(Req, State) ->
  {[
    {<<"application/json">>, jobs_router},
    {<<"text/plain">>, jobs_router},
    {<<"text/html">>, jobs_router}
  ], Req, State}.
content_types_accepted(Req, State) ->
  {[
    {<<"application/json">>, jobs_router}
  ], Req, State}.

jobs_router(#{method := ?GET} = Req, #state{op=query} = State) ->
  case query_jobs(Req, State) of
    {ok, Result} ->
      {jsx:encode(Result), Req, State};
    {error, Reason} ->
      Error = {json, #{error => Reason}},
      utils:http_finish(404, Error, Req, State)
  end;
jobs_router(#{method := ?GET} = Req, #state{op=any} = State) ->
  case query_jobs(Req, State) of
    {ok, Result} -> 
      {jsx:encode(Result), Req, State};
    {error, Reason} ->
      Error = {json, #{error => Reason}},
      utils:http_finish(500, Error, Req, State)
  end;
jobs_router(#{method := ?GET} = Req, #state{op=dump} = State) ->
  case dump_job(Req, State) of
    {ok, Script} ->
      {Script, Req, State};
    {error, Reason} ->
      utils:http_finish(500, {json, #{error => Reason}}, Req, State)
  end;
jobs_router(#{method := ?POST} = Req, #state{op=schedule} = State) ->
  case schedule_job(Req, State) of
    {ok, Tasks} -> 
      Reply = cowboy_req:set_resp_body(jsx:encode(Tasks), Req),
      {true, Reply, State};
    {error, Reason} -> 
      Error = #{error => Reason},
      utils:http_finish(500, {json, Error}, Req, State)
  end;
jobs_router(#{method := ?POST} = Req, State) ->
  case submit_job(Req, State) of
    {ok, Id, Next, _} ->
      Body = #{job_id => Id},
      utils:http_finish(201, {json, Body}, Next, State);
    {error, _Reason} ->
      {false, Req, State}
  end;
jobs_router(#{method := ?DELETE} = Req, State) ->
  {true, Req, State};
jobs_router(Req, _State) ->
  cowboy_req:reply(405, Req).

query_jobs(Req, _State) ->
  JobQuery = 
    case cowboy_req:binding(job_id, Req) of
      undefined -> all;
      JobId -> JobId
    end,
  jobctrl:lookup(JobQuery).

submit_job(Req, State) ->
  {ok, Body, Next} = cowboy_req:read_body(Req),
  Job = jsx:decode(Body),
  lager:info("[job_api] submitting job ~p...", [Job]),
  case jobctrl:submit(Job) of
    {ok, {_Pid, Id}} ->
      {ok, Id, Next, State};
    {error, Reason} ->
      lager:info("[job_api] error: ~p...", [Reason]),
      {error, Reason}
  end.

dump_job(Req, _State) ->
  JobId = cowboy_req:binding(job_id, Req),
  lager:info("[job_api] dumping job ~p...", [JobId]),
  jobctrl:dump(JobId).

schedule_job(Req, _State) ->
  JobId = cowboy_req:binding(job_id, Req),
  lager:info("[job_api] scheduling job ~p...", [JobId]),
  jobctrl:schedule(JobId).