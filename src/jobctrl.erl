-module(jobctrl).

-export([
  submit/1, 
  %enum/0, 
  lookup/1,
  schedule/1, 
  dump/1,
  delete/1
]).

-export_type([job_id/0, job_def/0, job_ref/0]).

-type job_id() :: binary().
-type job_def() :: {file, binary()} | map().
-type job_ref() :: {job_id(), pid()}.

-spec submit(job_def()) -> {ok, job_ref()} | {error, term()}.
submit(Job) ->
  job_queue:enqueue(Job).

-spec lookup(all | job_id()) -> {ok, job_ref()} | {error, term()}.
lookup(all = _JobId) ->
  Jobs = job_queue:all(),
  Result = lists:map(
    fun({JobId, _Pid} = _Elem) -> JobId end, 
  Jobs),
  {ok, Result};
lookup(JobId) ->
  case job_queue:find({job_id, JobId}) of
    {ok, {_, Pid}} ->
      gen_server:call(Pid, fetch);
    {error, Reason} ->
      {error, Reason}
  end.

-spec delete(job_id()) -> ok | {error, term()}.
delete(JobId) ->
  job_queue:dequeue(JobId).

-spec schedule(job_id()) -> list() | {error, term()}.
schedule(JobId) ->
  case job_queue:find({job_id, JobId}) of
    {ok, {_, Pid}} ->
      gen_server:call(Pid, schedule);
    _ ->
      {error, not_found}
  end.

-spec dump(job_id()) -> {ok, string()} | {error, not_found}.
dump(JobId) ->
  case job_queue:find({job_id, JobId}) of
    {ok, {_, Pid}} ->
      gen_server:call(Pid, dump);
    _ ->
      {error, not_found}
  end.