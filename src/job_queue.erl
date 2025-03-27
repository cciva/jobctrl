-module(job_queue).
-behaviour(supervisor).

% public
-export([enqueue/1, dequeue/1, find/1, all/0]).
-export([start_link/0, start_link/1]).

% callbacks
-export([init/1]).

%% public API

-spec enqueue(jobctrl:job_def()) -> {ok, jobctrl:job_ref()} | {error, term()}.
enqueue({file, Path} = _Job) ->
  case file:read_file(Path) of
    {ok, Contents} ->
      enqueue(jsx:decode(Contents));
    {error, Reason} ->
      {error, Reason}
  end;
enqueue(Job) when is_map(Job) ->
  {JobId, ChildSpec} = job_spec(Job),
  case supervisor:start_child(?MODULE, ChildSpec) of
    {ok, Pid} ->
      {ok, {Pid, JobId}};
    {error, Reason} ->
      {error, Reason}
  end;
enqueue(PathLike) ->
  case io_lib:printable_unicode_list(PathLike) of
    true -> enqueue({file, PathLike});
    _ -> {error, invalid}
  end.

%% @doc Dequeues/deletes a job by id
-spec dequeue(jobctrl:job_id()) -> ok | {error, term()}.
dequeue(JobId) ->
  case find({job_id, JobId}) of
    Pid when is_pid(Pid) ->
      supervisor:terminate_child(?MODULE, Pid);
    _ ->
      {error, not_found}
  end.

%% @doc Finds job reference by job id
-spec find({job_id, jobctrl:job_id()}) -> {ok, jobctrl:job_ref()} | {error, not_found}.
find({job_id, JobId} = _Opts) ->
  case utils:child_by(?MODULE, {job, JobId}) of
    Pid when is_pid(Pid) -> {ok, {JobId, Pid}};
    undefined -> {error, not_found}
  end;
find(_Opts) -> {error, not_found}.

%% @doc Returns all job references
-spec all() -> [jobctrl:job_ref()].
all() ->
  utils:children_of(?MODULE, fun job_proc_selector/1).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).
start_link(Args) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

init(_Args) ->
  SupFlags = #{
    strategy => one_for_one,
    intensity => 5, 
    period => 10
  },
  {ok, {SupFlags, []}}.

%% @doc Creates childspec for a job
-spec job_spec(jobctrl:job_def()) -> {jobctrl:job_id(), map()}.
job_spec(Job) ->
  JobId = utils:uuid_v4(),
  Spec = #{
    id => {job, JobId},
    start => {job_stub, start_link, [{JobId, Job}]},
    modules => [job_stub],
    type => worker
  },
  {JobId, Spec}.

job_proc_selector(Proc) ->
  case Proc of
    {{job, JobId}, Pid, _, _} -> {JobId, Pid};
    _ -> false
  end.
