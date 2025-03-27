-module(job_stub).
-behaviour(gen_server).

% callback functions
-export([
  init/1,
  handle_continue/2, 
  handle_call/3, 
  handle_cast/2, 
  handle_info/2, 
  terminate/2
]).
% start, stop
-export([start_link/1]).

% for testing
-ifdef(TEST).
% -export([]).
-endif.

-record(state, {id::string(), tasks::list(), resolved::boolean()}).

% callbacks
start_link(Args) ->
  gen_server:start_link(?MODULE, [Args], []).

init([Job|_]) ->
  process_flag(trap_exit, true),
  State = 
    case Job of
      {JobId, #{<<"tasks">> := Tasks}} ->
        lager:info("[job (~w)] accepted.", [self()]),
        #state{id=JobId, tasks=Tasks, resolved=false};
      _ ->
        lager:error("[job] invalid structure.", [self()]),
        #state{id=undefined, tasks=[], resolved=false}
    end,

  {ok, State, {continue, resolve}}.

handle_continue(resolve, #state{id=JobId, tasks=Tasks} = State) ->
  lager:info("[job (~w)] resolving tasks for job ~p...", [self(), JobId]),
  case resolve(Tasks) of
    {ok, Result} ->
      {noreply, State#state{tasks=Result, resolved=true}};
    {error, Reason} ->
      lager:error("[job (~w)] unable to resolve tasks for job ~p due to ~p~n", [self(), JobId, Reason]),
      {stop, {error, Reason}, State}
  end.

handle_call(dump, _From, #state{id=JobId, tasks=Tasks} = State) ->
  % dump all tasks inside job and return shell script representation
  lager:info("[job (~w)] dumping job: ~p...", [self(), JobId]),
  {reply, {ok, interpret(Tasks)}, State};
handle_call(schedule, _From, #state{tasks=[]} = State) ->
  {reply, {error, no_tasks}, State};
handle_call(schedule, _From, #state{tasks=Tasks, resolved=true} = State) ->
  {reply, {ok, Tasks}, State};
handle_call(schedule, _From, #state{id=JobId, tasks=Tasks} = State) ->
  lager:info("[job (~w)] trying to resolve tasks for job ~p...", [self(), JobId]),
  case resolve(Tasks) of
    {ok, Result} ->
      NewState = State#state{tasks=Result, resolved=true},
      {reply, {ok, Result}, NewState};
    {error, Reason} ->
      lager:error("[job (~w)] unable to schedule tasks for job ~p due to ~p~n", [self(), JobId, Reason]),
      {stop, {error, Reason}, State}
  end;
handle_call(fetch, _From, #state{id=JobId, tasks=Tasks} = State) ->
  % dump all tasks inside job and return shell script representation
  lager:info("[job (~w)] fetching job: ~p...", [self(), JobId]),
  Job = #{id => JobId, tasks => Tasks},
  {reply, {ok, Job}, State};
handle_call(complete, _From, #state{id=JobId} = State) ->
  lager:info("[job (~w)] completing job: ~p...", [self(), JobId]),
  % we stop here
  {stop, normal, stopped, State}.

handle_info(Request, State) ->
  lager:warn("[job (~w)] unexpected message: ~p", [self(), Request]),
  {noreply, State}.

handle_cast(_Request, State) ->
  {noreply, State}.
terminate(normal, #state{id=JobId} = _State) ->
  lager:info("[job (~w)] job ~p completed.", [self(), JobId]),
  ok;
terminate(Reason, #state{id=JobId} = _State) ->
  lager:info("[job (~w)] interrupting job ~p abruptly due to ~p.", [self(), JobId, Reason]),
  ok.

%% Task sorting, reordering etc.
adjacency_fn(#{<<"name">> := Name} = Task) ->
  Deps = maps:get(<<"requires">>, Task, []),
  {Name, Deps}.

match_fn(Vertex) ->
  fun(Task) ->
    maps:get(<<"name">>, Task, <<>>) =:= Vertex
  end.

select_fn(Tasks, MatchFn) ->
  fun(Vertex, Acc) ->
    case utils:first_match(Tasks, MatchFn(Vertex)) of
      undefined -> Acc;
      Task -> Acc ++ [maps:without([<<"requires">>], Task)]
    end
  end.

reorder([], _Vertices) ->
  [];
reorder(Tasks, []) ->
  Tasks;
reorder(Tasks, Vertices) ->
  Selector = select_fn(Tasks, fun match_fn/1),
  lists:foldl(Selector, [], Vertices).

%% @docs Reorders tasks inside the job taking care of dependencies
resolve(Tasks) ->
  Ordering = utils:pipe(Tasks, [
    fun(X) -> graph:describe(X, fun adjacency_fn/1) end,
    fun(X) -> graph:sort(X) end
  ]),

  case Ordering of
    {ok, Vertices} ->
      {ok, reorder(Tasks, Vertices)};
    {error, Reason} ->
      {error, Reason}
  end.

interpret(Tasks) ->
  Script = include(Tasks, []),
  lists:join("\n", Script).

include([], Script) ->
  [<<"#!/usr/bin/env bash">>|Script];
include([Task|Tail] = _Tasks, Script) ->
  Cmd = maps:get(<<"command">>, Task),
  include(Tail, [Cmd|Script]).
