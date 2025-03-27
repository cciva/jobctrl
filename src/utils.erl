-module(utils).

-export([
  uuid_v4/0, 
  pipe/2,
  first_match/2,
  child_by/2,
  children_of/2,
  read_conf/2,
  http_respond/3,
  http_finish/4
]).

-define(APPLICATION, jobctrl).

%% @doc Generates new v4 uuid
uuid_v4() ->
  UUID = uuid:to_string(uuid:uuid4()),
  unicode:characters_to_binary(UUID).

%% @doc Simple function piping/composition.
%% https://erlangforums.com/t/is-it-possible-to-add-piping-ala-to-erlang/1100/3
-spec pipe(term(), function()) -> term().
pipe(Input, Funs) ->
  lists:foldl(fun(Fn, Arg) -> Fn(Arg) end, Input, Funs).

%% @doc Finds first child under a supervisor by specified key
-spec child_by(module(), term()) -> pid() | undefined.
child_by(SupRef, Key) ->
  find_child(SupRef, fun({Id, _, _, _}) -> Id =:= Key end).

%% @doc All children under a supervisor
-spec children_of(module(), function()) -> list().
children_of(SupRef, MapFn) ->
  Children = supervisor:which_children(SupRef),
  lists:foldl(
    fun(Child, Acc) ->
      case MapFn(Child) of
        false -> Acc;
        Proc -> [Proc|Acc]
      end
    end, 
  [], Children).

%% @doc Finds first child under a supervisor by passing a condition function
-spec find_child(module(), function()) -> pid() | undefined.
find_child(SupRef, Cond) ->
  Children = supervisor:which_children(SupRef),
  case first_match(Children, Cond) of
    {_, Pid, _, _} -> Pid;
    _ -> undefined
  end.

read_conf(all = _Key, _Defaults) ->
  Conf = application:get_all_env(?APPLICATION),
  {ok, Conf};
read_conf(Key, Defaults) when is_atom(Key) ->
  case application:get_env(?APPLICATION, Key) of
    {ok, Value} -> Value;
    undefined -> Defaults
  end;
read_conf(Key, Defaults) when is_list(Key) ->
  Defaults.

http_respond(Code, {json, Body}, Req) when is_binary(Body) ->
  cowboy_req:reply(Code, #{}, Body, Req);
http_respond(Code, {json, Body}, Req) ->
  cowboy_req:reply(Code, #{}, jsx:encode(Body), Req);
http_respond(Code, Body, Req) ->
  cowboy_req:reply(Code, #{}, Body, Req).

http_finish(Status, {json, Body}, Req, State) when is_binary(Body) ->
  http_done(Status, Body, Req, State);
http_finish(Status, {json, Body}, Req, State) ->
  http_done(Status, jsx:encode(Body), Req, State).

http_done(Status, Body, #{method := <<"GET">>} = Req, State) ->
  Next = cowboy_req:set_resp_body(Body, Req),
  Reply = cowboy_req:reply(Status, Next),
  {stop, Reply, State};
http_done(Status, Body, #{method := <<"POST">>} = Req, State) ->
  Reply = cowboy_req:reply(Status, #{}, Body, Req),
  {stop, Reply, State};
http_done(Status, Body, Req, State) ->
  Reply = cowboy_req:reply(Status, #{}, Body, Req),
  {stop, Reply, State}.

%% @doc First element in list that satisfies application of Cond function
-spec first_match(list(), function()) -> term() | undefined.
first_match([Elem|Tail], Cond) ->
  case Cond(Elem) of
    true -> Elem;
    false -> first_match(Tail, Cond)
  end;
first_match([], _Cond) -> undefined.

