-module(graph).

-export([describe/2, sort/1]).

%% @doc Create representation of graph with nodes and their deps (edges)
%% we are using erlang's digraph
-spec describe(list(), function()) -> term().
describe(Data, AdjFn) when is_list(Data) ->
  Graph = digraph:new(),
  lists:foreach(
    fun(Elem) ->
      {Vertex, Edges} = AdjFn(Elem),
      graph_apply(Graph, {Vertex, Edges})
    end, 
  Data),
  Graph;
describe(_Data, _AdjFn) -> 
  digraph:new().

%% @doc Apply topological sorting to graph (which should be DAG).
%% If graph do have cycles, return an error
-spec sort(term()) -> {ok, term()} | {error, term()}.
sort(Graph)->
  case digraph_utils:is_acyclic(Graph) of
    true ->
      case digraph_utils:topsort(Graph) of
        false ->
          {error, {invalid, unresolvable}};
        Vertices ->
          {ok, Vertices}
      end;
    _ ->
      {error, {invalid, has_cycles}}
  end.

graph_apply(Graph, {Vertex, []}) ->
  digraph:add_vertex(Graph, Vertex);
graph_apply(Graph, {Vertex, Edges} = _Node) ->
  digraph:add_vertex(Graph, Vertex),
  graph_apply(Graph, Vertex, Edges).

graph_apply(Graph, _Vertex, []) ->
  Graph;
graph_apply(Graph, Vertex, [Edge|Tail] = _Edges) ->
  digraph:add_vertex(Graph, Edge),
  digraph:add_edge(Graph, Edge, Vertex),
  graph_apply(Graph, Vertex, Tail).