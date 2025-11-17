open Hypergraph_sig
val add_hyperedge :
  (Node.t -> IdSet.t -> 'a -> 'a) ->
  (Node.t -> Node.t -> 'a -> 'a) ->
  (Node.t -> Node.t -> 'a -> 'b) ->
  ('a -> 'b) ->
  IdSet.t ->
  Node.t ->
  Node.t ->
  'a ->
  'b

val add_clauses : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b

val remove_hyperedge : 
  (Node.t -> 'a -> 'b -> 'c) ->
  ('d -> bool) ->
  ('c -> Node.t -> 'd) ->
  (Node.t -> succ:Node.t -> 'c -> 'c) ->
  ('c -> Node.t -> 'c) ->
  IdSet.t ->
  Node.t ->
  'a ->
  'b ->
  'c
