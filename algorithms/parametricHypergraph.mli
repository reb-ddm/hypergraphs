open Kappa_mvbdu
open Hypergraph_sig 

module Edges_intbdu :
  Mvbdu_wrapper.Internalized_mvbdu
  with type key = Id.t
   and type value = int


module AdjacencyList :
sig
  type t
  val empty : int -> t
  val mem : Node.t -> t -> bool
  val add :
    Node.t -> NodeSet.t -> t -> unit
  val get : Node.t -> t -> NodeSet.t
  val remove : Node.t -> t -> unit
  val iter :
    (Node.t -> NodeSet.t -> unit) ->
    t -> unit
  val map :
    (Node.t ->
     NodeSet.t -> NodeSet.t) ->
    t -> t
end
module ReachabilityStatus :
sig
  type t
  val empty : int -> t
  val mem : Node.t -> t -> bool
  val add : Node.t -> Edges_intbdu.mvbdu -> t -> unit
  val get : Node.t -> t -> Edges_intbdu.mvbdu
  val remove : Node.t -> t -> unit
  val iter :
    (Node.t -> Edges_intbdu.mvbdu -> unit) -> t -> unit
  val map :
    (Node.t -> Edges_intbdu.mvbdu -> Edges_intbdu.mvbdu) ->
    t -> t
end

module EdgeSet : Set.S with type elt = Edge.t

type parametric_hypergraph = {
  adjacency_list : AdjacencyList.t;
  reachability_status : ReachabilityStatus.t;
  restriction_bdu : Edges_intbdu.mvbdu;
  preds : AdjacencyList.t;
  edge_to_int : EdgeIntConversion.t;
  current_parameter_valuation : bool Res.Array.t;
  is_parametrized : Edge.t -> bool;
  parametrized_edges : EdgeSet.t;
  add_parameter_for_new_edges : bool;
  is_semi_parametrized : bool;
}

include (FullyDynamicHornClauses with type t = parametric_hypergraph)
exception OutOfBounds
val print_mvbdu : Edges_intbdu.mvbdu -> unit
val mvbdu_and : Edges_intbdu.mvbdu -> Edges_intbdu.mvbdu -> Edges_intbdu.mvbdu
val mvbdu_or : t -> Edges_intbdu.mvbdu -> Edges_intbdu.mvbdu -> Edges_intbdu.mvbdu
val mvbdu_equal : t -> Edges_intbdu.mvbdu -> Edges_intbdu.mvbdu -> bool
val get_int_of_edge : Edge.t -> t -> t * int * bool
val preds_of : t -> Node.t -> NodeSet.t
val reachability_mvbdu_of : Node.t -> t -> Edges_intbdu.mvbdu
val set_successors :
  Node.t -> NodeSet.t -> t -> t
val add_successor :
  Node.t -> succ:Node.t -> t -> t
val set_predecessors :
  Node.t -> NodeSet.t -> t -> t
val add_predecessor :
  Node.t -> pred:Node.t -> t -> t
val add_parameter_valuation : int -> bool -> t -> t
val add_new_edge_reachabilities :
  Node.t -> Node.t -> t -> t
val add_simple_edge :
  Node.t -> Node.t -> t -> t
val add_hyperedge :
  IdSet.t ->
  Node.t -> Node.t -> t -> t
val closure : Node.t -> t -> t
