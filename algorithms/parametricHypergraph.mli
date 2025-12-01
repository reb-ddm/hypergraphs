open Kappa_mvbdu
open Hypergraph_sig 

module Edges_intbdu : sig

  module Internal_mvbdu :
    Mvbdu_wrapper.Internalized_mvbdu
    with type key = Id.t
     and type value = int

  val build_association_list : (int * int) list -> Internal_mvbdu.hconsed_association_list
  val mvbdu_of_hconsed_asso : Internal_mvbdu.hconsed_association_list -> Internal_mvbdu.mvbdu
  val mvbdu_project_abstract_away : Internal_mvbdu.mvbdu -> Internal_mvbdu.hconsed_variables_list -> Internal_mvbdu.mvbdu
  val build_variables_list : Internal_mvbdu.key list -> Internal_mvbdu.hconsed_variables_list
  val mvbdu_of_association_list : (int * int) list -> Internal_mvbdu.mvbdu
  val mvbdu_of_range_list : (int * (int option * int option)) list -> Internal_mvbdu.mvbdu
  val variables_list_of_mvbdu : Internal_mvbdu.mvbdu -> Internal_mvbdu.hconsed_variables_list
  val extensional_of_variables_list :
    Internal_mvbdu.hconsed_variables_list -> Internal_mvbdu.key list

  type t = Bool of bool | Mvbdu of Internal_mvbdu.mvbdu

  val init : unit -> unit 
  val print : t -> unit
  val mvbdu_equal : t -> t -> bool
  val mvbdu_or : t -> t -> t
  val mvbdu_and : t -> t -> t
  val mvbdu_true : unit -> t
  val mvbdu_false : unit -> t

  (**If the mvbdu is equal to mvbdu_true of mvbdu_false then instead of storing the bdu, we only store true/false*)
  val mvbdu_normalize : Internal_mvbdu.mvbdu -> t
end


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
  val add : Node.t -> Edges_intbdu.t -> t -> unit
  val get : Node.t -> t -> Edges_intbdu.t
  val remove : Node.t -> t -> unit
  val iter :
    (Node.t -> Edges_intbdu.t -> unit) -> t -> unit
  val map :
    (Node.t -> Edges_intbdu.t -> Edges_intbdu.t) ->
    t -> t
end

module EdgeSet : Set.S with type elt = Edge.t

type parametric_hypergraph = {
  adjacency_list : AdjacencyList.t;
  reachability_status : ReachabilityStatus.t;
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
val mvbdu_or :  Edges_intbdu.t -> Edges_intbdu.t -> Edges_intbdu.t
val mvbdu_equal : Edges_intbdu.t -> Edges_intbdu.t -> bool
val get_int_of_edge : Edge.t -> t -> t * int * bool
val preds_of : t -> Node.t -> NodeSet.t
val reachability_mvbdu_of : Node.t -> t -> Edges_intbdu.t
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
val closure_process : EdgeSet.t -> t -> t
val closure : Node.t -> t -> t
