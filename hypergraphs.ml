
module Var = Int

module type HornClauses = sig
  type t
  type clause = Var.t * Var.t list
  val empty : t
  val add_clause : clause -> t -> t
  val add_clauses : clause list -> t -> t
  val remove_clause : clause -> t -> t
  val reachable_vars : t -> Var.t list
  val is_reachable : Var.t -> t -> bool
end

module NodeMap = Map.Make(Var)

module Hypergraph : HornClauses = struct

  type reachability_status = int NodeMap.t
  type adjacency_list = int (*TODO*)

  type t = {
    reachability_status : reachability_status;
    adjacency_list : adjacency_list;
    (*TODO data structure for removal*)
  }
  type clause = Var.t * Var.t list
  let empty = {
    reachability_status = NodeMap.empty;
    adjacency_list = 0;
    (*TODO data structure for removal*)
  }
  let add_clause _clause t = t
  let add_clauses _clause t = t
  let remove_clause _clause t = t
  let reachable_vars _t = []
  let is_reachable _var _t = false
end


module ParametricBDDs : HornClauses = struct

  type orthogonal_set = int NodeMap.t
  type adjacency_list = int (*TODO*)
  type bdu = int (*TODO*)

  type t = {
    orthogonal_sets : orthogonal_set list;
    adjacency_list : adjacency_list;
    bdus: bdu;
    (*TODO data structure for removal*)
  }
  type clause = Var.t * Var.t list
  let empty = {
    orthogonal_sets = [];
    adjacency_list = 0;
    bdus = 0;
    (*TODO data structure for removal*)
  }
  let add_clause _clause t = t
  let add_clauses _clauses t = t
  let remove_clause _clause t = t
  let reachable_vars _t = []
  let is_reachable _var _t = false
end
