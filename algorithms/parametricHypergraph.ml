(* open Hypergraph_sig

module ParametricHypergraph : FullyDynamicHornClauses = struct

  type orthogonal_set = int NodeMap.t
  type adjacency_list = int (*TODO*)
  type bdu = Ckappa_sig.Views_intbdu.mvbdu (*TODO*)

  type t = {
    orthogonal_sets : orthogonal_set list;
    adjacency_list : adjacency_list;
    bdus: bdu;
    nr_sites: int list;
    nr_vars: int;
    (*TODO data structure for removal*)
  }
  type edge = Node.t * Node.t
  let init ~nr_vars = {
    orthogonal_sets = [];
    adjacency_list = 0;
    bdus = Ckappa_sig.Views_intbdu.mvbdu_false ();
    nr_sites = [];
    nr_vars;
  }

  let add_node _node t = t
  let add_clause _clause t = t
  let add_clauses _clauses t = t
  let remove_clause _clause t = t
  let reachable_vars _t = []
  let is_reachable _var _t = false
  let to_dot _ _ = ""
end

include ParametricHypergraph *)