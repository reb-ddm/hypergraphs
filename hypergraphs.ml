

module NodeMap = Map.Make(Int)
module StringMap = Map.Make(String)
exception ExceededNrOfVars

module Vars = struct
  type var = string
  type var_to_int_map = int StringMap.t

  type t = {
    var_name_to_int : var_to_int_map;
    nr_vars: int;
    next_unused_int: int;
  }

  let init ~nr_vars= {
    var_name_to_int = StringMap.empty;
    nr_vars;
    next_unused_int = 0;
  }

  let var_to_int vars var = 
    match StringMap.find_opt var vars.var_name_to_int with
    | Some i -> vars, i
    | None -> if vars.next_unused_int = vars.nr_vars then
        raise ExceededNrOfVars (*TODO catch and print error message*)
      else
        {
          vars with 
          var_name_to_int = StringMap.add var vars.next_unused_int vars.var_name_to_int;
          next_unused_int = vars.next_unused_int + 1;
        }, vars.next_unused_int
end

module type HornClauses = sig
  type t
  type clause = Vars.var * Vars.var list
  val init : nr_vars:int -> t
  val add_clause : clause -> t -> t
  val add_clauses : clause list -> t -> t
  val remove_clause : clause -> t -> t
  val reachable_vars : t -> Vars.var list
  val is_reachable : Vars.var -> t -> bool
end


module Hypergraph : HornClauses = struct

  type reachability_status = int NodeMap.t
  type adjacency_list = int (*TODO*)

  type t = {
    reachability_status : reachability_status;
    adjacency_list : adjacency_list;
    vars: Vars.t
    (*TODO data structure for removal*)
  }
  type clause = Vars.var * Vars.var list
  
  let init ~nr_vars = 
    {
      reachability_status = NodeMap.empty;
      adjacency_list = 0;
      vars = Vars.init ~nr_vars;
    }

  let add_clause (lhs, rhs_list) t = 
    let vars, lhs = Vars.var_to_int t.vars lhs in
    t

  let add_clauses _clause t = t
  let remove_clause _clause t = t
  let reachable_vars _t = []
  let is_reachable _var _t = false
end


module ParametricHypergraph : HornClauses = struct

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
  type clause = Vars.var * Vars.var list
  let init ~nr_vars = {
    orthogonal_sets = [];
    adjacency_list = 0;
    bdus = Ckappa_sig.Views_intbdu.mvbdu_false ();
    nr_sites = [];
    nr_vars;
  }

  let add_clause _clause t = t
  let add_clauses _clauses t = t
  let remove_clause _clause t = t
  let reachable_vars _t = []
  let is_reachable _var _t = false
end

let calculate r = r