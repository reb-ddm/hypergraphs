
module Var = Int

module type HornClauses = sig
  type t
  type clause = Var.t * Var.t list
  val init : nr_vars:int -> t
  val add_clause : clause -> t -> t
  val add_clauses : clause list -> t -> t
  val remove_clause : clause -> t -> t
  val reachable_vars : t -> Var.t list
  val is_reachable : Var.t -> t -> bool
end

module NodeMap = Map.Make(Var)
module StringMap = Map.Make(String)
exception ExceededNrOfVariables

module Variables = struct
  type var_name_to_int = int StringMap.t

  type t = {
    var_name_to_int : var_name_to_int;
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
        raise ExceededNrOfVariables (*TODO catch and print error message*)
      else
        {
          vars with 
          var_name_to_int = StringMap.add var vars.next_unused_int vars.var_name_to_int;
          next_unused_int = vars.next_unused_int + 1;
        }, vars.next_unused_int
end

module Hypergraph : HornClauses = struct

  type reachability_status = int NodeMap.t
  type adjacency_list = int (*TODO*)

  type t = {
    reachability_status : reachability_status;
    adjacency_list : adjacency_list;
    vars: Variables.t
    (*TODO data structure for removal*)
  }
  type clause = Var.t * Var.t list
  
  let init ~nr_vars = 
    {
      reachability_status = NodeMap.empty;
      adjacency_list = 0;
      vars = Variables.init ~nr_vars;
    }

  let add_clause (lhs, rhs_list) t = 
    let vars, lhs = Variables.var_to_int t.vars lhs in
    t
    
  let add_clauses _clause t = t
  let remove_clause _clause t = t
  let reachable_vars _t = []
  let is_reachable _var _t = false
end


module ParametricBDDs : HornClauses = struct

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
  type clause = Var.t * Var.t list
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