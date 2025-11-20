open Hypergraph_sig
exception InvalidVarName of string
exception InvalidCharachter of string
exception InvalidCommand of string
module HornClauseHandler :
  functor (HC : Hypergraph_sig.FullyDynamicHornClauses) ->
  sig
    type clause = int * Hypergraph_sig.IdSet.t
    type t = {
      vars : Hypergraph_sig.Vars.t;
      horn_clauses_rev : clause list;
      horn_clause_handler : HC.t;
      compound_to_int : CompoundIntConversion.t;
    }
    val update_vars : t -> Hypergraph_sig.Vars.t -> t
    val update_handler : t -> HC.t -> t
    val update_hcr : t -> clause list -> t
    val insert_var : t -> string -> t * int
    val init : nr_vars:int -> t
    val clause_to_edge :
      int * Hypergraph_sig.IdSet.t -> t -> 
      (Hypergraph_sig.Node.t * Hypergraph_sig.Node.t * IdSet.t) * t
    val add_clause : int * Hypergraph_sig.IdSet.t -> t -> t
    val reachable_vars : t -> string list
    val first_reachable_vars : t -> string list (*returns only the first 1000 reachable vars*)
    val reachable_vars_string : t -> string
    val to_dot : t -> string
    type parse_output =  
        Add of clause | Remove of clause * int | OpenChapter of clause list | CloseChapter
    val parse_clause : string -> t -> t * parse_output
    val to_string : t -> string
    val remove_clause : t -> int * Hypergraph_sig.IdSet.t -> int -> t
    val open_chapter : t -> clause list -> t
    val close_chapter : t -> t
  end
