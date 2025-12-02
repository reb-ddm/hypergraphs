open Hypergraph_sig

exception InvalidVarName of string
exception InvalidCharachter of string
exception InvalidCommand of string

module HornClauseHandler : functor (_ : FullyDynamicHornClauses) -> sig
  type clause = int * IdSet.t
  type t

  val init : nr_vars:int -> t
  val add_clause : int * IdSet.t -> t -> t
  val reachable_vars : t -> string list

  val first_reachable_vars : t -> string Seq.t
  (**Returns only the first 1000 reachable vars.*)

  val reachable_vars_string : t -> string
  val to_dot : t -> string

  type parse_output =
    | Add of clause
    | Remove of clause * int
    | OpenChapter of clause list
    | CloseChapter

  val parse_clause : string -> t -> t * parse_output
  val to_string : t -> string
  val remove_clause : t -> int * IdSet.t -> int -> t
  val open_chapter : t -> clause list -> t
  val close_chapter : t -> t
end
