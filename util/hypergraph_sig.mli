module Id : sig 
  type t = int 
  val compare : 'a -> 'a -> int 
end

module IdSet : sig
  include Set.S
  val to_string : t -> string
end with type elt = Id.t
module StringMap : Map.S with type key = string
module IntMap : Map.S with type key = int
exception ExceededNrOfVars

module type OrderedAndPrintable = sig
  include Set.OrderedType 
  val to_string : t -> string
  val empty : t
end

module PriorityQueue :
  functor (Elt : OrderedAndPrintable) ->
  sig
    module EltWithPrio :
    sig
      type t = int option * Elt.t
      val compare : int option * Elt.t -> int option * Elt.t -> int
    end
    module IntOptionSet : Set.S
    val empty : IntOptionSet.t
    val is_empty : IntOptionSet.t -> bool
    val add : prio:int option -> Elt.t -> IntOptionSet.t -> IntOptionSet.t
    val remove_min_opt :
      IntOptionSet.t -> (int option * Elt.t * IntOptionSet.t) option
    val decrease_key :
      Elt.t -> prio:int option -> IntOptionSet.t -> IntOptionSet.t
    val print_all_elements : IntOptionSet.t -> unit
  end

module Vars :
sig
  type var = string
  type var_to_int_map = int StringMap.t
  type int_to_var_map = var IntMap.t
  type t = {
    var_name_to_int : var_to_int_map;
    int_name_to_var : int_to_var_map;
    nr_vars : int;
    next_unused_int : int;
  }
  val init : nr_vars:int -> t
  val var_to_int : t -> string -> t * int * bool
  val int_to_var : t -> int -> string
end

module Node : sig
  type t =
    | True
    | Simple of Id.t
    | Compound of IdSet.t

  val empty: t

  val is_true_node : t -> bool
  val is_simple : t -> bool
  val is_compound : t -> bool

  val simple_node_of_id : Id.t -> t
  val compound_node_of_intset : IdSet.t -> t

  val compare : t -> t -> int

  val is_reflexive_edge : t -> t -> bool
  val fold : (t -> 'a -> 'a) -> t -> 'a -> 'a

  val to_string : t -> string

  val dot_of_node : t -> (Id.t -> string) -> Buffer.t -> int option -> unit

  val dot_of_edge : t -> t -> Buffer.t -> unit
end

module NodeSet : sig 
  include Set.S 
  val to_string : t -> string 
end with type elt = Node.t
module NodeMap : Map.S with type key = Node.t

module type IncrementalHornClauses =
sig
  type t
  type edge = Node.t * Node.t
  val init : nr_vars:int -> t
  val add_node : Node.t -> t -> t
  val add_clause : edge -> t -> t
  val add_clauses : edge list -> t -> t
  val is_reachable : Node.t -> t -> bool
  val to_dot : t -> (int -> string) -> string
end

module type FullyDynamicHornClauses =
sig
  type t
  type edge = Node.t * Node.t
  val init : nr_vars:int -> t
  val add_node : Node.t -> t -> t
  val add_clause : edge -> t -> t
  val add_clauses : edge list -> t -> t
  val is_reachable : Node.t -> t -> bool
  val to_dot : t -> (int -> string) -> string
  val remove_clause : edge -> t -> t
end
