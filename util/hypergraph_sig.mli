module Id : sig 
  type t = int
  val compare : 'a -> 'a -> t 
end

module IdSet : sig
  include Set.S
  val to_string : t -> string
  val empty : unit -> t
end with type elt = Id.t
module StringMap : Map.S with type key = string
module IntMap : Map.S with type key = int
exception ExceededNrOfVars


module type OrderedAndPrintable = sig
  include Set.OrderedType 
  val to_string : t -> string
end

module type HasEmpty = sig
  type t
  val empty : unit -> t
end

module Vars :
sig
  type t
  val get_nr_vars : t -> int
  val get_vars : t -> (int * string) list
  val get_vars_in_alphabetical_order : t -> (string * int) list
  val init : nr_vars:int -> t
  val add_var : t -> string -> int -> t
  val var_to_int : t -> string -> t * int * bool
  val int_to_var : t -> int -> string
end

module CompoundIntConversion : sig
  type t
  val empty : t
  val val_to_int : t  -> IdSet.t -> t * int * bool
  val bindings : t -> (IdSet.t * int) list
end

module Node : sig
  (**The "true" node is a compound node with Id 0 (It has no incoming edge)*)
  type t =
    | Simple of Id.t
    | Compound of Id.t

  val is_simple : t -> bool
  val is_compound : t -> bool
  val is_true : t -> bool

  val simple_node_of_id : Id.t -> t
  val compound_node_of_intset : 
    IdSet.t -> CompoundIntConversion.t -> CompoundIntConversion.t * t

  val compare : t -> t -> int

  val is_reflexive_edge : IdSet.t -> t -> bool

  val to_string : t -> string

  val dot_of_node : t -> (Id.t -> string) -> Buffer.t -> string option -> unit

  val dot_of_edge : t -> t -> Buffer.t -> unit
end

module Edge : sig
  type t = Node.t * Node.t
  val compare : t -> t -> int
end
module EdgeIntConversion : sig
  type t
  val empty : t
  val val_to_int : t  -> Edge.t -> t * int * bool
  val bindings : t -> (Edge.t * int) list
end

module NodeSet : sig 
  include Set.S 
  val empty : unit -> t
  val to_string : t -> string 
end with type elt = Node.t
module NodeMap : Map.S with type key = Node.t

val add_element_to_res_array : 'a Res.Array.t -> length:int -> index:int ->
  elt:'a -> empty:'a -> unit

module MutableNodeMap :
  functor (Elt: HasEmpty) -> sig
    type t
    val empty : int -> t
    val mem : Node.t -> t -> bool
    val add : Node.t -> Elt.t -> t -> unit
    val get : Node.t -> t -> Elt.t
    val remove : Node.t -> t -> unit
    val iter : (Node.t -> Elt.t -> unit) -> t -> unit
    val map : (Node.t -> Elt.t -> Elt.t) -> t -> t
  end

type edge = Node.t * Node.t * IdSet.t

module type FullyDynamicHornClauses =
sig
  type t
  val name : string
  val init : nr_vars:int -> Node.t -> t
  val add_clause : edge -> t -> t
  val add_clauses : edge list -> t -> t
  val is_reachable : Node.t -> t -> bool
  val weight_of : t -> Node.t -> string option
  val succs_of : t -> Node.t -> NodeSet.t
  val remove_clause : edge -> t -> t
  val open_chapter : edge list -> t -> t
  val close_chapter : t -> t
end

type test_suite = string * (int * int) option * int * (string * string list) list
