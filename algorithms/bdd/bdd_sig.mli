module type Bdd = sig
  type mvbdu
  type key = int

  val variables_list_of_mvbdu : mvbdu -> key list

  type t = Bool of bool | Mvbdu of mvbdu

  val init : unit -> unit
  val print : t -> unit
  val mvbdu_equal : t -> t -> bool
  val mvbdu_or : t -> t -> t
  val mvbdu_and : t -> t -> t
  val mvbdu_true : unit -> t
  val mvbdu_false : unit -> t
  val mvbdu_of_association_list : (int * int) list -> t
  val mvbdu_project_abstract_away : t -> key list -> t
  val mvbdu_to_formula : t -> int Logical_formulae.formula
end
