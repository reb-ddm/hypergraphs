module type Bdd = sig
  type key = int
  type t

  val init : unit -> unit
  val print : t -> unit
  val mvbdu_equal : t -> t -> bool
  val mvbdu_or : t -> t -> t
  val mvbdu_and : t -> t -> t
  val mvbdu_true : unit -> t
  val mvbdu_false : unit -> t
  val is_true : t -> bool
  val is_false : t -> bool
  val mvbdu_of_association_list : (int * int) list -> t
  val mvbdu_project_abstract_away : t -> key list -> t
  val mvbdu_to_formula : t -> int Logical_formulae.formula
  val variables_list_of_mvbdu : t -> key list
  val quit : unit -> unit
end
