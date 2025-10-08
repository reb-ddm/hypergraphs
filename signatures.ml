
module StringMap = Map.Make(String)
module IntMap = Map.Make(Int)
exception ExceededNrOfVars

module Vars = struct
  type var = string
  type var_to_int_map = int StringMap.t
  type int_to_var_map = string IntMap.t

  type t = {
    var_name_to_int : var_to_int_map;
    int_name_to_var : int_to_var_map;
    nr_vars: int;
    next_unused_int: int;
  }

  let init ~nr_vars= {
    var_name_to_int = StringMap.empty;
    int_name_to_var = IntMap.empty;
    nr_vars;
    next_unused_int = 0;
  }

  let var_to_int vars var =
    match StringMap.find_opt var vars.var_name_to_int with
    | Some i -> (vars, i)
    | None ->
      if vars.next_unused_int = vars.nr_vars then (*TODO this seems unnecessary*)
        raise ExceededNrOfVars
      else
        let i = vars.next_unused_int in
        ({
          vars with
            var_name_to_int = StringMap.add var i vars.var_name_to_int;
      int_name_to_var = IntMap.add i var vars.int_name_to_var;
            next_unused_int = vars.next_unused_int + 1;
        }, i)

let int_to_var vars id =
   IntMap.find id vars.int_name_to_var


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
  val to_dot : t -> string
end


