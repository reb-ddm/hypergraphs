module type ProgramToHCS = 
sig
  type program
  type horn_clause_scheme
  type vars

  (** list of removed and added horn clause schemes*)
  val diff_hcschemes : vars -> program -> program -> vars * horn_clause_scheme list * (horn_clause_scheme * int) list 
  val empty_program : program
  val empty_vars : vars
  val clause_of_index : int -> vars -> horn_clause_scheme option
  val var_to_string : int -> vars -> string
  val print_clause : vars -> horn_clause_scheme -> unit
end

module type IncrementalSymbolicHornClauseSolverStrategy =
sig
  type t
  type horn_clause_scheme
  type horn_clause
  type analysis_result
  type init_data
  val empty_datastructure : t * init_data

  (** The added Horn Clause schemes are registered to the fixpoint algorithm, which updates the next horn clauses to be iterated *)
  val update_add_hcs : t -> horn_clause_scheme list ->t

  (** The removed Horn Clause schemes are registered to the fixpoint algorithm, which updates the next horn clauses to be iterated *)
  val update_remove_hcs : t -> horn_clause_scheme list -> t 

  val next_hc_add : t -> t * horn_clause option

  (** Given a Horn Clause that modified the result, add all its successors to the next horn clauses to be iterated. Used when a Horn Clause is added. *)
  val propagate_change_add : t -> horn_clause -> t

  val next_hc_remove_inf : t -> t * horn_clause option

  (** Given a Horn Clause that modified the result, add all its successors tp the next horn clauses to be iterated. Used in the first phase of the removal of a clause. *)
  val propagate_change_remove_inf : t -> horn_clause -> t

  val next_hc_restore_inf : t -> t * horn_clause option

  (** Given a Horn Clause that modified the result, add all its successors tp the next horn clauses to be iterated. Used in the second phase of the removal of a clause. *)
  val propagate_change_restore_inf : t -> horn_clause -> t
end

module type AbstractionProof = 
sig
  type t 
  type horn_clause
  type analysis_result
  type init_data
  val name : string
  val empty : init_data -> t

  (** one edge is visited during the adding of some edges. Returns true if the result changed.*)
  val visit_edge_add : t -> horn_clause -> t * bool 

  (** one edge is visited during the removing of some edges and it is set to unreachable. Returns true if the result changed.*)
  val remove_inf_edge : t -> horn_clause -> t * bool 

  (** one edge is visited during the removal of some edges in order to restore the information that was lost during the "remove information" phase. Returns true if the result changed.*)
  val restore_inf_edge : t -> horn_clause -> t * bool 

  val return_result : t -> analysis_result
end

module Framework
    (ProgramTrans : ProgramToHCS) 
    (Abstraction : AbstractionProof) 
    (IncStrategy : IncrementalSymbolicHornClauseSolverStrategy with type horn_clause_scheme = ProgramTrans.horn_clause_scheme and type horn_clause = Abstraction.horn_clause and type init_data = Abstraction.init_data) 
= 
struct

  type hc_scheme = ProgramTrans.horn_clause_scheme
  type t = ProgramTrans.vars * IncStrategy.t * Abstraction.t

  let init_analyis : t = 
    let inc, init = IncStrategy.empty_datastructure in
    ProgramTrans.empty_vars, inc, Abstraction.empty init

  let name = "FRAMEWORK: " ^ Abstraction.name

  let diff (vars : ProgramTrans.vars) (p_before : ProgramTrans.program) (p_after : ProgramTrans.program) : ProgramTrans.vars * hc_scheme list * hc_scheme list =
    let t, add, remove = ProgramTrans.diff_hcschemes vars p_before p_after
  in t, add, List.map fst remove


  let fixpoint next_hc visit_edge propagate_change fixpoint_strat result = 
    let rec fixpoint fixpoint_strat result = 
      match next_hc fixpoint_strat with
      | fixpoint_strat, None -> fixpoint_strat, result
      | fixpoint_strat, Some hc -> 
        let result, changed = visit_edge result hc in
        let fixpoint_strat = if changed then propagate_change fixpoint_strat hc else fixpoint_strat in
        fixpoint fixpoint_strat result
    in
    fixpoint fixpoint_strat result

  let fixpoint_add = fixpoint IncStrategy.next_hc_add Abstraction.visit_edge_add IncStrategy.propagate_change_add

  let fixpoint_remove_inf = fixpoint IncStrategy.next_hc_remove_inf Abstraction.remove_inf_edge IncStrategy.propagate_change_remove_inf

  let fixpoint_restore_inf = fixpoint IncStrategy.next_hc_restore_inf Abstraction.restore_inf_edge IncStrategy.propagate_change_restore_inf

  let incremental_fixpoint ((vars, fixpoint_strat, result) : t) (old_program : ProgramTrans.program) (new_program : ProgramTrans.program) =
    let vars, added_hc, removed_hcs = ProgramTrans.diff_hcschemes vars old_program new_program in
    let fixpoint_strat = IncStrategy.update_remove_hcs fixpoint_strat (List.map fst removed_hcs) in
    let fixpoint_strat, result = fixpoint_remove_inf fixpoint_strat result in
    let fixpoint_strat, result = fixpoint_restore_inf fixpoint_strat result in
    let fixpoint_strat = IncStrategy.update_add_hcs fixpoint_strat added_hc in
    let fixpoint_strat, result = fixpoint_add fixpoint_strat result in
    vars, fixpoint_strat, result

  let analysis_result ((_v, _f, datastructure) : t) : Abstraction.analysis_result = 
    Abstraction.return_result datastructure

  let initial_analysis (program : ProgramTrans.program) =
    incremental_fixpoint init_analyis ProgramTrans.empty_program program


end

open Hypergraph_sig
module HCHandlerFramework  
(ProgramTrans : ProgramToHCS with type horn_clause_scheme = int * IdSet.t and type program = string) 
    (Abstraction : AbstractionProof with type analysis_result = IdSet.t) 
    (IncStrategy : IncrementalSymbolicHornClauseSolverStrategy with type horn_clause_scheme = ProgramTrans.horn_clause_scheme and type horn_clause = Abstraction.horn_clause and type init_data = Abstraction.init_data)
    : HcHandler.HornClauseHandlerType =
struct
  type clause = int * IdSet.t
  include Framework (ProgramTrans) (Abstraction) (IncStrategy)

  let init ~nr_vars =
    let _ = nr_vars in
    init_analyis

  let add_clause new_hc (vars, fixpoint_strat, result) =
    let fixpoint_strat = IncStrategy.update_add_hcs fixpoint_strat [new_hc] in
    let fixpoint_strat, result = fixpoint_add fixpoint_strat result in
    vars, fixpoint_strat, result

  let clause_of_index i (vars, _, _) =
    ProgramTrans.clause_of_index i vars

  let reachable_vars (vars, fixpoint_strat, result) = 
    IdSet.fold (fun l v -> v @ [ProgramTrans.var_to_string l vars]) (analysis_result (vars, fixpoint_strat, result)) []

  let first_reachable_vars t = 
    List.to_seq (reachable_vars t)

  let reachable_vars_string hc = String.concat ", " (reachable_vars hc)


  let to_dot _ = "" (*TODO*)

  type parse_output =
    | Add of clause list
    | Remove of clause * int
    | OpenChapter of clause list
    | CloseChapter


  let print_clause (vars, _, _) (head, body) =
    ProgramTrans.print_clause vars (head, body)
  let parse_clause s (vars, inc, abs) = 
    let vars, add, remove = ProgramTrans.diff_hcschemes vars ProgramTrans.empty_program s in
    if add = [] && List.length remove = 1 then let a,b = List.hd remove in (vars, inc, abs), Remove (a,b) else
      (vars, inc, abs), Add add
  let to_string _ = "" (*TODO*)
  let remove_clause (vars, fixpoint_strat, result) removed_hcs _ = 
    let fixpoint_strat = IncStrategy.update_remove_hcs fixpoint_strat [removed_hcs] in
    let fixpoint_strat, result = fixpoint_remove_inf fixpoint_strat result in
    let fixpoint_strat, result = fixpoint_restore_inf fixpoint_strat result in
    let fixpoint_strat, result = fixpoint_add fixpoint_strat result in
    vars, fixpoint_strat, result
  let open_chapter t _ = t
  let close_chapter t = t
end
