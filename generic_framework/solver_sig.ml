module type ProgramToHCS = 
sig
  type program
  type hs_id
  type horn_clause_scheme
  type vars
  val diff_hcschemes : vars -> program -> program -> vars * horn_clause_scheme list * hs_id list
  val empty_program : program
  val empty_vars : vars
    val hs_of_id : vars -> hs_id -> horn_clause_scheme

end

module type IncrementalSymbolicHornClauseSolverStrategy =
sig
  type t
  type horn_clause_scheme
  type horn_clause
  type hs_id
  type literal
  type analysis_result
  type init_data
  val update_add_hcs : t -> horn_clause_scheme list -> t (* The Horn Clause scheme is registered to the fixpoint algorithm, which updates the next horn clauses to be iterated *)
    val update_remove_hcs : t -> horn_clause_scheme list -> t
  val next_hc : t -> t * horn_clause option
  val propagate_change : t -> horn_clause -> t
  val empty_datastructure : t * init_data
end

module type AbstractionProof = 
sig
  type t 
  type node
  type hs_id
  type edge = hs_id * node * node list(* (hs_id, conclusion, premise) *)
  type analysis_result
  type init_data
  val name : string
  val empty : init_data -> t
  val visit_edge : t -> edge -> t * bool (* one edge is visited during the adding of some edges. Returns true if the result changed.*)
  val remove_edges : t -> hs_id list -> t (* removes the edges, updates the abstraction and potentially recomputes some of the information that was lost *)
  val return_result : t -> analysis_result
end

module Framework
    (ProgramTrans : ProgramToHCS) 
    (Abstraction : AbstractionProof with type hs_id = ProgramTrans.hs_id) 
    (IncStrategy : IncrementalSymbolicHornClauseSolverStrategy with type horn_clause_scheme = ProgramTrans.horn_clause_scheme and type horn_clause = Abstraction.edge and type literal = Abstraction.node and type hs_id = Abstraction.hs_id and type init_data = Abstraction.init_data) 
   = 
struct

  type hc_scheme = ProgramTrans.horn_clause_scheme
  type t = ProgramTrans.vars * IncStrategy.t * Abstraction.t

  let init_analyis : t = 
    let inc, init = IncStrategy.empty_datastructure in
    ProgramTrans.empty_vars, inc, Abstraction.empty init

  let name = "FRAMEWORK: " ^ Abstraction.name

  let diff (vars : ProgramTrans.vars) (p_before : ProgramTrans.program) (p_after : ProgramTrans.program) : ProgramTrans.vars * hc_scheme list * Abstraction.hs_id list =
    ProgramTrans.diff_hcschemes vars p_before p_after

  let rec fixpoint fixpoint_strat result = 
    match IncStrategy.next_hc fixpoint_strat with
    | fixpoint_strat, None -> fixpoint_strat, result
    | fixpoint_strat, Some hc -> 
      let result, changed = Abstraction.visit_edge result hc in
      let fixpoint_strat = if changed then IncStrategy.propagate_change fixpoint_strat hc else fixpoint_strat in
      fixpoint fixpoint_strat result
  
  let incremental_fixpoint ((vars, fixpoint_strat, result) : t) (old_program : ProgramTrans.program) (new_program : ProgramTrans.program) =
    let vars, added_hc, removed_hc = ProgramTrans.diff_hcschemes vars old_program new_program in
    let removed_hc_id = List.map (ProgramTrans.hs_of_id vars) removed_hc in
    let fixpoint_strat = IncStrategy.update_remove_hcs fixpoint_strat removed_hc_id in
    let result = Abstraction.remove_edges result removed_hc in
    (*TODO remove all information and then restart the fixpoint analysis with the set of removed edges.*)
    let fixpoint_strat = IncStrategy.update_add_hcs fixpoint_strat added_hc in
    
    let fixpoint_strat, result = fixpoint fixpoint_strat result in
    vars, fixpoint_strat, result
  
  let analysis_result (datastructure : Abstraction.t) : Abstraction.analysis_result = 
    Abstraction.return_result datastructure

  let initial_analysis (program : ProgramTrans.program) =
    incremental_fixpoint init_analyis ProgramTrans.empty_program program
    
end