module type ProgramToHCS = 
sig
  type program
  type horn_clause_scheme
  type vars

  (** list of removed and added horn clause schemes*)
  val diff_hcschemes : vars -> program -> program -> vars * horn_clause_scheme list * horn_clause_scheme list 
  val empty_program : program
  val empty_vars : vars

end

module type IncrementalSymbolicHornClauseSolverStrategy =
sig
  type t
  type horn_clause_scheme
  type horn_clause
  type literal
  type analysis_result
  type init_data
  val empty_datastructure : t * init_data

  (** The added Horn Clause schemes are registered to the fixpoint algorithm, which updates the next horn clauses to be iterated *)
  val update_add_hcs : t -> horn_clause_scheme list -> t

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

module Framework :
  functor 
    (ProgramTrans : ProgramToHCS) 
    (Abstraction : AbstractionProof) 
    (IncStrategy : IncrementalSymbolicHornClauseSolverStrategy with type horn_clause_scheme = ProgramTrans.horn_clause_scheme and type horn_clause = Abstraction.horn_clause and type init_data = Abstraction.init_data)
    ->
    sig
      type hc_scheme = ProgramTrans.horn_clause_scheme
      type t = ProgramTrans.vars * IncStrategy.t * Abstraction.t
      val init_analyis : t
      val name : string
      val diff :
        ProgramTrans.vars ->
        ProgramTrans.program ->
        ProgramTrans.program ->
        ProgramTrans.vars * hc_scheme list * hc_scheme list
      val incremental_fixpoint :
        t ->
        ProgramTrans.program ->
        ProgramTrans.program ->
        ProgramTrans.vars * IncStrategy.t * Abstraction.t
      val analysis_result : Abstraction.t -> Abstraction.analysis_result
      val initial_analysis :
        ProgramTrans.program ->
        ProgramTrans.vars * IncStrategy.t * Abstraction.t
    end
