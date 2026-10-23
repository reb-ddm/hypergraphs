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
    val update_add_hcs : t -> horn_clause_scheme list -> t
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
    type edge = hs_id * node * node list
    type analysis_result
        type init_data
      val name : string
    val empty : init_data -> t
    val visit_edge : t -> edge -> t * bool
    val remove_edges : t -> hs_id list -> t
    val return_result : t -> analysis_result
  end
module Framework :
  functor 
    (ProgramTrans : ProgramToHCS) 
    (Abstraction : AbstractionProof with type hs_id = ProgramTrans.hs_id) 
    (IncStrategy : IncrementalSymbolicHornClauseSolverStrategy with type horn_clause_scheme = ProgramTrans.horn_clause_scheme and type horn_clause = Abstraction.edge and type literal = Abstraction.node and type hs_id = Abstraction.hs_id and type init_data = Abstraction.init_data)
    ->
    sig
      type hc_scheme = ProgramTrans.horn_clause_scheme
      type t = ProgramTrans.vars * IncStrategy.t * Abstraction.t
      val name : string
      val init_analyis : t
      val diff :
        ProgramTrans.vars -> ProgramTrans.program ->
        ProgramTrans.program -> ProgramTrans.vars * hc_scheme list * IncStrategy.hs_id list
      val incremental_fixpoint :
        t ->
        ProgramTrans.program ->
        ProgramTrans.program -> t
      val analysis_result : Abstraction.t -> Abstraction.analysis_result
      val initial_analysis :
        ProgramTrans.program -> t
    end
