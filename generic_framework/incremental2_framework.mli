open Hypergraph_sig
module BasicHornClauses: Solver_sig.ProgramToHCS with type horn_clause_scheme = int * IdSet.t and type program = string
module BasicFixpointHypergraph :
  Solver_sig.IncrementalSymbolicHornClauseSolverStrategy with type horn_clause_scheme = int * IdSet.t and type horn_clause = Node.t * Node.t and type init_data = Node.t
module Incremental2Abstraction : Solver_sig.AbstractionProof with type analysis_result = IdSet.t and type horn_clause = Node.t * Node.t and type init_data = Node.t
module Incremental2Framework : HcHandler.HornClauseHandlerType