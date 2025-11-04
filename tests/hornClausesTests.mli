module RunTests : 
  Hypergraph_sig.FullyDynamicHornClauses -> sig
    val run_tests : string -> Hypergraph_sig.test_suite list -> unit
  end
