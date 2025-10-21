module RunTests : 
  Hypergraph_sig.FullyDynamicHornClauses -> sig
    val run_tests : string ->  (string * int * (string * string list) list) list -> unit
  end
