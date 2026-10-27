module RunTests : functor (_ : HcHandler.HornClauseHandlerType) -> sig
  val run_tests : string -> Hypergraph_sig.test_suite list -> unit
end
