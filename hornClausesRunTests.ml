open HornClausesTests
open Enabled_tests

let run_tests enabled_tests = 
let module HypergraphAlg = (val (InteractiveAgent.ask_module "Input the algorithm version: (incremental/mst/parametric)")) in 
  let module RunTestsIncremental = RunTests(HypergraphAlg) in
  RunTestsIncremental.run_tests "RUNNING TESTS" enabled_tests 

let _ = run_tests (if debug then debug_enabled_tests () else enabled_tests ())
