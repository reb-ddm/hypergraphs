open HornClausesTests
open Enabled_tests

let run_tests (enabled_tests, s) = 
  let module HypergraphAlg = (val (InteractiveAgent.ask_module ())) in 
  let module RunTestsIncremental = RunTests(HypergraphAlg) in
  RunTestsIncremental.run_tests ("BENCHMARKS " ^ HypergraphAlg.name ^ " Hypergraph " ^ s) enabled_tests 

let _ = 
  run_tests (if debug then debug_enabled_tests (), "" else enabled_tests ())
