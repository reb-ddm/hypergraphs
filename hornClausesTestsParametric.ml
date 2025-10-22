open HornClausesTests
open Incremental_analysis_algorithms
open Enabled_tests

module RunTestsIncremental = RunTests(ParametricHypergraph)

let run_tests enabled_tests = 
  RunTestsIncremental.run_tests "PARAMETRIC MVBDUS" enabled_tests 

let _ = run_tests (if debug then debug_enabled_tests () else enabled_tests ())
