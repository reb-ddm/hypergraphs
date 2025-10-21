open HornClausesTests
open Incremental_analysis_algorithms
open Enabled_tests

module RunTestsIncremental = RunTests(IncrementalHypergraph)

let run_tests enabled_tests = 
  RunTestsIncremental.run_tests "INCREMENTAL REACHABILITY" enabled_tests 

let _ = run_tests (if debug then debug_enabled_tests () else enabled_tests ())