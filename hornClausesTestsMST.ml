open HornClausesTests
open Incremental_analysis_algorithms
open Enabled_tests

module RunTestsDynamic = RunTests(FullyDynamicHypergraph)

let run_tests enabled_tests = 
  RunTestsDynamic.run_tests "MINIMAL SHORTEST PATH" enabled_tests

let _ = run_tests (if debug then debug_enabled_tests () else enabled_tests ())