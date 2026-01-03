open HornClausesTests
open Enabled_tests

let run_tests (enabled_tests, s) =
  let module HypergraphAlg = (val InteractiveAgent.ask_module ()) in
  let module RunTestsIncremental = RunTests (HypergraphAlg) in
  RunTestsIncremental.run_tests
    ("BENCHMARKS " ^ HypergraphAlg.name ^ " Hypergraph " ^ s)
    enabled_tests

let _ =
  run_tests
    (if debug then
       debug_enabled_tests (), ""
     else
       enabled_tests ())

(* let main () =
   let open Sylvan_wrapper in
      let () = init () in
      let m = mvbdu_of_association_list [ 0, 1; 2, 0; 3, 4 ] in
      let () = print m in
      (* let proj = mvbdu_of_association_list [ 0, 0; 1,0; 2, 1; 3, 0 ]
     in *)
      let m = mvbdu_project_abstract_away m [0;1;3] in
      print m

    let _ = main () *)
