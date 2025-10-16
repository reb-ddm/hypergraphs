open HcHandler
open Hypergraph_sig
open Enabled_tests

let debug = false

let debug_print s = 
  if debug then print_endline s else ()

module RunTestsAdd (HC : IncrementalHornClauses) = struct
  module HypergraphHCH = IncrementalHandler(HC) 

  let run_test hc testid (input, expected_output) remove = 
    let _ = debug_print input in
    let _ = debug_print (String.concat "," expected_output) in
    let hc, index, clause, must_be_added = HypergraphHCH.parse_clause input hc in
    let t1 = Sys.time() in
    let hc = if must_be_added then
        HypergraphHCH.add_clause clause hc
      else 
        remove hc clause index 
    in 
    let actual_output = HypergraphHCH.reachable_vars hc in
    let t2 = Sys.time() in
    let success = 
      if List.compare String.compare actual_output expected_output = 0 then
        (* let () = print_endline ("   SUCCESS") in *)
        true
      else
        let () = print_endline ("   TEST " ^ testid ^ " FAILED!") in
        let _ = debug_print (String.concat "," actual_output) in
        false 
    in hc, success, t2-.t1

  let run_tests_remove title remove enabled_tests =
    let _ = print_endline title in 
    List.iter (fun (test_name, nr_vars, commands) ->
        print_endline ("Starting test "^test_name);
        let hc = HypergraphHCH.init ~nr_vars:nr_vars in 
        let _,_,_,total_time = List.fold_left (fun (hc,i,success,time) test ->
            if success then 
              let testid = test_name ^ string_of_int i in
              let hc, success, test_time = run_test hc testid test remove in
              hc, i + 1, success, test_time+.time
            else hc,0,false, time
          ) (hc, 0, true,0.) commands
        in 
        print_endline ("Total time: " ^ string_of_float total_time)
      ) enabled_tests

  (* let ignore_remove hc _ _ = hc *)

  (* let run_tests title  = run_tests_remove title ignore_remove *)
end

module RunTests (HC : FullyDynamicHornClauses) = struct
  include RunTestsAdd(HC)
  module HypergraphHCH = HornClauseHandler(HC) 
  let run_tests title  = run_tests_remove title HypergraphHCH.remove_clause
end

(* module RunTestsIncremental = RunTestsAdd(IncrementalHypergraph) *)
module RunTestsDynamic = RunTests(FullyDynamicHypergraph)

let run_tests enabled_tests = 
  (* let _ = RunTestsIncremental.run_tests "INCREMENTAL REACHABILITY" enabled_tests in *)
  RunTestsDynamic.run_tests "MINIMAL SHORTEST PATH" enabled_tests

let _ = run_tests (enabled_tests ())