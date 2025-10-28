open HcHandler
open Hypergraph_sig
open Enabled_tests

let debug_print s = 
  if debug then print_endline s else ()

module RunTests (HC : FullyDynamicHornClauses) = struct
  module HypergraphHCH = HornClauseHandler(HC) 

  let run_test hc testid (input, expected_output) = 
    let _ = debug_print input in
    let _ = debug_print (String.concat "," expected_output) in
    let hc, index, clause, must_be_added = HypergraphHCH.parse_clause input hc in
    let t1 = Sys.time() in
    let hc = if must_be_added then
        HypergraphHCH.add_clause clause hc
      else 
        HypergraphHCH.remove_clause hc clause index 
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
    in hc, success, must_be_added, t2-.t1

  let run_tests title enabled_tests =
    let _ = print_endline title in 
    List.iter (fun (test_name, nr_vars, commands) ->
        print_endline ("Starting test "^test_name);
        let hc = HypergraphHCH.init ~nr_vars:nr_vars in 
        let _,_,_,add_time, remove_time = 
          List.fold_left (fun (hc,i,success, add_time, remove_time) test ->
              if success then 
                let testid = test_name ^ string_of_int i in
                let hc, success, is_add, test_time = run_test hc testid test in
                let add_time, remove_time = 
                  if is_add 
                  then 
                    add_time+. test_time, remove_time 
                  else 
                    add_time, remove_time +. test_time in
                hc, i + 1, success, add_time, remove_time
              else hc,0,false, add_time, remove_time
            ) (hc, 0, true,0.,0.) commands
        in 
        print_endline ("Total time adding: " ^ string_of_float add_time);
        print_endline ("Total time removing: " ^ string_of_float remove_time)
      ) enabled_tests

end

