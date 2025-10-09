open HornClauseHandler

let t1 = "very_simple_add", [
    ("add a:=b,c", []);
    ("add b:=", ["b"]);
    ("add c:=", ["a";"b";"c"]);
  ]

let t2 = "very_simple_remove", [
    ("add a:=", ["a"]);
    ("add b:=", ["a";"b"]);
    ("remove 1", ["b"]);
    ("remove 1", []);
  ]

let enabled_tests = [t1; t2]

module HypergraphHCH = HornClauseHandler(ClassicalHypergraph)

let run_test hc testid (input, expected_output) = 
  let hc, index, clause, must_be_added = HypergraphHCH.parse_clause input hc in
  let hc = if must_be_added then
      HypergraphHCH.add_clause clause hc
    else
      HypergraphHCH.remove_clause hc clause index 
  in 
  let actual_output = HypergraphHCH.reachable_vars hc in
  let success = 
    if List.compare String.compare actual_output expected_output = 0 then
      true
    else
      let () = print_endline ("   TEST " ^ testid ^ " FAILED!")
      in false 
  in hc, success


let run_tests () = 
  List.iter (fun (test_name, commands) ->
    print_endline ("Starting test "^test_name);
      let hc = HypergraphHCH.init ~nr_vars:400 in 
      let _ = List.fold_left (fun (hc,i,success) test ->
          if success then 
            let testid = test_name ^ string_of_int i in
            let hc, success = run_test hc testid test in
            hc, i + 1, success
          else hc,0,false
        ) (hc, 0, true) commands
      in ()
    ) enabled_tests