open HornClauseHandler
open GenerateTests

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

let t3 = "simple_loop", [
    ("add x1:=", ["x1"]);
    ("add x2:=", ["x1";"x2"]);
    ("add x3:=", ["x1";"x2";"x3"]);
    ("add x4:=", ["x1";"x2";"x3";"x4"]);
    ("add x5:=x1,x2", ["x1";"x2";"x3";"x4";"x5"]);
    ("add x6:=x3,x4,x5", ["x1";"x2";"x3";"x4";"x5";"x6"]);
    ("add x5:=x6", ["x1";"x2";"x3";"x4";"x5";"x6"]);
    ("remove 5", ["x1";"x2";"x3";"x4"]);
  ]

let t4 = "simple_loop_wrong_order", [
    ("add x5:=x1,x2", []);
    ("add x1:=", ["x1"]);
    ("add x6:=x3,x4,x5", ["x1"]);
    ("add x5:=x6", ["x1"]);
    ("add x3:=", ["x1"; "x3"]);
    ("add x4:=", ["x1"; "x3"; "x4"]);
    ("add x2:=", ["x1";"x2";"x3";"x4";"x5";"x6"]);
    ("remove 1", ["x1";"x2";"x3";"x4"]);
    ("add x6:=x4", ["x1";"x2";"x3";"x4";"x5";"x6"]);
  ]

let t5 = "reflexive_edge",[
    ("add a:=a", []);
    ("remove 1", []);
    ("add a:=f,a,w", []);
    ("remove 1", []);
]

let simple_tests = [t1; t2; t3; t4; t5]
let generated_tests = [test_add 10; test_add 100; test_add 1000; test_add 10000; test_add 100000]
let enabled_tests = simple_tests

module HypergraphHCH = HornClauseHandler(ClassicalHypergraph)

let run_test hc testid (input, expected_output) = 
  (* let () = print_endline input in
    let () = print_endline (String.concat "," expected_output) in *)
  let hc, index, clause, must_be_added = HypergraphHCH.parse_clause input hc in
  let hc = if must_be_added then
      HypergraphHCH.add_clause clause hc
    else
      HypergraphHCH.remove_clause hc clause index 
  in 
  let actual_output = HypergraphHCH.reachable_vars hc in
  let success = 
    if List.compare String.compare actual_output expected_output = 0 then
      (* let () = print_endline ("   SUCCESS") in *)
      true
    else
      let () = print_endline ("   TEST " ^ testid ^ " FAILED!") in
      false 
  in hc, success


let run_tests () = 
  List.iter (fun (test_name, commands) ->
      print_endline ("Starting test "^test_name);
      let hc = HypergraphHCH.init ~nr_vars:40000 in 
      let _ = List.fold_left (fun (hc,i,success) test ->
          if success then 
            let testid = test_name ^ string_of_int i in
            let hc, success = run_test hc testid test in
            hc, i + 1, success
          else hc,0,false
        ) (hc, 0, true) commands
      in ()
    ) enabled_tests