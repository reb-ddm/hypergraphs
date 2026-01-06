let string_of_int_with_n_digits n nr_digits =
  let s = string_of_int n in
  let prefix = String.make (nr_digits - String.length s) '0' in
  prefix ^ s

let reachable_vars_append current_length reachable_vars v =
  if current_length > 1000 then
    reachable_vars
  else
    reachable_vars @ [ v ]

(*nodes true -> a_1 -> a_2 -> ....a_steps*)
let chain_of_nodes_from_true reachable_vars tests j steps var_name nr_digits =
  let pre_steps_vars =
    List.init steps (fun i ->
        var_name ^ string_of_int_with_n_digits i nr_digits)
  in
  ( pre_steps_vars,
    List.fold_left
      (fun (tests, reachable_vars, previous_var, i) var ->
        let reachable_vars = reachable_vars_append i reachable_vars var in
        ( ("add " ^ var ^ ":=" ^ previous_var, reachable_vars) :: tests,
          reachable_vars,
          var,
          i + 1 ))
      (tests, reachable_vars, "", j)
      pre_steps_vars )

(*edges node -> a_1; node -> a_2 ..... node -> a_size*)
let simple_edges_starting_from_a_node node size name_vars nr_digits
    reachable_vars tests j =
  let vars =
    List.init size (fun i ->
        name_vars ^ string_of_int_with_n_digits i nr_digits)
  in
  ( vars,
    List.fold_left
      (fun (tests, reachable_vars, i) var ->
        let reachable_vars = reachable_vars_append i reachable_vars var in
        ( ("add " ^ var ^ ":=" ^ node, reachable_vars) :: tests,
          reachable_vars,
          i + 1 ))
      (tests, reachable_vars, j) vars )

(*remove all edges node -> a_1; node -> a_2 ..... node -> a_size*)
let remove_simple_edges_starting_from_a_node vars =
  let a, _, _ =
    List.fold_left
      (fun (tests, reachable_vars, i) var ->
        ( ("remove " ^ string_of_int i, reachable_vars) :: tests,
          reachable_vars_append i reachable_vars var,
          i + 1 ))
      ([], [], 0) vars
  in
  a

(*add direct edges true -> a_2, then true -> a_3 and so on*)
let add_simple_edges_from_true tests pre_steps_vars reachable_vars =
  List.fold_left
    (fun tests var -> ("add " ^ var ^ ":=", reachable_vars) :: tests)
    tests (List.tl pre_steps_vars)

let add_alternative_paths_edges tests pre_steps_vars reachable_vars copy_var =
  List.fold_left
    (fun (tests, previous_var, reachable_vars) var ->
      let var2 = copy_var var in
      let tests = ("add " ^ var ^ ":=" ^ var2, reachable_vars) :: tests in
      let reachable_vars = reachable_vars @ [ var2 ] in
      ( ("add " ^ var2 ^ ":=" ^ previous_var, reachable_vars) :: tests,
        var,
        reachable_vars ))
    (tests, "", reachable_vars)
    pre_steps_vars

(*creates a hypergraph with 2*s + p nodes and 2*s + 2*p edges*)
let test_add size pre_steps =
  let nr_nodes = (2 * size) + pre_steps in
  let nr_digits_size = String.length (string_of_int size) in
  let nr_digits_pre_steps = String.length (string_of_int pre_steps) in
  let name_pre_steps_var = "a" in
  let name_vars = "x" in
  let name =
    "generated_test_add_of_size_" ^ string_of_int size ^ "_with_"
    ^ string_of_int pre_steps ^ "_preceding_nodes"
  in
  let pre_steps_vars, (tests, reachable_vars, last_var, j) =
    chain_of_nodes_from_true [] [] 0 pre_steps name_pre_steps_var
      nr_digits_pre_steps
  in
  let _, (tests, reachable_vars, j) =
    simple_edges_starting_from_a_node last_var size name_vars nr_digits_size
      reachable_vars tests j
  in
  (*subsequent layers*)
  let rec next_layer var_prefix nr_vars tests reachable_vars =
    if nr_vars = 1 then
      tests, reachable_vars
    else (
      let next_prefix = name_vars ^ var_prefix in
      let next_size = nr_vars / 2 in
      let vars =
        List.init next_size (fun i ->
            next_prefix ^ string_of_int_with_n_digits i nr_digits_size)
      in
      let tests, reachable_vars, _ =
        List.fold_left
          (fun (tests, reachable_vars, i) var ->
            let reachable_vars =
              reachable_vars_append (j + i) reachable_vars var
            in
            let v1 =
              var_prefix ^ string_of_int_with_n_digits (i * 2) nr_digits_size
            in
            let v2 =
              var_prefix
              ^ string_of_int_with_n_digits ((i * 2) + 1) nr_digits_size
            in
            ( ("add " ^ var ^ ":=" ^ v1 ^ "," ^ v2, reachable_vars) :: tests,
              reachable_vars,
              i + 1 ))
          (tests, reachable_vars, 0) vars
      in
      next_layer next_prefix next_size tests reachable_vars
    )
  in
  let tests, reachable_vars = next_layer name_vars size tests reachable_vars in
  let tests2 = add_simple_edges_from_true [] pre_steps_vars reachable_vars in
  name, Some (size, pre_steps), nr_nodes, List.rev tests, List.rev tests2

let test_add2 size pre_steps =
  let nr_nodes = size + pre_steps + 1 in
  let nr_digits_size = String.length (string_of_int size) in
  let nr_digits_pre_steps = String.length (string_of_int pre_steps) in
  let name_pre_steps_var = "a" in
  let name_vars = "x" in
  let name_goal_var = "y" in
  let name =
    "generated_test_add2_of_size_" ^ string_of_int size ^ "_with_"
    ^ string_of_int pre_steps ^ "_preceding_nodes"
  in
  let pre_steps_vars, (tests, reachable_vars, last_var, j) =
    chain_of_nodes_from_true [] [] 0 pre_steps name_pre_steps_var
      nr_digits_pre_steps
  in
  let vars, (tests, reachable_vars, j) =
    simple_edges_starting_from_a_node last_var size name_vars nr_digits_size
      reachable_vars
      (tests @ [ "close", [] ])
      j
  in
  (* edge with source the set of all x_i and with goal node y*)
  let last_test = "add " ^ name_goal_var ^ ":=" ^ String.concat "," vars in
  let reachable_vars =
    reachable_vars_append (j + 1) reachable_vars name_goal_var
  in
  let tests, reachable_vars =
    (last_test, reachable_vars) :: tests, reachable_vars
  in
  let tests2 = add_simple_edges_from_true [] pre_steps_vars reachable_vars in
  name, Some (size, pre_steps), nr_nodes, List.rev tests, List.rev tests2

let test_remove size pre_steps =
  let nr_nodes = size + pre_steps + 1 in
  let nr_digits_size = String.length (string_of_int size) in
  let nr_digits_pre_steps = String.length (string_of_int pre_steps) in
  let name_pre_steps_var = "a" in
  let name_vars = "x" in
  let name_goal_var = "y" in
  let name =
    "generated_test_remove_of_size_" ^ string_of_int size ^ "_with_"
    ^ string_of_int pre_steps ^ "_preceding_nodes"
  in
  let pre_steps_vars, (tests, reachable_vars, last_var, j) =
    chain_of_nodes_from_true [] [] 0 pre_steps name_pre_steps_var
      nr_digits_pre_steps
  in
  let vars, (tests, reachable_vars, j) =
    simple_edges_starting_from_a_node last_var size name_vars nr_digits_size
      reachable_vars tests j
  in
  (* edge with source the set of all x_i and with goal node y*)
  let last_test = "add " ^ name_goal_var ^ ":=" ^ String.concat "," vars in
  let reachable_vars = reachable_vars_append j reachable_vars name_goal_var in
  let tests = List.rev ((last_test, reachable_vars) :: tests) in
  let remove_tests = remove_simple_edges_starting_from_a_node pre_steps_vars in
  name, Some (size, pre_steps), nr_nodes, tests, remove_tests

let test_remove2 size steps =
  let nr_nodes = size + 2 in
  let nr_digits_size = String.length (string_of_int size) in
  let name_pre_steps_var = "a1" in
  let name_vars = "x" in
  let name_goal_var = "y" in
  let name =
    "generated_test_remove2_of_size_" ^ string_of_int size ^ "_with_"
    ^ string_of_int steps ^ "_steps"
  in
  let close = "close", [] in
  let tests, reachable_vars =
    ( [ "add " ^ name_pre_steps_var ^ ":=", [ name_pre_steps_var ]; close ],
      [ name_pre_steps_var ] )
  in
  let vars, (tests, reachable_vars, j) =
    simple_edges_starting_from_a_node name_pre_steps_var size name_vars
      nr_digits_size reachable_vars tests 1
  in
  (* edge with source the set of all x_i and with goal node y*)
  let open_chapter = "open 0" in
  let last_test = "add " ^ name_goal_var ^ ":=" ^ String.concat "," vars in
  let reachable_vars = reachable_vars_append j reachable_vars name_goal_var in
  let tests = (last_test, reachable_vars) :: tests in
  let remove_tests =
    List.init steps (fun i ->
        if i mod 2 = 0 then (
          let index =
            if i = 0 then
              0
            else
              1 + size + (i / 2)
          in
          "remove " ^ string_of_int index, []
        ) else
          "add " ^ name_pre_steps_var ^ ":=", reachable_vars)
  in
  ( name,
    Some (size, steps),
    nr_nodes,
    List.rev tests,
    (open_chapter, reachable_vars) :: remove_tests )

let test_remove3 size steps =
  let nr_nodes = size + 3 in
  let nr_digits_size = String.length (string_of_int size) in
  let name_pre_steps_var = "a1" in
  let name_vars = "x" in
  let name_goal_var = "y" in
  let name_goal_var2 = "y2" in
  let name =
    "generated_test_remove3_of_size_" ^ string_of_int size ^ "_with_"
    ^ string_of_int steps ^ "_steps"
  in
  let close = "close", [] in
  let tests, reachable_vars =
    ( [ "add " ^ name_pre_steps_var ^ ":=", [ name_pre_steps_var ]; close ],
      [ name_pre_steps_var ] )
  in
  let vars, (tests, reachable_vars, j) =
    simple_edges_starting_from_a_node name_pre_steps_var size name_vars
      nr_digits_size reachable_vars tests 1
  in
  (* edge with source the set of all x_i and with goal node y*)
  let last_test = "add " ^ name_goal_var ^ ":=" ^ String.concat "," vars in
  let reachable_vars = reachable_vars_append j reachable_vars name_goal_var in
  let open_test = "open ()", reachable_vars in
  let last_test2 = "add " ^ name_goal_var2 ^ ":=" ^ name_goal_var in
  let tests = (last_test, reachable_vars) :: tests in
  let reachable_vars2 = reachable_vars @ [ name_goal_var2 ] in
  let remove_tests =
    List.init steps (fun i ->
        if i mod 2 = 0 then (
          let index = 2 + size + (i / 2) in
          "remove " ^ string_of_int index, reachable_vars
        ) else
          "add " ^ name_goal_var2 ^ ":=" ^ name_goal_var, reachable_vars2)
  in
  ( name,
    Some (size, steps),
    nr_nodes,
    List.rev tests,
    open_test
    :: (last_test2, reachable_vars @ [ name_goal_var2 ])
    :: remove_tests )

let test_remove4_5 size pre_steps remove_first_edge name =
  let nr_nodes = size + pre_steps + 1 in
  let nr_digits_size = String.length (string_of_int size) in
  let nr_digits_pre_steps = String.length (string_of_int pre_steps) in
  let name_pre_steps_var = "a" in
  let name_vars = "x" in
  let name_goal_var = "y" in
  let name =
    "generated_test_remove" ^ name ^ "_of_size_" ^ string_of_int size ^ "_with_"
    ^ string_of_int pre_steps ^ "_preceding_nodes"
  in
  let close = "close", [] in
  let pre_steps_vars, (tests, reachable_vars, last_var, j) =
    chain_of_nodes_from_true [] [] 0 pre_steps name_pre_steps_var
      nr_digits_pre_steps
  in
  let vars, (tests, reachable_vars, j) =
    simple_edges_starting_from_a_node last_var size name_vars nr_digits_size
      reachable_vars (tests @ [ close ]) j
  in
  (* edge with source the set of all x_i and with goal node y*)
  let last_test = "add " ^ name_goal_var ^ ":=" ^ String.concat "," vars in
  let reachable_vars = reachable_vars_append j reachable_vars name_goal_var in
  let tests = (last_test, reachable_vars) :: tests in
  let open_test =
    if remove_first_edge then
      "open 0", reachable_vars
    else
      "open ()", reachable_vars
  in
  let prefix_remove_tests =
    List.rev (add_simple_edges_from_true [] pre_steps_vars reachable_vars)
    @ [ open_test ]
  in
  let reachable_vars =
    if remove_first_edge then
      List.tl reachable_vars
    else
      reachable_vars
  in
  let remove_first_edge_test =
    if remove_first_edge then
      [ "remove 0", reachable_vars ]
    else
      []
  in
  let remove_tests =
    List.init (pre_steps - 1) (fun i ->
        let reachable_vars =
          if i = pre_steps - 2 && remove_first_edge then
            []
          else
            reachable_vars
        in
        "remove " ^ string_of_int (pre_steps + size + 1 + i), reachable_vars)
  in
  let remove_tests =
    if remove_first_edge then
      List.rev remove_tests
    else
      remove_tests
  in
  ( name,
    Some (size, pre_steps),
    nr_nodes,
    List.rev tests,
    List.rev (remove_tests @ remove_first_edge_test @ prefix_remove_tests) )

let test_remove4 size p = test_remove4_5 size p false "4"
let test_remove5 size p = test_remove4_5 size p true "5"

let test_remove6 size pre_steps =
  let nr_nodes = size + (2 * pre_steps) + 1 in
  let nr_digits_size = String.length (string_of_int size) in
  let nr_digits_pre_steps = String.length (string_of_int pre_steps) in
  let name_pre_steps_var = "a" in
  let name_vars = "x" in
  let name_goal_var = "y" in
  let name =
    "generated_test_remove6_of_size_" ^ string_of_int size ^ "_with_"
    ^ string_of_int pre_steps ^ "_preceding_nodes"
  in
  let close = "close", [] in
  let pre_steps_vars, (tests, reachable_vars, last_var, j) =
    chain_of_nodes_from_true [] [] 0 pre_steps name_pre_steps_var
      nr_digits_pre_steps
  in
  let vars, (tests, reachable_vars, j) =
    simple_edges_starting_from_a_node last_var size name_vars nr_digits_size
      reachable_vars (tests @ [ close ]) j
  in
  (* edge with source the set of all x_i and with goal node y*)
  let last_test = "add " ^ name_goal_var ^ ":=" ^ String.concat "," vars in
  let reachable_vars = reachable_vars_append j reachable_vars name_goal_var in
  let tests = (last_test, reachable_vars) :: tests in
  let open_test = "open 0-" ^ string_of_int (pre_steps - 1) in
  let copy_var var = "z" ^ var ^ "_2" in
  let addpath_tests, _, reachable_vars =
    add_alternative_paths_edges [] pre_steps_vars reachable_vars copy_var
  in
  let tests = addpath_tests @ tests in
  let remove_tests =
    List.init pre_steps (fun i -> "remove " ^ string_of_int i, reachable_vars)
  in
  let tests = tests in
  ( name,
    Some (size, pre_steps),
    nr_nodes,
    List.rev tests,
    (open_test, reachable_vars) :: List.rev remove_tests )

let test_remove7 size pre_steps =
  let nr_nodes = size + (2 * pre_steps) + 2 in
  let nr_digits_size = String.length (string_of_int size) in
  let nr_digits_pre_steps = String.length (string_of_int pre_steps) in
  let name_pre_steps_var1 = "a" in
  let name_pre_steps_var2 = "b" in
  let last_var = "c" in
  let name_vars = "x" in
  let name_goal_var = "y" in
  let name =
    "generated_test_remove7_of_size_" ^ string_of_int size ^ "_with_"
    ^ string_of_int pre_steps ^ "_preceding_nodes"
  in
  let close = "close", [] in
  let _pre_steps_vars1, (tests, reachable_vars, last_var1, j) =
    chain_of_nodes_from_true [] [ close ] 0 pre_steps name_pre_steps_var1
      nr_digits_pre_steps
  in
  let _pre_steps_vars2, (tests, reachable_vars, last_var2, j) =
    chain_of_nodes_from_true reachable_vars tests j pre_steps
      name_pre_steps_var2 nr_digits_pre_steps
  in
  let additional_edge =
    "add " ^ last_var ^ ":=" ^ last_var1 ^ "," ^ last_var2
  in
  let reachable_vars = reachable_vars_append j reachable_vars last_var in
  let vars, (tests, reachable_vars, j) =
    simple_edges_starting_from_a_node last_var size name_vars nr_digits_size
      reachable_vars
      ((additional_edge, reachable_vars) :: tests)
      (j + 1)
  in
  (* edge with source the set of all x_i and with goal node y*)
  let last_test = "add " ^ name_goal_var ^ ":=" ^ String.concat "," vars in
  let reachable_vars = reachable_vars_append j reachable_vars name_goal_var in
  let tests = (last_test, reachable_vars) :: tests in
  let remove_tests =
    List.init pre_steps (fun i ->
        let var =
          name_pre_steps_var1 ^ string_of_int_with_n_digits i nr_digits_pre_steps
        in
        let previous_var =
          if i = 0 then
            ""
          else
            name_pre_steps_var1 ^ string_of_int_with_n_digits (i - 1) nr_digits_pre_steps
        in
        [
          "add " ^ var ^ ":=" ^ previous_var, reachable_vars;
          "remove " ^ string_of_int i, [];
          (*Not the right set, but it doesnt matter*)
          
        ])
  in
  let open_test = "open 0-" ^ string_of_int (pre_steps - 1) in
  ( name,
    Some (size, pre_steps),
    nr_nodes,
    List.rev tests,
    (open_test, reachable_vars) :: List.rev (List.flatten remove_tests) )


    
let test_remove8 size steps =
  let og_steps = steps in
  let nr_nodes = steps + size + 3 in
  let nr_digits_size = String.length (string_of_int size) in
  let name_pre_steps_var1 = "a1" in
    let name_pre_steps_var2 = "a2" in
  let name_vars = "x" in
  let name_goal_var = "y" in
  let name =
    "generated_test_remove8_of_size_" ^ string_of_int size ^ "_with_"
    ^ string_of_int steps ^ "_steps"
  in
  let close = "close", [] in
  let tests =
    [ "add " ^ name_pre_steps_var1 ^ ":=", [ name_pre_steps_var1 ]; close ]
  in
  let tests, reachable_vars =
    ( ("add " ^ name_pre_steps_var2 ^ ":=", [ name_pre_steps_var1; name_pre_steps_var2 ]) :: tests ,
      [ name_pre_steps_var1; name_pre_steps_var2 ] )
  in
  let vars, (tests, reachable_vars, j) =
    simple_edges_starting_from_a_node name_pre_steps_var1 size name_vars
      nr_digits_size reachable_vars tests 1
  in
  (* edge with source the set of all x_i and with goal node y*)
  let open_chapter = "open 2-" ^string_of_int (min size steps + 2) in
  let last_test = "add " ^ name_goal_var ^ ":=" ^ String.concat "," vars in
  let reachable_vars = reachable_vars_append j reachable_vars name_goal_var in
  let tests = (last_test, reachable_vars) :: tests in
  let steps = min steps size in
  let add_tests =
    List.init steps (fun i ->
          "add " ^  name_vars ^ string_of_int_with_n_digits i nr_digits_size ^ ":=" ^ name_pre_steps_var2, reachable_vars)
  in
  let remove_tests =
    List.init steps (fun i ->
      let index = i + 3 + size in
          "remove " ^ string_of_int index, []
          )
  in
  ( name,
    Some (size, og_steps),
    nr_nodes,
    List.rev tests,
    (open_chapter, reachable_vars) :: add_tests @ remove_tests )
