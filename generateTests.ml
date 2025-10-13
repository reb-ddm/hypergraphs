(*creates a hypergraph with 2*s + p nodes and 2*s + 2*p edges*)
let test_add size pre_steps =
  let nr_digits_size = String.length (string_of_int size) in
  let nr_digits_pre_steps = String.length (string_of_int pre_steps) in
  let name_pre_steps_var = "a" in
  let name_vars = "x" in
  let name = 
    "generated_test_add_of_size_" ^ string_of_int size ^ 
    "_with_" ^ string_of_int pre_steps ^ "_preceding_nodes" in
  let string_of_int_with_n_digits n nr_digits = 
    let s = string_of_int n in
    let prefix = 
      String.make (nr_digits - String.length s) '0' in
    prefix ^ s 
  in
  (*nodes true -> a_1 -> a_2 -> ....*)
  let pre_steps_vars = 
    List.init pre_steps 
      (fun i -> name_pre_steps_var ^ string_of_int_with_n_digits i nr_digits_pre_steps) 
  in
  let tests, reachable_vars, last_var = 
    List.fold_left (fun (tests, reachable_vars, previous_var) var -> 
        let reachable_vars = reachable_vars @ [var] in 
        ("add " ^ var ^ ":=" ^ previous_var, reachable_vars)::tests, reachable_vars, var
      ) ([], [], "") pre_steps_vars in
  let vars = 
    List.init size 
      (fun i -> name_vars ^ string_of_int_with_n_digits i nr_digits_size) in 
  (*first layer of nodes*)
  let tests, reachable_vars = List.fold_left (fun (tests, reachable_vars) var -> 
      let reachable_vars = reachable_vars @ [var] in 
      ("add " ^ var ^ ":=" ^ last_var, reachable_vars)::tests, reachable_vars
    ) (tests, reachable_vars) vars in
  (*subsequent layers*)
  let rec next_layer var_prefix nr_vars tests reachable_vars =
    if nr_vars = 1 then 
      tests, reachable_vars
    else
      let next_prefix = name_vars ^ var_prefix in
      let next_size = nr_vars / 2 in
      let vars = List.init next_size 
          (fun i -> next_prefix ^ string_of_int_with_n_digits i nr_digits_size) 
      in 
      let tests, reachable_vars, _ = 
        List.fold_left (fun (tests, reachable_vars, i) var -> 
            let reachable_vars = reachable_vars @ [var] in 
            let v1 = var_prefix ^ string_of_int_with_n_digits (i * 2) nr_digits_size in
            let v2 = var_prefix ^ string_of_int_with_n_digits (i * 2 + 1) nr_digits_size in
            ("add " ^ var ^ ":=" ^v1^","^v2, reachable_vars)::tests, reachable_vars, i + 1
          ) (tests, reachable_vars, 0) vars in
      next_layer next_prefix next_size tests reachable_vars
  in
  let tests, reachable_vars = next_layer name_vars size tests reachable_vars in
  (*add direct edges true -> a_2, then true -> a_3 and so on*)
  let tests = 
    List.fold_left (fun tests var -> 
        ("add " ^ var ^ ":=", reachable_vars)::tests
      ) tests (List.tl pre_steps_vars) in
  name, List.rev tests

