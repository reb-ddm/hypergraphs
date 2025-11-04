open Printf
open HcHandler
open Hypergraph_sig
open Enabled_tests

let debug_print s = 
  if debug then print_endline s else ()

module RunTests (HC : FullyDynamicHornClauses) = struct
  module HypergraphHCH = HornClauseHandler(HC)

  module IntMap = Map.Make(Int)

  type bench_result = {
    add_time : float;
    remove_time : float;
  }

  (* results: size -> steps -> bench_result *)
  type results = bench_result IntMap.t IntMap.t

  let empty_results : results = IntMap.empty

  let update_results results size steps add_time remove_time =
    let step_map =
      match IntMap.find_opt size results with
      | Some sm -> sm
      | None -> IntMap.empty
    in
    let updated_step_map =
      IntMap.add steps { add_time; remove_time } step_map
    in
    IntMap.add size updated_step_map results

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
        true
      else
        let () = print_endline ("   TEST " ^ testid ^ " FAILED!") in
        let _ = debug_print (String.concat "," actual_output) in
        false 
    in hc, success, must_be_added, t2-.t1

  let run_tests title enabled_tests =
    let _ = print_endline title in 
    (* collect results *)
    let results =
      List.fold_left
        (fun results (test_name, coordinates, nr_vars, commands) ->
           (* infer number of steps from test_name, e.g. generated_test_remove2_of_size_10_with_2_steps *)
           print_endline ("Starting test "^test_name);
           let hc = HypergraphHCH.init ~nr_vars in
           let _,_,_,add_time, remove_time = 
             List.fold_left
               (fun (hc,i,success, add_time, remove_time) test ->
                  if success then
                    let testid = test_name ^ string_of_int i in
                    let hc, success, is_add, test_time = run_test hc testid test in
                    let add_time, remove_time =
                      if is_add then 
                        add_time+. test_time, remove_time
                      else 
                        add_time, remove_time +. test_time
                    in
                    hc, i + 1, success, add_time, remove_time
                  else
                    hc,0,false, add_time, remove_time)
               (hc, 0, true, 0., 0.) commands
           in
           match coordinates with 
           Some (size, steps) ->
           update_results results size steps add_time remove_time
           | None -> results)
        empty_results enabled_tests
    in
    (* print markdown table *)
    let all_sizes = IntMap.bindings results |> List.map fst |> List.sort compare in
    let all_steps =
      List.flatten
        (List.map
           (fun (_, sm) -> IntMap.bindings sm |> List.map fst)
           (IntMap.bindings results))
      |> List.sort_uniq compare
    in
    let print_table label selector =
      printf "\n- %s\n\n" (String.uppercase_ascii label);
      printf "|       | %s |\n"
        (String.concat " | " (List.map string_of_int all_steps));
      printf "| ----- %s |\n"
        (String.concat "" (List.map (fun _ -> "| ---- ") all_steps));
      List.iter
        (fun size ->
           printf "| %-5d " size;
           List.iter
             (fun step ->
                let v =
                  match IntMap.find_opt size results with
                  | None -> ""
                  | Some sm -> (
                      match IntMap.find_opt step sm with
                      | None -> ""
                      | Some { add_time; remove_time } ->
                        let t = selector add_time remove_time in
                        if t < 0.00005 then "0"
                        else if t < 1. then sprintf "%.2f" t
                        else sprintf "%.1f" t)
                in
                printf "| %-4s " v)
             all_steps;
           print_endline "|")
        all_sizes
    in

    print_table "ADDING" (fun add _ -> add);
    print_table "REMOVING" (fun _ rem -> rem)
end


