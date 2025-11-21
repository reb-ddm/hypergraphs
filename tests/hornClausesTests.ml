open Printf
open HcHandler
open Hypergraph_sig
open Enabled_tests
open Lwt.Infix

let debug_print s = 
  if debug then print_endline s else ()

module RunTests (HC : FullyDynamicHornClauses) = struct
  module HypergraphHCH = HornClauseHandler(HC)

  module IntMap = Map.Make(Int)

  let timeout = 20.

  type bench_result = {
    add_time : float;
    remove_time : float;
    open_chapter_time : float;
    reachable_vars_time : float;
  }

  (* results: size -> steps -> bench_result *)
  type results = bench_result IntMap.t IntMap.t

  let empty_results : results = IntMap.empty

  let run_with_timeout timeout f =
    let with_timeout () = 
      let f = fun () -> Lwt.return (
          let start_time = Sys.time () in  
          let result = f() in
          let end_time = Sys.time () in
          result, end_time -. start_time
        ) in
      Lwt.catch
        (fun () -> Lwt_unix.with_timeout timeout f >|= fun x -> Some x)
        (function Lwt_unix.Timeout -> Lwt.return_none | exn -> Lwt.fail exn)
    in
    Lwt_main.run (with_timeout ())

  let update_results results size steps add_time remove_time open_chapter_time reachable_vars_time =
    let step_map =
      match IntMap.find_opt size results with
      | Some sm -> sm
      | None -> IntMap.empty
    in
    let updated_step_map =
      IntMap.add steps { add_time; remove_time ; open_chapter_time ; reachable_vars_time } step_map
    in
    IntMap.add size updated_step_map results

  let rec compare_list_first_n cmp xs ys n = 
    if n = 0 then 0 else
      match xs(), ys() with
      | Seq.Nil, Seq.Nil ->
        0
      | Seq.Cons (x, xs), Seq.Cons (y, ys) ->
        let c = cmp x y in
        if c <> 0 then c else compare_list_first_n cmp xs ys (n-1)
      | Seq.Nil, Seq.Cons (_, _) ->
        print_endline "Lengths do not match";
        0
      | Seq.Cons (_, _), Seq.Nil ->
        print_endline "Lengths do not match";
        0

  let run_test hc testid (input, expected_output) = 
    let _ = debug_print input in
    let _ = debug_print (String.concat "," expected_output) in
    let hc, parse_output = HypergraphHCH.parse_clause input hc in
    let hc_opt, must_be_added = match parse_output with
      | Add clause ->
        run_with_timeout timeout (fun () -> HypergraphHCH.add_clause clause hc), Some true
      | Remove (clause, index) ->
        run_with_timeout timeout (fun () -> HypergraphHCH.remove_clause hc clause index), Some false
      | OpenChapter clauses -> 
        run_with_timeout timeout (fun () -> HypergraphHCH.open_chapter hc clauses), None
      | CloseChapter ->
        run_with_timeout timeout (fun () -> HypergraphHCH.close_chapter hc), None
    in 
    match hc_opt with 
    | None -> 
      hc, false, must_be_added, timeout, 0.
    | Some (hc, time) ->
      let start_time = Sys.time () in  
      let actual_output = HypergraphHCH.first_reachable_vars hc in
      let end_time = Sys.time () in  
      let success = 
        let expected_output = Seq.take 1000 (List.to_seq expected_output) in
        if compare_list_first_n String.compare actual_output expected_output 1000 = 0 then
          true
        else
          let () = print_endline ("   TEST " ^ testid ^ " FAILED!") in
          let _ = debug_print (String.concat "," (List.of_seq actual_output)) in
          false 
      in let reachable_vars_time = end_time -. start_time
      in hc, success, must_be_added, time, reachable_vars_time

  let run_tests title enabled_tests =
    let _ = print_endline title in 
    (* collect results *)
    let results =
      List.fold_left
        (fun results (test_name, coordinates, nr_vars, commands) ->
           (* infer number of steps from test_name, e.g. generated_test_remove2_of_size_10_with_2_steps *)
           print_endline ("Starting test "^test_name);
           let hc = HypergraphHCH.init ~nr_vars in
           let _,_,success,add_time, remove_time, open_chapter_time, reachable_vars_time = 
             List.fold_left
               (fun (hc,i,success, add_time, remove_time, open_chapter_time, reachable_vars_time) test ->
                  if success then
                    let testid = test_name ^ string_of_int i in
                    let hc, success, is_add, test_time, test_reachable_vars_time = run_test hc testid test in
                    let add_time, remove_time, open_chapter_time =
                      if is_add = Some true then 
                        add_time+. test_time, remove_time, open_chapter_time
                      else if is_add = Some false then
                        add_time, remove_time +. test_time, open_chapter_time
                      else 
                        add_time, remove_time, open_chapter_time +. test_time
                    in
                    if add_time +. remove_time +. open_chapter_time > timeout then
                      let () = print_endline ("   TIMEOUT FOR TEST " ^ testid ^ "!") in
                      hc, 0, false, add_time, remove_time, open_chapter_time, reachable_vars_time
                    else
                      hc, i + 1, success, add_time, remove_time, open_chapter_time, reachable_vars_time +. test_reachable_vars_time
                  else
                    hc,0,false, add_time, remove_time, open_chapter_time, reachable_vars_time)
               (hc, 0, true, 0., 0., 0., 0.) commands
           in
           if success then
             match coordinates with 
               Some (size, steps) ->
               update_results results size steps add_time remove_time open_chapter_time reachable_vars_time
             | None -> results
           else 
             match coordinates with 
               Some (size, steps) ->
               update_results results size steps timeout timeout timeout timeout
             | None -> results
        )
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
                      | Some result ->
                        let t = selector result in
                        if 0. < t && t < 0.00005 then "0"
                        else if t < 1. then sprintf "%.2f" t
                        else sprintf "%.1f" t)
                in
                printf "| %-4s " v)
             all_steps;
           print_endline "|")
        all_sizes
    in

    print_table "ADDING" (fun t -> t.add_time);
    print_table "REMOVING" (fun t -> t.remove_time);
    print_table "OPEN/CLOSE CHAPTER" (fun t -> t.open_chapter_time);
    print_table "REACHABLE VARS" (fun t -> t.reachable_vars_time);

    (* PLOT WITH GNUPLOT *)
    let module Gp = Gnuplot in

    let plot_add_time_third_step ~results ~steps =
      (* pick the third step *)
      let step_to_plot =
        match steps with
        | fst :: _ -> fst
        | _ -> invalid_arg "Need at least one step"
      in

      (* build the data series *)
      let extract_series f =
        IntMap.fold (fun size step_map acc ->
            match IntMap.find_opt step_to_plot step_map with
            | Some br ->
              (float_of_int size, f br) :: acc
            | None -> acc) results []
      in

      let add_series = Gp.Series.linespoints_xy ~title:"add time" (extract_series (fun br -> br.add_time)) in
      let remove_series = Gp.Series.linespoints_xy ~title:"remove time" (extract_series (fun br -> br.remove_time)) in
      let open_chapter_series = Gp.Series.linespoints_xy ~title:"open chapter time" (extract_series (fun br -> br.open_chapter_time)) in
      let reachable_vars_series = Gp.Series.linespoints_xy ~title:"reachable vars time" (extract_series (fun br -> br.reachable_vars_time)) in


      let gp = Gp.create () in
      Gp.plot_many gp 
        ~output:(Gp.Output.create (`Png_cairo ("output/"^ title ^".png"))) 
        [add_series; remove_series; open_chapter_series; reachable_vars_series];
      Gp.close gp
    in 
    plot_add_time_third_step ~results ~steps:all_steps
end
