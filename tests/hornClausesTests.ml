open HcHandler
open Hypergraph_sig
open Enabled_tests
open Lwt.Infix

let debug_print s =
  if debug then
    print_endline s
  else
    ()

module RunTests (HC : FullyDynamicHornClauses) = struct
  module HypergraphHCH = HornClauseHandler (HC)
  module IntMap = Map.Make (Int)

  let timeout = 180.

  type bench_result = {
    add_time: float;
    remove_time: float;
    open_chapter_time: float;
    reachable_vars_time: float;
    build_time: float;
  }

  (* results: size -> steps -> bench_result *)
  type results = bench_result IntMap.t IntMap.t

  let empty_results : results = IntMap.empty

  let run_with_timeout timeout f =
    let with_timeout () =
      let f () =
        Lwt.return
          (let start_time = Sys.time () in
           let result = f () in
           let end_time = Sys.time () in
           result, end_time -. start_time)
      in
      Lwt.catch
        (fun () -> Lwt_unix.with_timeout timeout f >|= fun x -> Some x)
        (function
          | Lwt_unix.Timeout -> Lwt.return_none
          | exn -> Lwt.fail exn)
    in
    Lwt_main.run (with_timeout ())

  let update_results results er size steps add_time remove_time
      open_chapter_time reachable_vars_time build_time =
    let step_map =
      match IntMap.find_opt size results with
      | Some sm -> sm
      | None -> IntMap.empty
    in
    let updated_step_map =
      IntMap.add steps
        {
          add_time;
          remove_time;
          open_chapter_time;
          reachable_vars_time;
          build_time;
        }
        step_map
    in
    ( IntMap.add size updated_step_map results,
      {
        add_time = er.add_time +. add_time;
        remove_time = er.remove_time +. remove_time;
        open_chapter_time = er.open_chapter_time +. open_chapter_time;
        reachable_vars_time = er.reachable_vars_time +. reachable_vars_time;
        build_time = er.build_time +. build_time;
      } )

  let rec compare_list_first_n cmp xs ys n =
    if n = 0 then
      0
    else (
      match xs (), ys () with
      | Seq.Nil, Seq.Nil -> 0
      | Seq.Cons (x, xs), Seq.Cons (y, ys) ->
        let c = cmp x y in
        if c <> 0 then
          c
        else
          compare_list_first_n cmp xs ys (n - 1)
      | Seq.Nil, Seq.Cons (_, _) ->
        print_endline "Lengths do not match";
        0
      | Seq.Cons (_, _), Seq.Nil ->
        print_endline "Lengths do not match";
        0
    )

  let run_test hc testid (input, expected_output) =
    let _ = debug_print input in
    let _ = debug_print (String.concat "," expected_output) in
    let hc, parse_output = HypergraphHCH.parse_clause input hc in
    let hc_opt, must_be_added =
      match parse_output with
      | Add clauses ->
        ( run_with_timeout timeout (fun () -> List.fold_left (fun hc clause -> HypergraphHCH.add_clause clause hc) hc clauses),
          Some true )
      | Remove (clause, index) ->
        ( run_with_timeout timeout (fun () ->
              HypergraphHCH.remove_clause hc clause index),
          Some false )
      | OpenChapter clauses ->
        ( run_with_timeout timeout (fun () ->
              HypergraphHCH.open_chapter hc clauses),
          None )
      | CloseChapter ->
        ( run_with_timeout timeout (fun () -> HypergraphHCH.close_chapter hc),
          None )
    in
    match hc_opt with
    | None -> hc, false, must_be_added, timeout, 0.
    | Some (hc, time) ->
      let start_time = Sys.time () in
      let actual_output = HypergraphHCH.first_reachable_vars hc in
      let end_time = Sys.time () in
      let success =
        let expected_output = Seq.take 1000 (List.to_seq expected_output) in
        if
          compare_list_first_n String.compare actual_output expected_output 1000
          = 0
        then
          true
        else (
          let () = print_endline ("   TEST " ^ testid ^ " FAILED!") in
          let _ = debug_print (String.concat "," (List.of_seq actual_output)) in
          false
        )
      in
      let reachable_vars_time = end_time -. start_time in
      hc, success, must_be_added, time, reachable_vars_time

  let run_tests title enabled_tests =
    let _ = print_endline title in
    let enabled_results =
      {
        add_time = 0.;
        remove_time = 0.;
        open_chapter_time = 0.;
        reachable_vars_time = 0.;
        build_time = 0.;
      }
    in
    let results, enabled_results =
      List.fold_left
        (fun (results, enabled_results)
             (test_name, coordinates, nr_vars, commands_build, commands) ->
          print_endline ("Starting test " ^ test_name);
          let hc = HypergraphHCH.init ~nr_vars in
          let hc, _, success1, build_time, reachable_vars_time1 =
            Hypergraph_sig.fold_until
              (fun (hc, i, _, build_time, reachable_vars_time) test ->
                let testid = test_name ^ "_build_graph" ^ string_of_int i in
                let hc, success, is_add, test_time, test_reachable_vars_time =
                  run_test hc testid test
                in
                let build_time, success =
                  match is_add with
                  | Some true -> build_time +. test_time, success
                  | Some false ->
                    let () =
                      print_endline
                        "Warning: there was an ignored 'remove' while building \
                         the graph"
                    in
                    0., false
                  | None ->
                    if i = 0 && fst test = "close" then
                      0., success
                    else (
                      let () =
                        print_endline
                          "Warning: there was an ignored 'open' or 'close' \
                           while building the graph"
                      in
                      0., false
                    )
                in
                if build_time > timeout then (
                  let () =
                    print_endline ("   TIMEOUT FOR TEST " ^ testid ^ "!")
                  in
                  hc, 0, false, build_time, reachable_vars_time
                ) else
                  ( hc,
                    i + 1,
                    success,
                    build_time,
                    reachable_vars_time +. test_reachable_vars_time ))
              (fun (_, _, success, _, _) -> not success)
              (hc, 0, true, 0., 0.) commands_build
          in
          let ( _,
                _,
                success,
                add_time,
                remove_time,
                open_chapter_time,
                reachable_vars_time ) =
            Hypergraph_sig.fold_until
              (fun ( hc,
                     i,
                     _,
                     add_time,
                     remove_time,
                     open_chapter_time,
                     reachable_vars_time ) test ->
                let testid = test_name ^ string_of_int i in
                let hc, success, is_add, test_time, test_reachable_vars_time =
                  run_test hc testid test
                in
                let add_time, remove_time, open_chapter_time =
                  match is_add with
                  | Some true ->
                    add_time +. test_time, remove_time, open_chapter_time
                  | Some false ->
                    add_time, remove_time +. test_time, open_chapter_time
                  | None ->
                    add_time, remove_time, open_chapter_time +. test_time
                in
                if add_time +. remove_time +. open_chapter_time > timeout then (
                  let () =
                    print_endline ("   TIMEOUT FOR TEST " ^ testid ^ "!")
                  in
                  ( hc,
                    0,
                    false,
                    add_time,
                    remove_time,
                    open_chapter_time,
                    reachable_vars_time )
                ) else
                  ( hc,
                    i + 1,
                    success,
                    add_time,
                    remove_time,
                    open_chapter_time,
                    reachable_vars_time +. test_reachable_vars_time ))
              (fun (_, _, success, _, _, _, _) -> not success)
              (hc, 0, success1, 0., 0., 0., reachable_vars_time1)
              commands
          in
          if success then (
            match coordinates with
            | Some (_size, steps) ->
              update_results results enabled_results nr_vars steps add_time
                remove_time open_chapter_time reachable_vars_time build_time
            | None -> results, enabled_results
          ) else (
            match coordinates with
            | Some (_size, steps) ->
              update_results results enabled_results nr_vars steps timeout
                timeout timeout timeout timeout
            | None -> results, enabled_results
          ))
        (empty_results, enabled_results)
        enabled_tests
    in
    let all_sizes =
      IntMap.bindings results |> List.map fst |> List.sort compare
    in
    let all_steps =
      List.flatten
        (List.map
           (fun (_, sm) -> IntMap.bindings sm |> List.map fst)
           (IntMap.bindings results))
      |> List.sort_uniq compare
    in
    (* PLOT WITH GNUPLOT *)
    let module Gp = Gnuplot in
    let extract_series_step f =
      let step_to_plot =
        match all_steps with
        | fst :: _ -> fst
        | _ -> invalid_arg "Need at least one step"
      in
      ( IntMap.fold
          (fun size step_map acc ->
            match IntMap.find_opt step_to_plot step_map with
            | Some br -> (float_of_int size, f br) :: acc
            | None -> acc)
          results [],
        "nr of nodes" )
    in

    let extract_series_size f =
      let size_to_plot =
        match all_sizes with
        | fst :: _ -> fst
        | _ -> invalid_arg "Need at least one size"
      in
      match IntMap.find_opt size_to_plot results with
      | None -> [], ""
      | Some step_map ->
        ( IntMap.fold
            (fun step br acc -> (float_of_int step, f br) :: acc)
            step_map [],
          "nr of steps" )
    in
    let _ = extract_series_size, extract_series_step in

    let plot_size_time_first_step extract_series =
      let add_series, x_label = extract_series (fun br -> br.add_time) in
      let build_graph_series =
        Gp.Series.linespoints_xy ~title:"build graph"
          ~color:(`Rgb (220, 161, 0))
          (fst (extract_series (fun br -> br.build_time)))
      in
      let open_chapter_series =
        Gp.Series.linespoints_xy ~title:"choose a subset of active clauses"
          ~color:(`Rgb (115, 180, 237))
          (fst (extract_series (fun br -> br.open_chapter_time)))
      in
      let add_series =
        Gp.Series.linespoints_xy ~title:"add clauses"
          ~color:(`Rgb (140, 43, 215))
          add_series
      in
      let remove_series =
        Gp.Series.linespoints_xy ~title:"remove clauses"
          ~color:(`Rgb (77, 158, 120))
          (fst (extract_series (fun br -> br.remove_time)))
      in
      let _reachable_vars_series =
        Gp.Series.linespoints_xy ~title:"check reachability of first 1000 nodes"
          ~color:`Red
          (fst (extract_series (fun br -> br.reachable_vars_time)))
      in

      let gp = Gp.create () in

      let labels = Gp.Labels.create ~x:x_label ~y:"runtime in s" () in

      let is_enabled tot_time = tot_time > 0.0005 in

      Gp.plot_many ~logscale:("y", None) gp
        ~output:(Gp.Output.create (`Png_cairo ("output/" ^ title ^ ".png")))
        ([ build_graph_series ]
        @ (if is_enabled enabled_results.open_chapter_time then
             [ open_chapter_series ]
           else
             [])
        @ (if is_enabled enabled_results.add_time then
             [ add_series ]
           else
             [])
        @
        if is_enabled enabled_results.remove_time then
          [ remove_series ]
        else
          []
        (* if is_enabled enabled_results.reachable_vars_time then [ reachable_vars_series ] else []
            @ *))
        ~labels;
      Gp.close gp
    in
    plot_size_time_first_step extract_series_step
end
