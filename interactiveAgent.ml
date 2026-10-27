open HcHandler
open Incremental_analysis_algorithms
open Hypergraph_sig

let help_message =
  "Available commands:\n\
   - quit\n\
   - help\n\
   - add <horn clause>\n\
   - remove <index of horn clause>\n\
   - open <list of indexes, or ()>\n\
   - close\n\
   - print\n\
   - reachable\n\
   - dot\n\
   - add schema v = <regex> # <horn clause using v>\n\
   Horn clause syntax:\n\
   <head> := <body>\n"

let init_loop n (module HypergraphAlg : FullyDynamicHornClauses) =
  let module HypergraphHCH = HornClauseHandler (HypergraphAlg) in
  let hc = HypergraphHCH.init ~nr_vars:n in
  let rec loop hc =
    print_string "> ";
    flush stdout;
    let error_message s =
      "Invalid input: " ^ s ^ ". Please try again. Type 'help' for help."
    in
    try
      match String.trim (read_line ()) with
      | "quit" -> ()
      | "help" ->
        print_endline help_message;
        loop hc
      | "dot" ->
        let output = HypergraphHCH.to_dot hc in
        print_endline output;
        loop hc
      | "print" ->
        let output = HypergraphHCH.to_string hc in
        print_endline output;
        loop hc
      | "reachable" ->
        let output = HypergraphHCH.reachable_vars_string hc in
        print_endline output;
        loop hc
      | input ->
        let hc, parse_output = HypergraphHCH.parse_clause input hc in
        let hc =
          match parse_output with
          | Add clauses -> 
            let () = print_string "Adding clauses:\n" in
            let () = List.iter (fun c -> HypergraphHCH.print_clause hc c) clauses in
            List.fold_left (fun hc clause -> HypergraphHCH.add_clause clause hc) hc clauses
          | Remove (clause, index) ->
            HypergraphHCH.remove_clause hc clause index
          | OpenChapter clauses -> HypergraphHCH.open_chapter hc clauses
          | CloseChapter -> HypergraphHCH.close_chapter hc
        in
        print_endline "done";
        loop hc
    with
    | FullyDynamicHypergraph.UnknownNodeId (loc, node) ->
      print_endline
        ("Exception: UnknownNodeId "
        ^ Hypergraph_sig.Node.to_string node
        ^ " in " ^ loc)
    | Hypergraph_sig.ExceededNrOfVars ->
      print_endline "Exception: ExceededNrOfVars"
    | InvalidVarName s ->
      print_endline (error_message ("Not a valid variable name: " ^ s));
      loop hc
    | InvalidCharachter s ->
      print_endline (error_message ("Unexpected charachter: " ^ s));
      loop hc
    | InvalidCommand s ->
      print_endline (error_message ("Command not recognized: " ^ s));
      loop hc
  in
  loop hc

let rec ask_nr_variables s =
  print_endline s;
  let s = String.trim (read_line ()) in
  match int_of_string_opt s with
  | Some n -> n
  | None ->
    ask_nr_variables "Please enter a number. Input the number of variables: "

let list_of_modules =
  [ "incremental"; "incremental2"; "mst"; "parametric"; "semiparametric" ]

let ask_module () =
  let rec ask_module s =
    print_endline s;
    match String.trim (read_line ()) with
    | "incremental" | "i" ->
      (module IncrementalHypergraph : FullyDynamicHornClauses)
    | "incremental2" | "i2" ->
      (module IncrementalHypergraph2 : FullyDynamicHornClauses)
    | "mst" | "m" -> (module FullyDynamicHypergraph : FullyDynamicHornClauses)
    | "parametric" | "p" ->
      (module ParametricHypergraph : FullyDynamicHornClauses)
    | "semiparametric" | "s" ->
      (module SemiParametricHypergraph : FullyDynamicHornClauses)
    | _ ->
      ask_module
        ("Please enter '" ^ String.concat "' or '" list_of_modules ^ "'. ")
  in
  ask_module
    ("Input the algorithm version: (" ^ String.concat "/" list_of_modules ^ ")")

let start_agent () =
  let n = ask_nr_variables "Input the number of variables: " in
  let module HypergraphAlg = (val ask_module ()) in
  print_endline "Please input a horn clause.";
  init_loop n (module HypergraphAlg)
