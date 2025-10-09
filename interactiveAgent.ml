open Util


exception InvalidVarName of string
exception InvalidCharachter of string
exception InvalidCommand of string

module HornClauseHandler (HC : HornClausesReachability) = struct
  (*TODO not add duplicate clauses?
    TODO all indexes change after removing something. This is not so good 
    TODO maybe print for each add/delete and accept "p" "a" "r" as commands
    TODO ignore case in commands
    TODO check that true is well handled in each function
    TODO write tests 
    TODO better help text*)
  type clause = (int * IdSet.t)
  type t = 
    {
      vars : Vars.t;
      horn_clauses_rev: clause list;
      horn_clause_handler: HC.t
    }

  let update_vars hc vars'= { hc with vars = vars' }
  let update_handler hc hch = { hc with horn_clause_handler = hch }
  let update_hcr hc hcr = { hc with horn_clauses_rev = hcr }

  let insert_var hc var =
    let (vars', id, is_new_var) = Vars.var_to_int hc.vars var in
    let node = Node.simple_node_of_id id in
    let hc = 
      if is_new_var then
        let hch = HC.add_node node hc.horn_clause_handler in
        update_handler hc hch
      else hc 
    in
    (update_vars hc vars', id)

  let init ~nr_vars = 
    { vars = Vars.init ~nr_vars;
      horn_clauses_rev= []; 
      horn_clause_handler= HC.init ~nr_vars }

  let clause_to_edge (head, body) =  
    let head_node = Node.simple_node_of_id head in
    if IdSet.is_empty body then 
      head_node, Node.True
    else if IdSet.cardinal body = 1 then
      head_node, Node.simple_node_of_id (IdSet.choose body)
    else 
      head_node, Node.compound_node_of_intset body

  let add_clause (head, body) hc =
    let edge = clause_to_edge (head, body) in
    { hc with
      horn_clauses_rev= (head, body)::hc.horn_clauses_rev; 
      horn_clause_handler= HC.add_clause edge hc.horn_clause_handler }

  let remove_clause hc (head, body) index = 
    let clause = (Node.simple_node_of_id head, Node.compound_node_of_intset body) in
    {hc with
     horn_clauses_rev = 
       List.filteri (fun i _ -> not (i = index)) hc.horn_clauses_rev; 
     horn_clause_handler = 
       HC.remove_clause clause hc.horn_clause_handler }

  let reachable_vars hc =
    StringMap.bindings hc.vars.var_name_to_int
    |> List.fold_left (fun acc (name, id) ->
        match HC.is_reachable (Node.simple_node_of_id id) hc.horn_clause_handler with
        | false -> acc
        | true -> name :: acc
      ) []
    |> List.rev

  let reachable_vars_string hc =
    String.concat ", " (reachable_vars hc)

  let to_dot hc = HC.to_dot hc.horn_clause_handler (Vars.int_to_var hc.vars)

  (**
     returns (index, clause, bool)
     It returns true if the clause needs to be added and false if it needs to be removed.
  *)
  let parse_clause s hc =
    let is_alpha_or_digit = function 'a' .. 'z' | 'A' .. 'Z'| '0' .. '9' -> true | _ -> false in
    let is_valid_var_name s = 
      String.length s > 0 && String.for_all is_alpha_or_digit s in
    let s = String.trim s in
    if String.starts_with ~prefix:"add" s then 
      let s = String.sub s 3 (String.length s - 3) in
      let index, (head, body) = 
        match String.split_on_char ':' s with
        | [lhs] -> if is_valid_var_name lhs then -1, (lhs, []) 
          else raise (InvalidVarName lhs)
        | [lhs; rest] ->
          let rest =
            if String.length rest > 0 && rest.[0] = '=' then
              String.sub rest 1 (String.length rest - 1)
            else
              raise (InvalidCharachter ":")
          in
          let rhs =
            rest
            |> String.trim
            |> (fun s ->
                if s = "" then []
                else
                  s
                  |> String.split_on_char ','
                  |> List.map String.trim
              )
          in
          let () = List.iter (fun v -> if not (is_valid_var_name v) then 
                                 raise (InvalidVarName v)) rhs in
          -1, (String.trim lhs, rhs)
        | _ ->
          raise (InvalidCharachter ":")
      in
      let (hc, head_id) = insert_var hc head in
      let (hc, body_set) =
        List.fold_left (fun (acc_hc, acc_ids) v ->
            let (acc_hc, id) = insert_var acc_hc v in
            (acc_hc, IdSet.add id acc_ids)
          ) (hc, IdSet.empty) body
      in
      hc, index, (head_id, body_set), true
    else if String.starts_with ~prefix:"remove" s then 
      let s = String.trim (String.sub s 6 (String.length s - 6)) in
      let n = try int_of_string s 
        with Failure s -> raise (InvalidCharachter s)
      in
      let hc_list = hc.horn_clauses_rev in
      let index_in_list = List.length hc_list - n in
      hc, index_in_list, List.nth hc_list index_in_list, false
    else raise (InvalidCommand s)

  let to_string hc =
    let _, lines =
      List.fold_left
        (fun (i, acc) (head, body) ->
           let lhs = Vars.int_to_var hc.vars head in
           let rhs_list = List.map (fun x -> Vars.int_to_var hc.vars x) (List.of_seq (IdSet.to_seq body)) in
           let rhs = String.concat "," rhs_list
           in
           let line = Printf.sprintf "%d: %s := %s" i lhs rhs in
           (i - 1, line :: acc))
        (List.length hc.horn_clauses_rev, []) hc.horn_clauses_rev
    in
    String.concat "\n" lines
end

module HypergraphHCH = HornClauseHandler(ClassicalHypergraph)

let help_message =
  "Available commands:
  - quit
  - help
  - add <horn clause>
  - remove <horn clause>
  - print
  - reachable
  - dot 

  Horn clause syntax:
    <head> := <body>
  "

let rec loop hc =
  print_string "> ";
  flush stdout;
  let error_message s = "Invalid input: " ^ s ^ ". Please try again. Type 'help' for help." in
  try
    match String.trim (read_line ()) with
    | "quit" -> print_endline "Goodbye!"
    | "help" -> 
      print_endline help_message
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
      let hc, index, clause, must_be_added = HypergraphHCH.parse_clause input hc in
      let hc = if must_be_added then
          HypergraphHCH.add_clause clause hc
        else
          HypergraphHCH.remove_clause hc clause index 
      in
      print_endline "done";
      loop hc
  with 
  | ClassicalHypergraph.UnknownNodeId (loc, node) ->
    print_endline ("Exception: UnknownNodeId " ^ Node.string_of_node node ^ " in " ^ loc)
  | Util.ExceededNrOfVars ->
    print_endline ("Exception: ExceededNrOfVars")
  | InvalidVarName s ->
    print_endline (error_message ("Not a valid variable name: " ^ s));
    loop hc
  | InvalidCharachter s ->
    print_endline (error_message ("Unexpected charachter: " ^ s));
    loop hc
  | InvalidCommand s ->
    print_endline (error_message ("Command not recognized: " ^ s));
    loop hc



let start_agent () =
  print_endline "This program bla bla please input a horn clause.";
  let hc = HypergraphHCH.init ~nr_vars:400 in 
  loop hc
