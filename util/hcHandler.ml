open Hypergraph_sig

exception InvalidVarName of string
exception InvalidCharachter of string
exception InvalidCommand of string

module HornClauseHandler (HC : FullyDynamicHornClauses) = struct
  (*TODO all indexes change after removing something. This is not so good 
    TODO maybe print for each add/delete and accept "p" "a" "r" as commands
    TODO ignore case in commands
    TODO better help text*)
  type clause = (int * IdSet.t)
  type t = 
    {
      vars : Vars.t;
      horn_clauses_rev: clause list;
      horn_clause_handler: HC.t;
      compound_to_int : CompoundIntConversion.t;
    }

  let update_vars hc vars'= { hc with vars = vars' }
  let update_handler hc hch = { hc with horn_clause_handler = hch }
  let update_hcr hc hcr = { hc with horn_clauses_rev = hcr }

  let insert_var hc var =
    let (vars', id, _) = Vars.var_to_int hc.vars var in
    (update_vars hc vars', id)

  let init ~nr_vars = 
    (*compound node for True*)
    let cti, true_node = Node.compound_node_of_intset (IdSet.empty ()) CompoundIntConversion.empty in
    { vars = Vars.init ~nr_vars;
      horn_clauses_rev= []; 
      horn_clause_handler= HC.init ~nr_vars true_node;
      compound_to_int = cti }

  let clause_to_edge (head, body) hc =  
    let head_node = Node.simple_node_of_id head in
    if IdSet.cardinal body = 1 then
      (head_node, Node.simple_node_of_id (IdSet.choose body), body), hc
    else
      let cti, node = Node.compound_node_of_intset body hc.compound_to_int in
      (head_node, node, body), {hc with compound_to_int = cti}

  let add_clause (head, body) hc =
    let edge, hc = clause_to_edge (head, body) hc in
    { hc with
      horn_clauses_rev = (head, body)::hc.horn_clauses_rev; 
      horn_clause_handler = HC.add_clause edge hc.horn_clause_handler; }

  let reachable_vars hc =
    Vars.get_vars_in_alphabetical_order hc.vars
    |> Seq.filter_map (fun (name, id) ->
        if HC.is_reachable (Node.simple_node_of_id id) hc.horn_clause_handler 
        then Some name else None 
      )
    |> List.of_seq
    |> List.rev
  (* let r_vars = HC.reachable_vars (Vars.get_nr_vars hc.vars) hc.horn_clause_handler in
     IdSet.fold (fun i vars -> Vars.int_to_var hc.vars i::vars) r_vars []
     |> List.rev *)

  let first_reachable_vars hc =
    Vars.get_vars_in_alphabetical_order hc.vars |>
    Seq.take 1000 |>
    Seq.filter_map (fun (name, id) ->
        if HC.is_reachable (Node.simple_node_of_id id) hc.horn_clause_handler 
        then Some name else None  
      )

  let reachable_vars_string hc =
    String.concat ", " (reachable_vars hc)

  let to_dot hg =
    let b = Buffer.create 1024 in
    Buffer.add_string b "digraph hypergraph {\n";
    Buffer.add_string b "  rankdir=LR;\n";
    Buffer.add_string b "  node [fontsize=10, penwidth=1.2];\n";
    let nodes = 
      List.map (fun (x, _) -> Node.Simple x) 
        (Vars.get_vars hg.vars) @ 
      List.map (fun (_, id) -> Node.Compound id) (CompoundIntConversion.bindings hg.compound_to_int) 
    in
    (* Add nodes to buffer *)
    List.iter (fun node ->
        let reach = HC.weight_of hg.horn_clause_handler node in
        Node.dot_of_node node (Vars.int_to_var hg.vars) b reach
      ) nodes;
    (* Add edges to buffer *)
    List.iter (fun node ->
        NodeSet.iter (fun successor ->
            Node.dot_of_edge node successor b 
          ) (HC.succs_of hg.horn_clause_handler node)
      ) nodes;
    Buffer.add_string b "}\n";
    Buffer.contents b

  type parse_output = 
      Add of clause | Remove of clause * int | OpenChapter of clause list | CloseChapter

  let parse_clause s hc =
    let is_alpha_or_digit = function 'a' .. 'z' | 'A' .. 'Z'| '0' .. '9' -> true | _ -> false in
    let is_valid_var_name s = 
      String.length s > 0 && String.for_all is_alpha_or_digit s in
    let s = String.trim s in
    (* ---- ADD ---- *)
    if String.starts_with ~prefix:"add" s then 
      let s = String.trim (String.sub s 3 (String.length s - 3)) in
      let (head, body) = 
        match String.split_on_char ':' s with
        | [lhs] -> if is_valid_var_name lhs then (lhs, []) 
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
          (String.trim lhs, rhs)
        | _ ->
          raise (InvalidCharachter ":")
      in
      let (hc, head_id) = insert_var hc head in
      let (hc, body_set) =
        List.fold_left (fun (acc_hc, acc_ids) v ->
            let (acc_hc, id) = insert_var acc_hc v in
            (acc_hc, IdSet.add id acc_ids)
          ) (hc, IdSet.empty ()) body
      in
      hc, Add (head_id, body_set)
      (* ---- REMOVE ---- *)
    else if String.starts_with ~prefix:"remove" s then 
      let s = String.trim (String.sub s 6 (String.length s - 6)) in
      let n = try int_of_string s 
        with Failure f -> raise (InvalidCharachter (s ^ " in function " ^ f))
      in
      let hc_list = hc.horn_clauses_rev in
      let index_in_list = List.length hc_list - n in
      hc, Remove (List.nth hc_list index_in_list, index_in_list)
      (* ---- OPEN ---- *)
    else if String.starts_with ~prefix:"open" s then 
      let s = String.trim (String.sub s 4 (String.length s - 4)) in
      (* "open ()" -> empty list *)
      if s = "()" || s = "" then
        hc, OpenChapter []
      else
        let s =
          if String.length s >= 2 && s.[0] = '(' && s.[String.length s - 1] = ')' then
            String.sub s 1 (String.length s - 2) |> String.trim
          else s
        in
        let parts = String.split_on_char ',' s |> List.map String.trim in
        let indices =
          List.concat_map (fun p ->
              match String.split_on_char '-' p |> List.map String.trim with
              | [a] ->
                begin match int_of_string_opt a with
                  | None -> raise (InvalidCharachter ("Invalid index: " ^ p))
                  | Some i -> [i]
                end
              | [a; b] ->
                let a, b = int_of_string a, int_of_string b in
                if a > b then raise (InvalidCharachter ("Invalid range: " ^ p))
                else List.init (b - a + 1) (fun i -> a + i)
              | _ -> raise (InvalidCharachter ("Invalid range syntax: " ^ p))
            ) parts
        in
        let hc_list = hc.horn_clauses_rev in
        let len = List.length hc_list in
        let selected_clauses =
          indices
          |> List.map (fun n ->
              if n <= 0 || n > len then
                raise (InvalidCharachter ("Index " ^ string_of_int n ^ " out of bounds"))
              else
                List.nth hc_list (len - n)
            )
        in
        hc, OpenChapter selected_clauses
        (* ---- CLOSE ---- *)
    else if String.equal "close" s then 
      hc, CloseChapter
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

  let remove_clause hc (head, body) index = 
    let horn_clauses_rev = 
      List.filteri (fun i _ -> not (i = index)) hc.horn_clauses_rev in
    let clause, hc = clause_to_edge (head, body) hc in
    {hc with
     horn_clauses_rev;       
     horn_clause_handler = 
       HC.remove_clause clause hc.horn_clause_handler }

  let open_chapter hc clause_list = 
    let clauses, hc = 
      List.fold_left (fun (clauses, hc) clause -> 
          let cl, hc = clause_to_edge clause hc 
          in cl :: clauses, hc) 
        ([], hc) clause_list in
    {hc with       
     horn_clause_handler = 
       HC.open_chapter clauses hc.horn_clause_handler }
  let close_chapter hc =
    {hc with       
     horn_clause_handler = 
       HC.close_chapter hc.horn_clause_handler }

end
