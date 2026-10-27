
open Hypergraph_sig
open HcHandler
open Solver_sig
module BasicHornClauses : ProgramToHCS with type horn_clause_scheme = int * IdSet.t and type program = string = 
struct
  type program = string
  type horn_clause_scheme = int * IdSet.t

  type vars = {
    vars: Vars.t;
    horn_clauses_array: horn_clause_scheme option Res.Array.t;
  }

  let insert_var t var =
    let vars, id, changed = Vars.var_to_int t.vars var in
    {t with vars}, id, changed 

  let parse_added_clause s hc =
    let is_alpha_or_digit = function
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
      | _ -> false
    in
    let is_valid_var_name s =
      String.length s > 0 && String.for_all is_alpha_or_digit s
    in
    let head, body = 
      match String.split_on_char ':' s with
      | [ lhs ] ->
        if is_valid_var_name lhs then
          lhs, []
        else
          raise (InvalidVarName lhs)
      | [ lhs; rest ] ->
        let rest =
          if String.length rest > 0 && rest.[0] = '=' then
            String.sub rest 1 (String.length rest - 1)
          else
            raise (InvalidCharachter ":")
        in
        let rhs =
          rest |> String.trim |> fun s ->
          if s = "" then
            []
          else
            s |> String.split_on_char ',' |> List.map String.trim
        in
        let () =
          List.iter
            (fun v ->
               if not (is_valid_var_name v) then raise (InvalidVarName v))
            rhs
        in
        String.trim lhs, rhs
      | _ -> raise (InvalidCharachter ":")
    in
    let hc, head_id, head_is_new = insert_var hc head in
    let new_vars = if head_is_new then [head] else [] in
    let hc, body_set, _new_vars =
      List.fold_left
        (fun (hc, ids, new_vars) v ->
           let hc, id, is_new = insert_var hc v in
           hc, IdSet.add id ids, if is_new then v::new_vars else new_vars)
        (hc, IdSet.empty (), new_vars)
        body
    in
    hc, (head_id, body_set) 

  let parse_clause s hc =
    let s = String.trim s in
    (* ---- ADD ---- *)
    if String.starts_with ~prefix:"add" s then (
      let s = String.trim (String.sub s 3 (String.length s - 3)) in
      let hc, new_clauses = parse_added_clause s hc in
      hc, [new_clauses], []
      (* ---- REMOVE ---- *)
    ) else if String.starts_with ~prefix:"remove" s then (
      let s = String.trim (String.sub s 6 (String.length s - 6)) in
      let n =
        try int_of_string s
        with Failure f -> raise (InvalidCharachter (s ^ " in function " ^ f))
      in
      let hc_list = hc.horn_clauses_array in
      match Res.Array.get hc_list n with
      | Some hcs -> hc, [],[hcs,n]
      | None | (exception Invalid_argument _) ->
        raise
          (InvalidCommand ("There is no horn clause with index " ^ string_of_int n))
          (* ---- OPEN ---- *)
    ) else if String.starts_with ~prefix:"open" s then (
      (* "open" -> empty list *)
      hc, [],[]
      (* ---- CLOSE ---- *)
    ) else if String.equal "close" s then
      hc, [],[]
    else
      raise (InvalidCommand s)

  (*PUBLIC FUNCTIONS*)
  let clause_of_index i hc = Res.Array.get hc.horn_clauses_array i

  let diff_hcschemes hc _ command = parse_clause command hc
  let empty_program = ""
  let empty_vars = {vars=Vars.init ~nr_vars:10000; horn_clauses_array = Res.Array.empty ();}

  let var_to_string i vars = Vars.int_to_var vars.vars i

  let print_clause vars (head, body) =
    print_string (Vars.int_to_var vars.vars head);
    print_string " := ";
    let var_names = IdSet.fold (fun v l -> Vars.int_to_var vars.vars v :: l) body [] in
    print_endline (String.concat ", " var_names)

end

module BasicFixpointHypergraph : IncrementalSymbolicHornClauseSolverStrategy with type horn_clause_scheme = int * IdSet.t and type horn_clause = Node.t * Node.t and type init_data = Node.t =
struct
  module AdjacencyList = MutableNodeMap (NodeSet)


  type horn_clause_scheme = int * IdSet.t
  type horn_clause = Edge.t
  type analysis_result = IdSet.t
  type init_data = Node.t
  type t = {
    succs: AdjacencyList.t;
    preds: AdjacencyList.t;
    compound_to_int: CompoundIntConversion.t;
    worklist_add: horn_clause list;
    worklist_remove_inf: horn_clause list;
    worklist_restore_inf: horn_clause list;
  }

  let succs_of t node = AdjacencyList.get node t.succs

  let preds_of t node = AdjacencyList.get node t.preds

  let clause_to_edge (head, body) hc =
    let head_node = Node.simple_node_of_id head in
    if IdSet.cardinal body = 1 then
      (head_node, Node.simple_node_of_id (IdSet.choose body), body), hc
    else (
      let cti, node = Node.compound_node_of_intset body hc.compound_to_int in
      (head_node, node, body), { hc with compound_to_int = cti }
    )

  (*ADDING CLAUSE*)

  let add_successor node ~succ t =
    let successors = succs_of t node in
    if NodeSet.mem succ successors then
      t
    else (
      let new_set = NodeSet.add succ successors in
      let () = AdjacencyList.add node new_set t.succs in
      t
    )


  let add_predecessor node ~pred t =
    let predecessors = preds_of t node in
    if NodeSet.mem pred predecessors then
      t
    else (
      let new_set = NodeSet.add pred predecessors in
      let () = AdjacencyList.add node new_set t.preds in
      t
    )

  let add_simple_edge q1 q2 hg =
    hg |> add_successor q1 ~succ:q2 |> add_predecessor q2 ~pred:q1

  let add_clause t (head, tail, tail_set) =
    if Node.is_reflexive_edge tail_set head then
      (*reflexive edges are not allowed*)
      t
    else (
      let t =
        if Node.is_simple tail then
          t
        else
          (* add nodes from the simple edges to the compound edge "tail" *)
          IdSet.fold
            (fun i hg -> add_simple_edge (Node.Simple i) tail hg)
            tail_set t
      in
      let t = add_simple_edge tail head t in
      {t with worklist_add = (head, tail) :: t.worklist_add}
    )

  (*REMOVING CLAUSES*)

  let remove_successor node ~succ t =
    let successors = succs_of t node in
    if NodeSet.mem succ successors then (
      let new_set = NodeSet.remove succ successors in
      let () = AdjacencyList.add node new_set t.succs in
      t
    ) else
      t

  let remove_predecessor node ~pred t =
    let predecessors = preds_of t node in
    if NodeSet.mem pred predecessors then (
      let new_set = NodeSet.remove pred predecessors in
      let () = AdjacencyList.add node new_set t.preds in
      t
    ) else
      t

  let remove_simple_edge tail head hg =
    hg |> remove_successor tail ~succ:head |> remove_predecessor head ~pred:tail

  let remove_compound_node t node =
    match node with
    | Node.Simple _ -> t
    | Node.Compound _ ->
      let () = AdjacencyList.remove node t.succs in
      let () = AdjacencyList.remove node t.preds in
      t

  let remove_hyperedge
      tail_set tail head hg =
    let hg = remove_simple_edge tail head hg in
    if
      Node.is_compound tail
      && (not (Node.is_true tail))
      && NodeSet.is_empty (succs_of hg tail)
    then (
      let hg =
        IdSet.fold
          (fun i -> remove_successor (Node.Simple i) ~succ:tail)
          tail_set hg
      in
      remove_compound_node hg tail
    ) else
      hg

  let remove_clause hc head body = 
    let (head, tail, body_set), hc = clause_to_edge (head, body) hc in
    if Node.is_reflexive_edge body_set head then
      (*reflexive edges are not allowed*)
      hc
    else (
      let hc = remove_hyperedge body_set tail head hc in
      let worklist_remove_inf = (head, tail) :: hc.worklist_remove_inf in
      {hc with worklist_remove_inf}
    )

  (*PUBLIC FUNCTIONS*)

  let empty_datastructure =
    let cti, true_node =
      Node.compound_node_of_intset (IdSet.empty ()) CompoundIntConversion.empty
    in
    {   succs = AdjacencyList.empty 10000;
        preds = AdjacencyList.empty 10000;
        compound_to_int =cti;
        worklist_add = [];
        worklist_remove_inf = [];
        worklist_restore_inf = [];
    }, true_node

  let update_add_hcs t hc_list  (* The Horn Clause scheme is registered to the fixpoint algorithm, which updates the next horn clauses to be iterated *)
    = 
    List.fold_left (fun acc c -> let c, acc = (clause_to_edge c acc) in add_clause acc c) t hc_list

  let update_remove_hcs t hc_list =
    List.fold_left (fun acc (head, body) -> remove_clause acc head body) t hc_list

  let next_hc_add t = 
    match t.worklist_add with
    | [] -> t, None
    | next :: worklist_add -> 
      {t with worklist_add}, Some next

  let propagate_change_add t (head, _body) = 
    let successors = succs_of t head in
    let worklist_add = NodeSet.fold (fun n worklist -> 
        (n, head) :: worklist) successors t.worklist_add in
    {t with worklist_add}

  let next_hc_remove_inf t = 
    match t.worklist_remove_inf with
    | [] -> t, None
    | next :: worklist_remove_inf -> 
      {t with worklist_remove_inf}, Some next

  let propagate_change_remove_inf t (head, _body) = 
    let worklist_remove_inf = NodeSet.fold
        (fun v worklist_remove_inf ->
           (v, head):: worklist_remove_inf
        )
        (succs_of t head) t.worklist_remove_inf in
    let worklist_restore_inf = 
      if not (Node.is_compound head) then 
        (let preds = preds_of t head in
         let edges =
           NodeSet.to_seq preds |> Seq.map (fun p -> (head, p)) |> List.of_seq in
         t.worklist_restore_inf @ edges) (*might not be very efficient*)
      else t.worklist_restore_inf in
    {t with worklist_remove_inf; worklist_restore_inf}


  let next_hc_restore_inf t = 
    match t.worklist_restore_inf with
    | [] -> t, None
    | next :: worklist_restore_inf -> 
      {t with worklist_restore_inf}, Some next

  let propagate_change_restore_inf t (head, body) = 
    let worklist_add = (head, body) :: t.worklist_add
    in {t with worklist_add}

end

module Incremental2Abstraction : AbstractionProof with type analysis_result = IdSet.t and type horn_clause = Node.t * Node.t and type init_data = Node.t = 
struct

  module ReachabilityStatus = MutableNodeMap (struct
      include Int

      let empty () = 1
    end)

  module MinPredsMap = MutableNodeMap (struct
      type t = Node.t option

      let empty () = None
    end)
  type t = {
    min_pred: MinPredsMap.t;
    reachability_status: ReachabilityStatus.t;
  }
  type horn_clause = Node.t * Node.t
  type analysis_result = IdSet.t
  type init_data = Node.t

  let decrement_reachability t node =
    let previous_i = ReachabilityStatus.get node t.reachability_status in
    let () =
      ReachabilityStatus.add node (previous_i - 1) t.reachability_status
    in
    t

  let is_reachable t node =
    0 = ReachabilityStatus.get node t.reachability_status


  let add_min_pred node ~min_pred t =
    let () = MinPredsMap.add node (Some min_pred) t.min_pred in
    t

  let remove_min_pred node t =
    let () = MinPredsMap.remove node t.min_pred in
    t

  let get_min_pred node t = MinPredsMap.get node t.min_pred

  let increment_reachability t node =
    let previous_i = ReachabilityStatus.get node t.reachability_status in
    let () =
      ReachabilityStatus.add node (previous_i + 1) t.reachability_status
    in
    t

  (* PUBLIC FUNCTIONS *)
  let name = "INCREMENTAL 2"
  let empty true_node = 
    let reachability_status = ReachabilityStatus.empty 10000 in
    let () = ReachabilityStatus.add true_node 0 reachability_status in
    {
      min_pred = MinPredsMap.empty 10000;
      reachability_status;
    }

  let visit_edge_add t (dest, source) =
    if is_reachable t dest then
      t, false
    else (
      if is_reachable t source then 
        (let t = decrement_reachability t dest in
         if is_reachable t dest then (
           if Node.is_simple dest then
             add_min_pred dest ~min_pred:source t, true
           else
             t, true
         )
         else (t, false))
      else (t, false)
    )


  let remove_inf_edge t (head, body) = 
    let visit_node v y hg =
      Node.is_simple v
      &&
      match get_min_pred v hg with
      | None -> false
      | Some min_pred -> Node.compare min_pred y = 0
    in
    if Node.is_compound head || visit_node head body t then
      let was_reachable = is_reachable t head in
      (let t = 
         if Node.is_compound head then
           increment_reachability t head
         else if was_reachable then
           let t = remove_min_pred head t in
           increment_reachability t head
           (*c_set contains only simple nodes*)
         else
           t
       in 
       if was_reachable then t, true else t, false
      )
    else t, false

  let restore_inf_edge t (head, body) = 
    if not (is_reachable t head) && is_reachable t body then 
      t, true
    else t, false

  let return_result t = 
    let reachable_vars = ref (IdSet.empty ()) in
    ReachabilityStatus.iter (fun node r -> 
        if r = 0 then match node with 
          | Simple i -> 
            reachable_vars := IdSet.add i !reachable_vars
          | Compound _ -> ()) 
      t.reachability_status;
    !reachable_vars


end

module Incremental2Framework = HCHandlerFramework (BasicHornClauses) (Incremental2Abstraction) (BasicFixpointHypergraph)