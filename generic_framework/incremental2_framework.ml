
open Hypergraph_sig
open HcHandler
open Solver_sig
module BasicHornClauses: ProgramToHCS = 
struct
  type program = string
  type hs_id = Id.t
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
      | Some _ -> hc, [],[n]
      | None | (exception Invalid_argument _) ->
        raise
          (InvalidCommand ("There is no horn clause with index " ^ string_of_int n))
          (* ---- OPEN ---- *)
    ) else if String.starts_with ~prefix:"open" s then (
      let s = String.trim (String.sub s 4 (String.length s - 4)) in
      (* "open" -> empty list *)
      hc, [],[]
      (* ---- CLOSE ---- *)
    ) else if String.equal "close" s then
      hc, [],[]
    else
      raise (InvalidCommand s)

let hs_of_id hc hs_id = 
  match Res.Array.get hc.horn_clauses_array hs_id with
      | Some hcs -> hcs
      | None | (exception Invalid_argument _) ->
        raise
          (InvalidCommand ("There is no horn clause with index " ^ string_of_int hs_id))

  let diff_hcschemes hc _ command = parse_clause command hc
  let empty_program = ""
  let empty_vars = {vars=Vars.init ~nr_vars:10000; horn_clauses_array = Res.Array.empty ();}

end

module BasicFixpointHypergraph : IncrementalSymbolicHornClauseSolverStrategy =
struct
  module AdjacencyList = MutableNodeMap (NodeSet)

 
  type horn_clause_scheme = int * IdSet.t
  type horn_clause = Edge.t
  type hs_id = Id.t
  type literal = Id.t
  type analysis_result = Id.t list
  type init_data = Node.t
 type t = {
    succs: AdjacencyList.t;
    preds: AdjacencyList.t;
    compound_to_int: CompoundIntConversion.t;
    worklist: horn_clause list;
  }
  let empty_datastructure =
    let cti, true_node =
      Node.compound_node_of_intset (IdSet.empty ()) CompoundIntConversion.empty
    in
    {   succs = AdjacencyList.empty 10000;
        preds = AdjacencyList.empty 10000;
        compound_to_int =cti;
        worklist = [];
    }, true_node

  let succs_of t node = AdjacencyList.get node t.succs

  let preds_of t node = AdjacencyList.get node t.preds

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
      add_simple_edge tail head t
    )

  let clause_to_edge (head, body) hc =
    let head_node = Node.simple_node_of_id head in
    if IdSet.cardinal body = 1 then
      (head_node, Node.simple_node_of_id (IdSet.choose body), body), hc
    else (
      let cti, node = Node.compound_node_of_intset body hc.compound_to_int in
      (head_node, node, body), { hc with compound_to_int = cti }
    )

  let update_add_hcs t hc_list  (* The Horn Clause scheme is registered to the fixpoint algorithm, which updates the next horn clauses to be iterated *)
    = 
    List.fold_left (fun acc c -> let c, acc = (clause_to_edge c acc) in add_clause acc c) t hc_list

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
    else remove_hyperedge body_set head tail hc 

  let update_remove_hcs t hc_list=
        List.fold_left (fun acc (head, body) -> remove_clause acc head body) t hc_list

    let next_hc t = 
      match t.worklist with
      | [] ->t, None
      | next :: worklist -> 
        {t with worklist}, Some next

    let propagate_change t (head, _body) = 
      let successors = succs_of t head in
     let worklist = NodeSet.fold (fun n worklist -> 
      (n, head)::worklist) successors t.worklist in
      {t with worklist}


end

module Incremental2Abstraction = 
(* : AbstractionProof  *)
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
  type node
  type hs_id
  type edge = hs_id * node * node list(* (hs_id, conclusion, premise) *)
  type analysis_result
   let name = "INCREMENTAL 2"
   let empty true_node = 
    let reachability_status = ReachabilityStatus.empty 10000 in
    let () = ReachabilityStatus.add true_node 0 reachability_status in
    {
      min_pred = MinPredsMap.empty 10000;
      reachability_status;
    }
    let decrement_reachability t node =
    let previous_i = ReachabilityStatus.get node t.reachability_status in
    let () =
      ReachabilityStatus.add node (previous_i - 1) t.reachability_status
    in
    t

    let is_reachable node t =
    0 = ReachabilityStatus.get node t.reachability_status


  let add_min_pred node ~min_pred t =
    let () = MinPredsMap.add node (Some min_pred) t.min_pred in
    t

    let visit_edge t (dest, source) =
      if is_reachable dest t then
      t, false
    else (
      let t = decrement_reachability t dest in
      if is_reachable dest t then (
          if Node.is_simple dest then
            add_min_pred dest ~min_pred:source t, true
          else
            t, true
            )
          else (t, false)
            )

              let get_min_pred node t = MinPredsMap.get node t.min_pred

            let remove_clause (y, x, x_set) hg =
    if Node.is_reflexive_edge x_set y then
      (*reflexive edges are not allowed*)
      hg
    else (
      let hg = remove_hyperedge x_set x y hg in
      match get_min_pred y hg with
      | None -> (* y is unreachable *) hg
      | Some min_pred ->
        if Node.compare min_pred x = 0 then (
          match search_alternative_local_path x_set y hg with
          | Some node -> add_min_pred y ~min_pred:node hg
          | None -> scan_successors_after_removal hg y
        ) else
          hg
    )

            let remove_edges t hs_list = 
              (* remove a hyperedge x -> y from the graph *)
  
     
  val remove_edges : t -> hs_id list -> t (* removes the edges, updates the abstraction and potentially recomputes some of the information that was lost *)
  val return_result : t -> analysis_result
end