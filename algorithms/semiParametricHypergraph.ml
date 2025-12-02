open Hypergraph_sig

module SemiParametricHypergraph = struct
  include ParametricHypergraph

  let name = "Semi-Parametric"

  let init ~nr_vars true_node =
    let t = init ~nr_vars true_node in
    {
      t with
      is_parametrized = (fun _ -> false);
      add_parameter_for_new_edges = true;
      is_semi_parametrized = true;
    }

  let is_reachable node t =
    let mvbdu = ReachabilityStatus.get node t.reachability_status in
    match mvbdu with
    | Edges_intbdu.Bool b -> b
    | Edges_intbdu.Mvbdu int_mvbdu ->
      let variables =
        Edges_intbdu.extensional_of_variables_list
          (Edges_intbdu.variables_list_of_mvbdu int_mvbdu)
      in
      let valuation_list =
        List.map
          (fun i ->
            ( i,
              if Res.Array.get t.current_parameter_valuation i then
                1
              else
                0 ))
          variables
      in
      let valuation_list = Edges_intbdu.build_association_list valuation_list in
      let mvbdu_valuation = Edges_intbdu.mvbdu_of_hconsed_asso valuation_list in
      let mvbdu_valuation = Edges_intbdu.mvbdu_normalize mvbdu_valuation in
      let mvbdu_inter = Edges_intbdu.mvbdu_and mvbdu mvbdu_valuation in
      let mvbdu_false = Edges_intbdu.mvbdu_false () in
      not (mvbdu_equal mvbdu_inter mvbdu_false)

  let reachable_vars =
    let fold f a b = NodeSet.fold (fun a b -> f b a) b a in
    Hypergraphs.reachable_vars succs_of is_reachable (Node.Compound 0) fold

  let abstract_parameters mvbdu mvbdu_valuation parameters =
    let mvbdu_inter = Edges_intbdu.mvbdu_and mvbdu mvbdu_valuation in
    match mvbdu_inter with
    | Bool _ -> mvbdu_inter
    | Mvbdu mvbdu_inter ->
      let mvbdu =
        Edges_intbdu.mvbdu_project_abstract_away mvbdu_inter parameters
      in
      Edges_intbdu.mvbdu_normalize mvbdu

  let remove_parameters_for_all_nodes valuation_list parameters t =
    let valuation_list = List.rev valuation_list in
    let parameters = List.rev parameters in
    let valuation_list = Edges_intbdu.build_association_list valuation_list in
    let vars = Edges_intbdu.build_variables_list parameters in
    let mvbdu_valuation = Edges_intbdu.mvbdu_of_hconsed_asso valuation_list in
    let mvbdu_valuation = Edges_intbdu.mvbdu_normalize mvbdu_valuation in
    let reachability_status =
      ReachabilityStatus.map
        (fun _ mvbdu -> abstract_parameters mvbdu mvbdu_valuation vars)
        t.reachability_status
    in
    { t with reachability_status }

  let set_unreachable t node =
    let () =
      ReachabilityStatus.add node
        (Edges_intbdu.mvbdu_false ())
        t.reachability_status
    in
    t

  let remove_successor node ~succ t =
    let successors = succs_of t node in
    let new_set = NodeSet.remove succ successors in
    set_successors node new_set t

  let remove_predecessor node ~pred t =
    let predecessors = preds_of t node in
    let new_set = NodeSet.remove pred predecessors in
    set_predecessors node new_set t

  let remove_simple_edge q1 q2 hg =
    hg |> remove_successor q1 ~succ:q2 |> remove_predecessor q2 ~pred:q1

  let remove_compound_node t node =
    match node with
    | Node.Simple _ -> t
    | Node.Compound _ ->
      let () = AdjacencyList.remove node t.adjacency_list in
      let () = AdjacencyList.remove node t.preds in
      let () = ReachabilityStatus.remove node t.reachability_status in
      t

  let remove_hyperedge =
    Hypergraphs.remove_hyperedge remove_simple_edge NodeSet.is_empty succs_of
      remove_successor remove_compound_node

  let remove_parameters t to_unparam =
    let valuation_list, vars, t =
      EdgeSet.fold
        (fun (tail, head) (valuation_list, vars, t) ->
          let _, i, is_new = get_int_of_edge (tail, head) t in
          if is_new then (
            let () = print_endline "????" in
            valuation_list, vars, t (*this should never happen*)
          ) else (
            let is_enabled = Res.Array.get t.current_parameter_valuation i in
            let t =
              if not is_enabled then (
                (* this edge was removed so when closing the chapter, it needs to be permanently removed.*)
                let preds =
                  NodeSet.fold
                    (fun x preds ->
                      match x with
                      | Simple i -> IdSet.add i preds
                      | Compound _ -> preds)
                    (preds_of t tail) (IdSet.empty ())
                in
                remove_hyperedge preds tail head t
              ) else
                t
            in
            ( ( i,
                if is_enabled then
                  1
                else
                  0 )
              :: valuation_list,
              i :: vars,
              t )
          ))
        to_unparam ([], [], t)
    in
    if valuation_list != [] then
      remove_parameters_for_all_nodes valuation_list vars t
    else
      t

  (** All the parameters are removed from all edges, all the edges that are deactivated are removed from the graph. After this function was invoked, the next edges that are added will not be parametrized. If you want the new edges to be parametrized, call "open ()".*)
  let close_chapter t =
    let t = remove_parameters t t.parametrized_edges in
    {
      t with
      parametrized_edges = EdgeSet.empty;
      is_parametrized = (fun _ -> false);
      current_parameter_valuation = Res.Array.empty ();
      edge_to_int = EdgeIntConversion.empty;
      add_parameter_for_new_edges = false;
    }

  let open_chapter edge_list t =
    let parametrized_edges, t =
      List.fold_left
        (fun (edges, t) (head, tail, _) ->
          let edge = tail, head in
          let edges = EdgeSet.add edge edges in
          edges, t)
        (EdgeSet.empty, t) edge_list
    in
    let to_unparam = EdgeSet.diff t.parametrized_edges parametrized_edges in
    let to_param = EdgeSet.diff parametrized_edges t.parametrized_edges in
    let is_parametrized edge = EdgeSet.mem edge parametrized_edges in
    let t =
      {
        t with
        parametrized_edges;
        is_parametrized;
        add_parameter_for_new_edges = true;
      }
    in
    let t = remove_parameters t to_unparam in
    let is_not_unreachable node t =
      let mvbdu = reachability_mvbdu_of node t in
      not (Edges_intbdu.mvbdu_equal mvbdu (Edges_intbdu.mvbdu_false ()))
    in
    let t, c_set =
      EdgeSet.fold
        (fun (tail, head) (t, c_set) ->
          let t, id, _ = get_int_of_edge (tail, head) t in
          let t = add_parameter_valuation id true t in
          Hypergraphs.update_successors_after_removal is_not_unreachable
            set_unreachable set_unreachable
            (fun _ _ _ -> true)
            succs_of head c_set t)
        to_param (t, [])
    in
    let non_param_edges, compound_nodes, parametrized_edges =
      List.fold_left
        (fun (non_param_edges, compound_nodes, parametrized_edges) z ->
          NodeSet.fold
            (fun pred (non_param_edges, compound_nodes, parametrized_edges) ->
              if Node.is_compound z then
                ( non_param_edges,
                  NodeSet.add z compound_nodes,
                  parametrized_edges )
              else (
                let edge = pred, z in
                if t.is_parametrized edge then
                  ( non_param_edges,
                    compound_nodes,
                    EdgeSet.add edge parametrized_edges )
                else
                  ( EdgeSet.add edge non_param_edges,
                    compound_nodes,
                    parametrized_edges )
              ))
            (preds_of t z)
            (non_param_edges, compound_nodes, parametrized_edges))
        (EdgeSet.empty, NodeSet.empty (), EdgeSet.empty)
        c_set
    in
    closure_process (non_param_edges, compound_nodes, parametrized_edges) t
end

include SemiParametricHypergraph
