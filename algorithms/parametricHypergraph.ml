open Hypergraph_sig
open Kappa_mvbdu.Mvbdu_wrapper

exception UnknownEdge

module Edges_bdu :
  Mvbdu
  with type key = Id.t
   and type value = int
  with type mvbdu = Mvbdu.mvbdu =
  Mvbdu

module Edges_intbdu = struct
  module Internal_mvbdu = Internalize (Edges_bdu)
  include Internal_mvbdu

  type t = Bool of bool | Mvbdu of mvbdu

  let init () =
    let parameters = Remanent_parameters.get_parameters ~html_mode:false ~called_from:Remanent_parameters_sig.Internalised () in
    if not (is_init ()) then 
      init parameters

  let print mvbdu =
    match mvbdu with 
    | Bool true -> print_endline "MVBDU_TRUE"
    | Bool false -> print_endline "MVBDU_FALSE"
    | Mvbdu mvbdu ->
      let parameters = Remanent_parameters.get_parameters ~html_mode:false ~called_from:Remanent_parameters_sig.Internalised () in
      print parameters mvbdu

  let mvbdu_normalize mvbdu restriction_bdu = 
    let mvbdu = match restriction_bdu with 
      | Bool _ -> mvbdu
      | Mvbdu restriction_bdu -> mvbdu_and mvbdu restriction_bdu in
    if equal mvbdu (mvbdu_true ()) then
      Bool true
    else if equal mvbdu (mvbdu_false ()) then 
      Bool false
    else 
      Mvbdu mvbdu

  let mvbdu_or mvbdu1 mvbdu2 ~restrict:restriction_bdu = 
    match mvbdu1, mvbdu2 with 
    | Bool b1, Bool b2 -> Bool (b1 || b2)
    | Bool true, _ | _, Bool true -> Bool true
    | Bool false, m | m, Bool false -> m
    | Mvbdu mvbdu1, Mvbdu mvbdu2 -> 
      let or_mvbdu = mvbdu_or mvbdu1 mvbdu2 in
      let result = match restriction_bdu with
        | Bool _ -> or_mvbdu
        | Mvbdu restriction_bdu -> mvbdu_and or_mvbdu restriction_bdu in
      if equal result (mvbdu_true ()) then
        Bool true
      else
        Mvbdu result

  let mvbdu_and mvbdu1 mvbdu2 = 
    match mvbdu1, mvbdu2 with 
    | Bool b1, Bool b2 -> Bool (b1 && b2)
    | Bool false, _ | _, Bool false -> Bool false
    | Bool true, m | m, Bool true -> m
    | Mvbdu m1, Mvbdu m2 -> 
      let result = mvbdu_and m1 m2 in
      if equal result (mvbdu_false ()) then
        Bool false
      else
        Mvbdu result

  let mvbdu_equal mvbdu1 mvbdu2 ~restrict:restriction_bdu =
    let mvbdu1 = mvbdu_and mvbdu1 restriction_bdu in
    let mvbdu2 = mvbdu_and mvbdu2 restriction_bdu in
    match mvbdu1, mvbdu2 with 
    | Bool b1, Bool b2 -> b1 = b2
    | Bool _, Mvbdu _ | Mvbdu _, Bool _ -> false
    | Mvbdu mvbdu1, Mvbdu mvbdu2 ->
      equal mvbdu1 mvbdu2

  let mvbdu_true () = Bool true
  let mvbdu_false () = Bool false

  let mvbdu_to_formula mvbdu = 
    match mvbdu with
    | Bool false -> Logical_formulae.False
    | Bool true -> Logical_formulae.True
    | Mvbdu mvbdu -> mvbdu_to_formula ~cartesian_decomposition:false mvbdu
end

module ParametricHypergraph = struct

  module AdjacencyList = MutableNodeMap(NodeSet)
  module ReachabilityStatus = MutableNodeMap(struct 
      include Edges_intbdu 
      let empty = mvbdu_false
    end)

  module EdgeSet = Set.Make(Edge)

  type parametric_hypergraph = {
    adjacency_list : AdjacencyList.t;
    reachability_status : ReachabilityStatus.t;
    restriction_bdu : Edges_intbdu.t;
    preds : AdjacencyList.t; (* incoming edges *)
    edge_to_int : EdgeIntConversion.t;
    current_parameter_valuation : bool Res.Array.t;
    is_parametrized : Edge.t -> bool;
    parametrized_edges : EdgeSet.t;
    add_parameter_for_new_edges : bool;
    is_semi_parametrized : bool;
  }

  type t = parametric_hypergraph

  let name = "Parameteric"

  exception OutOfBounds

  let init ~nr_vars true_node = 
    let () = Edges_intbdu.init () in
    let reachability_status = ReachabilityStatus.empty nr_vars in
    let () = ReachabilityStatus.add true_node (Edges_intbdu.mvbdu_true ()) reachability_status in
    {
      adjacency_list = AdjacencyList.empty nr_vars;
      reachability_status;
      restriction_bdu = Edges_intbdu.mvbdu_true ();
      preds = AdjacencyList.empty nr_vars;
      edge_to_int = EdgeIntConversion.empty;
      current_parameter_valuation = Res.Array.empty ();
      is_parametrized = (fun _ -> true);
      parametrized_edges = EdgeSet.empty;
      add_parameter_for_new_edges = true;
      is_semi_parametrized = false;
    }

  let mvbdu_equal t mvbdu1 mvbdu2 =
    Edges_intbdu.mvbdu_equal mvbdu1 mvbdu2 ~restrict:t.restriction_bdu

  let mvbdu_or t mvbdu1 mvbdu2 =
    Edges_intbdu.mvbdu_or mvbdu1 mvbdu2 ~restrict:t.restriction_bdu

  let is_reachable node t = 
    let mvbdu = ReachabilityStatus.get node t.reachability_status in 
    let valuation_list = 
      List.init (Res.Array.length t.current_parameter_valuation) 
        (fun i -> (i, if Res.Array.get t.current_parameter_valuation i then 1 else 0)) in
    let valuation_list = Edges_intbdu.build_association_list valuation_list in
    let mvbdu_valuation = Edges_intbdu.mvbdu_of_hconsed_asso valuation_list in
    let mvbdu_valuation = Edges_intbdu.mvbdu_normalize mvbdu_valuation t.restriction_bdu in
    let mvbdu_inter = Edges_intbdu.mvbdu_and mvbdu mvbdu_valuation in
    let mvbdu_false = Edges_intbdu.mvbdu_false () in
    not (mvbdu_equal t mvbdu_inter mvbdu_false)

  let get_int_of_edge edge t = 
    let edge_to_int, i, is_new = EdgeIntConversion.val_to_int t.edge_to_int edge in
    {t with edge_to_int}, i, is_new

  let or_reachability node mvbdu t = 
    let previous_mvbdu = 
      ReachabilityStatus.get node t.reachability_status in 
    let or_mvbdu = mvbdu_or t previous_mvbdu mvbdu in
    let () = ReachabilityStatus.add node or_mvbdu t.reachability_status
    in t 

  let succs_of t node =
    AdjacencyList.get node t.adjacency_list

  let preds_of t node =
    AdjacencyList.get node t.preds

  let reachability_mvbdu_of node t =
    ReachabilityStatus.get node t.reachability_status

  let set_successors node succs t = 
    let () = 
      AdjacencyList.add node succs t.adjacency_list in
    t

  let add_successor node ~succ t = 
    let successors = succs_of t node in 
    if NodeSet.mem succ successors then 
      t
    else
      let new_set = NodeSet.add succ successors in
      set_successors node new_set t

  let set_predecessors node p t = 
    let () = 
      AdjacencyList.add node p t.preds in
    t

  let add_predecessor node ~pred t = 
    let predecessors = preds_of t node in 
    if NodeSet.mem pred predecessors then 
      t
    else
      let new_set = NodeSet.add pred predecessors in
      set_predecessors node new_set t

  let add_parameter_valuation index bool t = 
    let () =
      let length = Res.Array.length t.current_parameter_valuation in
      if index < length then 
        Res.Array.set t.current_parameter_valuation index bool 
      else
        add_element_to_res_array t.current_parameter_valuation ~length ~index ~elt:bool ~empty:false
    in t

  let recalculate_mvbdu_simple_edge (node, head) t add_parameter =
    let current_node_mvbdu = reachability_mvbdu_of node t in
    let t, reach = 
      if add_parameter then 
        let t, id, _ = get_int_of_edge (node, head) t in  
        let association_list = [(id, 1)] in
        let new_mvbdu_node2 = Edges_intbdu.mvbdu_of_association_list association_list in
        let new_mvbdu_node2 = Edges_intbdu.Mvbdu new_mvbdu_node2 in
        t, Edges_intbdu.mvbdu_and current_node_mvbdu new_mvbdu_node2 
      else
        t, current_node_mvbdu
    in
    or_reachability head reach t

  let recalculate_mvbdu_compound_node node1 t = 
    let reach = NodeSet.fold (fun node reach ->
        let status = 
          reachability_mvbdu_of node t
        in Edges_intbdu.mvbdu_and reach status
      ) (preds_of t node1) (Edges_intbdu.mvbdu_true ()) in
    let () = 
      ReachabilityStatus.add node1 reach t.reachability_status in
    t

  let add_parametrized_edge (x, y) t =
    if t.is_semi_parametrized then
      let parametrized_edges = EdgeSet.add (x, y) t.parametrized_edges in
      let is_parametrized = (fun edge -> EdgeSet.mem edge parametrized_edges) in
      { t with parametrized_edges; is_parametrized }
    else 
      t

  let add_new_edge_reachabilities node head t =
    let t =
      if Node.is_simple node || ReachabilityStatus.mem node t.reachability_status then 
        t
      else
        recalculate_mvbdu_compound_node node t
    in
    (* new added edges are parametrized if t.add_param is true *)
    let add_parameter = t.add_parameter_for_new_edges in
    let t = recalculate_mvbdu_simple_edge (node, head) t add_parameter in
    if add_parameter then 
      let t = add_parametrized_edge (node, head) t in
      let t, id, _ = get_int_of_edge (node, head) t in 
      let t = add_parameter_valuation id true t in
      let range_mvbdu = Edges_intbdu.mvbdu_of_range_list ([id, (Some 0, Some 1)]) in 
      let range_mvbdu = Edges_intbdu.Mvbdu range_mvbdu in
      let restriction_bdu = Edges_intbdu.mvbdu_and range_mvbdu t.restriction_bdu in
      {t with restriction_bdu}
    else t

  (* ---------- Add edges ---------- *)
  let add_simple_edge q1 q2 hg =
    hg 
    |> add_successor q1 ~succ:q2
    |> add_predecessor q2 ~pred:q1

  let add_hyperedge tail_set tail head t =
    let add_simple_edge2 tail head t =
      t |> add_simple_edge tail head |> add_new_edge_reachabilities tail head in
    Hypergraphs.add_hyperedge (fun _ _ t -> t) add_simple_edge add_simple_edge2 (fun a -> a) tail_set tail head t

  (*BFS*)
  let closure node t =
    let get_edge_set node t = 
      NodeSet.fold (fun succ edges -> 
          let edge = if Node.is_compound succ then (Node.Compound 0, succ) else
              (node, succ) in 
          EdgeSet.add edge edges) (succs_of t node) (EdgeSet.empty) in
    let rec process nodes t = 
      if EdgeSet.is_empty nodes then t 
      else
        let pred, node = EdgeSet.choose nodes in
        let nodes = EdgeSet.remove (pred, node) nodes in
        let t, nodes =
          let old_mvbdu = reachability_mvbdu_of node t in
          let t = if Node.is_compound node then 
              (*look at all the predecessors*)
              recalculate_mvbdu_compound_node node t
            else 
              let add_parameter = t.is_parametrized (pred, node) in
              recalculate_mvbdu_simple_edge (pred, node) t add_parameter
          in
          let new_mvbdu = reachability_mvbdu_of node t in
          if mvbdu_equal t old_mvbdu new_mvbdu then
            t, nodes 
          else
            t, EdgeSet.union (get_edge_set node t) nodes
        in process nodes t
    in 
    process (get_edge_set node t) t

  (* add an edge x -> y to the hypergraph *)
  let add_clause (y, x, x_set) t =
    let t, id, is_new = get_int_of_edge (x, y) t in
    if is_new || not (t.is_parametrized (x,y)) then 
      let old_mvbdu = reachability_mvbdu_of y t in
      let t = add_hyperedge x_set x y t in
      let new_mvbdu = reachability_mvbdu_of y t in
      if mvbdu_equal t old_mvbdu new_mvbdu then
        t
      else closure y t 
    else 
      add_parameter_valuation id true t

  let add_clauses =
    Hypergraphs.add_clauses add_clause

  let weight_of t node =  
    let mvbdu = reachability_mvbdu_of node t in
    let formula = Edges_intbdu.mvbdu_to_formula mvbdu in
    let formula = Logical_formulae.simplify formula in
    let _, string_formula = Logical_formulae.convert_p (fun x _ -> (), string_of_int x)  () formula in
    let string = Logical_formulae.print_formula "" (^) string_formula in
    Some string

  (** Recalculates all weights from scratch. *)
  let remove_clause (y, x, x_set) hg = 
    if Node.is_reflexive_edge x_set y then 
      (*reflexive edges are not allowed*)
      hg 
    else
      let hg, id, is_new = get_int_of_edge (x, y) hg in
      if is_new || not (hg.is_parametrized (x,y)) then raise UnknownEdge else
        hg 
        |> add_parameter_valuation id false

  let open_chapter _ t = t
  let close_chapter t = t

end

include ParametricHypergraph
