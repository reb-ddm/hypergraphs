open Hypergraph_sig

exception UnknownEdge

module Edges_intbdu = struct
  open Cudd

  type key = Id.t
  type mvbdu = Bdd.dt
  type t = Bool of bool | Mvbdu of mvbdu

  let man_ref : Man.dt option ref = ref None
  let btrue_ref : mvbdu option ref = ref None
  let bfalse_ref : mvbdu option ref = ref None

  let ensure_init () =
    match !man_ref with
    | Some _ -> ()
    | None ->
      let m = Man.make_d () in
      man_ref := Some m;
      btrue_ref := Some (Bdd.dtrue m);
      bfalse_ref := Some (Bdd.dfalse m)

  let get_man () =
    ensure_init ();
    match !man_ref with
    | Some m -> m
    | None -> assert false

  let ithvar (v : key) =
    let m = get_man () in
    Bdd.ithvar m v

  let init () = ensure_init ()

  let variables_list_of_mvbdu (m : mvbdu) : key list =
    let supp = Bdd.support m in
    Bdd.list_of_support supp

  let print (x : t) : unit =
    match x with
    | Bool b -> Format.printf "Bool %b\n" b
    | Mvbdu m ->
      let printer fmt k = Format.fprintf fmt "%d" k in
      Bdd.print printer Format.std_formatter m;
      print_newline ()

  let mvbdu_equal (a : t) (b : t) : bool =
    match a, b with
    | Bool x, Bool y -> x = y
    | Bool true, Mvbdu m | Mvbdu m, Bool true -> Bdd.is_true m
    | Bool false, Mvbdu m | Mvbdu m, Bool false -> Bdd.is_false m
    | Mvbdu m1, Mvbdu m2 -> Bdd.is_equal m1 m2

  let mvbdu_or (a : t) (b : t) : t =
    match a, b with
    | Bool false, x | x, Bool false -> x
    | Bool true, _ | _, Bool true -> Bool true
    | Mvbdu m1, Mvbdu m2 -> Mvbdu (Bdd.dor m1 m2)

  let mvbdu_and (a : t) (b : t) : t =
    match a, b with
    | Bool true, x | x, Bool true -> x
    | Bool false, _ | _, Bool false -> Bool false
    | Mvbdu m1, Mvbdu m2 -> Mvbdu (Bdd.dand m1 m2)

  let mvbdu_true () = Bool true
  let mvbdu_false () = Bool false

  let btrue () =
    ensure_init ();
    match !btrue_ref with
    | Some b -> b
    | None -> assert false

  let mvbdu_of_association_list (alist : (int * int) list) : t =
    ensure_init ();
    match alist with
    | [] -> Bool true
    | _ ->
      let cube =
        List.fold_left
          (fun acc (v, value) ->
            let var = ithvar v in
            let lit =
              match value with
              | 1 -> var
              | 0 -> Bdd.dnot var
              | _ -> failwith "mvbdu_of_association_list: value must be 0 or 1"
            in
            if Bdd.is_false acc then
              lit
            else
              Bdd.dand acc lit)
          (btrue ()) alist
      in
      Mvbdu cube

  let mvbdu_project_abstract_away (x : t) (vars : key list) : t =
    match x with
    | Bool b -> Bool b
    | Mvbdu m ->
      if vars = [] then
        Mvbdu m
      else (
        let var_array = Array.of_list (List.map ithvar vars) in
        let varset = Bdd.vectorsupport var_array in
        Mvbdu (Bdd.exist varset m)
      )

  let mvbdu_to_formula mvbdu =
    match mvbdu with
    | Bool true -> Logical_formulae.True
    | Bool false -> Logical_formulae.False
    | Mvbdu mvbdu ->
      let rec to_formula mvbdu =
        match Bdd.inspect mvbdu with
        | Bdd.Bool true -> Logical_formulae.True
        | Bdd.Bool false -> Logical_formulae.False
        | Bdd.Ite (var, branch_true, branch_false) ->
          let atom = Logical_formulae.P var in
          let output_true = to_formula branch_true in
          let output_false = to_formula branch_false in
          let imply_false =
            Logical_formulae.IMPLY (Logical_formulae.NOT atom, output_false)
          in
          let imply_true = Logical_formulae.IMPLY (atom, output_true) in
          Logical_formulae.AND (imply_false, imply_true)
      in
      Logical_formulae.simplify (to_formula mvbdu)
end

module ParametricHypergraph = struct
  module AdjacencyList = MutableNodeMap (NodeSet)

  module ReachabilityStatus = MutableNodeMap (struct
    include Edges_intbdu

    let empty = mvbdu_false
  end)

  module EdgeSet = Set.Make (Edge)

  type parametric_hypergraph = {
    adjacency_list: AdjacencyList.t;
    reachability_status: ReachabilityStatus.t;
    preds: AdjacencyList.t; (* incoming edges *)
    edge_to_int: EdgeIntConversion.t;
    current_parameter_valuation: bool Res.Array.t;
    is_parametrized: Edge.t -> bool;
    parametrized_edges: EdgeSet.t;
    add_parameter_for_new_edges: bool;
    is_semi_parametrized: bool;
  }

  type t = parametric_hypergraph

  let name = "Parameteric"

  exception OutOfBounds

  let init ~nr_vars true_node =
    let () = Edges_intbdu.init () in
    let reachability_status = ReachabilityStatus.empty nr_vars in
    let () =
      ReachabilityStatus.add true_node
        (Edges_intbdu.mvbdu_true ())
        reachability_status
    in
    {
      adjacency_list = AdjacencyList.empty nr_vars;
      reachability_status;
      preds = AdjacencyList.empty nr_vars;
      edge_to_int = EdgeIntConversion.empty;
      current_parameter_valuation = Res.Array.empty ();
      is_parametrized = (fun _ -> true);
      parametrized_edges = EdgeSet.empty;
      add_parameter_for_new_edges = true;
      is_semi_parametrized = false;
    }

  let mvbdu_equal mvbdu1 mvbdu2 = Edges_intbdu.mvbdu_equal mvbdu1 mvbdu2
  let mvbdu_or mvbdu1 mvbdu2 = Edges_intbdu.mvbdu_or mvbdu1 mvbdu2

  let is_reachable node t =
    let mvbdu = ReachabilityStatus.get node t.reachability_status in
    let valuation_list =
      List.init (Res.Array.length t.current_parameter_valuation) (fun i ->
          ( i,
            if Res.Array.get t.current_parameter_valuation i then
              1
            else
              0 ))
    in
    let mvbdu_valuation =
      Edges_intbdu.mvbdu_of_association_list valuation_list
    in
    let mvbdu_inter = Edges_intbdu.mvbdu_and mvbdu mvbdu_valuation in
    let mvbdu_false = Edges_intbdu.mvbdu_false () in
    not (mvbdu_equal mvbdu_inter mvbdu_false)

  let get_int_of_edge edge t =
    let edge_to_int, i, is_new =
      EdgeIntConversion.val_to_int t.edge_to_int edge
    in
    { t with edge_to_int }, i, is_new

  let or_reachability node mvbdu t =
    let previous_mvbdu = ReachabilityStatus.get node t.reachability_status in
    let or_mvbdu = mvbdu_or previous_mvbdu mvbdu in
    let () = ReachabilityStatus.add node or_mvbdu t.reachability_status in
    t

  let succs_of t node = AdjacencyList.get node t.adjacency_list

  let reachable_vars =
    let fold f a b = NodeSet.fold (fun a b -> f b a) b a in
    Hypergraphs.reachable_vars succs_of is_reachable (Node.Compound 0) fold

  let preds_of t node = AdjacencyList.get node t.preds

  let reachability_mvbdu_of node t =
    ReachabilityStatus.get node t.reachability_status

  let set_successors node succs t =
    let () = AdjacencyList.add node succs t.adjacency_list in
    t

  let add_successor node ~succ t =
    let successors = succs_of t node in
    if NodeSet.mem succ successors then
      t
    else (
      let new_set = NodeSet.add succ successors in
      set_successors node new_set t
    )

  let set_predecessors node p t =
    let () = AdjacencyList.add node p t.preds in
    t

  let add_predecessor node ~pred t =
    let predecessors = preds_of t node in
    if NodeSet.mem pred predecessors then
      t
    else (
      let new_set = NodeSet.add pred predecessors in
      set_predecessors node new_set t
    )

  let add_parameter_valuation index bool t =
    let () =
      let length = Res.Array.length t.current_parameter_valuation in
      if index < length then
        Res.Array.set t.current_parameter_valuation index bool
      else
        add_element_to_res_array t.current_parameter_valuation ~length ~index
          ~elt:bool ~empty:false
    in
    t

  let recalculate_mvbdu_simple_edge (node, head) t add_parameter =
    let current_node_mvbdu = reachability_mvbdu_of node t in
    match current_node_mvbdu with
    | Bool false -> t
    | Bool true | Mvbdu _ ->
      (match reachability_mvbdu_of head t with
      | Bool true -> t
      | Bool false | Mvbdu _ ->
        let t, reach =
          if add_parameter then (
            let t, id, _ = get_int_of_edge (node, head) t in
            let association_list = [ id, 1 ] in
            let new_mvbdu_node2 =
              Edges_intbdu.mvbdu_of_association_list association_list
            in
            t, Edges_intbdu.mvbdu_and current_node_mvbdu new_mvbdu_node2
          ) else
            t, current_node_mvbdu
        in
        or_reachability head reach t)

  let recalculate_mvbdu_compound_node node1 t =
    let reach =
      NodeSet.fold
        (fun node reach ->
          let status = reachability_mvbdu_of node t in
          Edges_intbdu.mvbdu_and reach status)
        (preds_of t node1)
        (Edges_intbdu.mvbdu_true ())
    in
    let () = ReachabilityStatus.add node1 reach t.reachability_status in
    t

  let add_parametrized_edge (x, y) t =
    if t.is_semi_parametrized then (
      let parametrized_edges = EdgeSet.add (x, y) t.parametrized_edges in
      let is_parametrized edge = EdgeSet.mem edge parametrized_edges in
      { t with parametrized_edges; is_parametrized }
    ) else
      t

  let add_new_edge_reachabilities node head t =
    let t =
      if
        Node.is_simple node || ReachabilityStatus.mem node t.reachability_status
      then
        t
      else
        recalculate_mvbdu_compound_node node t
    in
    (* new added edges are parametrized if t.add_param is true *)
    let add_parameter = t.add_parameter_for_new_edges in
    let t = recalculate_mvbdu_simple_edge (node, head) t add_parameter in
    if add_parameter then (
      let t = add_parametrized_edge (node, head) t in
      let t, id, _ = get_int_of_edge (node, head) t in
      add_parameter_valuation id true t
    ) else
      t

  (* ---------- Add edges ---------- *)
  let add_simple_edge q1 q2 hg =
    hg |> add_successor q1 ~succ:q2 |> add_predecessor q2 ~pred:q1

  let add_hyperedge tail_set tail head t =
    let add_simple_edge2 tail head t =
      t |> add_simple_edge tail head |> add_new_edge_reachabilities tail head
    in
    Hypergraphs.add_hyperedge
      (fun _ _ t -> t)
      add_simple_edge add_simple_edge2
      (fun a -> a)
      tail_set tail head t

  let add_successors_to_sets node t
      (non_param_edges, compound_nodes, param_edges) =
    NodeSet.fold
      (fun succ (non_param_edges, compound_nodes, param_edges) ->
        if Node.is_compound succ then
          non_param_edges, NodeSet.add succ compound_nodes, param_edges
        else (
          let edge = node, succ in
          if t.is_parametrized edge then
            non_param_edges, compound_nodes, EdgeSet.add edge param_edges
          else
            EdgeSet.add edge non_param_edges, compound_nodes, param_edges
        ))
      (succs_of t node)
      (non_param_edges, compound_nodes, param_edges)

  let rec closure_process (non_param_edges, compound_nodes, param_edges) t =
    (*first process all non-parametrized edges, then compound nodes, and the the parametrized edges. Such that we can in some cases avoid mvbdus altogether.*)
    let empty_non_param, empty_compound, empty_param =
      ( EdgeSet.is_empty non_param_edges,
        NodeSet.is_empty compound_nodes,
        EdgeSet.is_empty param_edges )
    in
    if empty_non_param && empty_compound && empty_param then
      t
    else (
      let t, (non_param_edges, compound_nodes, param_edges) =
        let pred, node, non_param_edges, compound_nodes, param_edges =
          if not empty_non_param then (
            let pred, node = EdgeSet.choose non_param_edges in
            let non_param_edges = EdgeSet.remove (pred, node) non_param_edges in
            pred, node, non_param_edges, compound_nodes, param_edges
          ) else if not empty_compound then (
            let pred, node = Node.Compound 0, NodeSet.choose compound_nodes in
            let compound_nodes = NodeSet.remove node compound_nodes in
            pred, node, non_param_edges, compound_nodes, param_edges
          ) else (
            let pred, node = EdgeSet.choose param_edges in
            let param_edges = EdgeSet.remove (pred, node) param_edges in
            pred, node, non_param_edges, compound_nodes, param_edges
          )
        in
        let old_mvbdu = reachability_mvbdu_of node t in
        let t =
          if Node.is_compound node then
            (*look at all the predecessors*)
            recalculate_mvbdu_compound_node node t
          else (
            let add_parameter = t.is_parametrized (pred, node) in
            recalculate_mvbdu_simple_edge (pred, node) t add_parameter
          )
        in
        let new_mvbdu = reachability_mvbdu_of node t in
        if mvbdu_equal old_mvbdu new_mvbdu then
          t, (non_param_edges, compound_nodes, param_edges)
        else
          ( t,
            add_successors_to_sets node t
              (non_param_edges, compound_nodes, param_edges) )
      in
      closure_process (non_param_edges, compound_nodes, param_edges) t
    )

  (*BFS*)
  let closure node t =
    closure_process
      (add_successors_to_sets node t
         (EdgeSet.empty, NodeSet.empty (), EdgeSet.empty))
      t

  (* add an edge x -> y to the hypergraph *)
  let add_clause (y, x, x_set) t =
    let t, id, is_new = get_int_of_edge (x, y) t in
    if is_new || not (t.is_parametrized (x, y)) then (
      let old_mvbdu = reachability_mvbdu_of y t in
      let t = add_hyperedge x_set x y t in
      let new_mvbdu = reachability_mvbdu_of y t in
      if mvbdu_equal old_mvbdu new_mvbdu then
        t
      else
        closure y t
    ) else
      add_parameter_valuation id true t

  let add_clauses = Hypergraphs.add_clauses add_clause

  let weight_of t node =
    let mvbdu = reachability_mvbdu_of node t in
    let formula = Edges_intbdu.mvbdu_to_formula mvbdu in
    let formula = Logical_formulae.simplify formula in
    let _, string_formula =
      Logical_formulae.convert_p (fun x _ -> (), string_of_int x) () formula
    in
    let string =
      Logical_formulae.print_formula "" (fun s s2 -> s2 ^ s) string_formula
    in
    Some string

  (** Recalculates all weights from scratch. *)
  let remove_clause (y, x, x_set) hg =
    if Node.is_reflexive_edge x_set y then
      (*reflexive edges are not allowed*)
      hg
    else (
      let hg, id, is_new = get_int_of_edge (x, y) hg in
      if is_new || not (hg.is_parametrized (x, y)) then
        raise UnknownEdge
      else
        hg |> add_parameter_valuation id false
    )

  let open_chapter _ t = t
  let close_chapter t = t
end

include ParametricHypergraph
