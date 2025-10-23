
module Id = struct
  type t = int
  let compare = Stdlib.compare
end

let set_to_string string_of_elt to_seq s = 
  String.concat ", " (List.map (fun e -> string_of_elt e) (List.of_seq (to_seq s)))

module IdSet = struct
  include Set.Make(Id)
  let to_string  = set_to_string string_of_int to_seq
  let empty () = empty

end

module IdSetMap = Map.Make(IdSet)

module CompoundIntConversion = struct

  type t = {
    compound_to_int : int IdSetMap.t;
    next_compound_int: int;
  }

  let empty = {compound_to_int = IdSetMap.empty;
               next_compound_int = 0}

  let bindings t = IdSetMap.bindings t.compound_to_int

  let compound_to_int cti node =
    match IdSetMap.find_opt node cti.compound_to_int with
    | Some i -> cti, i
    | None -> 
      let next_int = cti.next_compound_int in
      let compound_to_int = IdSetMap.add node next_int cti.compound_to_int
      in {compound_to_int; next_compound_int = next_int + 1}, next_int

  let remove_node cti node =
    match IdSetMap.find_opt node cti.compound_to_int with
    | Some i -> 
      let next_compound_int = 
        if i = cti.next_compound_int - 1 then i 
        else cti.next_compound_int in (*TODO fill blanks?*)
      let compound_to_int = IdSetMap.remove node cti.compound_to_int in
      {compound_to_int; next_compound_int}
    | None -> cti
end

module Node = struct
  type t =
    | Simple of Id.t
    | Compound of Id.t

  let is_simple = function Simple _ -> true | Compound _ -> false
  let is_compound = function Simple _ -> false | Compound _ -> true

  let compare a b =
    match a, b with
    | Simple i, Simple j -> Id.compare i j
    | Simple _, Compound _ -> -1
    | Compound _, Simple _ -> 1
    | Compound s1, Compound s2 -> Id.compare s1 s2

  let simple_node_of_id id = Simple id
  let compound_node_of_intset s cti = 
    let cti, i = CompoundIntConversion.compound_to_int cti s in
    cti, Compound i

  let is_reflexive_edge tail_set head =
    match head with
    | Simple x -> IdSet.mem x tail_set
    | Compound _ -> false

  let to_string node =
    match node with
    | Simple id -> "s" ^ string_of_int id
    | Compound id -> 
      "c" ^ string_of_int id

  let dot_of_node node int_to_string b weight =
    let w_string = match weight with
      | None -> ""
      | Some w -> "("^string_of_int w^")" in
    match node with
    | Simple id ->
      let vname = int_to_string id in
      Buffer.add_string b
        (Printf.sprintf "  n%s [label=\"%s%s\", shape=plaintext];\n"
           (to_string node) vname w_string)
    | Compound _ -> 
      let id = to_string node in 
      Buffer.add_string b
        (Printf.sprintf "  n%s [label=\"%s%s\", shape=circle, style=filled, fillcolor=white, color=black, width=0.2, fixedsize=true];\n"
           id id w_string)

  let dot_of_edge node successor b =
    let edge_style =
      match node, successor with
      | Simple _, Compound _ -> " [style=dashed]"
      | Simple _, _ | Compound _, _ -> ""
    in
    Buffer.add_string b
      (Printf.sprintf "  n%s -> n%s%s;\n" (to_string node) (to_string successor) edge_style)
end

module NodeSet = struct
  include Set.Make(Node)
  let empty () = empty
  let to_string = set_to_string Node.to_string to_seq
end

module NodeMap = Map.Make(Node)

module type OrderedAndPrintable = sig
  include Set.OrderedType 
  val to_string : t -> string
end

module type HasEmpty = sig
  type t
  val empty : unit -> t
end

(**
   Priority queue 
   None represents the value infinity.
   Source = https://lewinb.net/posts/22_priorityqueue_ocaml/
*)
module PriorityQueue (Elt : OrderedAndPrintable) = struct
  module EltWithPrio = struct
    type t = int option * Elt.t
    let compare (a,e1) (b,e2) = 
      match a, b with 
      | None, None -> 0
      | None, _ -> 1
      | _, None -> -1
      | Some x, Some y -> 
        let c = Int.compare x y in
        if c = 0 then Elt.compare e1 e2 else c
  end
  module IntOptionSet = Set.Make(EltWithPrio)
  module EltSet = Set.Make(Elt)

  type t = IntOptionSet.t * EltSet.t
  let empty () = IntOptionSet.empty, EltSet.empty
  let is_empty (_prio_set, set) = EltSet.is_empty set
  let mem elt (_prio_set, set) = EltSet.mem elt set
  let add ~prio elt (prio_set, set) = 
    if mem elt (prio_set, set) then (prio_set, set) else
      IntOptionSet.add (prio, elt) prio_set, EltSet.add elt set

  let remove_min_opt (prio_set, set) =
    match IntOptionSet.min_elt_opt prio_set with
    | None -> None
    | Some (prio, elt) ->
      Some (prio, elt, (IntOptionSet.remove (prio, elt) prio_set, EltSet.remove elt set))


  let decrease_key elt ~prio:new_prio (q, set) = (* TODO this is not very efficient *)
    match IntOptionSet.find_first_opt (fun (_, q_elt) -> Elt.compare q_elt elt = 0) q with
    | None -> q, set
    | Some old_elt ->
      let q = IntOptionSet.remove old_elt q in
      add ~prio:new_prio elt (q, set)

  let print_all_elements (q, _set) = 
    let () = IntOptionSet.iter (fun (i,n) -> 
        let node_string = Elt.to_string n in
        Option.iter print_int i;
        print_string ";";
        print_string node_string;
        print_string ","
      ) q in print_newline ()
end


module StringMap = Map.Make(String)
module IntMap = Map.Make(Int)
exception ExceededNrOfVars

module Vars = struct
  type var_to_int_map = int StringMap.t
  type int_to_var_map = string IntMap.t

  type t = {
    var_name_to_int : var_to_int_map;
    int_name_to_var : int_to_var_map;
    nr_vars: int;
    next_unused_int: int;
  }

  let get_nr_vars v = v.nr_vars
  let get_vars_in_alphabetical_order v = StringMap.bindings v.var_name_to_int
  let get_vars v = IntMap.bindings v.int_name_to_var


  let init ~nr_vars= {
    var_name_to_int = StringMap.empty;
    int_name_to_var = IntMap.empty;
    nr_vars;
    next_unused_int = 0;
  }

  let add_var vars var id =
    if id >= vars.nr_vars then
      raise ExceededNrOfVars
    else
      let next_unused_int = 
        if id >= vars.next_unused_int then id + 1 else vars.next_unused_int
        (*TODO if you remember the free sites then remove id and add everything between unused_int and id*)
      in
      {
        vars with
        var_name_to_int = StringMap.add var id vars.var_name_to_int;
        int_name_to_var = IntMap.add id var vars.int_name_to_var;
        next_unused_int;
      }

  (**If the variable is new then it is added to the data structure and the function returns true.*)
  let var_to_int vars var =
    match StringMap.find_opt var vars.var_name_to_int with
    | Some i -> (vars, i, false)
    | None ->
      if vars.next_unused_int = vars.nr_vars then
        raise ExceededNrOfVars
      else
        let i = vars.next_unused_int in
        ({
          vars with
          var_name_to_int = StringMap.add var i vars.var_name_to_int;
          int_name_to_var = IntMap.add i var vars.int_name_to_var;
          next_unused_int = i + 1;
        }, i, true)

  let int_to_var vars id =
    IntMap.find id vars.int_name_to_var
end

module MutableNodeMap (Elt: HasEmpty) = struct
  (*TODO remember and reuse empty index?*)
  type t = {
    simple_nodes_map: Elt.t Array.t;
    compound_nodes_map: Elt.t option Res.Array.t;
  }
  
  let empty size = {
    simple_nodes_map = Array.make size (Elt.empty ());
    compound_nodes_map = Res.Array.empty ();
  }

  let add node elt map = 
    let rec add_element_to_array ar length index elt = 
      if length = index then 
        Res.Array.add_one ar (Some elt)
      else 
        let _ = Res.Array.add_one ar None in
        add_element_to_array ar (length + 1) index elt in
    match node with 
    | Node.Simple i -> 
      Array.set map.simple_nodes_map i elt
    | Node.Compound i ->
      let n = Res.Array.length map.compound_nodes_map in
        if i >= n then 
          add_element_to_array map.compound_nodes_map n i elt 
        else
          Res.Array.set map.compound_nodes_map i (Some elt) 

  let get node map = 
    match node with
    | Node.Simple i -> 
      map.simple_nodes_map.(i)
    | Node.Compound i ->
      if Res.Array.length map.compound_nodes_map <= i then
        Elt.empty ()
      else
        match Res.Array.get map.compound_nodes_map i with 
        | None -> 
          Elt.empty ()
        | Some elt -> elt

  let remove node map = 
    match node with 
    | Node.Simple i -> 
      Array.set map.simple_nodes_map i (Elt.empty ())
    | Node.Compound i ->
      Res.Array.set map.compound_nodes_map i None

  let iter f t = 
    let _ = Array.iteri (fun i elt -> f (Node.Simple i) elt) t.simple_nodes_map in
    Res.Array.iteri (fun i elt -> 
        match elt with | None -> ()
                       | Some elt -> f (Node.Compound i) elt) t.compound_nodes_map 

  let map f t =
    let simple_nodes_map = Array.mapi (fun i elt -> f (Node.Simple i) elt) t.simple_nodes_map in
    let compound_nodes_map = Res.Array.mapi (fun i elt -> 
        match elt with | None -> None
                       | Some elt -> Some (f (Node.Compound i) elt)) t.compound_nodes_map in
    { simple_nodes_map; compound_nodes_map }
end

(**(lhs, rhs, set of literals in the rhs)*)
type edge = Node.t * Node.t * IdSet.t 

module type FullyDynamicHornClauses = sig
  type t
  val init : nr_vars:int -> Node.t -> t
  val add_clause : edge -> t -> t
  val add_clauses : edge list -> t -> t
  val is_reachable : Node.t -> t -> bool
  val weight_of : t -> Node.t -> int option
  val succs_of : t -> Node.t -> NodeSet.t
  val remove_clause : edge -> t -> t
end
