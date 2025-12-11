module Id = struct
  type t = int

  let compare = Stdlib.compare
end

let set_to_string string_of_elt to_seq s =
  String.concat ", "
    (List.map (fun e -> string_of_elt e) (List.of_seq (to_seq s)))

module IdSet = struct
  include Set.Make (Id)

  let to_string = set_to_string string_of_int to_seq
  let empty () = empty
end

let rec fold_until f p acc = function
  | _ :: _ when p acc -> acc
  | x :: xs -> fold_until f p (f acc x) xs
  | [] -> acc

module IntConversion (ConversionMap : Map.S) = struct
  type t = { val_to_int: int ConversionMap.t; next_compound_int: int }

  let empty = { val_to_int = ConversionMap.empty; next_compound_int = 0 }
  let bindings t = ConversionMap.bindings t.val_to_int

  (*returns true if it is a new binding*)
  let val_to_int cti node =
    match ConversionMap.find_opt node cti.val_to_int with
    | Some i -> cti, i, false
    | None ->
      let next_int = cti.next_compound_int in
      let val_to_int = ConversionMap.add node next_int cti.val_to_int in
      { val_to_int; next_compound_int = next_int + 1 }, next_int, true
end

module IdSetMap = Map.Make (IdSet)
module CompoundIntConversion = IntConversion (IdSetMap)

module Node = struct
  type t = Simple of Id.t | Compound of Id.t

  let is_simple = function
    | Simple _ -> true
    | Compound _ -> false

  let is_compound = function
    | Simple _ -> false
    | Compound _ -> true

  let is_true = function
    | Simple _ -> false
    | Compound i when i = 0 -> true
    | Compound _ -> false

  let compare a b =
    match a, b with
    | Simple i, Simple j -> Id.compare i j
    | Simple _, Compound _ -> -1
    | Compound _, Simple _ -> 1
    | Compound s1, Compound s2 -> Id.compare s1 s2

  let simple_node_of_id id = Simple id

  let compound_node_of_intset s cti =
    let cti, i, _ = CompoundIntConversion.val_to_int cti s in
    cti, Compound i

  let is_reflexive_edge tail_set head =
    match head with
    | Simple x -> IdSet.mem x tail_set
    | Compound _ -> false

  let to_string node =
    match node with
    | Simple id -> "s" ^ string_of_int id
    | Compound id -> "c" ^ string_of_int id

  let dot_of_node node int_to_string b weight =
    let w_string =
      match weight with
      | None -> ""
      | Some w -> "(" ^ w ^ ")"
    in
    match node with
    | Simple id ->
      let vname = int_to_string id in
      Buffer.add_string b
        (Printf.sprintf "  n%s [label=\"%s%s\", shape=plaintext];\n"
           (to_string node) vname w_string)
    | Compound _ ->
      let id = to_string node in
      Buffer.add_string b
        (Printf.sprintf
           "  n%s [label=\"%s%s\", shape=circle, style=filled, \
            fillcolor=white, color=black, width=0.2, fixedsize=true];\n"
           id id w_string)

  let dot_of_edge node successor b =
    let edge_style =
      match node, successor with
      | Simple _, Compound _ -> " [style=dashed]"
      | Simple _, _ | Compound _, _ -> ""
    in
    Buffer.add_string b
      (Printf.sprintf "  n%s -> n%s%s;\n" (to_string node) (to_string successor)
         edge_style)
end

module Edge = struct
  type t = Node.t * Node.t [@@deriving ord]
end

module EdgeMap = Map.Make (Edge)
module EdgeIntConversion = IntConversion (EdgeMap)

module NodeSet = struct
  include Set.Make (Node)

  let empty () = empty
  let to_string = set_to_string Node.to_string to_seq
end

module NodeMap = Map.Make (Node)

module type OrderedAndPrintable = sig
  include Set.OrderedType

  val to_string : t -> string
end

module type HasEmpty = sig
  type t

  val empty : unit -> t
end

module StringMap = Map.Make (String)
module IntMap = Map.Make (Int)

exception ExceededNrOfVars

module Vars = struct
  type var_to_int_map = int StringMap.t
  type int_to_var_map = string IntMap.t

  type t = {
    var_name_to_int: var_to_int_map;
    int_name_to_var: int_to_var_map;
    nr_vars: int;
    next_unused_int: int;
  }

  let get_nr_vars v = v.nr_vars

  let get_vars_in_alphabetical_order v =
    let rec aux v () =
      match StringMap.min_binding_opt v with
      | Some (s, b) ->
        let v = StringMap.remove s v in
        Seq.Cons ((s, b), aux v)
      | None -> Seq.Nil
    in
    aux v.var_name_to_int

  let get_vars v = IntMap.bindings v.int_name_to_var

  let init ~nr_vars =
    {
      var_name_to_int = StringMap.empty;
      int_name_to_var = IntMap.empty;
      nr_vars;
      next_unused_int = 0;
    }

  let add_var vars var id =
    if id >= vars.nr_vars then
      raise ExceededNrOfVars
    else (
      let next_unused_int =
        if id >= vars.next_unused_int then
          id + 1
        else
          vars.next_unused_int
          (*TODO if you remember the free sites then remove id and add everything between unused_int and id*)
      in
      {
        vars with
        var_name_to_int = StringMap.add var id vars.var_name_to_int;
        int_name_to_var = IntMap.add id var vars.int_name_to_var;
        next_unused_int;
      }
    )

  (**If the variable is new then it is added to the data structure and the function returns true.*)
  let var_to_int vars var =
    match StringMap.find_opt var vars.var_name_to_int with
    | Some i -> vars, i, false
    | None ->
      if vars.next_unused_int = vars.nr_vars then
        raise ExceededNrOfVars
      else (
        let i = vars.next_unused_int in
        ( {
          vars with
          var_name_to_int = StringMap.add var i vars.var_name_to_int;
          int_name_to_var = IntMap.add i var vars.int_name_to_var;
          next_unused_int = i + 1;
        },
          i,
          true )
      )

  let int_to_var vars id = IntMap.find id vars.int_name_to_var
end

let add_element_to_res_array ar ~length ~index ~elt ~empty =
  let rec add_element length =
    if length = index then
      Res.Array.add_one ar elt
    else (
      let _ = Res.Array.add_one ar empty in
      add_element (length + 1)
    )
  in
  add_element length

module MutableNodeMap (Elt : HasEmpty) = struct
  (*TODO remember and reuse empty index?*)
  type t = {
    simple_nodes_map: Elt.t Array.t;
    compound_nodes_map: Elt.t option Res.Array.t;
  }

  let empty size =
    {
      simple_nodes_map = Array.make size (Elt.empty ());
      compound_nodes_map = Res.Array.empty ();
    }

  let mem node map =
    match node with
    | Node.Simple i -> i < Array.length map.simple_nodes_map
    | Node.Compound i ->
      let n = Res.Array.length map.compound_nodes_map in
      if i < n then (
        match Res.Array.get map.compound_nodes_map i with
        | None -> false
        | Some _ -> true
      ) else
        false

  let add node elt map =
    match node with
    | Node.Simple i -> Array.set map.simple_nodes_map i elt
    | Node.Compound i ->
      let n = Res.Array.length map.compound_nodes_map in
      if i >= n then
        add_element_to_res_array map.compound_nodes_map ~length:n ~index:i
          ~elt:(Some elt) ~empty:None
      else
        Res.Array.set map.compound_nodes_map i (Some elt)

  let get node map =
    match node with
    | Node.Simple i -> map.simple_nodes_map.(i)
    | Node.Compound i ->
      if Res.Array.length map.compound_nodes_map <= i then
        Elt.empty ()
      else (
        match Res.Array.get map.compound_nodes_map i with
        | None -> Elt.empty ()
        | Some elt -> elt
      )

  let remove node map =
    match node with
    | Node.Simple i -> Array.set map.simple_nodes_map i (Elt.empty ())
    | Node.Compound i -> Res.Array.set map.compound_nodes_map i None

  let iter f t =
    let _ =
      Array.iteri (fun i elt -> f (Node.Simple i) elt) t.simple_nodes_map
    in
    Res.Array.iteri
      (fun i elt ->
         match elt with
         | None -> ()
         | Some elt -> f (Node.Compound i) elt)
      t.compound_nodes_map

  let map f t =
    let simple_nodes_map =
      Array.mapi (fun i elt -> f (Node.Simple i) elt) t.simple_nodes_map
    in
    let compound_nodes_map =
      Res.Array.mapi
        (fun i elt ->
           match elt with
           | None -> None
           | Some elt -> Some (f (Node.Compound i) elt))
        t.compound_nodes_map
    in
    { simple_nodes_map; compound_nodes_map }
end

type edge = Node.t * Node.t * IdSet.t
(**(lhs, rhs, set of literals in the rhs)*)

module type FullyDynamicHornClauses = sig
  type t

  val name : string
  val init : nr_vars:int -> Node.t -> t
  val add_clause : edge -> t -> t
  val add_clauses : edge list -> t -> t
  val is_reachable : Node.t -> t -> bool
  val reachable_vars : int -> t -> IdSet.t
  val weight_of : t -> Node.t -> string option
  val succs_of : t -> Node.t -> NodeSet.t
  val remove_clause : edge -> t -> t
  val open_chapter : edge list -> t -> t
  val close_chapter : t -> t
end

type test_step = 
  (** (command to add or remove an edge, list of reachable vars after the graph was modified)*)
  string * string list

type test_suite =
  (** (test name, optional coordinates for the runtime plot, nr of nodes, commands for the hypergraph creation, commands for the hypergraph modification) *)
  string * (int * int) option * int * test_step list * test_step list
