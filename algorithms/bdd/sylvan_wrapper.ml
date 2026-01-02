open Sylvan_bindings_library

exception InvalidRepresentation

type key = int
type t = int * Sylvan_bindings.mdd (* depth, mdd *)

let is_init = ref false

open Sylvan_bindings
open Sylvan_bindings.Bindings(Bindings_generated)

let init () =
  if not !is_init then (
    let heap_nodes = 100_000_000 in
    let () = lace_start (Unsigned.UInt.of_int 0) (Unsigned.Size_t.of_int 0) in
    sylvan_set_limits (Unsigned.Size_t.of_int heap_nodes) 1 1;
    sylvan_init_package ();
    sylvan_init_ldd ();
    is_init := true;
    ()
  )

let quit () = sylvan_quit ()
let mvbdu_true () = 0, lddmc_true
let mvbdu_false () = 0, lddmc_false

let is_true (d, ldd) =
  let () = if d <> 0 then print_endline "strange" in
  (* Verify that all levels are {0,1}. TODO adapt for MDDs *)
  (* Build bottom-up *)
  let rec verify_level level ldd =
    if level = 0 then
      lddmc_equal ldd lddmc_true
    else (
      let down = lddmc_getdown ldd in
      let value = lddmc_getvalue ldd in
      let follow1 = lddmc_follow ldd branch_1 in
      Unsigned.UInt32.equal value branch_0
      && lddmc_equal down follow1
      && verify_level (level - 1) down
    )
  in
  verify_level d ldd

let is_false (d, ldd) =
  let is_false = lddmc_equal ldd lddmc_false in
  if is_false then
    if d != 0 then
      raise InvalidRepresentation
    else
      true
  else
    false

let print (d, ldd) =
  print_endline ("heigt: " ^ string_of_int d);
  lddmc_print ldd;
  print_newline ()

let all_values_level down =
  let node1 = lddmc_makenode branch_1 down lddmc_false in
  lddmc_makenode branch_0 down node1

let normalize_depth (d1, a) (d2, b) =
  if d1 = d2 then
    (d1, a), (d2, b)
  else (
    let (d1, a), (d2, b) =
      if d1 > d2 then
        (d2, b), (d1, a)
      else
        (d1, a), (d2, b)
    in
    let rec build level down =
      if level = d2 then
        down
      else (
        let node = all_values_level down in
        build (level + 1) node
      )
    in
    (d2, build d1 a), (d2, b)
  )

let mvbdu_equal a b =
  if is_false a || is_false b then
    if is_false a && is_false b then
      true
    else
      false
  else (
    let (_, a), (_, b) = normalize_depth a b in
    (*TODO optimizations?*)
    lddmc_equal a b
  )

let mvbdu_or a b =
  if is_false a then
    b
  else if is_false b then
    a
  else if is_true a || is_true b then
    mvbdu_true () 
  else (
    let (_, a), (d2, b) = normalize_depth a b in
    d2, lddmc_union a b
  )

let mvbdu_and a b =
  if is_false a || is_false b then
    mvbdu_false () else
  if is_true a then b else 
  if is_true b then a 
  else (
    let (_, a), (d2, b) = normalize_depth a b in
    let ldd = lddmc_intersect a b in
    if lddmc_equal ldd lddmc_false then
      mvbdu_false ()
    else
      d2, ldd
  )

let variables_list_of_mvbdu (d, _) = List.init d (fun x -> x)

let mvbdu_of_association_list alist =
  (* Build a level where all values {0,1} are allowed. TODO adapt for MDDs *)
  (* Build bottom-up *)
  let rec build level vars down =
    match vars with
    | [] -> level, down
    | (v, value) :: rest when v = level ->
      (* Variable constrained to a single value *)
      let node =
        lddmc_makenode (Unsigned.UInt32.of_int value) down lddmc_false
      in
      build (level + 1) rest node
    | _ ->
      (* Variable unconstrained: allow {0,1} *)
      let node = all_values_level down in
      build (level + 1) vars node
  in
  build 0 alist lddmc_true

let mvbdu_project_abstract_away (d, ldd) vars =
  let rec build_proj level = function
    | _ when level = d -> []
    | [] -> (level, 1) :: build_proj (level + 1) []
    | v :: vars when v = level -> (level, 0) :: build_proj (level + 1) vars
    | vars -> (level, 1) :: build_proj (level + 1) vars
  in
  let proj = build_proj 0 vars in
  (*TODO add -1 at the beginning*)
  let _, proj = mvbdu_of_association_list proj in
  let result = lddmc_project ldd proj in
  if lddmc_equal result lddmc_false then
    0, result
  else
    d, result

let mvbdu_to_formula (d, ldd) =
  let rec to_formula level ldd =
    if lddmc_equal ldd lddmc_false then
      Logical_formulae.False
    else if lddmc_equal ldd lddmc_true then
      Logical_formulae.True
    else (
      let atom = Logical_formulae.P level in
      let branch_true = lddmc_follow ldd branch_1 in
      let branch_false = lddmc_follow ldd branch_0 in
      let output_true = to_formula (level - 1) branch_true in
      let output_false = to_formula (level - 1) branch_false in
      let imply_false =
        Logical_formulae.IMPLY (Logical_formulae.NOT atom, output_false)
      in
      let imply_true = Logical_formulae.IMPLY (atom, output_true) in
      Logical_formulae.AND (imply_false, imply_true)
    )
  in
  Logical_formulae.simplify (to_formula d ldd)
