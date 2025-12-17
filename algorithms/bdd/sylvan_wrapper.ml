open Sylvan_bindings

type key = int
type t = int * Sylvan_bindings.mdd (* depth, mdd *)

let init () =
  let heap_nodes = 100_000_000 in
  let () = lace_start (Unsigned.UInt.of_int 0) (Unsigned.Size_t.of_int 0) in
  sylvan_set_limits (Unsigned.Size_t.of_int heap_nodes) 1 1;
  sylvan_init_package ();
  sylvan_init_ldd ();
  ()

let mvbdu_true () = 0, lddmc_true
let mvbdu_false () = 0, lddmc_false

let is_true (d, ldd) =
  (* Verify that all levels are {0,1}. TODO adapt for MDDs *)
  (* Build bottom-up *)
  let rec verify_level level ldd =
    if level = 0 then
      (0, ldd) = mvbdu_true ()
    else (
      let down = lddmc_getdown ldd in
      let value = lddmc_getvalue ldd in
      let follow1 = lddmc_follow ldd Unsigned.UInt32.one in
      Unsigned.UInt32.(equal value zero)
      && lddmc_equal down follow1
      && verify_level (level - 1) down
    )
  in
  verify_level d ldd

let is_false ldd = ldd = mvbdu_false ()

let print (d, ldd) =
  print_endline ("heigt: " ^ string_of_int d);
  lddmc_print ldd;
  print_newline ()

let all_values_level down =
  let node1 = lddmc_makenode (Unsigned.UInt32.of_int 1) down lddmc_false in
  lddmc_makenode (Unsigned.UInt32.of_int 0) down node1

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
  else (
    let (_, a), (d2, b) = normalize_depth a b in
    d2, lddmc_union a b
  )

let mvbdu_and a b =
  if is_false a || is_false b then
    mvbdu_false ()
  else (
    let (_, a), (d2, b) = normalize_depth a b in
    d2, lddmc_intersect a b
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

let mvbdu_project_abstract_away t _ = t (*TODO*)
let mvbdu_to_formula _ = Logical_formulae.True (*TODO*)
