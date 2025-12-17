open Kappa_mvbdu.Mvbdu_wrapper

module Edges_bdu :
  Mvbdu with type key = int and type value = int with type mvbdu = Mvbdu.mvbdu =
  Mvbdu

module Internal_mvbdu = Internalize (Edges_bdu)
include Internal_mvbdu

type t = Bool of bool | Mvbdu of mvbdu

let init () =
  let parameters =
    Remanent_parameters.get_parameters ~html_mode:false
      ~called_from:Remanent_parameters_sig.Internalised ()
  in
  if not (is_init ()) then init parameters

let print mvbdu =
  match mvbdu with
  | Bool true -> print_endline "MVBDU_TRUE"
  | Bool false -> print_endline "MVBDU_FALSE"
  | Mvbdu mvbdu ->
    let parameters =
      Remanent_parameters.get_parameters ~html_mode:false
        ~called_from:Remanent_parameters_sig.Internalised ()
    in
    print parameters mvbdu

let variables_list_of_mvbdu mvbdu =
  let variables_list = variables_list_of_mvbdu mvbdu in
  extensional_of_variables_list variables_list

let build_restriction_bdu variables_list =
  let range = List.map (fun var -> var, (Some 0, Some 1)) variables_list in
  mvbdu_of_range_list range

let build_restriction_bdu_1 mvbdu =
  let variables_list = variables_list_of_mvbdu mvbdu in
  build_restriction_bdu variables_list

let rec merge_list_no_duplicates l1 l2 =
  match l1, l2 with
  | [], l2 -> l2
  | l1, [] -> l1
  | h1 :: t1, h2 :: t2 ->
    if h1 = h2 then
      h1 :: merge_list_no_duplicates t1 t2
    else if h1 < h2 then
      h1 :: merge_list_no_duplicates t1 l2
    else
      h2 :: merge_list_no_duplicates l1 t2

let build_restriction_bdu_2 mvbdu1 mvbdu2 =
  let variables_list1 = variables_list_of_mvbdu mvbdu1 in
  let variables_list2 = variables_list_of_mvbdu mvbdu2 in
  let variables_list =
    merge_list_no_duplicates variables_list1 variables_list2
  in
  build_restriction_bdu variables_list

let mvbdu_normalize mvbdu =
  let restriction_bdu = build_restriction_bdu_1 mvbdu in
  let mvbdu = mvbdu_and mvbdu restriction_bdu in
  if equal mvbdu (mvbdu_true ()) then
    Bool true
  else if equal mvbdu (mvbdu_false ()) then
    Bool false
  else
    Mvbdu mvbdu

let mvbdu_or mvbdu1 mvbdu2 =
  match mvbdu1, mvbdu2 with
  | Bool b1, Bool b2 -> Bool (b1 || b2)
  | Bool true, _ | _, Bool true -> Bool true
  | Bool false, m | m, Bool false -> m
  | Mvbdu mvbdu1, Mvbdu mvbdu2 ->
    let or_mvbdu = mvbdu_or mvbdu1 mvbdu2 in
    let restriction_bdu = build_restriction_bdu_2 mvbdu1 mvbdu2 in
    let result = mvbdu_and or_mvbdu restriction_bdu in
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

let mvbdu_equal mvbdu1 mvbdu2 =
  match mvbdu1, mvbdu2 with
  | Bool b1, Bool b2 -> b1 = b2
  | Bool _, Mvbdu _ | Mvbdu _, Bool _ -> false
  | Mvbdu mvbdu1, Mvbdu mvbdu2 -> equal mvbdu1 mvbdu2

let mvbdu_true () = Bool true
let mvbdu_false () = Bool false

let is_true = function
  | Bool true -> true
  | Bool _ | Mvbdu _ -> false

let is_false = function
  | Bool false -> true
  | Bool _ | Mvbdu _ -> false

let mvbdu_to_formula mvbdu =
  match mvbdu with
  | Bool false -> Logical_formulae.False
  | Bool true -> Logical_formulae.True
  | Mvbdu mvbdu -> mvbdu_to_formula ~cartesian_decomposition:false mvbdu

let mvbdu_of_association_list association_list =
  let valuation_list = build_association_list association_list in
  let mvbdu_valuation = mvbdu_of_hconsed_asso valuation_list in
  mvbdu_normalize mvbdu_valuation

let mvbdu_project_abstract_away mvbdu variables_list =
  let variables_list = build_variables_list variables_list in
  match mvbdu with
  | Bool _ -> mvbdu
  | Mvbdu mvbdu_inter ->
    let mvbdu = mvbdu_project_abstract_away mvbdu_inter variables_list in
    mvbdu_normalize mvbdu

let variables_list_of_mvbdu mvbdu =
  match mvbdu with
  | Bool _ -> []
  | Mvbdu mvbdu -> variables_list_of_mvbdu mvbdu
