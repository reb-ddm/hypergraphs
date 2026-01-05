open Kappa_mvbdu.Mvbdu_wrapper

module Edges_bdu :
  Mvbdu with type key = int and type value = int with type mvbdu = Mvbdu.mvbdu =
  Mvbdu

module Internal_mvbdu = Internalize (Edges_bdu)
include Internal_mvbdu

type t = mvbdu

let init () =
  let parameters =
    Remanent_parameters.get_parameters ~html_mode:false
      ~called_from:Remanent_parameters_sig.Internalised ()
  in
  if not (is_init ()) then init parameters

let quit () = ()

let print mvbdu =
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
  mvbdu_and mvbdu restriction_bdu 

let mvbdu_or mvbdu1 mvbdu2 =
  let or_mvbdu = mvbdu_or mvbdu1 mvbdu2 in
  let restriction_bdu = build_restriction_bdu_2 mvbdu1 mvbdu2 in
  mvbdu_and or_mvbdu restriction_bdu

let mvbdu_and m1 m2 =
  mvbdu_and m1 m2

let mvbdu_equal mvbdu1 mvbdu2 =
  equal mvbdu1 mvbdu2

let mvbdu_true () = mvbdu_true ()
let mvbdu_false () = mvbdu_false ()

let is_true mvbdu = mvbdu_equal mvbdu (mvbdu_true())

let is_false mvbdu = mvbdu_equal mvbdu (mvbdu_false())

let mvbdu_to_formula mvbdu =
  mvbdu_to_formula ~cartesian_decomposition:false mvbdu

let mvbdu_of_association_list association_list =
  let valuation_list = build_association_list association_list in
  let mvbdu_valuation = mvbdu_of_hconsed_asso valuation_list in
  mvbdu_normalize mvbdu_valuation

let mvbdu_project_abstract_away mvbdu variables_list =
  let variables_list = build_variables_list variables_list in
  let mvbdu = mvbdu_project_abstract_away mvbdu variables_list in
  mvbdu_normalize mvbdu

let variables_list_of_mvbdu mvbdu =
  variables_list_of_mvbdu mvbdu
