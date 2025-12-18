open Cudd

type key = int
type t = Bdd.dt

let man_ref : Man.dt option ref = ref None
let btrue_ref : t option ref = ref None
let bfalse_ref : t option ref = ref None

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

let quit () =
  man_ref := None;
  btrue_ref := None;
  bfalse_ref := None

let variables_list_of_mvbdu m =
  let supp = Bdd.support m in
  Bdd.list_of_support supp

let print m =
  let printer fmt k = Format.fprintf fmt "%d" k in
  Bdd.print printer Format.std_formatter m;
  print_newline ()

let mvbdu_equal a b = Bdd.is_equal a b
let mvbdu_or a b = Bdd.dor a b
let mvbdu_and a b = Bdd.dand a b

let mvbdu_true () =
  let m = get_man () in
  Bdd.dtrue m

let mvbdu_false () =
  let m = get_man () in
  Bdd.dfalse m

let is_true mvbdu =
  match Bdd.inspect mvbdu with
  | Bdd.Bool true -> true
  | Bdd.Bool _ | Bdd.Ite _ -> false

let is_false mvbdu =
  match Bdd.inspect mvbdu with
  | Bdd.Bool false -> true
  | Bdd.Bool _ | Bdd.Ite _ -> false

let btrue () =
  ensure_init ();
  match !btrue_ref with
  | Some b -> b
  | None -> assert false

let mvbdu_of_association_list alist =
  ensure_init ();
  match alist with
  | [] -> mvbdu_true ()
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
    cube

let mvbdu_project_abstract_away m vars =
  if vars = [] then
    m
  else (
    let var_array = Array.of_list (List.map ithvar vars) in
    let varset = Bdd.vectorsupport var_array in
    Bdd.exist varset m
  )

let mvbdu_to_formula mvbdu =
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
