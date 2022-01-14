open Lang

let rec exp_const_rank : exp -> int =
  function
    | EFix (_, _, body) ->
        exp_const_rank body

    | EApp (_, e1, EAExp e2) ->
        exp_const_rank e1 + exp_const_rank e2

    | EApp (_, e1, EAType _) ->
        exp_const_rank e1

    | EVar _ ->
        0

    | ETuple components ->
        if components = [] then
          0
        else
          List2.sum (List.map exp_const_rank components)

    | EProj (_, _, arg) ->
        exp_const_rank arg

    | ECtor (_, _, arg) ->
      (match arg with
      | ETuple [] -> 1
      | _ -> exp_const_rank arg)

    | ECase (scrutinee, branches) ->
          + exp_const_rank scrutinee
          + List2.sum (List.map (fun (_, (_, e)) -> exp_const_rank e) branches)

    | EHole _ ->
        0

    | EAssert (e1, e2) ->
        exp_const_rank e1 + exp_const_rank e2

    | ETypeAnnotation (e, _) ->
        exp_const_rank e

let rec exp_size_rank : exp -> int =
  function
    | EFix (_, _, body) ->
        1 + exp_size_rank body

    | EApp (_, e1, EAExp e2) ->
        1 + exp_size_rank e1 + exp_size_rank e2

    | EApp (_, e1, EAType _) ->
        1 + exp_size_rank e1

    | EVar _ ->
        1

    | ETuple components ->
        (* Don't penalize units *)
        if components = [] then
          0
        else
          1 + List2.sum (List.map exp_size_rank components)

    | EProj (_, _, arg) ->
        (* "Focusing": projections don't add to the size rank *)
        exp_size_rank arg

    | ECtor (_, _, arg) ->
        1 + exp_size_rank arg

    | ECase (scrutinee, branches) ->
        1
          + exp_size_rank scrutinee
          + List2.sum (List.map (fun (_, (_, e)) -> exp_size_rank e) branches)

    | EHole _ ->
        1

    | EAssert (e1, e2) ->
        1 + exp_size_rank e1 + exp_size_rank e2

    | ETypeAnnotation (e, _) ->
        (* Do not penalize for type annotations *)
        exp_size_rank e

let exp_rank : exp -> int =
  match !Params.ranking_method with
    | Params.Size -> exp_size_rank
    | Params.SizeCostConst -> 
      fun e -> 
        int_of_float ((float_of_int (exp_const_rank e)) /. (float_of_int (exp_size_rank e)))

let rank : (hole_name * exp) list -> int =
  List.map (fun (_, e) -> exp_rank e) >> List2.sum

let sort :
 (hole_name * exp) list list -> (hole_name * exp) list list =
  List.sort
    ( fun hf1 hf2 ->
        Int.compare (rank hf1) (rank hf2)
    )

let first_recursive :
 (hole_name * exp) list list -> (hole_name * exp) list option =
  List.find_opt
    (List.map snd >> List.exists Exp.has_special_recursion)

let filter_non_recursive :
 (hole_name * exp) list list -> (hole_name * exp) list list =
  let not_exists f a = not (List.exists f a) in
  List.filter
    (List.map snd >> not_exists Exp.has_special_recursion)
