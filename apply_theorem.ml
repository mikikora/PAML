open Syntax
open Proof_syntax
open Proof
open Error

(* FOr debbuging *)
let print_assignments lst =
  if lst = [] then print_string "Empty\n"
  else
    List.iter
      (function
        | name, prop ->
            Format.print_string name;
            Format.print_string " - ";
            print_prop prop;
            Format.print_string "; ")
      lst;
  Format.print_string "\n";
  Format.print_flush ()

(* Try match appling theorem *)
let match_jgmt_to_goal jgmt_to_convert jgmt =
  let is_proper_assignment lst =
    let () = print_assignments lst in
    let rec aux lst =
      match lst with
      | [] -> true
      | (name, binding) :: tl ->
          let antoher_binding = List.assoc_opt name tl in
          (antoher_binding = None || antoher_binding = Some binding) && aux tl
    in
    (not @@ List.mem ("", F) lst) && aux lst
  in
  let rec match_props prop1 prop2 =
    match (prop1, prop2) with
    | F, F -> []
    | Var x, prop -> [ (x, prop) ]
    | Con (p1, p2), Con (p1', p2') | Alt (p1, p2), Alt (p1', p2') ->
        match_props p1 p1' @ match_props p2 p2'
    | Box p1, Box p2 | Dia p1, Dia p2 -> match_props p1 p2
    | Imp _, _ ->
        let rec match_imp_props prop1 prop2 =
          match (prop1, prop2) with
          | Imp (p1, p2), Imp (p1', p2') ->
              let assignment = match_props p1 p1' @ match_props p2 p2' in
              if is_proper_assignment assignment then assignment
              else match_imp_props p2 prop2
          | Imp (p1, p2), _ -> match_props p2 prop2
          | _ -> match_props prop1 prop2
        in
        match_imp_props prop1 prop2
    | _ -> [ ("", F) ]
  in
  match (jgmt_to_convert, jgmt) with
  | J (x, prop1), J (y, prop2) ->
      let converted_prop_assignment = match_props prop1 prop2 in
      if is_proper_assignment converted_prop_assignment then
        (x, Var y) :: converted_prop_assignment
      else []
  | _ -> failwith "judgements in theorems are not J type"

let create_new_var_name name assignments =
  let rec aux acc =
    let new_name = name ^ String.make acc '\'' in
    if List.exists (function _, var -> var = Var new_name) assignments then
      aux @@ succ acc
    else new_name
  in
  aux 0

let rec convert_prop assignments = function
  | F -> (F, assignments)
  | Var str -> (
      match List.assoc_opt str assignments with
      | Some p -> (p, assignments)
      | None ->
          let new_name = create_new_var_name str assignments in
          (Var new_name, (str, Var new_name) :: assignments))
  | Con (p1, p2) ->
      let new_p1, assignments = convert_prop assignments p1 in
      let new_p2, assignments = convert_prop assignments p2 in
      (Con (new_p1, new_p2), assignments)
  | Alt (p1, p2) ->
      let new_p1, assignments = convert_prop assignments p1 in
      let new_p2, assignments = convert_prop assignments p2 in
      (Alt (new_p1, new_p2), assignments)
  | Imp (p1, p2) ->
      let new_p1, assignments = convert_prop assignments p1 in
      let new_p2, assignments = convert_prop assignments p2 in
      (Imp (new_p1, new_p2), assignments)
  | Box p ->
      let new_p, assignments = convert_prop assignments p in
      (Box new_p, assignments)
  | Dia p ->
      let new_p, assignments = convert_prop assignments p in
      (Dia new_p, assignments)

let convert_judgement assignments = function
  | J (x, prop) ->
      let world =
        match List.assoc_opt x assignments with
        | Some (Var w) -> w
        | _ -> raise (UnlocatedError ("No assignment for world " ^ x))
      in
      let converted_prop, assignments = convert_prop assignments prop in
      (J (world, converted_prop), assignments)
  | R (x, y) -> (
      match (List.assoc_opt x assignments, List.assoc_opt y assignments) with
      | Some (Var x'), Some (Var y') -> R (x', y'), assignments
      | Some _, None -> raise (UnlocatedError ("No assignment for world " ^ y))
      | _, _ -> raise (UnlocatedError ("No assignment for world " ^ x)))

let create_fresh_world_name world ass ctx assignments =
  let world_in_ass world ass =
    List.exists
      (function
        | jgmt -> (
            match jgmt with
            | J (x, _) -> x = world
            | R (x, y) -> x = world || y = world))
      ass
  in
  let world_in_ass_or_ctx world =
    world_in_ass world ass || world_in_context world ctx
  in
  let rec generate_name n acc =
    let name = n ^ String.make acc '\'' in
    if world_in_ass_or_ctx name then generate_name n (succ acc) else name
  in
  let name = generate_name world 0 in
  (world, Var name) :: assignments

let rec convert_theorem rel assignments ctx th =
  let world_in_assignments world assignments ass =
    if List.assoc_opt world assignments = None then
      create_fresh_world_name world ass ctx assignments
    else assignments
  in
  let convert_theorem_context assignments ass jgmt =
    match jgmt with
    | J (x, _) ->
        let assignments = world_in_assignments x assignments ass in
        let new_jgmt, assignments = convert_judgement assignments jgmt in
        let new_ass, assignments =
          List.fold_right
            (fun elem (lst, assignments) ->
              let new_jgmt, assignments = convert_judgement assignments elem in
              (new_jgmt :: lst, assignments))
            ass ([], assignments)
        in
        ((rel, new_ass, new_jgmt), assignments)
    | R (x, y) ->
        let assignments = world_in_assignments x assignments ass in
        let assignments = world_in_assignments y assignments ass in
        let new_jgmt, assignments = convert_judgement assignments jgmt in
        let new_ass, assignments =
          List.fold_right
            (fun elem (lst, assignments) ->
              let new_jgmt, assignments = convert_judgement assignments elem in
              (new_jgmt :: lst, assignments))
            ass ([], assignments)
        in
        ((rel, new_ass, new_jgmt), assignments)
  in

  match th with
  | Assumption (rule, (_, ass, jgmt)) ->
      let th_ass, _ = convert_theorem_context assignments ass jgmt in
      Assumption (rule, th_ass)
  | Single (rule, th1, (_, ass, jgmt)) ->
      let th_ass, assignments = convert_theorem_context assignments ass jgmt in
      let new_th1 = convert_theorem rel assignments ctx th1 in
      Single (rule, new_th1, th_ass)
  | Double (rule, th1, th2, (_, ass, jgmt)) ->
      let th_ass, assignments = convert_theorem_context assignments ass jgmt in
      let new_th1 = convert_theorem rel assignments ctx th1
      and new_th2 = convert_theorem rel assignments ctx th2 in
      Double (rule, new_th1, new_th2, th_ass)
  | Triple (rule, th1, th2, th3, (_, ass, jgmt)) ->
      let th_ass, assignments = convert_theorem_context assignments ass jgmt in
      let new_th1 = convert_theorem rel assignments ctx th1
      and new_th2 = convert_theorem rel assignments ctx th2
      and new_th3 = convert_theorem rel assignments ctx th3 in
      Triple (rule, new_th1, new_th2, new_th3, th_ass)

let apply_th name1 name2 world th assignments = function
  | pf, path -> (
      match pf with
      | Empty (rel, ctx, jgmt) ->
          if not (Relation.is_sub_rel (relation th) rel) then
            raise (UnlocatedError "Theorems use different relations")
          else
            let jgmt_to_covert = consequence th in
            let full_assignments =
              assignments @ match_jgmt_to_goal jgmt_to_covert jgmt
            in
            if full_assignments = [] then
              raise
                (UnlocatedError "couldn't find proper match for current goal")
            else
              let () = print_assignments full_assignments in
              let jgmt_to_apply, full_assignments =
                convert_judgement full_assignments jgmt_to_covert
              in
              let _, new_path =
                apply name1 name2 world jgmt_to_apply (pf, path)
              in
              let converted_theorem =
                convert_theorem rel full_assignments ctx th
              in
              (Leaf converted_theorem, new_path)
      | _ -> raise (UnlocatedError "Not in empty goal"))
