open Proof_syntax
open Proof
open Relation
open Ast
open Syntax

let worlds_in_context ctx =
  List.fold_left
    (fun acc (_, jgmt) ->
      match jgmt with R (x, y) -> [ x; y ] @ acc | J (x, _) -> x :: acc)
    [] ctx

let rec remove_duplicates lst =
  match lst with
  | [] -> []
  | hd :: tl ->
      hd :: (List.filter (function el -> el <> hd) @@ remove_duplicates tl)

let rec create_fresh_world_name used_worlds acc =
  let common_names = [ "x"; "y"; "z"; "w"; "v" ] in
  let posible_names =
    List.map (fun name -> name ^ String.make acc '\'') common_names
  in
  let new_name =
    List.fold_left
      (fun acc name ->
        if acc = None then if List.mem name used_worlds then None else Some name
        else acc)
      None posible_names
  in
  match new_name with
  | None -> create_fresh_world_name used_worlds (acc + 1)
  | Some name -> name

let get_intro_hints (r, ctx, jgmt) =
  match jgmt with
  | J (_, Con (_, _)) -> [ SplitCmd ]
  | J (_, Alt (_, _)) -> [ LeftCmd; RightCmd ]
  | J (_, Imp (_, _)) -> [ IntroCmd (None, None) ]
  | J (x, Box _) ->
      let used_worlds = x :: worlds_in_context ctx in
      let world = create_fresh_world_name used_worlds 0 in
      [ IntroCmd (None, Some world) ]
  | J (x, Dia _) ->
      List.fold_left
        (fun acc (_, jgmt) ->
          match jgmt with
          | R (z, y) -> if z = x then IntroCmd (None, Some y) :: acc else acc
          | _ -> acc)
        [] ctx
  | _ -> []

let get_apply_hints (r, ctx, jgmt) =
  match jgmt with
  | J (x, prop) ->
      remove_duplicates
        (List.fold_left
           (fun acc (name, jgmt') ->
             match jgmt' with
             | J (y, F) -> ContraCmd y :: acc (* contra *)
             | J (_, Dia p1) ->
                 (* diamond *)
                 let used_worlds = x :: worlds_in_context ctx in
                 let world = create_fresh_world_name used_worlds 0 in
                 ApplyAssmCmd (None, None, Some world, name) :: acc
             | _ -> (
                 (* rest *)
                 try
                   let _ =
                     apply_assm None None None name (Empty (r, ctx, jgmt), Root)
                   in
                   ApplyAssmCmd (None, None, None, name) :: acc
                 with Error.UnlocatedError _ -> acc))
           [] ctx)
  | _ -> []

let get_property_hints (r, ctx, jgmt) =
  match jgmt with
  | J (v, _) ->
      let used_worlds = v :: worlds_in_context ctx in
      let fresh_world = create_fresh_world_name used_worlds 0 in
      let ctx_with_rel_jgmts =
        List.filter_map
          (fun (_, jgmt) ->
            match jgmt with R (_, _) -> Some jgmt | J (_, _) -> None)
          ctx
      in
      let properties = (get_relation r).properties in
      remove_duplicates
        (List.fold_left
           (fun acc property ->
             match property with
             | Seriality ->
                 List.map
                   (fun world -> SerialCmd (None, Some fresh_world, world))
                   used_worlds
                 @ acc
             | Reflexivity ->
                 List.fold_left
                   (fun acc world ->
                     if List.mem (R (world, world)) ctx_with_rel_jgmts then acc
                     else ReflCmd (None, world) :: acc)
                   acc used_worlds
             | Symmetry ->
                 List.fold_left
                   (fun acc jgmt ->
                     match jgmt with
                     | R (x, y) ->
                         if List.mem (R (y, x)) ctx_with_rel_jgmts then acc
                         else SymmCmd (None, x, y) :: acc
                     | _ -> acc)
                   acc ctx_with_rel_jgmts
             | Transitivity ->
                 List.fold_left
                   (fun acc jgmt ->
                     match jgmt with
                     | R (x, y) ->
                         List.fold_left
                           (fun acc jgmt ->
                             match jgmt with
                             | R (y', z) ->
                                 if
                                   y <> y'
                                   || List.mem (R (x, z)) ctx_with_rel_jgmts
                                 then acc
                                 else TransCmd (None, x, y, z) :: acc
                             | _ -> acc)
                           acc ctx_with_rel_jgmts
                     | _ -> acc)
                   acc ctx_with_rel_jgmts
             | Euclideanness ->
                 List.fold_left
                   (fun acc jgmt ->
                     match jgmt with
                     | R (x, y) ->
                         List.fold_left
                           (fun acc jgmt ->
                             match jgmt with
                             | R (x', z) ->
                                 if
                                   x <> x'
                                   || List.mem (R (y, z)) ctx_with_rel_jgmts
                                 then acc
                                 else EuclCmd (None, x, y, z) :: acc
                             | _ -> acc)
                           acc ctx_with_rel_jgmts
                     | _ -> acc)
                   acc ctx_with_rel_jgmts
             | Directedness ->
                 List.fold_left
                   (fun acc jgmt ->
                     match jgmt with
                     | R (x, y) ->
                         List.fold_left
                           (fun acc jgmt ->
                             match jgmt with
                             | R (x', z) ->
                                 if x <> x' then acc
                                 else
                                   DirectCmd
                                     (None, None, x, y, z, Some fresh_world)
                                   :: acc
                             | _ -> acc)
                           acc ctx_with_rel_jgmts
                     | _ -> acc)
                   acc ctx_with_rel_jgmts)
           [] properties)
  | _ -> []

let get_hints goal_desc =
  get_intro_hints goal_desc @ get_apply_hints goal_desc
  @ get_property_hints goal_desc

let hint_to_string = function
  | SplitCmd -> "split"
  | LeftCmd -> "left"
  | RightCmd -> "right"
  | IntroCmd (_, None) -> "intro"
  | IntroCmd (_, Some w) -> "intro with " ^ w
  | ContraCmd w -> "contra with " ^ w
  | ApplyAssmCmd (_, _, Some w, name) -> "apply " ^ name ^ " with " ^ w
  | ApplyAssmCmd (_, _, None, name) -> "apply " ^ name
  | ReflCmd (_, world) -> "refl with " ^ world
  | SymmCmd (_, x, y) -> "symm with " ^ x ^ ", " ^ y
  | TransCmd (_, x, y, z) -> "trans with " ^ x ^ ", " ^ y ^ ", " ^ z
  | EuclCmd (_, x, y, z) -> "eucl with " ^ x ^ ", " ^ y ^ ", " ^ z
  | DirectCmd (_, _, x, y, z, Some w) ->
      "direct with " ^ x ^ ", " ^ y ^ ", " ^ z ^ ", " ^ w
  | _ -> failwith "not a hint"

let get_hint_string goal_desc =
  String.concat "\n" (List.map hint_to_string (get_hints goal_desc))
