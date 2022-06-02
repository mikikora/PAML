open Proof
open Hint
open Error
open Proof_syntax
open Ast

exception CouldNotProove

let auto cmd_to_proof_function depth (pf, path) =
  let rec auto_tactic depth (pf, path) =
    let rec unfocus_to_starting_point (res_pf, res_path) =
      if are_paths_equal res_path path then (res_pf, res_path)
      else
        match res_path with
        | Root -> failwith "No common father"
        | _ -> unfocus_to_starting_point @@ get_father (res_pf, res_path)
    in
    let rec use_auto_on_all_subgoals new_pf =
      let no_goals_start = no_goals new_pf in
      let new_pf =
        unfocus @@ auto_tactic (depth - 1) (focus 1 new_pf)
      in
      if no_goals new_pf = no_goals_start - 1 then
        if no_goals new_pf = 0 then new_pf
        else use_auto_on_all_subgoals new_pf
      else raise CouldNotProove
    in
    if depth = 0 then (pf, path)
    else
      match pf with
      | Empty (rel, ctx, jgmt) ->
          let intro_hints = get_intro_hints (rel, ctx, jgmt) in
          if intro_hints <> [] then
            (* intro phase *)
            match intro_hints with
            | [ SplitCmd ] ->
                let new_pf, new_path =
                  get_father @@ (cmd_to_proof_function SplitCmd) (pf, path)
                in
                let new_pf, new_path =
                  get_father @@ auto_tactic depth (focus 1 new_pf)
                in
                if no_goals new_pf <> 1 then (pf, path)
                else
                  let new_pf, new_path =
                    get_father @@ auto_tactic depth (focus 1 new_pf)
                  in
                  if no_goals new_pf = 0 then (new_pf, new_path) else (pf, path)
            | [ LeftCmd; RightCmd ] ->
                let left_pf, left_path =
                  (cmd_to_proof_function LeftCmd) (pf, path)
                in
                let left_pf, left_path =
                  get_father @@ auto_tactic depth (left_pf, left_path)
                in
                if no_goals left_pf = 0 then (left_pf, left_path)
                else
                  let right_pf, right_path =
                    (cmd_to_proof_function RightCmd) (pf, path)
                  in
                  let right_pf, right_path =
                    get_father @@ auto_tactic depth (right_pf, right_path)
                  in
                  if no_goals right_pf = 0 then (right_pf, right_path)
                  else (pf, path)
            | [ intro_cmd ] ->
                let new_pf, new_path =
                  (cmd_to_proof_function intro_cmd) (pf, path)
                in
                let new_pf, new_path =
                  get_father @@ auto_tactic depth (new_pf, new_path)
                in
                if no_goals new_pf = 0 then (new_pf, new_path) else (pf, path)
            | _ -> failwith "absurd"
          else
            (* apply phase *)
            let apply_hints = get_apply_hints (rel, ctx, jgmt) in
            let rec evaluate_apply_hints = function
              | [] -> (pf, path)
              | apply_cmd :: tl -> (
                  let new_th, new_path =
                    unfocus_to_starting_point
                    @@ (cmd_to_proof_function apply_cmd) (pf, path)
                  in
                  if no_goals new_th = 0 then (new_th, new_path)
                  else
                    try (use_auto_on_all_subgoals new_th, new_path)
                    with CouldNotProove -> evaluate_apply_hints tl)
            in
            evaluate_apply_hints apply_hints
      | _ -> raise (UnlocatedError "Not in empty goal")
  in
  auto_tactic depth (pf, path)
