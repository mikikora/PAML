open Syntax
open Format
open Relation

type context = (string * judgement) list
type goal_desc = relation * context * judgement

type proof =
  | Empty of goal_desc
  | Node1 of proof * (theorem -> theorem)
  | Node2 of proof * proof * (theorem -> theorem -> theorem)
  | Node3 of proof * proof * proof * (theorem -> theorem -> theorem -> theorem)
  | Leaf of theorem

type path =
  | Root
  | Left of path * proof * (theorem -> theorem -> theorem)
  | Right of path * proof * (theorem -> theorem -> theorem)
  | Mid of path * (theorem -> theorem)
  (*
     In 3 node proofs always goes: left, mid, right (without the one from the constructor)
     Sof of example in Left3 it goes: mid, right and in Right3: left, mid
  *)
  | Left3 of path * proof * proof * (theorem -> theorem -> theorem -> theorem)
  | Mid3 of path * proof * proof * (theorem -> theorem -> theorem -> theorem)
  | Right3 of path * proof * proof * (theorem -> theorem -> theorem -> theorem)

type goal = proof * path

(* For modal rules function world_in_context will be helpful *)
let world_in_context world ctx =
  List.exists
    (function
      | _, jgmt -> (
          match jgmt with
          | J (x, _) -> x = world
          | R (x, y) -> x = world || y = world))
    ctx

(* Supproting function for context *)

let add_to_ctx ?name ctx jgmt =
  match name with
  | Some n ->
      if List.exists (function str, _ -> str = n) ctx then
        failwith (n ^ " is already in the context")
      else (n, jgmt) :: ctx
  | None ->
      let rec generate_name n acc =
        let name = n ^ string_of_int acc in
        if List.exists (function str, _ -> str = name) ctx then
          generate_name n (succ acc)
        else name
      in
      (generate_name "H" 0, jgmt) :: ctx

let create_fresh_world_name ctx =
  let rec generate_name n acc =
    let name = n ^ string_of_int acc in
    if world_in_context name ctx then generate_name n (succ acc) else name
  in
  generate_name "x" 0

(* Destructors for proof type *)
let rec no_goals pf =
  match pf with
  | Empty _ -> 1
  | Leaf _ -> 0
  | Node1 (pf, _) -> no_goals pf
  | Node2 (pf1, pf2, _) -> no_goals pf1 + no_goals pf2
  | Node3 (pf1, pf2, pf3, _) -> no_goals pf1 + no_goals pf2 + no_goals pf3

let rec goals pf =
  match pf with
  | Empty x -> [ x ]
  | Leaf _ -> []
  | Node1 (pf, _) -> goals pf
  | Node2 (pf1, pf2, _) -> goals pf1 @ goals pf2
  | Node3 (pf1, pf2, pf3, _) -> goals pf1 @ goals pf2 @ goals pf3

(* Exceptions *)

(* Printers *)
let pp_print_unfocused_proof fmtr pf =
  let ngoals = no_goals pf and goals = goals pf in
  if ngoals = 0 then pp_print_string fmtr "No more subgoals."
  else pp_open_vbox fmtr (-100);
  pp_open_hbox fmtr ();
  pp_print_string fmtr "There are ";
  pp_print_int fmtr ngoals;
  pp_print_string fmtr " subgoals:";
  pp_close_box fmtr ();
  pp_print_cut fmtr ();
  List.iteri
    (fun n (r, _, f) ->
      pp_print_cut fmtr ();
      pp_open_hbox fmtr ();
      pp_print_int fmtr (n + 1);
      pp_print_string fmtr " : ";
      pp_print_judgement fmtr ~r f;
      pp_close_box fmtr ())
    goals;
  pp_close_box fmtr ()

let pp_print_current_goal fmtr = function
  | pf, path -> (
      match pf with
      | Empty (rel, ctx, jgmt) ->
          pp_open_vbox fmtr (-100);
          let print_context ctx =
            List.iter
              (function
                | name, f ->
                    pp_print_cut fmtr ();
                    pp_open_hbox fmtr ();
                    pp_print_string fmtr (name ^ ": ");
                    pp_print_judgement fmtr ~r:rel f;
                    pp_close_box fmtr ())
              ctx
          in
          print_context ctx;
          pp_print_cut fmtr ();
          pp_print_string fmtr (String.make 40 '-');
          pp_print_cut fmtr ();
          pp_print_judgement fmtr ~r:rel jgmt;
          pp_close_box fmtr ()
      | _ ->
          pp_print_string fmtr
            "This path is not a subgoal. Focus to proper subgoal")

let print_unfocused_proof = pp_print_unfocused_proof std_formatter
let print_current_goal = pp_print_current_goal std_formatter
