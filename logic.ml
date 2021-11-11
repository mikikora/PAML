type formula = False | Var of string | Imp of formula * formula

let rec string_of_formula f =
  match f with
  | False -> "⊥"
  | Var s -> s
  | Imp (a, b) -> (
      match (a, b) with
      | Imp (_, _), Imp (_, _) ->
          "(" ^ string_of_formula a ^ ") -> " ^ string_of_formula b
      | Imp (_, _), _ ->
          "(" ^ string_of_formula a ^ ") -> " ^ string_of_formula b
      | _, Imp (_, _) -> string_of_formula a ^ " -> " ^ string_of_formula b
      | _, _ -> string_of_formula a ^ " -> " ^ string_of_formula b)

let pp_print_formula fmtr f = Format.pp_print_string fmtr (string_of_formula f)

type theorem =
  | Ax of formula list * formula
  | ImpI of theorem * (formula list * formula)
  | ImpE of theorem * theorem * (formula list * formula)
  | BotE of theorem * (formula list * formula)

let assumptions thm =
  match thm with
  | Ax (xs, _) | ImpI (_, (xs, _)) | ImpE (_, _, (xs, _)) | BotE (_, (xs, _)) ->
      xs

let consequence thm =
  match thm with
  | Ax (_, x) | ImpI (_, (_, x)) | ImpE (_, _, (_, x)) | BotE (_, (_, x)) -> x

let pp_print_theorem fmtr thm =
  let open Format in
  pp_open_hvbox fmtr 2;
  (match assumptions thm with
  | [] -> ()
  | f :: fs ->
      pp_print_formula fmtr f;
      fs
      |> List.iter (fun f ->
             pp_print_string fmtr ",";
             pp_print_space fmtr ();
             pp_print_formula fmtr f);
      pp_print_space fmtr ());
  pp_open_hbox fmtr ();
  pp_print_string fmtr "⊢";
  pp_print_space fmtr ();
  pp_print_formula fmtr (consequence thm);
  pp_close_box fmtr ();
  pp_close_box fmtr ()

let by_assumption f = Ax ([ f ], f)

let imp_i f thm =
  ImpI
    ( thm,
      ( List.filter (function x -> x <> f) (assumptions thm),
        Imp (f, consequence thm) ) )

let imp_e th1 th2 =
  match consequence th1 with
  | Imp (x, y) ->
      if x = consequence th2 then
        ImpE (th1, th2, (assumptions th1 @ assumptions th2, y))
      else failwith "imp_e got wring arguments"
  | _ -> failwith "imp_e got wrong arguments"

let bot_e f thm =
  if consequence thm = False then BotE (thm, (assumptions thm, f))
  else failwith "bot_e got wrong arguments"
