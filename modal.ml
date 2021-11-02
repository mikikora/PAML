type proposition =
  | P
  | Imp of proposition * proposition
  | Box of proposition
  | Dia of proposition

let rec string_of_prop p =
  match p with
  | P -> "P"
  | Imp (a, b) -> (
      match (a, b) with
      | Imp (_, _), Imp (_, _) ->
          "(" ^ string_of_prop a ^ ") -> " ^ string_of_prop b
      | Imp (_, _), _ -> "(" ^ string_of_prop a ^ ") -> " ^ string_of_prop b
      | _, Imp (_, _) -> string_of_prop a ^ " -> " ^ string_of_prop b
      | _, _ -> string_of_prop a ^ " -> " ^ string_of_prop b)
  | Box p1 -> "□" ^ string_of_prop p1
  | Dia p1 -> "◇" ^ string_of_prop p1

let pp_print_prop fmtr f = Format.pp_print_string fmtr (string_of_prop f)

type proof_term =
  | Varx of string
  | Abs of string * proposition * proof_term
  | App of proof_term * proof_term
  | Boxp of proof_term
  | LetBox of string * proof_term * proof_term
  | Diap of proof_expr

and proof_expr =
  | I of proof_term
  | LetDiae of string * proof_term * proof_expr
  | LetBoxe of string * proof_term * proof_expr

type context = C of (string * proposition) list

type tru_jmnt = TJ of context * context * proof_term * proposition

type pos_jmnt = PJ of context * context * proof_expr * proposition

(*let true_context_t (tjmnt : tru_jmnt) =
    match tjmnt with
    | TJ(_, c, _, _) -> c

  let valid_context_t (tjmnt : tru_jmnt) =
    match tjmnt with
    | TJ(c, _, _, _) -> c

  let true_context_p (pjmnt : pos_jmnt) =
    match pjmnt with
    | PJ(_, c, _, _) -> c

  let valid_context_p (pjmnt : pos_jmnt) =
    match pjmnt with
    | PJ(c, _, _, _) -> c
*)

let rec pp_print_proof_term fmtr pt =
  let open Format in
  pp_open_hvbox fmtr 2;
  match pt with
  | Varx x -> pp_print_string fmtr x
  | Abs (x, a, m) ->
      pp_print_string fmtr ("λ" ^ x ^ ":");
      pp_print_prop fmtr a;
      pp_print_string fmtr ".";
      pp_print_proof_term fmtr m
  | App (m1, m2) ->
      pp_print_proof_term fmtr m1;
      pp_print_proof_term fmtr m2
  | Boxp m ->
      pp_print_string fmtr "box ";
      pp_print_proof_term fmtr m
  | LetBox (x, m1, m2) ->
      pp_print_string fmtr ("let box " ^ x ^ " =");
      pp_print_proof_term fmtr m1;
      pp_print_string fmtr " in ";
      pp_print_proof_term fmtr m2
  | Diap e ->
      pp_print_string fmtr "dia ";
      pp_print_proof_expression fmtr e

and pp_print_proof_expression fmtr e =
  let open Format in
  pp_open_hvbox fmtr 2;
  match e with
  | I m -> pp_print_proof_term fmtr m
  | LetDiae (x, m, e) ->
      pp_print_string fmtr ("let dia " ^ x ^ " = ");
      pp_print_proof_term fmtr m;
      pp_print_string fmtr " in ";
      pp_print_proof_expression fmtr e
  | LetBoxe (x, m, e) ->
      pp_print_string fmtr ("let box " ^ x ^ " = ");
      pp_print_proof_term fmtr m;
      pp_print_string fmtr " in ";
      pp_print_proof_expression fmtr e

let pp_print_tru_jmnt fmtr tjmnt =
  let (TJ (vc, tc, pt, prop)) = tjmnt in
  let open Format in
  pp_open_hvbox fmtr 2;
  let rec print_context ctx dots =
    match ctx with
    | C [] -> pp_print_string fmtr "•"
    | C ((v, p) :: tl) ->
        pp_print_string fmtr (v ^ dots);
        pp_print_prop fmtr p;
        pp_print_string fmtr ", ";
        print_context (C tl) dots
  in
  print_context vc "::";
  pp_print_string fmtr "; ";
  print_context tc ":";
  pp_print_string fmtr "⊢";
  pp_print_proof_term fmtr pt;
  pp_print_string fmtr " : ";
  pp_print_prop fmtr prop

let pp_print_pos_jmnt fmtr pjmnt =
  let (PJ (vc, tc, pe, prop)) = pjmnt in
  let open Format in
  pp_open_hvbox fmtr 2;
  let rec print_context ctx dots =
    match ctx with
    | C [] -> pp_print_string fmtr "•"
    | C ((v, p) :: tl) ->
        pp_print_string fmtr (v ^ dots);
        pp_print_prop fmtr p;
        pp_print_string fmtr ", ";
        print_context (C tl) dots
  in
  print_context vc "::";
  pp_print_string fmtr "; ";
  print_context tc ":";
  pp_print_string fmtr "⊢";
  pp_print_proof_expression fmtr pe;
  pp_print_string fmtr " ÷ ";
  pp_print_prop fmtr prop

type theorem =
  | Hyp of tru_jmnt
  | ImpI of tru_jmnt * tru_jmnt
  | ImpE of tru_jmnt * tru_jmnt * tru_jmnt
  | HypS of tru_jmnt
  | BoxI of tru_jmnt * tru_jmnt
  | BoxE of tru_jmnt * tru_jmnt * tru_jmnt
  | BoxEp of tru_jmnt * pos_jmnt * pos_jmnt
  | DiaI of pos_jmnt * tru_jmnt
  | DiaE of tru_jmnt * pos_jmnt * pos_jmnt

let hyp jmnt =
  let (TJ (vc, C tc, m, a)) = jmnt in
  let x = match m with Varx x -> x | _ -> failwith "invalid judgment" in
  if List.exists (function v, a1 -> v = x && a1 = a) tc then Hyp jmnt
  else failwith "invalid judgnemnt"
