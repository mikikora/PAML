type prop =
  | P
  | Var of string
  | Con of prop * prop
  | Alt of prop * prop
  | Imp of prop * prop
  | Box of prop
  | Dia of prop

type world = string
         

type rel= string
type relation= rel* world * world


type judgement =
  | J of world * prop
  | R of relation

type assumptions = judgement List
                 
              
type theorem =
  | Hyp of assumptions * judgement
  | ConI of theorem * theorem * (assumotions * judgement)
  | ConE of theorem * (assumotions * judgement)
  | AltI of theorem * (assumptions * judgement)
  | AltE of theorem * theorem * theorem * (assumptions * judgement)
  | ImpI of theorem * (assumotions * judment)
  | ImpE of theorem * theorem * (assumotions * judgement)
  | BoxI of theorem * (assumptions * judgement)
  | BoxE of theorem * theorem * (assumptions * judgement)
  | DiaI of theorem * theorem * (assumptions * judgement)
  | DiaE of theorem * theorem * (assumptions * judgement)

let assumptions = function
  | Hyp (l, _)
  | ConI (_, _, (l, _))
  | ConE (_, (l, _))
  | AltI (_, (l, _))
  | AltE (_, _, _, (l, _))
  | ImpI (_, (l, _))
  | ImpE (_, _, (l, _))
  | BoxI (_, (l, _))
  | BoxE (_, _, (l, _, _))
  | DiaI (_, _, (l, _))
  | DiaE (_, _, (l, _))
      l

let consequence = function
  | Hyp (_, x)
  | ConI (_, _, (_, x))
  | ConE (_, (_, x))
  | AltI (_, (_, x))
  | AltE (_, _, _, (_, x))
  | ImpI (_, (_, x))
  | ImpE (_, _, (_, x))
  | BoxI (_, (_, x))
  | BoxE (_, _, (_, _, x))
  | DiaI (_, _, (_, x))
  | DiaE (_, _, (_, x))
      x

let destruct_th th = (assumtion th, consequence th)


let rec remove_duplicates lst =
  match lst with
  | [] -> []
  | hd :: tl ->
      hd :: (List.filter (function el -> el <> hd) @@ remove_duplicates tl)


let hyp ass jgmt =
  if List.exists (function a -> a = jgmt) ass then Hyp (ass, jgmt)
  else failwith "no assumption matches goal"

let coni th1 th2 =
  let ass1, jgmt1 = destruct_th th1
  and ass2, jgmt2 = destruct_th th2 in
  match jgmt1, jgmt2 with
  | (J (x, p1), J (y, p2)) -> if x = y then ConI(th1, th2, (remove_duplicates ass1 ass2, J(x, Con(p1, p2))))
                              else failwith "worlds don't match"
  | _ -> failwith "can't use coni on this judgements"

let cone1 thm =
  let ass, jgmt = destruct_th in
  match jgmt with
  | J (x, Con(a,b)) -> ConE(thm, (ass, J(x, a)))
  | _ -> failwith "can't use cone on this judgement"

let cone2 thm =
  let ass, jgmt = destruct_th in
  match jgmt with
  | J (x, Con(a,b)) -> ConE(thm, (ass, J(x, b)))
  | _ -> failwith "can't use cone on this judgement"


let alti1 thm prop =
  let ass, jgmt = destruct_th thm in
  match jgmt with
  | J (x, p) -> AltI (thm, (ass, J (x, Alt(p, prop))))
  | _ -> failwith "can't use alti on this judgement"
           
 let alti2 thm prop =
  let ass, jgmt = destruct_th thm in
  match jgmt with
  | J (x, p) -> AltI (thm, (ass, J (x, Alt(prop, p))))
  | _ -> failwith "can't use alti on this judgement"
                                         
 let alte thm1 thm2 thm3 =
   let ass1, jgmt1 = destruct_th thm1
   and ass2, jgmt2 = destruct_th thm2
   and ass3, jgmt3 = destruct_th thm3 in
   match jgmt1 with
   | J (x, Alt(p1,p2)) ->
      if List.exists (function v -> v = (J (x, p1))) ass2 &&
           List.exists (function v -> v = (J (x, p2))) ass3 &&
             jgmt2 = jgmt3 then
        let ass = List.filter (function v -> v <> (J (x,p1))) (ass2 @ ass3)
        AltE (thm1, thm2, thm3, (remove_duplicates @@ ass1 @ ass, jgmt2))
   | _ -> failwith "can't use alte on this judgement"

 let impi th left_jgmt =
   let y, prop = (match left_jgmt with
                  | J(y, prop) -> y, prop
                  | _ -> failwith "this judgement can't be used in implication") in
  let ass, jgmt = destruct_th th in
  match jgmt with
  | J (x, p) ->
      if List.exists (function v -> v = jmnt) ass && x = y then
        ImpI
          ( th,
            ( 
              List.filter (function v -> v <> jgmt) ass,
              J (x, Imp (prop, p)) ) )
      else failwith "can't use impi with this proposition here"
  | _ -> failwith "can't use impi on not true judgment"

let impe thm1 thm2 =
  let ass1, jgmt1 = destruct_th thm1
  and ass2, jgmt2 = destruct_th thm2 in
  match jgmt1 with
  | 
