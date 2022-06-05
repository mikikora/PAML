open Format

type rel_property =
  | Seriality
  | Reflexivity
  | Symmetry
  | Transitivity
  | Euclideanness
  | Directedness

let property_to_string = function
  | Seriality -> "Seriality"
  | Reflexivity -> "Reflexivity"
  | Symmetry -> "Symmetry"
  | Transitivity -> "Transitivity"
  | Euclideanness -> "Euclideanness"
  | Directedness -> "Directedness"

type system = K | D | T | B | S4 | S5

let system_to_string = function
  | K -> "K"
  | D -> "D"
  | T -> "T"
  | B -> "B"
  | S4 -> "S4"
  | S5 -> "S5"

let system_to_properties =
  [
    (K, []);
    (D, [ Seriality ]);
    (T, [ Transitivity ]);
    (B, [ Transitivity; Symmetry ]);
    (S4, [ Reflexivity; Transitivity ]);
    (S5, [ Reflexivity; Euclideanness ]);
  ]

type relation_name = string
type relation = { name : relation_name; properties : rel_property list }

exception Error of relation_name * string
exception RelationDoesNotExist of string

let relation_map : (string, relation) Hashtbl.t = Hashtbl.create 5
let used_relations : relation_name list ref = ref []

let make_relation_unmutable name =
  if Hashtbl.mem relation_map name then
    if List.mem name !used_relations then
      used_relations := name :: !used_relations
    else ()
  else raise (RelationDoesNotExist name)

let is_relation_unmutable name = List.mem name !used_relations
let create_relation name properties = { name; properties }

let add_new_relation name properties =
  match Hashtbl.find_opt relation_map name with
  | Some r -> raise (Error (r.name, "Relation already exists"))
  | None -> Hashtbl.add relation_map name (create_relation name properties)

let create_relation_name () =
  let rec generate_name acc =
    let name = "R" ^ String.make acc '\'' in
    if Hashtbl.mem relation_map name then generate_name (acc + 1) else name
  in
  generate_name 0

let create_relation_for_system name sys =
  let name = match name with Some n -> n | None -> create_relation_name () in
  let properties = List.assoc sys system_to_properties in
  add_new_relation name properties;
  name

let get_relation = Hashtbl.find relation_map

let add_properties name properties =
  if is_relation_unmutable name then
    raise (Error (name, "Relation is used in a theorem and can't be modified"))
  else
    let r = Hashtbl.find relation_map name in
    let new_r = { name = r.name; properties = properties @ r.properties } in
    Hashtbl.replace relation_map name new_r

let remove_properties name properties =
  if is_relation_unmutable name then
    raise (Error (name, "Relation is used in a theorem and can't be modified"))
  else
    let r = Hashtbl.find relation_map name in
    let new_properties =
      List.filter (fun p -> not @@ List.mem p properties) r.properties
    in
    let new_r = { name = r.name; properties = new_properties } in
    Hashtbl.replace relation_map name new_r

let get_declared_relations () = Hashtbl.to_seq_values relation_map

let has_property property relation =
  List.mem property (get_relation relation).properties

let relation_exists = Hashtbl.mem relation_map

let is_sub_rel r1 r2 =
  if r1 = r2 then true
  else 
    let r1 = get_relation r1
    and r2 = get_relation r2 in
    List.fold_left (fun acc prop -> List.mem prop r2.properties && acc) true r1.properties

let pp_print_relation fmtr relation =
  pp_open_vbox fmtr 1;
  pp_print_string fmtr "{";
  pp_print_cut fmtr ();
  pp_print_string fmtr relation.name;
  List.iter
    (function
      | value ->
          pp_print_cut fmtr ();
          pp_print_string fmtr (property_to_string value))
    relation.properties;
  pp_close_box fmtr ();
  pp_print_newline fmtr ();
  pp_print_string fmtr "}"
