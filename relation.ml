type rel_properties =
  | Seriality
  | Reflexivity
  | Symmetry
  | Transitivity
  | Euclideanness
  | Directedness

type relation = { name : string; properties : rel_properties list }

exception Error of string * string
exception RelationDoesNotExist of string

let relation_map : (string, relation) Hashtbl.t = Hashtbl.create 5
let used_relations : string list ref = ref []

let make_relation_unmutable name =
  if Hashtbl.mem relation_map name then
    used_relations := name :: !used_relations
  else raise (RelationDoesNotExist name)

let is_relation_unmutable name = List.mem name !used_relations
let create_relation name properties = { name; properties }

let add_new_relation name properties =
  match Hashtbl.find_opt relation_map name with
  | Some r -> raise (Error (r.name, "Relation already exists"))
  | None -> Hashtbl.add relation_map name (create_relation name properties)

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
