open Ast

type relation = {
  name : string;
  seriality : ref bool;
  reflexivity : ref bool;
  symmetry : ref bool;
  transitivity : ref bool;
  euclideanness : ref bool;
  directedness : ref bool;
}

exception RelationExists of relation

let relation_map : (string, relation) Hashtbl.t = Hashtbl.create 5

let set_properties r set_value properties =
  List.iter
    (function
      | property -> (
          match property with
          | Seriality -> r.seriality := set_value
          | Reflexivity -> r.reflexivity := set_value
          | Symmetry -> r.symmetry := set_value
          | Transitivity -> r.transitivity := set_value
          | Euclideanness -> r.euclideanness := set_value
          | Directedness -> r.directedness := set_value))
    properties;
  r

let create_relation name : relation =
  let r =
    {
      name;
      seriality = ref false;
      reflexivity = ref false;
      symmetry = ref false;
      transitivity = ref false;
      euclideanness = ref false;
      directedness = ref false;
    }
  in
  set_properties r true

let add_new_relation name properties =
  match Hashtbl.find_opt relation_map name with
  | Some r -> raise (RelationExists r)
  | None -> Hashtbl.add relation_map name (create_relation name properties)

let get_relation = Hashtbl.find relation_map

let add_properties name properties =
  let r = Hashtbl.find relation_map name in
  let r = set_properties r true properties in
  Hashtbl.replace relation_map name r

let remove_properties name properties =
  let r = Hashtbl.find relation_map name in
  let r = set_properties r false properties in
  Hashtbl.replace relation_map name r
