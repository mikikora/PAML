type rel_properties =
  | Seriality
  | Reflexivity
  | Symmetry
  | Transitivity
  | Euclideanness
  | Directedness

type relation = {
  name : string;
  seriality : bool ref;
  reflexivity : bool ref;
  symmetry : bool ref;
  transitivity : bool ref;
  euclideanness : bool ref;
  directedness : bool ref;
}

val add_new_relation : string -> rel_properties list -> unit
val add_properties : string -> rel_properties list -> unit
val remove_properties : string -> rel_properties list -> unit
val get_relation : string -> relation
