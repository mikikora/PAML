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

val add_new_relation : string -> rel_properties list -> unit
val add_properties : string -> rel_properties list -> unit
val remove_properties : string -> rel_properties list -> unit
val get_relation : string -> relation