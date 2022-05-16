type rel_properties =
  | Seriality
  | Reflexivity
  | Symmetry
  | Transitivity
  | Euclideanness
  | Directedness

val property_to_string : rel_properties -> string

type relation = { name : string; properties : rel_properties list }

exception Error of string * string
exception RelationDoesNotExist of string

val add_new_relation : string -> rel_properties list -> unit
val add_properties : string -> rel_properties list -> unit
val remove_properties : string -> rel_properties list -> unit
val get_relation : string -> relation
val get_declared_relations : unit -> relation Seq.t
val make_relation_unmutable : string -> unit
val has_property : rel_properties -> string -> bool
val pp_print_relation : Format.formatter -> relation -> unit
