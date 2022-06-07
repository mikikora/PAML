type rel_property =
  | Seriality
  | Reflexivity
  | Symmetry
  | Transitivity
  | Euclideanness
  | Directedness

val property_to_string : rel_property -> string

type system = K | D | T | B | S4 | S5

val system_to_string : system -> string

type relation_name = string
type relation = { name : relation_name; properties : rel_property list }

val add_new_relation : relation_name -> rel_property list -> unit
val replace_relation : relation -> unit
val create_relation_for_system : relation_name option -> system -> relation_name
val add_properties : relation_name -> rel_property list -> unit
val remove_properties : relation_name -> rel_property list -> unit
val get_relation : relation_name -> relation
val get_declared_relations : unit -> relation Seq.t
val make_relation_unmutable : relation_name -> unit
val has_property : rel_property -> relation_name -> bool
val relation_exists : relation_name -> bool
val is_sub_rel : relation_name -> relation_name -> bool
val pp_print_relation : Format.formatter -> relation -> unit
