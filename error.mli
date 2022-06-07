type location = { loc_file : string; loc_char : int; loc_line : int }
type 'a located = { v : 'a; l : location }

exception Error of string located
exception UnlocatedError of string

val report_error : string located -> unit
