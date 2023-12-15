val clear_unused_pages : string -> string -> unit
val get_image_shape : string -> string -> int * int

val slice_image :
  string -> int -> string -> string -> string * int * int -> unit
(** directory -> size -> prefix -> output_directory followed by a tuple of file * width * height *)

val get_black_threshold : string -> string -> int
val pwd : string -> string list
val ls : ?filter:string -> string -> string list
val rm : string -> string list -> unit
