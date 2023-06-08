val read_file : string -> string
val prepare_data : string -> string list
val take : int -> 'a list -> 'a list
val sum : int list -> int
val list_min : int list -> int
val split_chars : string -> char list

val find_in_array : 'a array -> 'a -> int
(** Recherche une valeur dans un tableau, renvoit -1 si elle n'existe pas ou son idex sinon *)

val time : ('a -> 'b) -> 'a -> ('b * float)