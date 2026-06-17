type 'a comparator = 'a -> 'a -> int

type 'a stored_node =
  | Values of 'a list
  | Leaf of 'a list
  | Branch of 'a list * string list

type 'a storage =
  { store_node : 'a stored_node -> string
  ; restore_node : string -> 'a stored_node option
  ; accessed : string -> unit
  }

type 'a t
type 'a seq

val empty : unit -> 'a t
val empty_by : 'a comparator -> 'a t
val of_list : 'a list -> 'a t
val of_list_by : 'a comparator -> 'a list -> 'a t
val add : ?cmp:'a comparator -> 'a -> 'a t -> 'a t
val remove : ?cmp:'a comparator -> 'a -> 'a t -> 'a t
val mem : ?cmp:'a comparator -> 'a -> 'a t -> bool
val count : 'a t -> int
val to_list : 'a t -> 'a list
val fold : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
val fold_list : ('acc -> 'a -> 'acc) -> 'acc -> 'a list -> 'acc
val seq : 'a t -> 'a seq
val rseq : 'a t -> 'a seq
val seq_to_list : 'a seq -> 'a list
val seq_reverse : 'a seq -> 'a seq
val fold_seq : ('acc -> 'a -> 'acc) -> 'acc -> 'a seq -> 'acc
val slice : ?from_:'a -> ?to_:'a -> ?cmp:'a comparator -> 'a t -> 'a list
val rslice : ?from_:'a -> ?to_:'a -> ?cmp:'a comparator -> 'a t -> 'a list
val slice_seq : ?from_:'a -> ?to_:'a -> ?cmp:'a comparator -> 'a t -> 'a seq
val rslice_seq : ?from_:'a -> ?to_:'a -> ?cmp:'a comparator -> 'a t -> 'a seq
val seek : ?cmp:'a comparator -> 'a -> 'a seq -> 'a seq
val store : 'a storage -> 'a t -> string * 'a t
val restore : cmp:'a comparator -> 'a storage -> string -> 'a t option
val walk_addresses : 'a t -> string list
