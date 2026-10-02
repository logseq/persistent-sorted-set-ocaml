type 'a comparator = 'a -> 'a -> int
type ref_type = Strong | Weak
type settings = { branching_factor : int; ref_type : ref_type }
type 'a stored_node = Leaf of 'a array | Branch of 'a array * string array

type 'a storage = {
  store_node : ?address:string -> 'a stored_node -> string;
      (** Write a node and return its address. When [address] is provided the
          caller asks for the node to be stored at that existing address (the
          cljs implementation's in-place rewrite of a modified stored node). *)
  restore_node : string -> 'a stored_node option;
  accessed : string -> unit;
}

type 'a t
type 'a node
type 'a seq

val default_settings : settings
val settings : 'a t -> settings
val empty : unit -> 'a t

val empty_by :
  ?settings:settings ->
  ?storage:'a storage ->
  ?cmp:'a comparator ->
  unit ->
  'a t

val of_list : 'a list -> 'a t

val of_list_by :
  ?settings:settings ->
  ?storage:'a storage ->
  ?cmp:'a comparator ->
  'a list ->
  'a t

val of_sorted_array : 'a array -> 'a t

val of_sorted_array_by :
  ?settings:settings ->
  ?storage:'a storage ->
  ?cmp:'a comparator ->
  'a array ->
  'a t

val add : 'a -> 'a t -> 'a t
val remove : 'a -> 'a t -> 'a t
val mem : 'a -> 'a t -> bool
val count : 'a t -> int
val to_list : 'a t -> 'a list
val fold : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
val fold_list : ('acc -> 'a -> 'acc) -> 'acc -> 'a list -> 'acc
val seq : 'a t -> 'a seq
val rseq : 'a t -> 'a seq
val seq_to_list : 'a seq -> 'a list
val to_seq : 'a seq -> 'a Seq.t
val seq_reverse : 'a seq -> 'a seq
val fold_seq : ('acc -> 'a -> 'acc) -> 'acc -> 'a seq -> 'acc
val slice : ?from_:'a -> ?to_:'a -> ?cmp:'a comparator -> 'a t -> 'a list
val rslice : ?from_:'a -> ?to_:'a -> ?cmp:'a comparator -> 'a t -> 'a list
val slice_seq : ?from_:'a -> ?to_:'a -> ?cmp:'a comparator -> 'a t -> 'a seq
val rslice_seq : ?from_:'a -> ?to_:'a -> ?cmp:'a comparator -> 'a t -> 'a seq
val seek : 'a -> 'a seq -> 'a seq
val store : 'a t -> string * 'a t

val restore :
  ?count:int ->
  ?cmp:'a comparator ->
  ?settings:settings ->
  'a storage ->
  string ->
  'a t option
(** [restore ~count storage address] restores a set lazily with its known
    cardinality. [count] must be the exact number of elements in the snapshot at
    [address], before any later edits. The caller is responsible for this
    consistency: restore does not read nodes to verify it. Negative counts are
    rejected. Without [count], cardinality is computed by traversing the set
    when [count set] is called, preserving the existing restore behavior. *)

(* DEBUG vals *)
val validate_invariants : 'a t -> unit
val root_node : 'a t -> 'a node option
val show_node : ('a -> string) -> 'a node -> string
