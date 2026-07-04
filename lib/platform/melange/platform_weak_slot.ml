type 'a weak_ref
type 'a t = 'a weak_ref option ref

external make_weak_ref : 'a -> 'a weak_ref = "WeakRef" [@@mel.new]

external deref_undefined : ('a weak_ref[@mel.this]) -> 'a Js.undefined = "deref"
[@@mel.send]

let create () = ref None
let set slot value = slot := Some (make_weak_ref value)

let get slot =
  Option.bind !slot (fun weak_ref ->
      Js.undefinedToOption (deref_undefined weak_ref))
