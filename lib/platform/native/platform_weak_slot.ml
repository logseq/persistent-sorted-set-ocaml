type 'a t = 'a Weak.t

let create () = Weak.create 1
let set slot value = Weak.set slot 0 (Some value)
let get slot = Weak.get slot 0
