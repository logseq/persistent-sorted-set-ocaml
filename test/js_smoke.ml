open Persistent_sorted_set

let () =
  let set = of_list [ 3; 1; 2 ] in
  if to_list set <> [ 1; 2; 3 ] then failwith "js smoke sorted order failed";
  if slice ~from_:2 ~to_:3 set <> [ 2; 3 ] then failwith "js smoke slice failed"
