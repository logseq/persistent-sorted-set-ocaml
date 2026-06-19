open Persistent_sorted_set

let () =
  let set =
    of_list_by ~settings:{ branching_factor = 4 } ~cmp:compare [ 3; 1; 2 ]
  in
  if to_list set <> [ 1; 2; 3 ] then failwith "js smoke sorted order failed";
  if slice ~from_:2 ~to_:3 set <> [ 2; 3 ] then failwith "js smoke slice failed";
  if settings set <> { branching_factor = 4 } then
    failwith "js smoke settings failed";
  if to_list (of_sorted_array [| 1; 1; 2; 3 |]) <> [ 1; 2; 3 ] then
    failwith "js smoke sorted array failed"
