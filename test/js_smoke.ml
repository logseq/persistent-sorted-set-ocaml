open Persistent_sorted_set

let () =
  let set =
    of_list_by
      ~settings:{ default_settings with branching_factor = 4 }
      ~cmp:compare [ 3; 1; 2 ]
  in
  if to_list set <> [ 1; 2; 3 ] then failwith "js smoke sorted order failed";
  if slice ~from_:2 ~to_:3 set <> [ 2; 3 ] then failwith "js smoke slice failed";
  if settings set <> { default_settings with branching_factor = 4 } then
    failwith "js smoke settings failed";
  if to_list (of_sorted_array [| 1; 1; 2; 3 |]) <> [ 1; 2; 3 ] then
    failwith "js smoke sorted array failed";
  let memory = Hashtbl.create 8 in
  let next = ref 0 and reads = ref 0 in
  let storage =
    {
      store_node =
        (fun node ->
          incr next;
          let address = string_of_int !next in
          Hashtbl.replace memory address node;
          address);
      restore_node =
        (fun address ->
          incr reads;
          Hashtbl.find_opt memory address);
      accessed = (fun _ -> ());
    }
  in
  let root, _ = store (of_list_by ~storage [ 1; 2; 3 ]) in
  let restored = Option.get (restore ~count:3 storage root) in
  if count restored <> 3 || count restored <> 3 || !reads <> 0 then
    failwith "known js_of_ocaml count loaded nodes"
