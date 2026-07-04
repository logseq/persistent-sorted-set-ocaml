module PSet = Persistent_sorted_set

let failf fmt = Printf.ksprintf failwith fmt

let require_equal_list label actual expected =
  if actual <> expected then
    failf "%s: expected %s, got %s" label
      (String.concat "," (List.map string_of_int expected))
      (String.concat "," (List.map string_of_int actual))

let () =
  let memory = Hashtbl.create 8 in
  let next_id = ref 0 in
  let storage =
    {
      PSet.store_node =
        (fun node ->
          incr next_id;
          let address = "node-" ^ string_of_int !next_id in
          Hashtbl.replace memory address node;
          address);
      restore_node = (fun address -> Hashtbl.find_opt memory address);
      accessed = (fun _address -> ());
    }
  in
  let set = PSet.of_list_by ~storage [ 3; 1; 2 ] in
  let root, _stored = PSet.store set in
  match PSet.restore storage root with
  | None -> failwith "expected stored set to restore"
  | Some restored ->
      require_equal_list "restored Melange stored set"
        (PSet.to_list restored) [ 1; 2; 3 ]
