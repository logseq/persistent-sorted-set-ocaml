open Persistent_sorted_set

type config = {
  warmup_ms : float;
  sample_ms : float;
  samples : int;
  names : string list;
}

let default_names =
  [
    "empty-1M";
    "settings-10K";
    "of-list-50K";
    "of-list-by-desc-50K";
    "of-sorted-array-50K";
    "conj-10K";
    "transient-create-300K";
    "transient-add-10K";
    "transient-single-add-50K";
    "transient-leaf-add-1K";
    "disj-10K";
    "transient-remove-10K";
    "transient-single-remove-50K";
    "contains-10K";
    "count-300K";
    "to-list-300K";
    "doseq-300K";
    "next-300K";
    "reduce-300K";
    "fold-list-300K";
    "seq-to-list-300K";
    "rseq-to-list-300K";
    "to-seq-300K";
    "seq-reverse-300K";
    "fold-seq-300K";
    "slice-300K";
    "rslice-300K";
    "slice-seq-300K";
    "rslice-seq-300K";
    "seek-300K";
    "store-10K";
    "restore-10K";
    "restored-contains-10K";
    "stored-ref-repeat-contains-10K";
    "restored-to-list-10K";
    "restored-wide-rslice-1";
    "restored-chained-add-10K";
    "stored-underfull-remove-4K";
    "stored-nested-underfull-remove-4K";
  ]

let default_config =
  { warmup_ms = 2000.; sample_ms = 1000.; samples = 5; names = default_names }

let parse_args () =
  let config = ref { default_config with names = [] } in
  let add_name name =
    config := { !config with names = !config.names @ [ name ] }
  in
  let rec loop = function
    | [] ->
        if !config.names = [] then { !config with names = default_names }
        else !config
    | "--warmup-ms" :: value :: rest ->
        config := { !config with warmup_ms = float_of_string value };
        loop rest
    | "--sample-ms" :: value :: rest ->
        config := { !config with sample_ms = float_of_string value };
        loop rest
    | "--samples" :: value :: rest ->
        config := { !config with samples = int_of_string value };
        loop rest
    | name :: rest ->
        add_name name;
        loop rest
  in
  Sys.argv |> Array.to_list |> List.tl |> loop

let now_ms () = Unix.gettimeofday () *. 1000.

let median values =
  let sorted = List.sort Float.compare values in
  List.nth sorted (List.length sorted / 2)

let format_ms value =
  if value > 1. then Printf.sprintf "%.1f" value
  else if value > 0.01 then Printf.sprintf "%.3f" value
  else Printf.sprintf "%.7f" value

let shuffled_array size =
  let values = Array.init size Fun.id in
  let seed = ref 0x5eed in
  for i = size - 1 downto 1 do
    seed := ((!seed * 1_103_515_245) + 12_345) land 0x3fffffff;
    let j = !seed mod (i + 1) in
    let value = values.(i) in
    values.(i) <- values.(j);
    values.(j) <- value
  done;
  values

let ints_10k = shuffled_array 10_000
let ints_50k = shuffled_array 50_000
let ints_50k_list = Array.to_list ints_50k
let sorted_50k = Array.init 50_000 Fun.id
let set_10k = lazy (of_sorted_array (Array.init 10_000 Fun.id))
let set_50k = lazy (of_sorted_array sorted_50k)
let set_300k = lazy (of_sorted_array (Array.init 300_000 Fun.id))
let list_300k = lazy (Array.to_list (Array.init 300_000 Fun.id))
let blackhole = ref 0
let consume_int value = blackhole := (!blackhole + value) land 0x3fffffff

let build_storage () =
  let memory = Hashtbl.create 1024 in
  let next_address = ref 0 in
  {
    store_node =
      (fun node ->
        incr next_address;
        let address = "node-" ^ string_of_int !next_address in
        Hashtbl.replace memory address node;
        address);
    restore_node = (fun address -> Hashtbl.find_opt memory address);
    accessed = (fun _ -> ());
  }

let restored_10k =
  lazy
    (let storage = build_storage () in
     let root, _ =
       store
         (of_sorted_array_by ~storage ~cmp:compare (Array.init 10_000 Fun.id))
     in
     (root, storage))

let restored_wide_4096 =
  lazy
    (let memory = Hashtbl.create 4097 in
     let keys = Array.to_list (Array.init 4_096 Fun.id) in
     let child_addresses =
       List.map
         (fun value ->
           let address = "leaf-" ^ string_of_int value in
           Hashtbl.add memory address (Leaf [ value ]);
           address)
         keys
     in
     Hashtbl.add memory "root" (Branch (keys, child_addresses));
     let storage =
       {
         store_node =
           (fun _ -> invalid_arg "wide benchmark storage is read-only");
         restore_node = (fun address -> Hashtbl.find_opt memory address);
         accessed = (fun _ -> ());
       }
     in
     storage)

let stored_underfull_4k =
  lazy
    (let storage = build_storage () in
     let settings = { branching_factor = 64 } in
     let _, stored =
       store
         (of_sorted_array_by ~storage ~settings ~cmp:compare
            (Array.init 4_096 Fun.id))
     in
     stored)

let stored_nested_underfull_4k =
  lazy
    (let storage = build_storage () in
     let settings = { branching_factor = 8 } in
     let _, stored =
       store
         (of_sorted_array_by ~storage ~settings ~cmp:compare
            (Array.init 4_096 Fun.id))
     in
     stored)

let bench_empty_1m () =
  let total = ref 0 in
  for _ = 1 to 1_000_000 do
    total := !total + count (empty ())
  done;
  consume_int !total

let bench_settings_10k () =
  Lazy.force set_10k |> settings |> fun settings ->
  consume_int settings.branching_factor

let bench_of_list_50k () = of_list ints_50k_list |> count |> consume_int

let bench_of_list_by_desc_50k () =
  of_list_by ~cmp:(fun left right -> compare right left) ints_50k_list
  |> count |> consume_int

let bench_of_sorted_array_50k () =
  of_sorted_array sorted_50k |> count |> consume_int

let bench_conj_10k () =
  let set = ref (empty ()) in
  for i = 0 to Array.length ints_10k - 1 do
    set := add ints_10k.(i) !set
  done;
  consume_int (count !set)

let bench_transient_create_300k () =
  let builder = transient (Lazy.force set_300k) in
  ignore (Sys.opaque_identity builder);
  consume_int 1

let bench_transient_add_10k () =
  let builder = transient (empty ()) in
  for i = 0 to Array.length ints_10k - 1 do
    add_transient ints_10k.(i) builder
  done;
  builder |> persistent |> count |> consume_int

let bench_transient_single_add_50k () =
  let builder = transient (Lazy.force set_50k) in
  add_transient 50_000 builder;
  builder |> persistent |> count |> consume_int

let bench_transient_leaf_add_1k () =
  let settings = { branching_factor = 4_096 } in
  let original =
    of_sorted_array_by ~settings ~cmp:compare (Array.init 2_048 Fun.id)
  in
  let builder = transient original in
  for value = 2_048 to 3_071 do
    add_transient value builder
  done;
  builder |> persistent |> count |> consume_int

let bench_disj_10k () =
  let set = ref (Lazy.force set_10k) in
  for i = 0 to Array.length ints_10k - 1 do
    set := remove ints_10k.(i) !set
  done;
  consume_int (count !set)

let bench_transient_remove_10k () =
  let builder = transient (Lazy.force set_10k) in
  for i = 0 to Array.length ints_10k - 1 do
    remove_transient ints_10k.(i) builder
  done;
  builder |> persistent |> count |> consume_int

let bench_transient_single_remove_50k () =
  let builder = transient (Lazy.force set_50k) in
  remove_transient 25_000 builder;
  builder |> persistent |> count |> consume_int

let bench_contains_10k () =
  let set = Lazy.force set_10k in
  let found = ref 0 in
  for i = 0 to Array.length ints_10k - 1 do
    if mem ints_10k.(i) set then incr found
  done;
  consume_int !found

let bench_count_300k () = Lazy.force set_300k |> count |> consume_int

let bench_to_list_300k () =
  Lazy.force set_300k |> to_list |> List.length |> consume_int

let bench_doseq_300k () =
  Lazy.force set_300k |> fold (fun () value -> consume_int value) ()

let bench_next_300k () =
  Lazy.force set_300k |> fold (fun sum value -> sum + value) 0 |> consume_int

let bench_reduce_300k () =
  Lazy.force set_300k |> fold (fun sum value -> sum + value) 0 |> consume_int

let bench_fold_list_300k () =
  Lazy.force list_300k
  |> fold_list (fun sum value -> sum + value) 0
  |> consume_int

let bench_seq_to_list_300k () =
  Lazy.force set_300k |> seq |> seq_to_list |> List.length |> consume_int

let bench_rseq_to_list_300k () =
  Lazy.force set_300k |> rseq |> seq_to_list |> List.length |> consume_int

let bench_to_seq_300k () =
  Lazy.force set_300k |> seq |> to_seq
  |> Seq.fold_left (fun sum value -> sum + value) 0
  |> consume_int

let bench_seq_reverse_300k () =
  Lazy.force set_300k |> seq |> seq_reverse |> seq_to_list |> List.length
  |> consume_int

let bench_fold_seq_300k () =
  Lazy.force set_300k |> seq
  |> fold_seq (fun sum value -> sum + value) 0
  |> consume_int

let bench_slice_300k () =
  Lazy.force set_300k
  |> slice ~from_:100_000 ~to_:199_999
  |> List.length |> consume_int

let bench_rslice_300k () =
  Lazy.force set_300k
  |> rslice ~from_:199_999 ~to_:100_000
  |> List.length |> consume_int

let bench_slice_seq_300k () =
  Lazy.force set_300k
  |> slice_seq ~from_:100_000 ~to_:199_999
  |> seq_to_list |> List.length |> consume_int

let bench_rslice_seq_300k () =
  Lazy.force set_300k
  |> rslice_seq ~from_:199_999 ~to_:100_000
  |> seq_to_list |> List.length |> consume_int

let bench_seek_300k () =
  Lazy.force set_300k |> seq |> seek 150_000 |> to_seq
  |> Seq.fold_left (fun count _ -> count + 1) 0
  |> consume_int

let bench_store_10k () =
  let storage = build_storage () in
  let set =
    of_sorted_array_by ~storage ~cmp:compare (Array.init 10_000 Fun.id)
  in
  set |> store |> fst |> String.length |> consume_int

let bench_restore_10k () =
  let restored_10k_root, restored_10k_storage = Lazy.force restored_10k in
  match restore ~cmp:compare restored_10k_storage restored_10k_root with
  | None -> invalid_arg "restored benchmark root missing"
  | Some restored ->
      settings restored |> fun settings -> consume_int settings.branching_factor

let bench_restored_contains_10k () =
  let restored_10k_root, restored_10k_storage = Lazy.force restored_10k in
  match restore ~cmp:compare restored_10k_storage restored_10k_root with
  | None -> invalid_arg "restored benchmark root missing"
  | Some restored ->
      let found = ref 0 in
      for i = 0 to Array.length ints_10k - 1 do
        if mem ints_10k.(i) restored then incr found
      done;
      consume_int !found

let bench_stored_ref_repeat_contains_10k () =
  let restored_10k_root, restored_10k_storage = Lazy.force restored_10k in
  match restore ~cmp:compare restored_10k_storage restored_10k_root with
  | None -> invalid_arg "restored benchmark root missing"
  | Some restored ->
      let modified = add 10_000 restored in
      let found = ref 0 in
      for i = 0 to Array.length ints_10k - 1 do
        if mem (ints_10k.(i) mod 32) modified then incr found
      done;
      consume_int !found

let bench_restored_to_list_10k () =
  let restored_10k_root, restored_10k_storage = Lazy.force restored_10k in
  match restore ~cmp:compare restored_10k_storage restored_10k_root with
  | None -> invalid_arg "restored benchmark root missing"
  | Some restored -> restored |> to_list |> List.length |> consume_int

let bench_restored_wide_rslice_1 () =
  let storage = Lazy.force restored_wide_4096 in
  match restore ~cmp:compare storage "root" with
  | None -> invalid_arg "wide restored benchmark root missing"
  | Some restored ->
      restored |> rslice ~from_:4095 ~to_:4095 |> List.length |> consume_int

let bench_restored_chained_add_10k () =
  let restored_10k_root, restored_10k_storage = Lazy.force restored_10k in
  match restore ~cmp:compare restored_10k_storage restored_10k_root with
  | None -> invalid_arg "restored benchmark root missing"
  | Some restored ->
      restored |> add 10_000 |> add 10_001 |> add 10_001 |> store |> fst
      |> String.length |> consume_int

let bench_stored_underfull_remove_4k () =
  let set = ref (Lazy.force stored_underfull_4k) in
  for value = 0 to 3_071 do
    set := remove value !set
  done;
  consume_int (count !set)

let bench_stored_nested_underfull_remove_4k () =
  let set = ref (Lazy.force stored_nested_underfull_4k) in
  for value = 0 to 399 do
    set := remove value !set
  done;
  consume_int (count !set)

let benches =
  [
    ("empty-1M", bench_empty_1m);
    ("settings-10K", bench_settings_10k);
    ("of-list-50K", bench_of_list_50k);
    ("of-list-by-desc-50K", bench_of_list_by_desc_50k);
    ("of-sorted-array-50K", bench_of_sorted_array_50k);
    ("conj-10K", bench_conj_10k);
    ("transient-create-300K", bench_transient_create_300k);
    ("transient-add-10K", bench_transient_add_10k);
    ("transient-single-add-50K", bench_transient_single_add_50k);
    ("transient-leaf-add-1K", bench_transient_leaf_add_1k);
    ("disj-10K", bench_disj_10k);
    ("transient-remove-10K", bench_transient_remove_10k);
    ("transient-single-remove-50K", bench_transient_single_remove_50k);
    ("contains-10K", bench_contains_10k);
    ("count-300K", bench_count_300k);
    ("to-list-300K", bench_to_list_300k);
    ("doseq-300K", bench_doseq_300k);
    ("next-300K", bench_next_300k);
    ("reduce-300K", bench_reduce_300k);
    ("fold-list-300K", bench_fold_list_300k);
    ("seq-to-list-300K", bench_seq_to_list_300k);
    ("rseq-to-list-300K", bench_rseq_to_list_300k);
    ("to-seq-300K", bench_to_seq_300k);
    ("seq-reverse-300K", bench_seq_reverse_300k);
    ("fold-seq-300K", bench_fold_seq_300k);
    ("slice-300K", bench_slice_300k);
    ("rslice-300K", bench_rslice_300k);
    ("slice-seq-300K", bench_slice_seq_300k);
    ("rslice-seq-300K", bench_rslice_seq_300k);
    ("seek-300K", bench_seek_300k);
    ("store-10K", bench_store_10k);
    ("restore-10K", bench_restore_10k);
    ("restored-contains-10K", bench_restored_contains_10k);
    ("stored-ref-repeat-contains-10K", bench_stored_ref_repeat_contains_10k);
    ("restored-to-list-10K", bench_restored_to_list_10k);
    ("restored-wide-rslice-1", bench_restored_wide_rslice_1);
    ("restored-chained-add-10K", bench_restored_chained_add_10k);
    ("stored-underfull-remove-4K", bench_stored_underfull_remove_4k);
    ( "stored-nested-underfull-remove-4K",
      bench_stored_nested_underfull_remove_4k );
  ]

let run_for duration_ms f =
  let start = now_ms () in
  let deadline = start +. duration_ms in
  let rec loop iterations =
    f ();
    let iterations = iterations + 1 in
    if now_ms () < deadline then loop iterations
    else (iterations, now_ms () -. start)
  in
  loop 0

let bench config name f =
  ignore (run_for config.warmup_ms f);
  let samples =
    List.init config.samples (fun _ ->
        let iterations, elapsed = run_for config.sample_ms f in
        elapsed /. float_of_int iterations)
  in
  Printf.printf "%s\t%s\n%!" name (format_ms (median samples))

let () =
  let config = parse_args () in
  Printf.printf
    "OCaml PSS benchmark\twarmup-ms=%.0f\tsample-ms=%.0f\tsamples=%d\n%!"
    config.warmup_ms config.sample_ms config.samples;
  List.iter
    (fun name ->
      match List.assoc_opt name benches with
      | Some f -> bench config name f
      | None -> Printf.eprintf "Unknown benchmark: %s\n%!" name)
    config.names;
  ignore !blackhole
