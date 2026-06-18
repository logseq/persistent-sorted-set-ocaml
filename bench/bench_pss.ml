open Persistent_sorted_set

type config =
  { warmup_ms : float
  ; sample_ms : float
  ; samples : int
  ; names : string list
  }

let default_names =
  [ "conj-10K"
  ; "disj-10K"
  ; "contains-10K"
  ; "doseq-300K"
  ; "next-300K"
  ; "reduce-300K"
  ; "restored-chained-add-10K"
  ]

let default_config =
  { warmup_ms = 2000.; sample_ms = 1000.; samples = 5; names = default_names }

let parse_args () =
  let config = ref { default_config with names = [] } in
  let add_name name = config := { !config with names = !config.names @ [ name ] } in
  let rec loop = function
    | [] ->
      if !config.names = [] then { !config with names = default_names } else !config
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

let shuffled_range size =
  let values = Array.init size Fun.id in
  let seed = ref 0x5eed in
  for i = size - 1 downto 1 do
    seed := ((!seed * 1_103_515_245) + 12_345) land 0x3fffffff;
    let j = !seed mod (i + 1) in
    let value = values.(i) in
    values.(i) <- values.(j);
    values.(j) <- value
  done;
  Array.to_list values

let ints_10k = shuffled_range 10_000
let set_10k = lazy (of_sorted_array (Array.init 10_000 Fun.id))
let set_300k = lazy (of_sorted_array (Array.init 300_000 Fun.id))

let blackhole = ref 0

let consume_int value =
  blackhole := (!blackhole + value) land 0x3fffffff

let build_storage () =
  let memory = Hashtbl.create 1024 in
  let next_address = ref 0 in
  { store_node =
      (fun node ->
        incr next_address;
        let address = "node-" ^ string_of_int !next_address in
        Hashtbl.replace memory address node;
        address)
  ; restore_node = (fun address -> Hashtbl.find_opt memory address)
  ; accessed = (fun _ -> ())
  }

let restored_10k = lazy (
  let storage = build_storage () in
  let root, _ = store storage (Lazy.force set_10k) in
  root, storage)

let bench_conj_10k () =
  ints_10k |> List.fold_left (fun set value -> add value set) (empty ()) |> count |> consume_int

let bench_disj_10k () =
  ints_10k |> List.fold_left (fun set value -> remove value set) (Lazy.force set_10k) |> count |> consume_int

let bench_contains_10k () =
  let set = Lazy.force set_10k in
  let found = List.fold_left (fun count value -> if mem value set then count + 1 else count) 0 ints_10k in
  consume_int found

let bench_doseq_300k () =
  Lazy.force set_300k |> fold (fun () value -> consume_int value) ()

let bench_next_300k () =
  Lazy.force set_300k |> fold (fun sum value -> sum + value) 0 |> consume_int

let bench_reduce_300k () =
  Lazy.force set_300k |> fold (fun sum value -> sum + value) 0 |> consume_int

let bench_restored_chained_add_10k () =
  let restored_10k_root, restored_10k_storage = Lazy.force restored_10k in
  match restore ~cmp:compare restored_10k_storage restored_10k_root with
  | None -> invalid_arg "restored benchmark root missing"
  | Some restored ->
    restored
    |> add 10_000
    |> add 10_001
    |> add 10_001
    |> store restored_10k_storage
    |> fst
    |> String.length
    |> consume_int

let benches =
  [ "conj-10K", bench_conj_10k
  ; "disj-10K", bench_disj_10k
  ; "contains-10K", bench_contains_10k
  ; "doseq-300K", bench_doseq_300k
  ; "next-300K", bench_next_300k
  ; "reduce-300K", bench_reduce_300k
  ; "restored-chained-add-10K", bench_restored_chained_add_10k
  ]

let run_for duration_ms f =
  let start = now_ms () in
  let deadline = start +. duration_ms in
  let rec loop iterations =
    f ();
    let iterations = iterations + 1 in
    if now_ms () < deadline then loop iterations else iterations, now_ms () -. start
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
  Printf.printf "OCaml PSS benchmark\twarmup-ms=%.0f\tsample-ms=%.0f\tsamples=%d\n%!"
    config.warmup_ms
    config.sample_ms
    config.samples;
  List.iter
    (fun name ->
      match List.assoc_opt name benches with
      | Some f -> bench config name f
      | None -> Printf.eprintf "Unknown benchmark: %s\n%!" name)
    config.names;
  ignore !blackhole
