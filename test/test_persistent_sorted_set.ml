open Persistent_sorted_set

let failf fmt = Printf.ksprintf failwith fmt

let assert_equal_list label expected actual =
  if expected <> actual then failf "%s: unexpected list" label

let assert_equal_string_list label expected actual =
  if expected <> actual then
    failf "%s: expected [%s], got [%s]" label
      (String.concat "; " expected)
      (String.concat "; " actual)

let assert_equal_int label expected actual =
  if expected <> actual then
    failf "%s: expected %d, got %d" label expected actual

let stored_addresses memory root =
  let rec loop address =
    match Hashtbl.find_opt memory address with
    | Some (Leaf _) -> [ address ]
    | Some (Branch (_, child_addresses)) ->
        address :: List.concat_map loop child_addresses
    | None -> failf "stored address not found: %s" address
  in
  loop root

let assert_raises_invalid_arg label f =
  match f () with
  | exception Invalid_argument _ -> ()
  | exception exn ->
      failf "%s: expected Invalid_argument, got %s" label
        (Printexc.to_string exn)
  | _ -> failf "%s: expected Invalid_argument" label

let irange from_ to_ =
  let rec loop acc current =
    if from_ <= to_ then
      if current < from_ then acc else loop (current :: acc) (current - 1)
    else if current > from_ then acc
    else loop (current :: acc) (current + 1)
  in
  loop [] to_

let shuffled values =
  values
  |> List.mapi (fun index value -> (index * 37 mod 97, value))
  |> List.sort compare |> List.map snd

let random_values seed length bound =
  let state = Random.State.make [| seed |] in
  List.init length (fun _ -> Random.State.int state bound)

let sorted_unique values = List.sort_uniq compare values

let remove_values values removals =
  let removals = sorted_unique removals in
  List.filter (fun value -> not (List.mem value removals)) values

let take n values =
  let rec loop acc n = function
    | _ when n <= 0 -> List.rev acc
    | [] -> List.rev acc
    | value :: rest -> loop (value :: acc) (n - 1) rest
  in
  loop [] n values

let roundtrip_set set =
  let memory = Hashtbl.create 64 in
  let writes = ref 0 in
  let storage =
    {
      store_node =
        (fun node ->
          incr writes;
          let address = "node-" ^ string_of_int !writes in
          Hashtbl.replace memory address node;
          address);
      restore_node = (fun address -> Hashtbl.find_opt memory address);
      accessed = (fun _ -> ());
    }
  in
  let root, _ = store (of_list_by ~storage ~cmp:compare (to_list set)) in
  match restore ~cmp:compare storage root with
  | Some restored -> restored
  | None -> failwith "roundtrip_set should restore the stored root"

let compare_pair_with_nil_wildcard (x0, x1) (y0, y1) =
  let compare_part left right =
    match (left, right) with
    | Some left, Some right -> compare left right
    | _ -> 0
  in
  match compare_part x0 y0 with
  | 0 -> compare_part x1 y1
  | n when n < 0 -> -1
  | _ -> 1

let quotient_compare divisor left right =
  compare (left / divisor) (right / divisor)

let bucket_representatives divisor values =
  let seen = Hashtbl.create 16 in
  values
  |> List.filter (fun value ->
      let bucket = value / divisor in
      if Hashtbl.mem seen bucket then false
      else (
        Hashtbl.add seen bucket ();
        true))
  |> List.sort (quotient_compare divisor)

let bucket_range divisor ~from_ ~to_ values =
  List.filter
    (fun value ->
      let bucket = value / divisor in
      bucket >= from_ / divisor && bucket <= to_ / divisor)
    values

let test_settings_control_storage_branching_factor () =
  let memory = Hashtbl.create 16 in
  let writes = ref 0 in
  let storage =
    {
      store_node =
        (fun node ->
          incr writes;
          let address = "node-" ^ string_of_int !writes in
          Hashtbl.replace memory address node;
          address);
      restore_node = (fun address -> Hashtbl.find_opt memory address);
      accessed = (fun _ -> ());
    }
  in
  let custom_settings = { branching_factor = 4 } in
  let set =
    of_list_by ~storage ~settings:custom_settings ~cmp:compare (irange 0 9)
  in
  if settings set <> custom_settings then
    failwith "settings should expose custom branching factor";
  let root, _ = store set in
  assert_equal_int "custom branching factor controls leaf count" 4 !writes;
  assert_equal_string_list
    "stored_addresses reports root and custom-sized leaves"
    [ root; "node-1"; "node-2"; "node-3" ]
    (stored_addresses memory root);
  (match Hashtbl.find_opt memory root with
  | Some (Branch (keys, child_addresses)) ->
      assert_equal_list "root branch keys use custom leaf boundaries"
        [ 3; 7; 9 ] keys;
      assert_equal_list "root branch addresses use custom leaf boundaries"
        [ "node-1"; "node-2"; "node-3" ]
        child_addresses
  | Some _ -> failwith "custom branching factor should create a branch root"
  | None -> failwith "custom root should be stored");
  if settings (empty ()) <> default_settings then
    failwith "empty should use default settings";
  let default_root, _ = store (of_list_by ~storage ~cmp:compare (irange 0 9)) in
  match Hashtbl.find_opt memory default_root with
  | Some (Leaf values) ->
      assert_equal_list "default branching factor keeps small sets in one leaf"
        (irange 0 9) values
  | Some _ ->
      failwith "default branching factor should keep ten values in one leaf"
  | None -> failwith "default root should be stored"

let test_restore_preserves_settings_for_later_edits () =
  let memory = Hashtbl.create 16 in
  let writes = ref 0 in
  let reads = ref 0 in
  let accessed = ref [] in
  let storage =
    {
      store_node =
        (fun node ->
          incr writes;
          let address = "node-" ^ string_of_int !writes in
          Hashtbl.replace memory address node;
          address);
      restore_node =
        (fun address ->
          incr reads;
          Hashtbl.find_opt memory address);
      accessed = (fun address -> accessed := address :: !accessed);
    }
  in
  let custom_settings = { branching_factor = 4 } in
  let root, _ =
    store
      (of_list_by ~storage ~settings:custom_settings ~cmp:compare (irange 0 15))
  in
  assert_equal_int "custom restore setup writes four leaves plus root" 5 !writes;
  reads := 0;
  accessed := [];
  let restored =
    match restore ~cmp:compare ~settings:custom_settings storage root with
    | Some restored -> restored
    | None -> failwith "restore should find the custom stored root"
  in
  if settings restored <> custom_settings then
    failwith "restore should remember supplied settings";
  let added = add 16 restored in
  assert_equal_int "custom restored add reads only root and target leaf" 2
    !reads;
  assert_equal_string_list
    "custom restored add accesses only root and target leaf" [ "node-4"; root ]
    !accessed;
  let added_root, _ = store added in
  if added_root = root then
    failwith "custom restored add should create a new root";
  assert_equal_int
    "custom restored add splits leaves and branch levels by restored branching \
     factor"
    10 !writes;
  assert_equal_string_list
    "custom restored add reuses unchanged leaves and stores split branch levels"
    [
      added_root;
      "node-7";
      "node-1";
      "node-2";
      "node-3";
      "node-6";
      "node-9";
      "node-8";
    ]
    (stored_addresses memory added_root);
  match Hashtbl.find_opt memory added_root with
  | Some (Branch (keys, child_addresses)) ->
      assert_equal_list "custom restored add root keys follow branching factor"
        [ 15; 16 ] keys;
      assert_equal_list
        "custom restored add root addresses point at split branch nodes"
        [ "node-7"; "node-9" ] child_addresses
  | Some _ -> failwith "custom restored add root should be a branch"
  | None -> failwith "custom restored add root should be stored"

let test_settings_validate_branching_factor () =
  assert_raises_invalid_arg "empty_by rejects branching factors below two"
    (fun () ->
      ignore (empty_by ~settings:{ branching_factor = 1 } ~cmp:compare ()));
  assert_raises_invalid_arg "of_list_by rejects non-positive branching factors"
    (fun () ->
      ignore
        (of_list_by ~settings:{ branching_factor = 0 } ~cmp:compare [ 1; 2; 3 ]));
  let memory = Hashtbl.create 1 in
  let storage =
    {
      store_node =
        (fun node ->
          Hashtbl.replace memory "root" node;
          "root");
      restore_node = (fun address -> Hashtbl.find_opt memory address);
      accessed = (fun _ -> ());
    }
  in
  assert_raises_invalid_arg "restore rejects invalid branching factors"
    (fun () ->
      ignore
        (restore ~cmp:compare ~settings:{ branching_factor = 1 } storage "root"))

let test_of_sorted_array_uses_sorted_input_and_settings () =
  let memory = Hashtbl.create 16 in
  let writes = ref 0 in
  let storage =
    {
      store_node =
        (fun node ->
          incr writes;
          let address = "node-" ^ string_of_int !writes in
          Hashtbl.replace memory address node;
          address);
      restore_node = (fun address -> Hashtbl.find_opt memory address);
      accessed = (fun _ -> ());
    }
  in
  let custom_settings = { branching_factor = 3 } in
  let set =
    of_sorted_array_by ~storage ~settings:custom_settings ~cmp:compare
      [| 0; 1; 1; 2; 3; 3; 4; 5; 6 |]
  in
  assert_equal_list "of_sorted_array_by drops adjacent comparator-equal values"
    (irange 0 6) (to_list set);
  if settings set <> custom_settings then
    failwith "of_sorted_array_by should preserve settings";
  let root, _ = store set in
  assert_equal_int "of_sorted_array_by settings control stored leaf count" 4
    !writes;
  assert_equal_string_list "of_sorted_array_by stores with custom-sized leaves"
    [ root; "node-1"; "node-2"; "node-3" ]
    (stored_addresses memory root);
  (match Hashtbl.find_opt memory root with
  | Some (Branch (keys, child_addresses)) ->
      assert_equal_list "of_sorted_array_by branch keys follow custom chunks"
        [ 2; 5; 6 ] keys;
      assert_equal_list
        "of_sorted_array_by branch addresses follow custom chunks"
        [ "node-1"; "node-2"; "node-3" ]
        child_addresses
  | Some _ ->
      failwith "of_sorted_array_by custom settings should create a branch root"
  | None -> failwith "of_sorted_array_by root should be stored");
  let descending =
    of_sorted_array_by
      ~cmp:(fun left right -> compare right left)
      [| 9; 7; 7; 5 |]
  in
  assert_equal_list "of_sorted_array_by respects custom comparator order"
    [ 9; 7; 5 ] (to_list descending);
  assert_equal_list "of_sorted_array uses default comparator" [ 1; 2; 3 ]
    (to_list (of_sorted_array [| 1; 2; 2; 3 |]))

let test_sorted_order_and_uniqueness () =
  let set = of_list (List.rev (irange 10 20)) in
  assert_equal_list "set iterates in sorted order" (irange 10 20) (to_list set);
  let with_duplicates = of_list [ 3; 2; 1; 2; 3; 4 ] in
  assert_equal_list "set removes duplicate comparator-equal values"
    [ 1; 2; 3; 4 ] (to_list with_duplicates);
  assert_equal_int "count reports unique values" 4 (count with_duplicates);
  if not (mem 3 with_duplicates) then failwith "mem should find present values";
  if mem 5 with_duplicates then failwith "mem should reject absent values";
  assert_equal_list "remove deletes present values" [ 1; 2; 4 ]
    (to_list (remove 3 with_duplicates))

let test_custom_comparator_and_uniqueness () =
  let descending =
    of_list_by ~cmp:(fun left right -> compare right left) [ 1; 2; 3 ]
  in
  assert_equal_list "custom comparator controls order" [ 3; 2; 1 ]
    (to_list descending);
  let values = shuffled (irange 0 120) in
  let representatives = bucket_representatives 10 values in
  let grouped =
    List.fold_left
      (fun set value -> add value set)
      (empty_by ~cmp:(quotient_compare 10) ())
      values
  in
  assert_equal_list "custom comparator keeps one value per comparator bucket"
    representatives (to_list grouped);
  assert_equal_list "slice returns the representative for one comparator bucket"
    (bucket_range 10 ~from_:30 ~to_:30 representatives)
    (slice ~from_:30 ~to_:30 grouped)

let test_equal_comparator_slice_ranges () =
  let values10 = shuffled (irange 0 5000) in
  let representatives10 = bucket_representatives 10 values10 in
  let set10 =
    List.fold_left
      (fun set value -> add value set)
      (empty_by ~cmp:(quotient_compare 10) ())
      values10
  in
  assert_equal_list
    "slice returns the representative comparator-equal to a single bound"
    (bucket_range 10 ~from_:30 ~to_:30 representatives10)
    (slice ~from_:30 ~to_:30 set10);
  assert_equal_list "slice returns representatives across a grouped range"
    (bucket_range 10 ~from_:130 ~to_:4970 representatives10)
    (slice ~from_:130 ~to_:4970 set10);
  assert_equal_list
    "reverse slice returns the representative comparator-equal to a single \
     bound"
    (List.rev (bucket_range 10 ~from_:30 ~to_:30 representatives10))
    (rslice ~from_:30 ~to_:30 set10);
  assert_equal_list
    "reverse slice returns representatives across a grouped range"
    (List.rev (bucket_range 10 ~from_:130 ~to_:4970 representatives10))
    (rslice ~from_:4970 ~to_:130 set10);
  let values100 = shuffled (irange 0 5000) in
  let representatives100 = bucket_representatives 100 values100 in
  let set100 =
    List.fold_left
      (fun set value -> add value set)
      (empty_by ~cmp:(quotient_compare 100) ())
      values100
  in
  assert_equal_list
    "coarse slice returns the lower comparator bucket representative"
    (bucket_range 100 ~from_:30 ~to_:30 representatives100)
    (slice ~from_:30 ~to_:30 set100);
  assert_equal_list
    "coarse slice returns representatives across a grouped range"
    (bucket_range 100 ~from_:130 ~to_:4850 representatives100)
    (slice ~from_:130 ~to_:4850 set100);
  assert_equal_list
    "coarse reverse slice returns the lower comparator bucket representative"
    (List.rev (bucket_range 100 ~from_:30 ~to_:30 representatives100))
    (rslice ~from_:30 ~to_:30 set100);
  assert_equal_list
    "coarse reverse slice returns representatives across a grouped range"
    (List.rev (bucket_range 100 ~from_:130 ~to_:4850 representatives100))
    (rslice ~from_:4850 ~to_:130 set100)

let test_restored_equal_comparator_slice_ranges () =
  let memory = Hashtbl.create 256 in
  let writes = ref 0 in
  let storage =
    {
      store_node =
        (fun node ->
          incr writes;
          let address = "node-" ^ string_of_int !writes in
          Hashtbl.replace memory address node;
          address);
      restore_node = (fun address -> Hashtbl.find_opt memory address);
      accessed = (fun _ -> ());
    }
  in
  let values = shuffled (irange 0 5000) in
  let representatives = bucket_representatives 10 values in
  let original =
    List.fold_left
      (fun set value -> add value set)
      (empty_by ~storage ~cmp:(quotient_compare 10) ())
      values
  in
  let root, _ = store original in
  let restored =
    match restore ~cmp:(quotient_compare 10) storage root with
    | Some restored -> restored
    | None -> failwith "restore should find the stored grouped set"
  in
  assert_equal_list
    "restored slice returns representatives across a grouped range"
    (bucket_range 10 ~from_:130 ~to_:4970 representatives)
    (slice ~from_:130 ~to_:4970 restored);
  assert_equal_list
    "restored reverse slice returns representatives across a grouped range"
    (List.rev (bucket_range 10 ~from_:130 ~to_:4970 representatives))
    (rslice ~from_:4970 ~to_:130 restored)

let test_pair_comparator_nil_wildcard_slices () =
  let set =
    empty_by ~cmp:compare_pair_with_nil_wildcard ()
    |> add (Some "a", Some "b")
    |> add (Some "b", Some "x")
    |> add (Some "b", Some "q")
    |> add (Some "a", Some "d")
  in
  assert_equal_list "wildcard slice matches all values"
    [
      (Some "a", Some "b");
      (Some "a", Some "d");
      (Some "b", Some "q");
      (Some "b", Some "x");
    ]
    (slice ~from_:(None, None) ~to_:(None, None) set);
  assert_equal_list "wildcard slice matches first component"
    [ (Some "a", Some "b"); (Some "a", Some "d") ]
    (slice ~from_:(Some "a", None) ~to_:(Some "a", None) set);
  assert_equal_list "wildcard slice matches exact tuple"
    [ (Some "b", Some "q") ]
    (slice ~from_:(Some "b", Some "q") ~to_:(Some "b", Some "q") set);
  assert_equal_list "wildcard slice handles non-matching subrange"
    [ (Some "a", Some "d"); (Some "b", Some "q") ]
    (slice ~from_:(Some "a", Some "c") ~to_:(Some "b", Some "r") set);
  assert_equal_list
    "wildcard slice includes later values for an open non-matching upper tuple"
    [ (Some "b", Some "x") ]
    (slice ~from_:(Some "b", Some "r") ~to_:(Some "c", None) set);
  assert_equal_list
    "wildcard slice returns empty values for an out-of-range tuple bucket" []
    (slice ~from_:(Some "c", None) ~to_:(Some "c", None) set)

let test_upstream_fractional_slice_boundaries () =
  let float_range from_ to_ = irange from_ to_ |> List.map float_of_int in
  let assert_float_slice label ?from_ ?to_ expected set =
    assert_equal_list label expected (slice ?from_ ?to_ set)
  in
  let assert_float_rslice label ?from_ ?to_ expected set =
    assert_equal_list label expected (rslice ?from_ ?to_ set)
  in
  let large = of_list (float_range 0 5000 |> shuffled) in
  assert_float_slice
    "upstream slice lower bound between keys skips earlier values" ~from_:0.5
    (float_range 1 5000) large;
  assert_float_slice
    "upstream slice upper bound between keys stops before following values"
    ~to_:4999.5 (float_range 0 4999) large;
  assert_float_slice
    "upstream slice fractional middle range includes only matching values"
    ~from_:2499.5 ~to_:2500.5 [ 2500.0 ] large;
  assert_float_slice "upstream slice fractional gap with no members is empty"
    ~from_:2500.1 ~to_:2500.9 [] large;
  assert_float_rslice
    "upstream reverse slice lower bound between keys skips higher values"
    ~from_:4999.5 (float_range 4999 0) large;
  assert_float_rslice
    "upstream reverse slice upper bound between keys stops before lower values"
    ~to_:0.5 (float_range 5000 1) large;
  assert_float_rslice
    "upstream reverse slice fractional middle range includes only matching \
     values"
    ~from_:2500.5 ~to_:2499.5 [ 2500.0 ] large;
  assert_float_rslice
    "upstream reverse slice fractional gap with no members is empty"
    ~from_:2500.9 ~to_:2500.1 [] large;
  let small = of_list (float_range 0 10 |> List.rev) in
  assert_float_slice "upstream one-leaf slice handles fractional lower bounds"
    ~from_:0.5 (float_range 1 10) small;
  assert_float_slice "upstream one-leaf slice handles fractional upper bounds"
    ~to_:9.5 (float_range 0 9) small;
  assert_float_rslice
    "upstream one-leaf reverse slice handles fractional lower bounds" ~from_:9.5
    (float_range 9 0) small;
  assert_float_rslice
    "upstream one-leaf reverse slice handles fractional upper bounds" ~to_:0.5
    (float_range 10 1) small

let test_slice_boundaries () =
  let set = of_list (shuffled (irange 0 5000)) in
  let expect label from_ to_ values =
    assert_equal_list label values (slice ?from_ ?to_ set)
  in
  expect "slice all" None None (irange 0 5000);
  expect "slice lower outside" (Some (-1)) None (irange 0 5000);
  expect "slice lower exact" (Some 1) None (irange 1 5000);
  expect "slice upper exact" None (Some 1) [ 0; 1 ];
  expect "slice inclusive middle" (Some 2499) (Some 2501) [ 2499; 2500; 2501 ];
  expect "slice single exact" (Some 2500) (Some 2500) [ 2500 ];
  expect "slice empty above" (Some 5001) (Some 5002) []

let test_reverse_slice_boundaries () =
  let set = of_list (shuffled (irange 0 5000)) in
  let expect label from_ to_ values =
    assert_equal_list label values (rslice ?from_ ?to_ set)
  in
  expect "rslice all" None None (irange 5000 0);
  expect "rslice upper outside" (Some 5001) None (irange 5000 0);
  expect "rslice lower exact" (Some 1) None [ 1; 0 ];
  expect "rslice to exact" None (Some 4999) [ 5000; 4999 ];
  expect "rslice inclusive middle" (Some 2501) (Some 2499) [ 2501; 2500; 2499 ];
  expect "rslice single exact" (Some 2500) (Some 2500) [ 2500 ];
  expect "rslice empty below" (Some (-1)) (Some (-2)) []

let test_seek () =
  let set = of_list (irange 0 1000) in
  let seq = seq set in
  let rseq = rseq set in
  assert_equal_list "seek on ascending sequence" (irange 500 1000)
    (seq_to_list (seek 500 seq));
  assert_equal_list "seek on descending sequence" (irange 500 0)
    (seq_to_list (seek 500 rseq));
  assert_equal_list "seek can be chained on ascending sequence"
    (irange 750 1000)
    (seq |> seek 250 |> seek 750 |> seq_to_list);
  assert_equal_list "ascending seek results can be reversed" (irange 1000 750)
    (seq |> seek 250 |> seek 750 |> seq_reverse |> seq_to_list);
  assert_equal_list "seek can be chained on descending sequence" (irange 250 0)
    (rseq |> seek 750 |> seek 250 |> seq_to_list);
  assert_equal_list "descending seek results can be reversed" (irange 0 250)
    (rseq |> seek 750 |> seek 250 |> seq_reverse |> seq_to_list)

let test_slice_sequences_are_seekable_and_reversible () =
  let set = of_list (irange 0 10000) in
  assert_equal_list "seek works on ascending slice sequences" (irange 5000 7500)
    (slice_seq ~from_:2500 ~to_:7500 set |> seek 5000 |> seq_to_list);
  assert_equal_list "seek can be chained on ascending slice sequences" [ 7500 ]
    (slice_seq ~from_:2500 ~to_:7500 set
    |> seek 5000 |> seek 7500 |> seq_to_list);
  assert_equal_list "ascending slice sequences can be reversed"
    (irange 7500 5000)
    (slice_seq ~from_:2500 ~to_:7500 set
    |> seek 5000 |> seq_reverse |> seq_to_list);
  assert_equal_list "seek works on reverse slice sequences" (irange 5000 2500)
    (rslice_seq ~from_:7500 ~to_:2500 set |> seek 5000 |> seq_to_list);
  assert_equal_list "seek can be chained on reverse slice sequences" [ 2500 ]
    (rslice_seq ~from_:7500 ~to_:2500 set
    |> seek 5000 |> seek 2500 |> seq_to_list);
  assert_equal_list "reverse slice sequences can be reversed" (irange 2500 5000)
    (rslice_seq ~from_:7500 ~to_:2500 set
    |> seek 5000 |> seq_reverse |> seq_to_list)

let test_fold_reduces_sets_and_sequences () =
  let sum acc value = acc + value in
  let set = of_list (irange 0 5000) in
  assert_equal_int "fold empty set returns init" 0 (fold sum 0 (empty ()));
  assert_equal_int "fold sums full set" 12_502_500 (fold sum 0 set);
  assert_equal_int "fold_seq sums ascending sequence" 12_502_500
    (fold_seq sum 0 (seq set));
  assert_equal_int "fold_seq sums descending sequence" 12_502_500
    (fold_seq sum 0 (rseq set));
  assert_equal_int "fold_list sums slice" 7_502_500
    (fold_list sum 0 (slice ~from_:1000 ~to_:4000 set));
  assert_equal_int "fold_list sums reverse slice" 7_502_500
    (fold_list sum 0 (rslice ~from_:4000 ~to_:1000 set));
  assert_equal_int "fold_seq sums seek result" 12_471_375
    (fold_seq sum 0 (seq set |> seek 250));
  assert_equal_int "fold_seq sums reversed seek result" 12_471_375
    (fold_seq sum 0 (seq set |> seek 250 |> seq_reverse))

let test_upstream_stresstest_btset_parity () =
  for iteration = 0 to 9 do
    let size = 1200 in
    let xs =
      random_values (1000 + iteration) (1 + (iteration * 137 mod size)) size
    in
    let xs_sorted = sorted_unique xs in
    let rm =
      random_values (2000 + iteration) (iteration * 311 mod (size * 3)) size
    in
    let full_rm = shuffled (xs @ rm) in
    let expected_after_remove = remove_values xs_sorted rm in
    let cases =
      [
        ("conj", of_list xs);
        ("bulk", of_list (List.rev xs));
        ("lazy", roundtrip_set (of_list xs));
      ]
    in
    List.iter
      (fun (method_name, set0) ->
        assert_equal_list
          ("stresstest-btset " ^ method_name ^ " builds sorted unique values")
          xs_sorted (to_list set0);
        assert_equal_int
          ("stresstest-btset " ^ method_name ^ " count")
          (List.length xs_sorted) (count set0);
        let set1 = List.fold_left (fun set value -> remove value set) set0 rm in
        assert_equal_list
          ("stresstest-btset " ^ method_name ^ " disj")
          expected_after_remove (to_list set1);
        assert_equal_int
          ("stresstest-btset " ^ method_name ^ " disj count")
          (List.length expected_after_remove)
          (count set1);
        let set2 =
          List.fold_left (fun set value -> remove value set) set0 full_rm
        in
        assert_equal_list
          ("stresstest-btset " ^ method_name ^ " full disj")
          [] (to_list set2))
      cases
  done

let test_upstream_stresstest_slice_parity () =
  for iteration = 0 to 11 do
    let size = 2000 in
    let xs =
      random_values (3000 + iteration) (1 + (iteration * 257 mod size)) size
    in
    let xs_sorted = sorted_unique xs in
    let left =
      1000 - Random.State.int (Random.State.make [| 4000 + iteration |]) 2000
    in
    let right =
      1000 + Random.State.int (Random.State.make [| 5000 + iteration |]) 2000
    in
    let from_ = min left right in
    let to_ = max left right in
    let expected =
      List.filter (fun value -> from_ <= value && value <= to_) xs_sorted
    in
    let cases =
      [ ("conj", of_list xs); ("lazy", roundtrip_set (of_list xs)) ]
    in
    List.iter
      (fun (method_name, set) ->
        let set_range = slice ~from_ ~to_ set in
        assert_equal_list
          ("stresstest-slice " ^ method_name ^ " slice")
          expected set_range;
        assert_equal_list
          ("stresstest-slice " ^ method_name ^ " slice_seq")
          expected
          (slice_seq ~from_ ~to_ set |> seq_to_list);
        assert_equal_list
          ("stresstest-slice " ^ method_name ^ " reverse view")
          (List.rev expected)
          (rslice ~from_:to_ ~to_:from_ set))
      cases
  done

let test_upstream_stresstest_rslice_parity () =
  for iteration = 0 to 19 do
    let len = 3000 in
    let xs = shuffled (irange 0 len) in
    let set = of_list xs in
    let from_ = len + 100 - (iteration mod 3) in
    let to_ = -100 + (iteration mod 5) in
    let expected = irange len 0 in
    assert_equal_list "stresstest-rslice returns descending full range" expected
      (rslice ~from_ ~to_ set);
    assert_equal_list "stresstest-rslice sequence reverse round-trips" expected
      (rslice_seq ~from_ ~to_ set |> seq_reverse |> seq_to_list |> List.rev)
  done

let test_upstream_stresstest_seek_parity () =
  for iteration = 0 to 15 do
    let size = 2000 in
    let xs =
      random_values (6000 + iteration)
        (1 + Random.State.int (Random.State.make [| 7000 + iteration |]) size)
        size
    in
    let xs_sorted = sorted_unique xs in
    let seek_to =
      Random.State.int (Random.State.make [| 8000 + iteration |]) size
    in
    let set = of_list xs_sorted in
    let expected_asc = List.filter (fun value -> value >= seek_to) xs_sorted in
    let expected_desc =
      xs_sorted |> List.filter (fun value -> value <= seek_to) |> List.rev
    in
    assert_equal_list "stresstest-seek asc" expected_asc
      (seq set |> seek seek_to |> seq_to_list);
    assert_equal_list "stresstest-seek asc near upper edge"
      (List.filter (fun value -> value >= size - 1) xs_sorted)
      (seq set |> seek (size - 1) |> seq_to_list);
    assert_equal_list "stresstest-seek desc" expected_desc
      (rseq set |> seek seek_to |> seq_to_list);
    assert_equal_list "stresstest-seek desc near lower edge"
      (List.filter (fun value -> value <= 1) xs_sorted |> List.rev)
      (rseq set |> seek 1 |> seq_to_list)
  done

let test_upstream_overflow_batched_insert_smoke () =
  let len = 12_000 in
  let part = len / 100 in
  let values = shuffled (irange 0 (len - 1)) in
  let rec partition acc current count = function
    | [] -> List.rev (if current = [] then acc else List.rev current :: acc)
    | value :: rest when count = part ->
        partition (List.rev current :: acc) [ value ] 1 rest
    | value :: rest -> partition acc (value :: current) (count + 1) rest
  in
  let batches = partition [] [] 0 values in
  let set =
    List.fold_left
      (fun set batch ->
        List.fold_left (fun set value -> add value set) set batch)
      (empty ()) batches
  in
  assert_equal_int "overflow batched insert count" len (count set);
  assert_equal_list "overflow batched insert can read first values" (irange 0 9)
    (take 10 (to_list set))

let test_storage_round_trip_and_stable_addresses () =
  let memory = Hashtbl.create 8 in
  let writes = ref 0 in
  let reads = ref 0 in
  let accessed = ref [] in
  let storage =
    {
      store_node =
        (fun node ->
          incr writes;
          let address = "node-" ^ string_of_int !writes in
          Hashtbl.replace memory address node;
          address);
      restore_node =
        (fun address ->
          incr reads;
          Hashtbl.find_opt memory address);
      accessed = (fun address -> accessed := address :: !accessed);
    }
  in
  let original = of_list_by ~storage ~cmp:compare (irange 0 10) in
  let root, stored = store original in
  assert_equal_int "store writes the root once" 1 !writes;
  assert_equal_list "stored_addresses reports stored root" [ root ]
    (stored_addresses memory root);
  let same_root, stored_again = store stored in
  if same_root <> root then
    failwith "store should return the stable root address";
  assert_equal_int "storing an already stored set does not write again" 1
    !writes;
  (match restore ~cmp:compare storage root with
  | Some restored ->
      assert_equal_int "restore should not read before access" 0 !reads;
      assert_equal_list
        "restore should not mark addresses accessed before access" [] !accessed;
      assert_equal_list "restore round-trips stored values" (to_list original)
        (to_list restored)
  | None -> failwith "restore should find the stored root");
  assert_equal_int "accessing restored values reads the root once" 1 !reads;
  assert_equal_list "accessing restored values marks the root as accessed"
    [ root ] !accessed;
  let changed = add 101 stored_again in
  let changed_root, changed_stored = store changed in
  if changed_root = root then
    failwith "modified stored set should get a new root address";
  assert_equal_int "modified set writes a new root" 2 !writes;
  assert_equal_list "stored_addresses reports the new root" [ changed_root ]
    (stored_addresses memory changed_root);
  let duplicate = add 101 changed_stored in
  let duplicate_root, _ = store duplicate in
  if duplicate_root <> changed_root then
    failwith "adding an existing value should keep the stored root";
  assert_equal_int "unchanged duplicate add does not write again" 2 !writes

let test_storage_uses_leaf_and_branch_nodes_for_large_sets () =
  let memory = Hashtbl.create 16 in
  let writes = ref 0 in
  let reads = ref 0 in
  let accessed = ref [] in
  let storage =
    {
      store_node =
        (fun node ->
          incr writes;
          let address = "node-" ^ string_of_int !writes in
          Hashtbl.replace memory address node;
          address);
      restore_node =
        (fun address ->
          incr reads;
          Hashtbl.find_opt memory address);
      accessed = (fun address -> accessed := address :: !accessed);
    }
  in
  let original = of_list_by ~storage ~cmp:compare (irange 0 100) in
  let root, stored = store original in
  let root_addresses = stored_addresses memory root in
  assert_equal_int "large store writes leaf nodes plus a root branch" 5 !writes;
  assert_equal_list "stored_addresses returns the root followed by leaves"
    [ root; "node-1"; "node-2"; "node-3"; "node-4" ]
    root_addresses;
  (match Hashtbl.find_opt memory root with
  | Some (Branch (keys, child_addresses)) ->
      assert_equal_list "root branch stores child max keys" [ 31; 63; 95; 100 ]
        keys;
      assert_equal_list "root branch stores child addresses"
        [ "node-1"; "node-2"; "node-3"; "node-4" ]
        child_addresses
  | Some _ -> failwith "large root should be a branch node"
  | None -> failwith "large root address should be stored");
  assert_equal_int "large restore construction stays lazy" 0 !reads;
  (match restore ~cmp:compare storage root with
  | Some restored ->
      assert_equal_int "large restore does not read before access" 0 !reads;
      assert_equal_list "large restore round-trips values" (to_list original)
        (to_list restored)
  | None -> failwith "large restore should find the stored root");
  assert_equal_int "large restored access reads branch and leaves" 5 !reads;
  assert_equal_list "large restored access marks branch and leaves accessed"
    [ "node-4"; "node-3"; "node-2"; "node-1"; root ]
    !accessed;
  let appended = add 101 stored in
  let appended_root, _ = store appended in
  if appended_root = root then
    failwith "appending to a stored set should create a new root";
  assert_equal_int "append writes one changed leaf and one new branch" 7 !writes;
  assert_equal_list "append reuses unchanged leaf addresses"
    [ appended_root; "node-1"; "node-2"; "node-3"; "node-6" ]
    (stored_addresses memory appended_root);
  match Hashtbl.find_opt memory appended_root with
  | Some (Branch (_, child_addresses)) ->
      assert_equal_list "appended root reuses unchanged leaves"
        [ "node-1"; "node-2"; "node-3"; "node-6" ]
        child_addresses
  | Some _ -> failwith "appended root should be a branch node"
  | None -> failwith "appended root address should be stored"

let test_storage_remove_preserves_unchanged_leaf_addresses () =
  let memory = Hashtbl.create 16 in
  let writes = ref 0 in
  let storage =
    {
      store_node =
        (fun node ->
          incr writes;
          let address = "node-" ^ string_of_int !writes in
          Hashtbl.replace memory address node;
          address);
      restore_node = (fun address -> Hashtbl.find_opt memory address);
      accessed = (fun _ -> ());
    }
  in
  let original = of_list_by ~storage ~cmp:compare (irange 0 100) in
  let root, stored = store original in
  assert_equal_int "large store writes leaf nodes plus a root" 5 !writes;
  let removed = remove 50 stored in
  let removed_root, removed_stored = store removed in
  if removed_root = root then
    failwith "removing a value should create a new root address";
  assert_equal_int "removing from one leaf rewrites one leaf and one root" 7
    !writes;
  assert_equal_list "remove reuses unaffected leaf addresses"
    [ removed_root; "node-1"; "node-6"; "node-3"; "node-4" ]
    (stored_addresses memory removed_root);
  (match Hashtbl.find_opt memory removed_root with
  | Some (Branch (_, child_addresses)) ->
      assert_equal_list "removed root points at reused sibling leaves"
        [ "node-1"; "node-6"; "node-3"; "node-4" ]
        child_addresses
  | Some _ -> failwith "removed root should be a branch node"
  | None -> failwith "removed root address should be stored");
  (match Hashtbl.find_opt memory "node-6" with
  | Some (Leaf values) ->
      assert_equal_list "changed leaf only drops the removed value"
        (irange 32 49 @ irange 51 63)
        values
  | Some _ -> failwith "changed node should be a leaf"
  | None -> failwith "changed leaf address should be stored");
  assert_equal_list "removed stored set keeps sorted values"
    (irange 0 49 @ irange 51 100)
    (to_list removed_stored)

let test_storage_add_preserves_unchanged_leaf_addresses () =
  let memory = Hashtbl.create 16 in
  let writes = ref 0 in
  let storage =
    {
      store_node =
        (fun node ->
          incr writes;
          let address = "node-" ^ string_of_int !writes in
          Hashtbl.replace memory address node;
          address);
      restore_node = (fun address -> Hashtbl.find_opt memory address);
      accessed = (fun _ -> ());
    }
  in
  let original =
    of_list_by ~storage ~cmp:compare (irange 0 49 @ irange 51 100)
  in
  let root, stored = store original in
  assert_equal_int "large gapped store writes leaf nodes plus a root" 5 !writes;
  let added = add 50 stored in
  let added_root, added_stored = store added in
  if added_root = root then
    failwith "adding a value should create a new root address";
  assert_equal_int
    "adding into one full leaf rewrites split leaves and one root" 8 !writes;
  assert_equal_list "add reuses unaffected leaf addresses"
    [ added_root; "node-1"; "node-6"; "node-7"; "node-3"; "node-4" ]
    (stored_addresses memory added_root);
  (match Hashtbl.find_opt memory added_root with
  | Some (Branch (_, child_addresses)) ->
      assert_equal_list "added root points at reused sibling leaves"
        [ "node-1"; "node-6"; "node-7"; "node-3"; "node-4" ]
        child_addresses
  | Some _ -> failwith "added root should be a branch node"
  | None -> failwith "added root address should be stored");
  (match
     (Hashtbl.find_opt memory "node-6", Hashtbl.find_opt memory "node-7")
   with
  | Some (Leaf left), Some (Leaf right) ->
      assert_equal_list "split left leaf contains the lower local values"
        (irange 32 63) left;
      assert_equal_list "split right leaf contains the upper local values"
        [ 64 ] right
  | _ -> failwith "changed leaf should split into two new leaves");
  assert_equal_list "added stored set keeps sorted values" (irange 0 100)
    (to_list added_stored)

let test_restored_add_preserves_unchanged_leaf_addresses_lazily () =
  let memory = Hashtbl.create 16 in
  let writes = ref 0 in
  let reads = ref 0 in
  let accessed = ref [] in
  let storage =
    {
      store_node =
        (fun node ->
          incr writes;
          let address = "node-" ^ string_of_int !writes in
          Hashtbl.replace memory address node;
          address);
      restore_node =
        (fun address ->
          incr reads;
          Hashtbl.find_opt memory address);
      accessed = (fun address -> accessed := address :: !accessed);
    }
  in
  let original = of_list_by ~storage ~cmp:compare (irange 0 100) in
  let root, _ = store original in
  reads := 0;
  accessed := [];
  let restored =
    match restore ~cmp:compare storage root with
    | Some restored -> restored
    | None -> failwith "restore should find the stored root"
  in
  let appended = add 101 restored in
  assert_equal_int "restored add should read only root and target leaf" 2 !reads;
  assert_equal_list "restored add should access only root and target leaf"
    [ "node-4"; root ] !accessed;
  let appended_root, appended_stored = store appended in
  if appended_root = root then failwith "restored add should create a new root";
  assert_equal_int "restored add should write one changed leaf and one new root"
    7 !writes;
  assert_equal_list "restored add reuses unchanged leaf addresses"
    [ appended_root; "node-1"; "node-2"; "node-3"; "node-6" ]
    (stored_addresses memory appended_root);
  assert_equal_list "restored add keeps sorted values" (irange 0 101)
    (to_list appended_stored)

let test_chained_restored_adds_keep_edit_path_without_rebuilding () =
  let memory = Hashtbl.create 16 in
  let writes = ref 0 in
  let reads = ref 0 in
  let accessed = ref [] in
  let storage =
    {
      store_node =
        (fun node ->
          incr writes;
          let address = "node-" ^ string_of_int !writes in
          Hashtbl.replace memory address node;
          address);
      restore_node =
        (fun address ->
          incr reads;
          Hashtbl.find_opt memory address);
      accessed = (fun address -> accessed := address :: !accessed);
    }
  in
  let root, _ = store (of_list_by ~storage ~cmp:compare (irange 0 100)) in
  reads := 0;
  accessed := [];
  let restored =
    match restore ~cmp:compare storage root with
    | Some restored -> restored
    | None -> failwith "restore should find the stored root"
  in
  let appended_once = add 101 restored in
  assert_equal_int "first restored add reads root and target leaf" 2 !reads;
  assert_equal_list "first restored add accesses root and target leaf"
    [ "node-4"; root ] !accessed;
  reads := 0;
  accessed := [];
  let appended_twice = add 102 appended_once in
  assert_equal_int
    "second add should update the edited leaf without reading siblings" 0 !reads;
  assert_equal_list "second add should not access stored siblings" [] !accessed;
  reads := 0;
  accessed := [];
  let appended_duplicate = add 102 appended_twice in
  assert_equal_int
    "duplicate add should check the edited leaf without reading siblings" 0
    !reads;
  assert_equal_list "duplicate add should not access stored siblings" []
    !accessed;
  let appended_root, appended_stored = store appended_duplicate in
  if appended_root = root then
    failwith "chained restored adds should create a new root";
  assert_equal_int
    "chained restored adds should write one final leaf and one new root" 7
    !writes;
  assert_equal_list "chained restored adds reuse unchanged leaf addresses"
    [ appended_root; "node-1"; "node-2"; "node-3"; "node-6" ]
    (stored_addresses memory appended_root);
  assert_equal_list "chained restored adds keep sorted values" (irange 0 102)
    (to_list appended_stored)

let test_restored_remove_preserves_unchanged_leaf_addresses_lazily () =
  let memory = Hashtbl.create 16 in
  let writes = ref 0 in
  let reads = ref 0 in
  let accessed = ref [] in
  let storage =
    {
      store_node =
        (fun node ->
          incr writes;
          let address = "node-" ^ string_of_int !writes in
          Hashtbl.replace memory address node;
          address);
      restore_node =
        (fun address ->
          incr reads;
          Hashtbl.find_opt memory address);
      accessed = (fun address -> accessed := address :: !accessed);
    }
  in
  let original = of_list_by ~storage ~cmp:compare (irange 0 100) in
  let root, _ = store original in
  reads := 0;
  accessed := [];
  let restored =
    match restore ~cmp:compare storage root with
    | Some restored -> restored
    | None -> failwith "restore should find the stored root"
  in
  let removed = remove 50 restored in
  assert_equal_int "restored remove should read only root and target leaf" 2
    !reads;
  assert_equal_list "restored remove should access only root and target leaf"
    [ "node-2"; root ] !accessed;
  let removed_root, removed_stored = store removed in
  if removed_root = root then
    failwith "restored remove should create a new root";
  assert_equal_int
    "restored remove should write one changed leaf and one new root" 7 !writes;
  assert_equal_list "restored remove reuses unchanged leaf addresses"
    [ removed_root; "node-1"; "node-6"; "node-3"; "node-4" ]
    (stored_addresses memory removed_root);
  assert_equal_list "restored remove keeps sorted values"
    (irange 0 49 @ irange 51 100)
    (to_list removed_stored)

let test_restored_mem_reads_only_needed_leaves () =
  let memory = Hashtbl.create 16 in
  let writes = ref 0 in
  let reads = ref 0 in
  let accessed = ref [] in
  let storage =
    {
      store_node =
        (fun node ->
          incr writes;
          let address = "node-" ^ string_of_int !writes in
          Hashtbl.replace memory address node;
          address);
      restore_node =
        (fun address ->
          incr reads;
          Hashtbl.find_opt memory address);
      accessed = (fun address -> accessed := address :: !accessed);
    }
  in
  let original = of_list_by ~storage ~cmp:compare (irange 0 100) in
  let root, _ = store original in
  let restored =
    match restore ~cmp:compare storage root with
    | Some restored -> restored
    | None -> failwith "restore should find the stored root"
  in
  if not (mem 5 restored) then
    failwith "mem should find values in a restored set";
  assert_equal_int "restored mem should read only root and matching leaf" 2
    !reads;
  assert_equal_list "restored mem should access only root and matching leaf"
    [ "node-1"; root ] !accessed;
  reads := 0;
  accessed := [];
  if not (mem 40 restored) then
    failwith "mem should find values after skipping earlier leaves";
  assert_equal_int
    "restored mem should use branch keys to skip irrelevant leaves" 2 !reads;
  assert_equal_list
    "restored mem should access only root and matching later leaf"
    [ "node-2"; root ] !accessed;
  reads := 0;
  accessed := [];
  if mem (-1) restored then
    failwith "mem should reject values below the first leaf";
  assert_equal_int "restored mem should stop after first leaf for lower misses"
    2 !reads;
  assert_equal_list "restored mem lower miss should access root and first leaf"
    [ "node-1"; root ] !accessed

let test_restored_slice_reads_only_overlapping_leaves () =
  let memory = Hashtbl.create 16 in
  let writes = ref 0 in
  let reads = ref 0 in
  let accessed = ref [] in
  let storage =
    {
      store_node =
        (fun node ->
          incr writes;
          let address = "node-" ^ string_of_int !writes in
          Hashtbl.replace memory address node;
          address);
      restore_node =
        (fun address ->
          incr reads;
          Hashtbl.find_opt memory address);
      accessed = (fun address -> accessed := address :: !accessed);
    }
  in
  let original = of_list_by ~storage ~cmp:compare (irange 0 100) in
  let root, _ = store original in
  let restored =
    match restore ~cmp:compare storage root with
    | Some restored -> restored
    | None -> failwith "restore should find the stored root"
  in
  assert_equal_list "restored slice returns values from one leaf" (irange 40 42)
    (slice ~from_:40 ~to_:42 restored);
  assert_equal_int "restored slice should read root and one overlapping leaf" 2
    !reads;
  assert_equal_list "restored slice should access only root and one leaf"
    [ "node-2"; root ] !accessed;
  reads := 0;
  accessed := [];
  assert_equal_list "restored slice returns values across adjacent leaves"
    (irange 62 65)
    (slice ~from_:62 ~to_:65 restored);
  assert_equal_int
    "restored slice across boundary should read root and two leaves" 3 !reads;
  assert_equal_list
    "restored slice across boundary should access root and two leaves"
    [ "node-3"; "node-2"; root ]
    !accessed

let test_restored_reverse_slice_reads_only_overlapping_leaves () =
  let memory = Hashtbl.create 16 in
  let writes = ref 0 in
  let reads = ref 0 in
  let accessed = ref [] in
  let storage =
    {
      store_node =
        (fun node ->
          incr writes;
          let address = "node-" ^ string_of_int !writes in
          Hashtbl.replace memory address node;
          address);
      restore_node =
        (fun address ->
          incr reads;
          Hashtbl.find_opt memory address);
      accessed = (fun address -> accessed := address :: !accessed);
    }
  in
  let original = of_list_by ~storage ~cmp:compare (irange 0 100) in
  let root, _ = store original in
  let restored =
    match restore ~cmp:compare storage root with
    | Some restored -> restored
    | None -> failwith "restore should find the stored root"
  in
  assert_equal_list "restored reverse slice returns values from one leaf"
    (irange 42 40)
    (rslice ~from_:42 ~to_:40 restored);
  assert_equal_int
    "restored reverse slice should read root and one overlapping leaf" 2 !reads;
  assert_equal_list
    "restored reverse slice should access only root and one leaf"
    [ "node-2"; root ] !accessed;
  reads := 0;
  accessed := [];
  assert_equal_list
    "restored reverse slice returns values across adjacent leaves"
    (irange 65 62)
    (rslice ~from_:65 ~to_:62 restored);
  assert_equal_int
    "restored reverse slice across boundary should read root and two leaves" 3
    !reads;
  assert_equal_list
    "restored reverse slice across boundary should access root and two leaves"
    [ "node-2"; "node-3"; root ]
    !accessed

let test_restored_seq_seek_is_lazy () =
  let memory = Hashtbl.create 16 in
  let writes = ref 0 in
  let reads = ref 0 in
  let accessed = ref [] in
  let storage =
    {
      store_node =
        (fun node ->
          incr writes;
          let address = "node-" ^ string_of_int !writes in
          Hashtbl.replace memory address node;
          address);
      restore_node =
        (fun address ->
          incr reads;
          Hashtbl.find_opt memory address);
      accessed = (fun address -> accessed := address :: !accessed);
    }
  in
  let root, _ = store (of_list_by ~storage ~cmp:compare (irange 0 100)) in
  let restored =
    match restore ~cmp:compare storage root with
    | Some restored -> restored
    | None -> failwith "restore should find the stored root"
  in
  let sequence = seq restored in
  assert_equal_int "seq construction should not read restored storage" 0 !reads;
  assert_equal_list "seq construction should not access restored storage" []
    !accessed;
  assert_equal_list "seeked restored seq returns the requested suffix"
    (irange 40 100)
    (sequence |> seek 40 |> seq_to_list);
  assert_equal_int "seeked restored seq should skip leaves before the seek key"
    4 !reads;
  assert_equal_list "seeked restored seq should access root and suffix leaves"
    [ "node-4"; "node-3"; "node-2"; root ]
    !accessed

let test_restored_rseq_seek_is_lazy () =
  let memory = Hashtbl.create 16 in
  let writes = ref 0 in
  let reads = ref 0 in
  let accessed = ref [] in
  let storage =
    {
      store_node =
        (fun node ->
          incr writes;
          let address = "node-" ^ string_of_int !writes in
          Hashtbl.replace memory address node;
          address);
      restore_node =
        (fun address ->
          incr reads;
          Hashtbl.find_opt memory address);
      accessed = (fun address -> accessed := address :: !accessed);
    }
  in
  let root, _ = store (of_list_by ~storage ~cmp:compare (irange 0 100)) in
  let restored =
    match restore ~cmp:compare storage root with
    | Some restored -> restored
    | None -> failwith "restore should find the stored root"
  in
  let sequence = rseq restored in
  assert_equal_int "rseq construction should not read restored storage" 0 !reads;
  assert_equal_list "rseq construction should not access restored storage" []
    !accessed;
  assert_equal_list "seeked restored rseq returns the requested suffix"
    (irange 65 0)
    (sequence |> seek 65 |> seq_to_list);
  assert_equal_int "seeked restored rseq should skip leaves above the seek key"
    4 !reads;
  assert_equal_list "seeked restored rseq should access root and suffix leaves"
    [ "node-1"; "node-2"; "node-3"; root ]
    !accessed

let test_restored_slice_seq_construction_is_lazy () =
  let memory = Hashtbl.create 16 in
  let writes = ref 0 in
  let reads = ref 0 in
  let accessed = ref [] in
  let storage =
    {
      store_node =
        (fun node ->
          incr writes;
          let address = "node-" ^ string_of_int !writes in
          Hashtbl.replace memory address node;
          address);
      restore_node =
        (fun address ->
          incr reads;
          Hashtbl.find_opt memory address);
      accessed = (fun address -> accessed := address :: !accessed);
    }
  in
  let root, _ = store (of_list_by ~storage ~cmp:compare (irange 0 100)) in
  let restored =
    match restore ~cmp:compare storage root with
    | Some restored -> restored
    | None -> failwith "restore should find the stored root"
  in
  let sequence = slice_seq ~from_:40 ~to_:65 restored in
  assert_equal_int "slice_seq construction should not read restored storage" 0
    !reads;
  assert_equal_list "slice_seq construction should not access restored storage"
    [] !accessed;
  assert_equal_list "seeked restored slice_seq stays inside the original slice"
    (irange 62 65)
    (sequence |> seek 62 |> seq_to_list);
  assert_equal_int
    "seeked restored slice_seq should read only overlapping suffix leaves" 3
    !reads;
  assert_equal_list
    "seeked restored slice_seq should access root and overlapping suffix leaves"
    [ "node-3"; "node-2"; root ]
    !accessed

let test_restored_rslice_seq_construction_is_lazy () =
  let memory = Hashtbl.create 16 in
  let writes = ref 0 in
  let reads = ref 0 in
  let accessed = ref [] in
  let storage =
    {
      store_node =
        (fun node ->
          incr writes;
          let address = "node-" ^ string_of_int !writes in
          Hashtbl.replace memory address node;
          address);
      restore_node =
        (fun address ->
          incr reads;
          Hashtbl.find_opt memory address);
      accessed = (fun address -> accessed := address :: !accessed);
    }
  in
  let root, _ = store (of_list_by ~storage ~cmp:compare (irange 0 100)) in
  let restored =
    match restore ~cmp:compare storage root with
    | Some restored -> restored
    | None -> failwith "restore should find the stored root"
  in
  let sequence = rslice_seq ~from_:65 ~to_:40 restored in
  assert_equal_int "rslice_seq construction should not read restored storage" 0
    !reads;
  assert_equal_list "rslice_seq construction should not access restored storage"
    [] !accessed;
  assert_equal_list "seeked restored rslice_seq stays inside the original slice"
    (irange 42 40)
    (sequence |> seek 42 |> seq_to_list);
  assert_equal_int
    "seeked restored rslice_seq should read only overlapping suffix leaf" 2
    !reads;
  assert_equal_list
    "seeked restored rslice_seq should access root and overlapping suffix leaf"
    [ "node-2"; root ] !accessed

let test_storage_uses_nested_branch_nodes_for_very_large_sets () =
  let memory = Hashtbl.create 64 in
  let writes = ref 0 in
  let reads = ref 0 in
  let storage =
    {
      store_node =
        (fun node ->
          incr writes;
          let address = "node-" ^ string_of_int !writes in
          Hashtbl.replace memory address node;
          address);
      restore_node =
        (fun address ->
          incr reads;
          Hashtbl.find_opt memory address);
      accessed = (fun _ -> ());
    }
  in
  let original = of_list_by ~storage ~cmp:compare (irange 0 1055) in
  let root, _ = store original in
  assert_equal_int
    "very large store writes leaves, intermediate branches, and root" 36 !writes;
  if root <> "node-36" then
    failwith "very large root should be written after intermediate branches";
  assert_equal_int "stored_addresses includes root and every stored descendant"
    36
    (List.length (stored_addresses memory root));
  (match Hashtbl.find_opt memory root with
  | Some (Branch (keys, child_addresses)) ->
      assert_equal_list "root branch stores intermediate max keys"
        [ 1023; 1055 ] keys;
      assert_equal_list "root branch points to intermediate branches"
        [ "node-34"; "node-35" ] child_addresses
  | Some _ -> failwith "very large root should be a branch node"
  | None -> failwith "very large root address should be stored");
  (match Hashtbl.find_opt memory "node-34" with
  | Some (Branch (keys, child_addresses)) ->
      assert_equal_list "first intermediate branch stores leaf max keys"
        (List.init 32 (fun index -> ((index + 1) * 32) - 1))
        keys;
      assert_equal_list
        "first intermediate branch points to the first leaf group"
        (List.init 32 (fun index -> "node-" ^ string_of_int (index + 1)))
        child_addresses
  | Some _ -> failwith "first intermediate node should be a branch"
  | None -> failwith "first intermediate branch should be stored");
  (match Hashtbl.find_opt memory "node-35" with
  | Some (Branch (keys, child_addresses)) ->
      assert_equal_list
        "second intermediate branch stores remaining leaf max key" [ 1055 ] keys;
      assert_equal_list
        "second intermediate branch points to the remaining leaf" [ "node-33" ]
        child_addresses
  | Some _ -> failwith "second intermediate node should be a branch"
  | None -> failwith "second intermediate branch should be stored");
  (match restore ~cmp:compare storage root with
  | Some restored ->
      assert_equal_int "very large restore stays lazy before access" 0 !reads;
      assert_equal_list "very large restore round-trips values"
        (to_list original) (to_list restored)
  | None -> failwith "very large restore should find the stored root");
  assert_equal_int "very large restored access reads every branch and leaf" 36
    !reads

let test_storage_addresses_visit_stored_descendants () =
  let make_storage () =
    let memory = Hashtbl.create 128 in
    let writes = ref 0 in
    let reads = ref 0 in
    let storage =
      {
        store_node =
          (fun node ->
            incr writes;
            let address = "node-" ^ string_of_int !writes in
            Hashtbl.replace memory address node;
            address);
        restore_node =
          (fun address ->
            incr reads;
            Hashtbl.find_opt memory address);
        accessed = (fun _ -> ());
      }
    in
    (memory, storage)
  in
  let memory, storage = make_storage () in
  let shallow_original = of_list_by ~storage ~cmp:compare (irange 0 100) in
  let shallow_root, _ = store shallow_original in
  (match restore ~cmp:compare storage shallow_root with
  | Some _ -> ()
  | None -> failwith "restore should return shallow stored set");
  assert_equal_string_list
    "restored shallow stored_addresses should include root and leaf addresses"
    [ shallow_root; "node-1"; "node-2"; "node-3"; "node-4" ]
    (stored_addresses memory shallow_root);
  let memory, storage = make_storage () in
  let nested_original = of_list_by ~storage ~cmp:compare (irange 0 1055) in
  let nested_root, _ = store nested_original in
  if List.length (stored_addresses memory nested_root) <> 36 then
    failwith
      "stored nested stored_addresses should include every stored descendant";
  (match restore ~cmp:compare storage nested_root with
  | Some _ -> ()
  | None -> failwith "restore should return nested stored set");
  assert_equal_string_list
    "restored nested stored_addresses should include root, branch, and leaf \
     addresses"
    ([ "node-36"; "node-34" ]
    @ List.init 32 (fun index -> "node-" ^ string_of_int (index + 1))
    @ [ "node-35"; "node-33" ])
    (stored_addresses memory nested_root)

let test_nested_storage_remove_reuses_unchanged_branch_addresses () =
  let memory = Hashtbl.create 64 in
  let writes = ref 0 in
  let storage =
    {
      store_node =
        (fun node ->
          incr writes;
          let address = "node-" ^ string_of_int !writes in
          Hashtbl.replace memory address node;
          address);
      restore_node = (fun address -> Hashtbl.find_opt memory address);
      accessed = (fun _ -> ());
    }
  in
  let original = of_list_by ~storage ~cmp:compare (irange 0 1055) in
  let root, stored = store original in
  if root <> "node-36" then failwith "initial nested root should be node-36";
  let removed = remove 50 stored in
  let removed_root, removed_stored = store removed in
  if removed_root = root then
    failwith "nested remove should create a new root address";
  assert_equal_int "nested remove rewrites changed leaf, one branch, and root"
    39 !writes;
  if removed_root <> "node-39" then
    failwith "nested remove root should be the third new node";
  (match Hashtbl.find_opt memory removed_root with
  | Some (Branch (keys, child_addresses)) ->
      assert_equal_list "nested remove root keeps old second branch max key"
        [ 1023; 1055 ] keys;
      assert_equal_list "nested remove root reuses unchanged second branch"
        [ "node-38"; "node-35" ] child_addresses
  | Some _ -> failwith "nested remove root should be a branch"
  | None -> failwith "nested remove root should be stored");
  (match Hashtbl.find_opt memory "node-38" with
  | Some (Branch (_, child_addresses)) ->
      let expected =
        List.init 32 (fun index ->
            if index = 1 then "node-37" else "node-" ^ string_of_int (index + 1))
      in
      assert_equal_list "nested remove rewrites only the changed leaf address"
        expected child_addresses
  | Some _ -> failwith "nested remove first branch should be a branch"
  | None -> failwith "nested remove changed branch should be stored");
  assert_equal_list "nested remove keeps sorted values"
    (irange 0 49 @ irange 51 1055)
    (to_list removed_stored)

let test_restored_nested_remove_reuses_unchanged_branch_addresses_lazily () =
  let memory = Hashtbl.create 64 in
  let writes = ref 0 in
  let reads = ref 0 in
  let accessed = ref [] in
  let storage =
    {
      store_node =
        (fun node ->
          incr writes;
          let address = "node-" ^ string_of_int !writes in
          Hashtbl.replace memory address node;
          address);
      restore_node =
        (fun address ->
          incr reads;
          Hashtbl.find_opt memory address);
      accessed = (fun address -> accessed := address :: !accessed);
    }
  in
  let root, _ = store (of_list_by ~storage ~cmp:compare (irange 0 1055)) in
  reads := 0;
  accessed := [];
  let restored =
    match restore ~cmp:compare storage root with
    | Some restored -> restored
    | None -> failwith "restore should find the stored root"
  in
  let removed = remove 50 restored in
  assert_equal_int
    "restored nested remove should read root, changed branch, and changed leaf"
    3 !reads;
  assert_equal_list "restored nested remove should access only the changed path"
    [ "node-2"; "node-34"; root ]
    !accessed;
  let removed_root, removed_stored = store removed in
  if removed_root = root then
    failwith "restored nested remove should create a new root address";
  assert_equal_int
    "restored nested remove rewrites changed leaf, one branch, and root" 39
    !writes;
  if removed_root <> "node-39" then
    failwith "restored nested remove root should be the third new node";
  (match Hashtbl.find_opt memory removed_root with
  | Some (Branch (keys, child_addresses)) ->
      assert_equal_list
        "restored nested remove root keeps old second branch max key"
        [ 1023; 1055 ] keys;
      assert_equal_list
        "restored nested remove root reuses unchanged second branch"
        [ "node-38"; "node-35" ] child_addresses
  | Some _ -> failwith "restored nested remove root should be a branch"
  | None -> failwith "restored nested remove root should be stored");
  assert_equal_list "restored nested remove keeps sorted values"
    (irange 0 49 @ irange 51 1055)
    (to_list removed_stored)

let test_restored_nested_add_reuses_unchanged_branch_addresses_lazily () =
  let memory = Hashtbl.create 64 in
  let writes = ref 0 in
  let reads = ref 0 in
  let accessed = ref [] in
  let storage =
    {
      store_node =
        (fun node ->
          incr writes;
          let address = "node-" ^ string_of_int !writes in
          Hashtbl.replace memory address node;
          address);
      restore_node =
        (fun address ->
          incr reads;
          Hashtbl.find_opt memory address);
      accessed = (fun address -> accessed := address :: !accessed);
    }
  in
  let original_values = irange 0 49 @ irange 51 1055 in
  let root, _ = store (of_list_by ~storage ~cmp:compare original_values) in
  reads := 0;
  accessed := [];
  let restored =
    match restore ~cmp:compare storage root with
    | Some restored -> restored
    | None -> failwith "restore should find the stored root"
  in
  let added = add 50 restored in
  assert_equal_int
    "restored nested add should read root, changed branch, and changed leaf" 3
    !reads;
  assert_equal_list "restored nested add should access only the changed path"
    [ "node-2"; "node-34"; root ]
    !accessed;
  let added_root, added_stored = store added in
  if added_root = root then
    failwith "restored nested add should create a new root address";
  assert_equal_int
    "restored nested add rewrites split leaves, split branches, and root" 41
    !writes;
  if added_root <> "node-41" then
    failwith "restored nested add root should be the fifth new node";
  (match Hashtbl.find_opt memory added_root with
  | Some (Branch (keys, child_addresses)) ->
      assert_equal_list
        "restored nested add root updates split first branch max keys"
        [ 992; 1024; 1055 ] keys;
      assert_equal_list
        "restored nested add root reuses unchanged second branch"
        [ "node-39"; "node-40"; "node-35" ]
        child_addresses
  | Some _ -> failwith "restored nested add root should be a branch"
  | None -> failwith "restored nested add root should be stored");
  assert_equal_list "restored nested add keeps sorted values" (irange 0 1055)
    (to_list added_stored)

let test_tree_slice_to_seq_seeks_lazily () =
  let comparisons = ref 0 in
  let cmp left right =
    incr comparisons;
    compare left right
  in
  let set = of_list_by ~cmp (irange 0 10_000) in
  comparisons := 0;
  let seq = slice_seq ~from_:5_000 ~to_:9_000 set |> to_seq in
  (match Seq.uncons seq with
  | Some (first, _) ->
      assert_equal_int "tree slice sequence starts at lower bound" 5_000 first
  | None -> failwith "tree slice sequence should return a first value");
  if !comparisons > 1_000 then
    failf
      "tree slice sequence should seek through branch bounds instead of \
       scanning the prefix: %d comparisons"
      !comparisons

let () =
  test_settings_control_storage_branching_factor ();
  test_restore_preserves_settings_for_later_edits ();
  test_settings_validate_branching_factor ();
  test_of_sorted_array_uses_sorted_input_and_settings ();
  test_sorted_order_and_uniqueness ();
  test_custom_comparator_and_uniqueness ();
  test_equal_comparator_slice_ranges ();
  test_restored_equal_comparator_slice_ranges ();
  test_pair_comparator_nil_wildcard_slices ();
  test_upstream_fractional_slice_boundaries ();
  test_slice_boundaries ();
  test_reverse_slice_boundaries ();
  test_seek ();
  test_slice_sequences_are_seekable_and_reversible ();
  test_fold_reduces_sets_and_sequences ();
  test_upstream_stresstest_btset_parity ();
  test_upstream_stresstest_slice_parity ();
  test_upstream_stresstest_rslice_parity ();
  test_upstream_stresstest_seek_parity ();
  test_upstream_overflow_batched_insert_smoke ();
  test_storage_round_trip_and_stable_addresses ();
  test_storage_uses_leaf_and_branch_nodes_for_large_sets ();
  test_storage_remove_preserves_unchanged_leaf_addresses ();
  test_storage_add_preserves_unchanged_leaf_addresses ();
  test_restored_add_preserves_unchanged_leaf_addresses_lazily ();
  test_chained_restored_adds_keep_edit_path_without_rebuilding ();
  test_restored_remove_preserves_unchanged_leaf_addresses_lazily ();
  test_restored_mem_reads_only_needed_leaves ();
  test_restored_slice_reads_only_overlapping_leaves ();
  test_restored_reverse_slice_reads_only_overlapping_leaves ();
  test_restored_seq_seek_is_lazy ();
  test_restored_rseq_seek_is_lazy ();
  test_restored_slice_seq_construction_is_lazy ();
  test_restored_rslice_seq_construction_is_lazy ();
  test_storage_uses_nested_branch_nodes_for_very_large_sets ();
  test_storage_addresses_visit_stored_descendants ();
  test_nested_storage_remove_reuses_unchanged_branch_addresses ();
  test_restored_nested_remove_reuses_unchanged_branch_addresses_lazily ();
  test_restored_nested_add_reuses_unchanged_branch_addresses_lazily ();
  test_tree_slice_to_seq_seeks_lazily ()
