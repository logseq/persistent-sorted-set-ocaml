open Persistent_sorted_set
open Js_of_ocaml

let set_result result =
  ignore
    (Js.Unsafe.fun_call
       (Js.Unsafe.js_expr
          {|
          (function(result) {
            globalThis.__PSS_MEMORY_TEST_RESULT = result;
            if (typeof document !== "undefined" && document.body) {
              document.body.setAttribute("data-pss-memory-result", result);
              document.body.textContent = result;
            }
          })
          |})
       [| Js.Unsafe.inject (Js.string result) |])

let failf fmt =
  Printf.ksprintf
    (fun message ->
      set_result ("fail: " ^ message);
      failwith message)
    fmt

let irange from_ to_ =
  let rec loop acc current =
    if current < from_ then acc else loop (current :: acc) (current - 1)
  in
  loop [] to_

let copy_stored_node = function
  | Leaf values -> Leaf values
  | Branch (keys, child_addresses) -> Branch (keys, child_addresses)

let set_timeout f milliseconds =
  ignore
    (Js.Unsafe.fun_call
       (Js.Unsafe.js_expr "setTimeout")
       [|
         Js.Unsafe.inject (Js.wrap_callback f);
         Js.Unsafe.inject (Js.number_of_float (float_of_int milliseconds));
       |])

let force_full_collection () =
  for _ = 1 to 5 do
    Gc.full_major ()
  done

let check ref_type label =
  let memory = Hashtbl.create 16 in
  let writes = ref 0 in
  let restored_slots = ref [] in
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
          match Hashtbl.find_opt memory address with
          | None -> None
          | Some stored ->
              let node = copy_stored_node stored in
              let slot = Weak.create 1 in
              Weak.set slot 0 (Some node);
              restored_slots := slot :: !restored_slots;
              Some node);
      accessed = (fun _ -> ());
    }
  in
  let settings = { branching_factor = 4; ref_type } in
  let root, _ =
    store (of_list_by ~storage ~settings ~cmp:compare (irange 0 15))
  in
  let restored =
    match restore ~cmp:compare ~settings storage root with
    | Some restored -> restored
    | None -> failf "%s restore should find the stored root" label
  in
  if not (mem 15 restored) then
    failf "%s restored set should contain stored value" label;
  if !restored_slots = [] then
    failf "%s test should observe restored nodes" label;
  !restored_slots

let () =
  let weak_slots = check Weak "weak" in
  set_timeout
    (fun () ->
      force_full_collection ();
      if List.exists (fun slot -> Weak.check slot 0) weak_slots then
        failf "weak ref-type should release restored nodes after JS GC"
      else set_result "pass")
    0
