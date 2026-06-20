type 'a comparator = 'a -> 'a -> int
type ref_type = Strong | Soft | Weak
type settings = { branching_factor : int; ref_type : ref_type }
type 'a stored_node = Leaf of 'a list | Branch of 'a list * string list

type 'a storage = {
  store_node : 'a stored_node -> string;
  restore_node : string -> 'a stored_node option;
  accessed : string -> unit;
}

module Node = struct
  type 'a t =
    | Ref of { max_key : 'a; address : string; mutable cached : 'a t option }
    | Leaf of { values : 'a array; len : int; address : string option }
    | Branch of {
        keys : 'a array;
        children : 'a t array;
        address : string option;
      }
end

type 'a node = 'a Node.t
type 'a data = Empty | Tree of 'a node | Deferred of { address : string }

type 'a t = {
  cmp : 'a comparator;
  set_settings : settings;
  set_storage : 'a storage option;
  data : 'a data;
  count_cache : int option;
}

type direction = Asc | Desc

type 'a seq_source =
  | Seq_empty
  | Seq_tree of { storage : 'a storage option; root : 'a node }
  | Seq_deferred of { storage : 'a storage; address : string }

type 'a seq = {
  set_cmp : 'a comparator;
  direction : direction;
  source : 'a seq_source;
  lower : 'a option;
  upper : 'a option;
}

let normalize_cmp cmp left right =
  match cmp left right with n when n < 0 -> -1 | 0 -> 0 | _ -> 1

let default_cmp left right = Stdlib.compare left right
let default_settings = { branching_factor = 32; ref_type = Soft }

let validate_settings settings =
  if settings.branching_factor < 2 then
    invalid_arg "branching_factor must be at least 2";
  settings

let settings set = set.set_settings

let cache_storage settings storage =
  let remember cache address node =
    let slot = Weak.create 1 in
    Weak.set slot 0 (Some node);
    Hashtbl.replace cache address slot
  in
  match settings.ref_type with
  | Strong ->
      let cache = Hashtbl.create 128 in
      {
        store_node =
          (fun node ->
            let address = storage.store_node node in
            Hashtbl.replace cache address node;
            address);
        restore_node =
          (fun address ->
            match Hashtbl.find_opt cache address with
            | Some node -> Some node
            | None -> (
                match storage.restore_node address with
                | Some node ->
                    Hashtbl.replace cache address node;
                    Some node
                | None -> None));
        accessed = storage.accessed;
      }
  | Soft | Weak ->
      let cache = Hashtbl.create 128 in
      {
        store_node =
          (fun node ->
            let address = storage.store_node node in
            remember cache address node;
            address);
        restore_node =
          (fun address ->
            match Hashtbl.find_opt cache address with
            | Some slot -> (
                match Weak.get slot 0 with
                | Some node -> Some node
                | None -> (
                    match storage.restore_node address with
                    | Some node ->
                        remember cache address node;
                        Some node
                    | None -> None))
            | None -> (
                match storage.restore_node address with
                | Some node ->
                    remember cache address node;
                    Some node
                | None -> None));
        accessed = storage.accessed;
      }

let empty_with_cmp ?storage settings cmp =
  let storage = Option.map (cache_storage settings) storage in
  {
    cmp;
    set_settings = settings;
    set_storage = storage;
    data = Empty;
    count_cache = Some 0;
  }

let empty_by ?(settings = default_settings) ?storage ?(cmp = default_cmp) () =
  let settings = validate_settings settings in
  empty_with_cmp ?storage settings (normalize_cmp cmp)

let empty () = empty_by ()

let chunks size values =
  let rec loop acc = function
    | [] -> List.rev acc
    | values ->
        let rec take count acc rest =
          if count = 0 then (List.rev acc, rest)
          else
            match rest with
            | [] -> (List.rev acc, [])
            | value :: rest -> take (count - 1) (value :: acc) rest
        in
        let chunk, rest = take size [] values in
        loop (chunk :: acc) rest
  in
  loop [] values

let prepend_array_prefix values len init =
  let acc = ref init in
  for i = len - 1 downto 0 do
    acc := values.(i) :: !acc
  done;
  !acc

let array_prefix_to_list values len = prepend_array_prefix values len []

let fold_array_prefix f init values len =
  let acc = ref init in
  for i = 0 to len - 1 do
    acc := f !acc values.(i)
  done;
  !acc

let prepend_list values init =
  List.fold_right (fun value acc -> value :: acc) values init

let storage_required = function
  | Some storage -> storage
  | None -> invalid_arg "storage-backed node requires storage"

let rec materialize_address_with storage address tail =
  storage.accessed address;
  match storage.restore_node address with
  | Some (Leaf values) -> prepend_list values (tail ())
  | Some (Branch (_, child_addresses)) ->
      materialize_addresses_with storage child_addresses tail
  | None -> invalid_arg ("stored node not found: " ^ address)

and materialize_addresses_with storage addresses tail =
  match addresses with
  | [] -> tail ()
  | address :: rest ->
      materialize_address_with storage address (fun () ->
          materialize_addresses_with storage rest tail)

let materialize_address storage address =
  materialize_address_with storage address (fun () -> [])

let rec materialize_node_into storage node acc =
  match node with
  | Node.Ref { address; _ } ->
      materialize_address_with (storage_required storage) address (fun () ->
          acc)
  | Node.Leaf { values; len; _ } -> prepend_array_prefix values len acc
  | Node.Branch { children; _ } ->
      let acc = ref acc in
      for i = Array.length children - 1 downto 0 do
        acc := materialize_node_into storage children.(i) !acc
      done;
      !acc

let materialize_node storage node = materialize_node_into storage node []

let materialize set =
  match set.data with
  | Empty -> []
  | Tree root -> materialize_node set.set_storage root
  | Deferred { address } ->
      materialize_address (storage_required set.set_storage) address

let root_node set =
  match set.data with Tree root -> Some root | Empty | Deferred _ -> None

let show_option_address = function None -> "none" | Some address -> address

let show_list show_value values =
  "[" ^ String.concat "; " (List.map show_value values) ^ "]"

let show_array show_value values = show_list show_value (Array.to_list values)

let show_node show_value root =
  let lines = ref [] in
  let add_line depth line =
    lines := (String.make (depth * 2) ' ' ^ line) :: !lines
  in
  let rec loop depth = function
    | Node.Ref { max_key; address; cached } ->
        add_line depth
          (Printf.sprintf "Ref(address=%s max_key=%s cached=%s)" address
             (show_value max_key)
             (match cached with None -> "none" | Some _ -> "some"));
        Option.iter (loop (depth + 1)) cached
    | Node.Leaf { values; len; address } ->
        add_line depth
          (Printf.sprintf "Leaf(address=%s len=%d values=%s)"
             (show_option_address address)
             len
             (show_list show_value (array_prefix_to_list values len)))
    | Node.Branch { keys; children; address } ->
        add_line depth
          (Printf.sprintf "Branch(address=%s keys=%s)"
             (show_option_address address)
             (show_array show_value keys));
        Array.iter (loop (depth + 1)) children
  in
  loop 0 root;
  String.concat "\n" (List.rev !lines)

let rec last = function
  | [] -> None
  | [ value ] -> Some value
  | _ :: rest -> last rest

let route_cmp order_cmp equality_cmp value key =
  match order_cmp value key with 0 -> equality_cmp value key | n -> n

let restore_stored_node storage address =
  storage.accessed address;
  match storage.restore_node address with
  | Some node -> node
  | None -> invalid_arg ("stored node not found: " ^ address)

let node_ref_key refs =
  match last refs with
  | Some (key, _) -> key
  | None -> invalid_arg "tree branch requires at least one child"

let node_branch_of_refs refs =
  let keys, children = List.split refs in
  Node.Branch
    {
      keys = Array.of_list keys;
      children = Array.of_list children;
      address = None;
    }

let node_leaf_refs_of_chunks chunks =
  chunks
  |> List.map (fun chunk ->
      let values = Array.of_list chunk in
      ( values.(Array.length values - 1),
        Node.Leaf { values; len = Array.length values; address = None } ))

let rec node_of_refs settings = function
  | [] -> None
  | [ (_, node) ] -> Some node
  | refs ->
      refs
      |> chunks settings.branching_factor
      |> List.map (fun refs -> (node_ref_key refs, node_branch_of_refs refs))
      |> node_of_refs settings

let data_of_sorted_values settings values =
  match
    values
    |> chunks settings.branching_factor
    |> node_leaf_refs_of_chunks |> node_of_refs settings
  with
  | None -> Empty
  | Some root -> Tree root

let data_of_changed_refs = function
  | [] -> Empty
  | [ (_, root) ] -> Tree root
  | children -> Tree (node_branch_of_refs children)

type 'a tree_edit_result =
  | Tree_edit_unchanged
  | Tree_edit_changed of ('a * 'a node) list

let array_insert_len values len index value =
  let result = Array.make (len + 1) value in
  Array.blit values 0 result 0 index;
  result.(index) <- value;
  Array.blit values index result (index + 1) (len - index);
  result

let array_remove_len values len index =
  if len = 1 then [||]
  else
    let first = if index = 0 then values.(1) else values.(0) in
    let result = Array.make (len - 1) first in
    Array.blit values 0 result 0 index;
    Array.blit values (index + 1) result index (len - index - 1);
    result

let array_split values =
  let length = Array.length values in
  let left_length = length / 2 in
  [
    Array.sub values 0 left_length;
    Array.sub values left_length (length - left_length);
  ]

let tree_leaf_refs_of_arrays arrays =
  arrays
  |> List.map (fun values ->
      ( values.(Array.length values - 1),
        Node.Leaf { values; len = Array.length values; address = None } ))

let tree_branch_refs_of_arrays settings keys children =
  let length = Array.length keys in
  if length = 0 then []
  else if length <= settings.branching_factor then
    [ (keys.(length - 1), Node.Branch { keys; children; address = None }) ]
  else
    let key_chunks = array_split keys in
    let child_chunks = array_split children in
    List.map2
      (fun keys children ->
        ( keys.(Array.length keys - 1),
          Node.Branch { keys; children; address = None } ))
      key_chunks child_chunks

let ref_arrays_of_list refs =
  let keys, children = List.split refs in
  (Array.of_list keys, Array.of_list children)

let branch_splice_one keys children index replacement =
  let length = Array.length keys in
  let replacement_keys, replacement_children = ref_arrays_of_list replacement in
  let replacement_length = Array.length replacement_keys in
  let result_length = length - 1 + replacement_length in
  if result_length = 0 then ([||], [||])
  else
    let first =
      if index > 0 then keys.(0)
      else if replacement_length > 0 then replacement_keys.(0)
      else keys.(1)
    in
    let first_child =
      if index > 0 then children.(0)
      else if replacement_length > 0 then replacement_children.(0)
      else children.(1)
    in
    let result_keys = Array.make result_length first in
    let result_children = Array.make result_length first_child in
    Array.blit keys 0 result_keys 0 index;
    Array.blit children 0 result_children 0 index;
    Array.blit replacement_keys 0 result_keys index replacement_length;
    Array.blit replacement_children 0 result_children index replacement_length;
    Array.blit keys (index + 1) result_keys
      (index + replacement_length)
      (length - index - 1);
    Array.blit children (index + 1) result_children
      (index + replacement_length)
      (length - index - 1);
    (result_keys, result_children)

let branch_replace_one settings keys children index key child =
  let keys = Array.copy keys in
  let children = Array.copy children in
  keys.(index) <- key;
  children.(index) <- child;
  tree_branch_refs_of_arrays settings keys children

let min_child_occupancy settings = max 1 ((settings.branching_factor + 1) / 2)

let array_append left right =
  let left_length = Array.length left in
  let right_length = Array.length right in
  if left_length = 0 then Array.copy right
  else if right_length = 0 then Array.copy left
  else
    let result = Array.make (left_length + right_length) left.(0) in
    Array.blit left 0 result 0 left_length;
    Array.blit right 0 result left_length right_length;
    result

let leaf_ref values =
  if Array.length values = 0 then invalid_arg "leaf ref requires values";
  ( values.(Array.length values - 1),
    Node.Leaf { values; len = Array.length values; address = None } )

let branch_ref keys children =
  let length = Array.length keys in
  if length = 0 then invalid_arg "branch ref requires keys";
  if length <> Array.length children then
    invalid_arg "branch keys and children arity mismatch";
  (keys.(length - 1), Node.Branch { keys; children; address = None })

let total_cmp order_cmp equality_cmp left right =
  match order_cmp left right with 0 -> equality_cmp left right | n -> n

let find_insert_index_len order_cmp equality_cmp value values length =
  let low = ref 0 in
  let high = ref (length - 1) in
  while !low <= !high do
    let middle = (!low + !high) / 2 in
    if total_cmp order_cmp equality_cmp values.(middle) value < 0 then
      low := middle + 1
    else high := middle - 1
  done;
  let index = !low in
  if index < length && total_cmp order_cmp equality_cmp values.(index) value = 0
  then `Found index
  else `Insert index

let find_remove_index_len order_cmp equality_cmp value values length =
  let low = ref 0 in
  let high = ref (length - 1) in
  while !low <= !high do
    let middle = (!low + !high) / 2 in
    if total_cmp order_cmp equality_cmp values.(middle) value < 0 then
      low := middle + 1
    else high := middle - 1
  done;
  let index = !low in
  if index < length && total_cmp order_cmp equality_cmp values.(index) value = 0
  then Some index
  else None

let find_index_by_cmp_len cmp value values length =
  let low = ref 0 in
  let high = ref (length - 1) in
  while !low <= !high do
    let middle = (!low + !high) / 2 in
    if cmp values.(middle) value < 0 then low := middle + 1
    else high := middle - 1
  done;
  let index = !low in
  if index < length && cmp values.(index) value = 0 then Some index else None

let find_index_by_cmp cmp value values =
  find_index_by_cmp_len cmp value values (Array.length values)

let array_mem_by_cmp cmp value values =
  match find_index_by_cmp cmp value values with Some _ -> true | None -> false

let array_mem_by_cmp_len cmp value values len =
  match find_index_by_cmp_len cmp value values len with
  | Some _ -> true
  | None -> false

let find_child_index key_cmp value keys =
  let length = Array.length keys in
  if length = 0 then invalid_arg "tree branch cannot be empty";
  let low = ref 0 in
  let high = ref (length - 1) in
  let best = ref (-1) in
  while !low <= !high do
    let middle = (!low + !high) / 2 in
    let key = keys.(middle) in
    if key_cmp value key <= 0 then (
      best := middle;
      high := middle - 1)
    else low := middle + 1
  done;
  if !best < 0 then length - 1 else !best

let node_of_stored_branch keys child_addresses address =
  if List.length keys <> List.length child_addresses then
    invalid_arg "branch keys and addresses arity mismatch";
  let keys = Array.of_list keys in
  let child_addresses = Array.of_list child_addresses in
  let children =
    Array.mapi
      (fun index child_address ->
        let key = keys.(index) in
        Node.Ref { max_key = key; address = child_address; cached = None })
      child_addresses
  in
  Node.Branch { keys = Array.copy keys; children; address = Some address }

let node_of_stored_node storage address =
  match restore_stored_node storage address with
  | Leaf values ->
      let values = Array.of_list values in
      Node.Leaf { values; len = Array.length values; address = Some address }
  | Branch (keys, child_addresses) ->
      node_of_stored_branch keys child_addresses address

let force_ref_node storage = function
  | Node.Ref ref_ -> (
      match ref_.cached with
      | Some node -> node
      | None ->
          let node = node_of_stored_node storage ref_.address in
          ref_.cached <- Some node;
          node)
  | node -> node

type 'a node_edit_mode = Pure_tree | Stored_tree of 'a storage

let storage_of_edit_mode = function
  | Stored_tree storage -> storage
  | Pure_tree -> invalid_arg "storage-backed node requires storage-aware edit"

let edit_mode_of_storage = function
  | Some storage -> Stored_tree storage
  | None -> Pure_tree

let node_leaf_values mode = function
  | Node.Leaf { values; len; _ } ->
      Some
        (if len = Array.length values then values else Array.sub values 0 len)
  | Node.Ref _ as node -> (
      match mode with
      | Pure_tree -> None
      | Stored_tree storage -> (
          match force_ref_node storage node with
          | Node.Leaf { values; len; _ } ->
              Some
                (if len = Array.length values then values
                 else Array.sub values 0 len)
          | Node.Ref _ | Node.Branch _ -> None))
  | Node.Branch _ -> None

let node_branch_parts mode = function
  | Node.Branch { keys; children; _ } -> Some (keys, children)
  | Node.Ref _ as node -> (
      match mode with
      | Pure_tree -> None
      | Stored_tree storage -> (
          match force_ref_node storage node with
          | Node.Branch { keys; children; _ } -> Some (keys, children)
          | Node.Ref _ | Node.Leaf _ -> None))
  | Node.Leaf _ -> None

let add_leaf_refs settings inserted =
  let changed =
    if Array.length inserted <= settings.branching_factor then [ inserted ]
    else array_split inserted
  in
  tree_leaf_refs_of_arrays changed

let branch_refs_of_arrays settings keys children =
  tree_branch_refs_of_arrays settings keys children

let branch_replace_range settings keys children start remove_count replacements
    =
  let refs = ref [] in
  for index = Array.length children - 1 downto 0 do
    if index = start then refs := replacements @ !refs;
    if index < start || index >= start + remove_count then
      refs := (keys.(index), children.(index)) :: !refs
  done;
  let keys, children = ref_arrays_of_list !refs in
  branch_refs_of_arrays settings keys children

let rebalance_leaf_child mode settings keys children index child =
  match node_leaf_values mode child with
  | None -> None
  | Some values -> (
      let minimum = min_child_occupancy settings in
      if Array.length values >= minimum then None
      else
        let rebalance_with_right () =
          if index + 1 >= Array.length children then None
          else
            match node_leaf_values mode children.(index + 1) with
            | None -> None
            | Some right ->
                let combined_length =
                  Array.length values + Array.length right
                in
                if combined_length <= settings.branching_factor then
                  let merged = array_append values right in
                  Some
                    (branch_replace_range settings keys children index 2
                       [ leaf_ref merged ])
                else
                  let needed = minimum - Array.length values in
                  if needed <= 0 || Array.length right - needed < minimum then
                    None
                  else
                    let borrowed = Array.sub right 0 needed in
                    let left = array_append values borrowed in
                    let right =
                      Array.sub right needed (Array.length right - needed)
                    in
                    Some
                      (branch_replace_range settings keys children index 2
                         [ leaf_ref left; leaf_ref right ])
        in
        let rebalance_with_left () =
          if index = 0 then None
          else
            match node_leaf_values mode children.(index - 1) with
            | None -> None
            | Some left ->
                let combined_length = Array.length left + Array.length values in
                if combined_length <= settings.branching_factor then
                  let merged = array_append left values in
                  Some
                    (branch_replace_range settings keys children (index - 1) 2
                       [ leaf_ref merged ])
                else
                  let needed = minimum - Array.length values in
                  if needed <= 0 || Array.length left - needed < minimum then
                    None
                  else
                    let left_keep = Array.length left - needed in
                    let borrowed = Array.sub left left_keep needed in
                    let left = Array.sub left 0 left_keep in
                    let right = array_append borrowed values in
                    Some
                      (branch_replace_range settings keys children (index - 1) 2
                         [ leaf_ref left; leaf_ref right ])
        in
        match rebalance_with_right () with
        | Some changed -> Some changed
        | None -> rebalance_with_left ())

let rebalance_branch_child mode settings keys children index child =
  match node_branch_parts mode child with
  | None -> None
  | Some (child_keys, child_children) -> (
      let minimum = min_child_occupancy settings in
      if Array.length child_keys >= minimum then None
      else
        let rebalance_with_right () =
          if index + 1 >= Array.length children then None
          else
            match node_branch_parts mode children.(index + 1) with
            | None -> None
            | Some (right_keys, right_children) ->
                let combined_length =
                  Array.length child_keys + Array.length right_keys
                in
                if combined_length <= settings.branching_factor then
                  let merged_keys = array_append child_keys right_keys in
                  let merged_children =
                    array_append child_children right_children
                  in
                  Some
                    (branch_replace_range settings keys children index 2
                       [ branch_ref merged_keys merged_children ])
                else
                  let needed = minimum - Array.length child_keys in
                  if needed <= 0 || Array.length right_keys - needed < minimum
                  then None
                  else
                    let borrowed_keys = Array.sub right_keys 0 needed in
                    let borrowed_children = Array.sub right_children 0 needed in
                    let left_keys = array_append child_keys borrowed_keys in
                    let left_children =
                      array_append child_children borrowed_children
                    in
                    let right_keys =
                      Array.sub right_keys needed
                        (Array.length right_keys - needed)
                    in
                    let right_children =
                      Array.sub right_children needed
                        (Array.length right_children - needed)
                    in
                    Some
                      (branch_replace_range settings keys children index 2
                         [
                           branch_ref left_keys left_children;
                           branch_ref right_keys right_children;
                         ])
        in
        let rebalance_with_left () =
          if index = 0 then None
          else
            match node_branch_parts mode children.(index - 1) with
            | None -> None
            | Some (left_keys, left_children) ->
                let combined_length =
                  Array.length left_keys + Array.length child_keys
                in
                if combined_length <= settings.branching_factor then
                  let merged_keys = array_append left_keys child_keys in
                  let merged_children =
                    array_append left_children child_children
                  in
                  Some
                    (branch_replace_range settings keys children (index - 1) 2
                       [ branch_ref merged_keys merged_children ])
                else
                  let needed = minimum - Array.length child_keys in
                  if needed <= 0 || Array.length left_keys - needed < minimum
                  then None
                  else
                    let left_keep = Array.length left_keys - needed in
                    let borrowed_keys = Array.sub left_keys left_keep needed in
                    let borrowed_children =
                      Array.sub left_children left_keep needed
                    in
                    let left_keys = Array.sub left_keys 0 left_keep in
                    let left_children = Array.sub left_children 0 left_keep in
                    let right_keys = array_append borrowed_keys child_keys in
                    let right_children =
                      array_append borrowed_children child_children
                    in
                    Some
                      (branch_replace_range settings keys children (index - 1) 2
                         [
                           branch_ref left_keys left_children;
                           branch_ref right_keys right_children;
                         ])
        in
        match rebalance_with_right () with
        | Some changed -> Some changed
        | None -> rebalance_with_left ())

let rec add_to_address storage settings order_cmp equality_cmp key_cmp value
    address =
  match restore_stored_node storage address with
  | Leaf values ->
      add_to_node (Stored_tree storage) settings order_cmp equality_cmp key_cmp
        value
        (Node.Leaf
           {
             values = Array.of_list values;
             len = List.length values;
             address = Some address;
           })
  | Branch (keys, child_addresses) ->
      add_to_node (Stored_tree storage) settings order_cmp equality_cmp key_cmp
        value
        (node_of_stored_branch keys child_addresses address)

and add_to_node mode settings order_cmp equality_cmp key_cmp value = function
  | Node.Ref _ as node ->
      let storage = storage_of_edit_mode mode in
      add_to_node mode settings order_cmp equality_cmp key_cmp value
        (force_ref_node storage node)
  | Node.Leaf { values; len; _ } -> (
      match find_insert_index_len order_cmp equality_cmp value values len with
      | `Found _ -> Tree_edit_unchanged
      | `Insert index ->
          let inserted = array_insert_len values len index value in
          Tree_edit_changed (add_leaf_refs settings inserted))
  | Node.Branch { keys; children; _ } -> (
      let index = find_child_index key_cmp value keys in
      match
        add_to_node mode settings order_cmp equality_cmp key_cmp value
          children.(index)
      with
      | Tree_edit_unchanged -> Tree_edit_unchanged
      | Tree_edit_changed [ (key, child) ] ->
          branch_replace_one settings keys children index key child
          |> fun changed -> Tree_edit_changed changed
      | Tree_edit_changed changed ->
          let keys, children = branch_splice_one keys children index changed in
          branch_refs_of_arrays settings keys children |> fun changed ->
          Tree_edit_changed changed)

let rec remove_from_address storage settings order_cmp equality_cmp key_cmp
    value address =
  match restore_stored_node storage address with
  | Leaf values ->
      remove_from_node (Stored_tree storage) settings order_cmp equality_cmp
        key_cmp value
        (Node.Leaf
           {
             values = Array.of_list values;
             len = List.length values;
             address = Some address;
           })
  | Branch (keys, child_addresses) ->
      remove_from_node (Stored_tree storage) settings order_cmp equality_cmp
        key_cmp value
        (node_of_stored_branch keys child_addresses address)

and remove_from_node mode settings order_cmp equality_cmp key_cmp value =
  function
  | Node.Ref _ as node ->
      let storage = storage_of_edit_mode mode in
      remove_from_node mode settings order_cmp equality_cmp key_cmp value
        (force_ref_node storage node)
  | Node.Leaf { values; len; _ } -> (
      match find_remove_index_len order_cmp equality_cmp value values len with
      | None -> Tree_edit_unchanged
      | Some index -> (
          array_remove_len values len index |> function
          | [||] -> Tree_edit_changed []
          | values ->
              Tree_edit_changed
                [
                  ( values.(Array.length values - 1),
                    Node.Leaf
                      { values; len = Array.length values; address = None } );
                ]))
  | Node.Branch { keys; children; _ } -> (
      let index = find_child_index key_cmp value keys in
      match
        remove_from_node mode settings order_cmp equality_cmp key_cmp value
          children.(index)
      with
      | Tree_edit_unchanged -> Tree_edit_unchanged
      | Tree_edit_changed [ (key, child) ] ->
          let changed =
            match
              rebalance_leaf_child mode settings keys children index child
            with
            | Some changed -> changed
            | None -> (
                match
                  rebalance_branch_child mode settings keys children index child
                with
                | Some changed -> changed
                | None ->
                    branch_replace_one settings keys children index key child)
          in
          Tree_edit_changed changed
      | Tree_edit_changed changed ->
          let keys, children = branch_splice_one keys children index changed in
          branch_refs_of_arrays settings keys children |> fun changed ->
          Tree_edit_changed changed)

let add value set =
  let equality_cmp = set.cmp in
  let key_cmp = set.cmp in
  let increment_count_cache set =
    {
      set with
      count_cache = Option.map (fun count -> count + 1) set.count_cache;
    }
  in
  match set.data with
  | Deferred { address } -> (
      let storage = storage_required set.set_storage in
      match
        add_to_address storage set.set_settings set.cmp equality_cmp key_cmp
          value address
      with
      | Tree_edit_unchanged -> set
      | Tree_edit_changed changed ->
          increment_count_cache { set with data = data_of_changed_refs changed }
      )
  | Tree root -> (
      match
        add_to_node
          (edit_mode_of_storage set.set_storage)
          set.set_settings set.cmp equality_cmp key_cmp value root
      with
      | Tree_edit_unchanged -> set
      | Tree_edit_changed changed ->
          increment_count_cache
            {
              set with
              data =
                (match node_of_refs set.set_settings changed with
                | None -> Empty
                | Some root -> Tree root);
            })
  | Empty ->
      {
        set with
        data = data_of_sorted_values set.set_settings [ value ];
        count_cache = Some 1;
      }

let distinct_sorted_values cmp values =
  let rec loop acc = function
    | [] -> List.rev acc
    | value :: rest -> (
        match acc with
        | previous :: _ when cmp previous value = 0 -> loop acc rest
        | _ -> loop (value :: acc) rest)
  in
  loop [] values

let distinct_sorted_array_values cmp values =
  let acc = ref [] in
  let previous = ref None in
  for i = 0 to Array.length values - 1 do
    let value = values.(i) in
    match !previous with
    | Some previous when cmp previous value = 0 -> ()
    | _ ->
        acc := value :: !acc;
        previous := Some value
  done;
  List.rev !acc

let of_list_by ?settings ?storage ?cmp values =
  let set = empty_by ?settings ?storage ?cmp () in
  let values = Array.of_list values in
  Array.stable_sort set.cmp values;
  let values = distinct_sorted_array_values set.cmp values in
  {
    set with
    data = data_of_sorted_values set.set_settings values;
    count_cache = Some (List.length values);
  }

let of_list values = of_list_by values

let of_sorted_array_by ?settings ?storage ?cmp values =
  let set = empty_by ?settings ?storage ?cmp () in
  let values_list = ref [] in
  for i = Array.length values - 1 downto 0 do
    values_list := values.(i) :: !values_list
  done;
  let values = distinct_sorted_values set.cmp !values_list in
  {
    set with
    data = data_of_sorted_values set.set_settings values;
    count_cache = Some (List.length values);
  }

let of_sorted_array values = of_sorted_array_by values

let remove value set =
  let equality_cmp = set.cmp in
  let key_cmp = set.cmp in
  let decrement_count_cache set =
    {
      set with
      count_cache = Option.map (fun count -> count - 1) set.count_cache;
    }
  in
  match set.data with
  | Deferred { address } -> (
      let storage = storage_required set.set_storage in
      match
        remove_from_address storage set.set_settings set.cmp equality_cmp
          key_cmp value address
      with
      | Tree_edit_unchanged -> set
      | Tree_edit_changed changed ->
          decrement_count_cache { set with data = data_of_changed_refs changed }
      )
  | Tree root -> (
      match
        remove_from_node
          (edit_mode_of_storage set.set_storage)
          set.set_settings set.cmp equality_cmp key_cmp value root
      with
      | Tree_edit_unchanged -> set
      | Tree_edit_changed changed ->
          decrement_count_cache
            {
              set with
              data =
                (match node_of_refs set.set_settings changed with
                | None -> Empty
                | Some root -> Tree root);
            })
  | Empty -> set

type 'a invariant_info = {
  min_key : 'a option;
  max_key : 'a option;
  count : int option;
  height : int option;
}

let invariant_error message =
  invalid_arg ("persistent sorted set invariant violation: " ^ message)

let require_invariant condition message =
  if not condition then invariant_error message

let option_map2 f left right =
  match (left, right) with
  | Some left, Some right -> Some (f left right)
  | _ -> None

let validate_leaf_invariants cmp settings values len =
  require_invariant (len > 0) "leaf must not be empty";
  require_invariant
    (len <= Array.length values)
    "leaf len must not exceed array capacity";
  require_invariant
    (len <= settings.branching_factor)
    "leaf len must not exceed branching_factor";
  for index = 1 to len - 1 do
    require_invariant
      (cmp values.(index - 1) values.(index) < 0)
      "leaf values must be strictly sorted and unique"
  done;
  {
    min_key = Some values.(0);
    max_key = Some values.(len - 1);
    count = Some len;
    height = Some 0;
  }

let rec validate_node_invariants storage cmp settings is_root = function
  | Node.Ref { max_key; _ } ->
      require_invariant (Option.is_some storage)
        "stored ref requires storage on the set";
      { min_key = None; max_key = Some max_key; count = None; height = None }
  | Node.Leaf { values; len; _ } ->
      validate_leaf_invariants cmp settings values len
  | Node.Branch { keys; children; _ } ->
      validate_branch_invariants storage cmp settings is_root keys children

and validate_branch_invariants storage cmp settings is_root keys children =
  let length = Array.length keys in
  require_invariant (length > 0) "branch must not be empty";
  require_invariant
    (length = Array.length children)
    "branch key count must equal child count";
  require_invariant
    (length <= settings.branching_factor)
    "branch child count must not exceed branching_factor";
  require_invariant
    (is_root || length >= 1)
    "non-root branch must have at least one child";
  let total_count = ref (Some 0) in
  let expected_height = ref None in
  let first_min = ref None in
  let previous_max = ref None in
  for index = 0 to length - 1 do
    if index > 0 then
      require_invariant
        (cmp keys.(index - 1) keys.(index) < 0)
        "branch keys must be strictly sorted";
    let child_info =
      validate_node_invariants storage cmp settings false children.(index)
    in
    (match child_info.max_key with
    | Some max_key ->
        require_invariant
          (cmp keys.(index) max_key = 0)
          "branch key must equal child max key"
    | None -> invariant_error "child max key must be known");
    (match (!previous_max, child_info.min_key) with
    | Some previous_max, Some child_min ->
        require_invariant
          (cmp previous_max child_min < 0)
          "child ranges must be disjoint and ordered"
    | _ -> ());
    if index = 0 then first_min := child_info.min_key;
    previous_max := child_info.max_key;
    total_count := option_map2 ( + ) !total_count child_info.count;
    match (!expected_height, child_info.height) with
    | None, Some height -> expected_height := Some height
    | Some expected, Some height ->
        require_invariant (expected = height)
          "all children under a branch must have the same height"
    | _ -> ()
  done;
  {
    min_key = !first_min;
    max_key = Some keys.(length - 1);
    count = !total_count;
    height = Option.map (fun height -> height + 1) !expected_height;
  }

let validate_invariants set =
  match set.data with
  | Empty -> (
      match set.count_cache with
      | Some 0 | None -> ()
      | Some _ -> invariant_error "empty set count cache must be zero")
  | Deferred _ ->
      require_invariant
        (Option.is_some set.set_storage)
        "deferred set requires storage"
  | Tree root -> (
      let info =
        validate_node_invariants set.set_storage set.cmp set.set_settings true
          root
      in
      match set.count_cache with
      | Some expected -> (
          match info.count with
          | Some actual ->
              require_invariant (expected = actual)
                "count cache must match actual tree cardinality"
          | None -> ())
      | None -> ())

type search_step = Found | Stop | Continue

let rec search_deferred storage order_cmp equality_cmp value address =
  storage.accessed address;
  match storage.restore_node address with
  | Some (Leaf values) ->
      let values = Array.of_list values in
      let cmp = total_cmp order_cmp equality_cmp in
      if array_mem_by_cmp cmp value values then Found
      else
        let length = Array.length values in
        if length > 0 && order_cmp value values.(length - 1) <= 0 then Stop
        else Continue
  | Some (Branch (keys, child_addresses)) -> (
      if List.length keys <> List.length child_addresses then
        invalid_arg "branch keys and addresses arity mismatch";
      let keys = Array.of_list keys in
      let child_addresses = Array.of_list child_addresses in
      let key_cmp = route_cmp order_cmp equality_cmp in
      let child_address =
        let length = Array.length keys in
        if length = 0 || key_cmp value keys.(length - 1) > 0 then None
        else Some child_addresses.(find_child_index key_cmp value keys)
      in
      match child_address with
      | Some child_address ->
          search_deferred storage order_cmp equality_cmp value child_address
      | None -> Continue)
  | None -> invalid_arg ("stored node not found: " ^ address)

let mem_in_deferred storage order_cmp equality_cmp value address =
  match search_deferred storage order_cmp equality_cmp value address with
  | Found -> true
  | Stop | Continue -> false

let rec search_node_by_cmp storage cmp value = function
  | Node.Ref _ as node ->
      search_node_by_cmp storage cmp value
        (force_ref_node (storage_required storage) node)
  | Node.Leaf { values; len; _ } ->
      if array_mem_by_cmp_len cmp value values len then Found
      else if len > 0 && cmp value values.(len - 1) <= 0 then Stop
      else Continue
  | Node.Branch { keys; children; _ } ->
      let index = find_child_index cmp value keys in
      search_node_by_cmp storage cmp value children.(index)

let mem_in_node_by_cmp storage cmp value node =
  match search_node_by_cmp storage cmp value node with
  | Found -> true
  | Stop | Continue -> false

let mem value set =
  match set.data with
  | Empty -> false
  | Tree root -> mem_in_node_by_cmp set.set_storage set.cmp value root
  | Deferred { address } ->
      mem_in_deferred
        (storage_required set.set_storage)
        set.cmp set.cmp value address

let rec fold_address storage f init address =
  storage.accessed address;
  match storage.restore_node address with
  | Some (Leaf values) -> List.fold_left f init values
  | Some (Branch (_, child_addresses)) ->
      List.fold_left
        (fun acc child_address -> fold_address storage f acc child_address)
        init child_addresses
  | None -> invalid_arg ("stored node not found: " ^ address)

let rec fold_node storage f init = function
  | Node.Ref _ as node ->
      fold_node storage f init (force_ref_node (storage_required storage) node)
  | Node.Leaf { values; len; _ } -> fold_array_prefix f init values len
  | Node.Branch { children; _ } ->
      Array.fold_left
        (fun acc child -> fold_node storage f acc child)
        init children

let count (set : 'a t) =
  match set.count_cache with
  | Some count -> count
  | None -> (
      match set.data with
      | Tree root -> fold_node set.set_storage (fun count _ -> count + 1) 0 root
      | Empty -> 0
      | Deferred { address } ->
          fold_address
            (storage_required set.set_storage)
            (fun count _ -> count + 1)
            0 address)

let to_list (set : 'a t) = materialize set

let fold f init set =
  match set.data with
  | Empty -> init
  | Tree root -> fold_node set.set_storage f init root
  | Deferred { address } ->
      fold_address (storage_required set.set_storage) f init address

let fold_list f init values = List.fold_left f init values

let lower_ok cmp from_ value =
  match from_ with None -> true | Some from_ -> cmp value from_ >= 0

let upper_ok cmp to_ value =
  match to_ with None -> true | Some to_ -> cmp value to_ <= 0

let child_before_range cmp from_ child_max =
  match from_ with None -> false | Some from_ -> cmp child_max from_ < 0

let child_after_range cmp to_ previous_child_max =
  match (to_, previous_child_max) with
  | Some to_, Some previous_child_max -> cmp previous_child_max to_ > 0
  | _ -> false

let slice_values cmp from_ to_ values =
  values
  |> List.filter (fun value ->
      lower_ok cmp from_ value && upper_ok cmp to_ value)

let reverse_slice_values cmp from_ to_ values =
  values |> List.rev
  |> List.filter (fun value ->
      match (from_, to_) with
      | None, None -> true
      | Some from_, None -> cmp value from_ <= 0
      | None, Some to_ -> cmp value to_ >= 0
      | Some from_, Some to_ -> cmp value from_ <= 0 && cmp value to_ >= 0)

let slice_array_into_len cmp from_ to_ values len acc =
  let rec loop acc index =
    if index >= len then acc
    else
      let value = values.(index) in
      if not (lower_ok cmp from_ value) then loop acc (index + 1)
      else if not (upper_ok cmp to_ value) then acc
      else loop (value :: acc) (index + 1)
  in
  loop acc 0

let reverse_slice_array_into_len cmp from_ to_ values len acc =
  let rec loop acc index =
    if index < 0 then acc
    else
      let value = values.(index) in
      let in_range =
        match (from_, to_) with
        | None, None -> true
        | Some from_, None -> cmp value from_ <= 0
        | None, Some to_ -> cmp value to_ >= 0
        | Some from_, Some to_ -> cmp value from_ <= 0 && cmp value to_ >= 0
      in
      if in_range then loop (value :: acc) (index - 1)
      else
        match from_ with
        | Some from_ when cmp value from_ > 0 -> loop acc (index - 1)
        | _ -> acc
  in
  loop acc (len - 1)

let rec slice_deferred_into storage cmp from_ to_ address acc =
  storage.accessed address;
  match storage.restore_node address with
  | Some (Leaf values) ->
      List.rev_append (slice_values cmp from_ to_ values) acc
  | Some (Branch (keys, child_addresses)) ->
      let rec collect previous_key acc keys child_addresses =
        match (keys, child_addresses) with
        | [], [] -> acc
        | key :: keys, child_address :: child_addresses ->
            if child_after_range cmp to_ previous_key then acc
            else if child_before_range cmp from_ key then
              collect (Some key) acc keys child_addresses
            else
              collect (Some key)
                (slice_deferred_into storage cmp from_ to_ child_address acc)
                keys child_addresses
        | [], _ :: _ | _ :: _, [] ->
            invalid_arg "branch keys and addresses arity mismatch"
      in
      collect None acc keys child_addresses
  | None -> invalid_arg ("stored node not found: " ^ address)

let slice_deferred storage cmp from_ to_ address =
  List.rev (slice_deferred_into storage cmp from_ to_ address [])

let rec reverse_slice_deferred_into storage cmp from_ to_ address acc =
  storage.accessed address;
  match storage.restore_node address with
  | Some (Leaf values) ->
      List.rev_append (reverse_slice_values cmp from_ to_ values) acc
  | Some (Branch (keys, child_addresses)) ->
      let rec collect previous_key keys child_addresses acc =
        match (keys, child_addresses) with
        | [], [] -> acc
        | key :: keys, child_address :: child_addresses ->
            let acc = collect (Some key) keys child_addresses acc in
            let child_above_range =
              match (from_, previous_key) with
              | Some from_, Some previous_key -> cmp previous_key from_ > 0
              | _ -> false
            in
            let child_below_range =
              match to_ with Some to_ -> cmp key to_ < 0 | None -> false
            in
            if child_above_range then acc
            else if child_below_range then acc
            else
              reverse_slice_deferred_into storage cmp from_ to_ child_address
                acc
        | [], _ :: _ | _ :: _, [] ->
            invalid_arg "branch keys and addresses arity mismatch"
      in
      collect None keys child_addresses acc
  | None -> invalid_arg ("stored node not found: " ^ address)

let reverse_slice_deferred storage cmp from_ to_ address =
  List.rev (reverse_slice_deferred_into storage cmp from_ to_ address [])

let rec slice_tree_into storage cmp from_ to_ node acc =
  match node with
  | Node.Ref _ ->
      slice_tree_into storage cmp from_ to_
        (force_ref_node (storage_required storage) node)
        acc
  | Node.Leaf { values; len; _ } ->
      slice_array_into_len cmp from_ to_ values len acc
  | Node.Branch { keys; children; _ } ->
      let rec collect previous_key acc index =
        if index >= Array.length children then acc
        else
          let key = keys.(index) in
          if child_after_range cmp to_ previous_key then acc
          else if child_before_range cmp from_ key then
            collect (Some key) acc (index + 1)
          else
            let acc =
              slice_tree_into storage cmp from_ to_ children.(index) acc
            in
            collect (Some key) acc (index + 1)
      in
      collect None acc 0

let slice_tree storage cmp from_ to_ root =
  List.rev (slice_tree_into storage cmp from_ to_ root [])

let rec reverse_slice_tree_into storage cmp from_ to_ node acc =
  match node with
  | Node.Ref _ ->
      reverse_slice_tree_into storage cmp from_ to_
        (force_ref_node (storage_required storage) node)
        acc
  | Node.Leaf { values; len; _ } ->
      reverse_slice_array_into_len cmp from_ to_ values len acc
  | Node.Branch { keys; children; _ } ->
      let rec collect acc index =
        if index < 0 then acc
        else
          let key = keys.(index) in
          let previous_key =
            if index = 0 then None else Some keys.(index - 1)
          in
          let child_above_range =
            match (from_, previous_key) with
            | Some from_, Some previous_key -> cmp previous_key from_ > 0
            | _ -> false
          in
          let child_below_range =
            match to_ with Some to_ -> cmp key to_ < 0 | None -> false
          in
          if child_above_range then collect acc (index - 1)
          else if child_below_range then acc
          else
            let acc =
              reverse_slice_tree_into storage cmp from_ to_ children.(index) acc
            in
            collect acc (index - 1)
      in
      collect acc (Array.length children - 1)

let reverse_slice_tree storage cmp from_ to_ root =
  List.rev (reverse_slice_tree_into storage cmp from_ to_ root [])

type 'a cursor_child = Cursor_node of 'a node | Cursor_address of string

type 'a cursor_frame = {
  keys : 'a array;
  children : 'a cursor_child array;
  mutable index : int;
}

type 'a cursor = {
  storage : 'a storage option;
  cmp : 'a comparator;
  direction : direction;
  lower : 'a option;
  upper : 'a option;
  root : 'a cursor_child;
  mutable initialized : bool;
  mutable stack : 'a cursor_frame list;
  mutable leaf : 'a array option;
  mutable leaf_len : int;
  mutable leaf_index : int;
}

let first_index_for_lower cmp lower keys =
  match lower with
  | None -> Some 0
  | Some lower ->
      let length = Array.length keys in
      if length = 0 then None
      else if cmp lower keys.(length - 1) > 0 then None
      else Some (find_child_index cmp lower keys)

let first_index_for_upper cmp upper keys =
  match upper with
  | None -> if Array.length keys = 0 then None else Some (Array.length keys - 1)
  | Some upper ->
      let length = Array.length keys in
      if length = 0 then None else Some (find_child_index cmp upper keys)

let lower_bound_index_len cmp lower values length =
  match lower with
  | None -> 0
  | Some lower ->
      let low = ref 0 in
      let high = ref (length - 1) in
      while !low <= !high do
        let middle = (!low + !high) / 2 in
        if cmp values.(middle) lower < 0 then low := middle + 1
        else high := middle - 1
      done;
      !low

let upper_bound_index_len cmp upper values length =
  match upper with
  | None -> length - 1
  | Some upper ->
      let low = ref 0 in
      let high = ref (length - 1) in
      while !low <= !high do
        let middle = (!low + !high) / 2 in
        if cmp values.(middle) upper <= 0 then low := middle + 1
        else high := middle - 1
      done;
      !low - 1

let cursor_children_of_addresses keys child_addresses =
  if List.length keys <> List.length child_addresses then
    invalid_arg "branch keys and addresses arity mismatch";
  ( Array.of_list keys,
    child_addresses
    |> List.map (fun address -> Cursor_address address)
    |> Array.of_list )

let cursor_children_of_nodes children =
  Array.map (fun child -> Cursor_node child) children

let restore_cursor_child storage address =
  storage.accessed address;
  match storage.restore_node address with
  | Some (Leaf values) ->
      let values = Array.of_list values in
      `Leaf (values, Array.length values)
  | Some (Branch (keys, child_addresses)) ->
      let keys, children = cursor_children_of_addresses keys child_addresses in
      `Branch (keys, children)
  | None -> invalid_arg ("stored node not found: " ^ address)

let rec read_cursor_child storage = function
  | Cursor_node (Node.Ref _ as node) ->
      read_cursor_child storage
        (Cursor_node (force_ref_node (storage_required storage) node))
  | Cursor_node (Node.Leaf { values; len; _ }) -> `Leaf (values, len)
  | Cursor_node (Node.Branch { keys; children; _ }) ->
      `Branch (keys, cursor_children_of_nodes children)
  | Cursor_address address ->
      restore_cursor_child (storage_required storage) address

let set_cursor_leaf cursor values len =
  cursor.leaf <- Some values;
  cursor.leaf_len <- len;
  cursor.leaf_index <-
    (match cursor.direction with
    | Asc -> lower_bound_index_len cursor.cmp cursor.lower values len
    | Desc -> upper_bound_index_len cursor.cmp cursor.upper values len)

let push_cursor_branch cursor keys children =
  if Array.length keys <> Array.length children then
    invalid_arg "branch keys and children arity mismatch";
  let index =
    match cursor.direction with
    | Asc -> first_index_for_lower cursor.cmp cursor.lower keys
    | Desc -> first_index_for_upper cursor.cmp cursor.upper keys
  in
  match index with
  | None -> ()
  | Some index -> cursor.stack <- { keys; children; index } :: cursor.stack

let rec load_cursor_child cursor child =
  match read_cursor_child cursor.storage child with
  | `Leaf (values, len) -> set_cursor_leaf cursor values len
  | `Branch (keys, children) ->
      push_cursor_branch cursor keys children;
      load_next_cursor_leaf cursor

and next_cursor_child cursor =
  match cursor.stack with
  | [] -> None
  | frame :: rest -> (
      match cursor.direction with
      | Asc ->
          if frame.index >= Array.length frame.children then (
            cursor.stack <- rest;
            next_cursor_child cursor)
          else
            let previous_key =
              if frame.index = 0 then None
              else Some frame.keys.(frame.index - 1)
            in
            if child_after_range cursor.cmp cursor.upper previous_key then (
              cursor.stack <- [];
              None)
            else
              let child = frame.children.(frame.index) in
              frame.index <- frame.index + 1;
              Some child
      | Desc ->
          if frame.index < 0 then (
            cursor.stack <- rest;
            next_cursor_child cursor)
          else
            let key = frame.keys.(frame.index) in
            let child_below_range =
              match cursor.lower with
              | Some lower -> cursor.cmp key lower < 0
              | None -> false
            in
            if child_below_range then (
              cursor.stack <- [];
              None)
            else
              let child = frame.children.(frame.index) in
              frame.index <- frame.index - 1;
              Some child)

and load_next_cursor_leaf cursor =
  match next_cursor_child cursor with
  | None -> cursor.leaf <- None
  | Some child -> (
      load_cursor_child cursor child;
      match cursor.leaf with
      | Some _ when cursor.leaf_len = 0 -> load_next_cursor_leaf cursor
      | _ -> ())

let init_cursor cursor =
  if not cursor.initialized then (
    cursor.initialized <- true;
    load_cursor_child cursor cursor.root)

let stop_cursor cursor =
  cursor.stack <- [];
  cursor.leaf <- None

let rec next_cursor_value cursor () =
  init_cursor cursor;
  match cursor.leaf with
  | None -> Seq.Nil
  | Some values -> (
      match cursor.direction with
      | Asc ->
          if cursor.leaf_index >= cursor.leaf_len then (
            cursor.leaf <- None;
            load_next_cursor_leaf cursor;
            next_cursor_value cursor ())
          else
            let value = values.(cursor.leaf_index) in
            if not (lower_ok cursor.cmp cursor.lower value) then (
              cursor.leaf_index <- cursor.leaf_index + 1;
              next_cursor_value cursor ())
            else if not (upper_ok cursor.cmp cursor.upper value) then (
              stop_cursor cursor;
              Seq.Nil)
            else (
              cursor.leaf_index <- cursor.leaf_index + 1;
              Seq.Cons (value, next_cursor_value cursor))
      | Desc ->
          if cursor.leaf_index < 0 then (
            cursor.leaf <- None;
            load_next_cursor_leaf cursor;
            next_cursor_value cursor ())
          else
            let value = values.(cursor.leaf_index) in
            let above_upper =
              match cursor.upper with
              | Some upper -> cursor.cmp value upper > 0
              | None -> false
            in
            let below_lower =
              match cursor.lower with
              | Some lower -> cursor.cmp value lower < 0
              | None -> false
            in
            if above_upper then (
              cursor.leaf_index <- cursor.leaf_index - 1;
              next_cursor_value cursor ())
            else if below_lower then (
              stop_cursor cursor;
              Seq.Nil)
            else (
              cursor.leaf_index <- cursor.leaf_index - 1;
              Seq.Cons (value, next_cursor_value cursor)))

let cursor_seq storage cmp direction lower upper root =
  let cursor =
    {
      storage;
      cmp;
      direction;
      lower;
      upper;
      root;
      initialized = false;
      stack = [];
      leaf = None;
      leaf_len = 0;
      leaf_index = 0;
    }
  in
  next_cursor_value cursor

let slice ?from_ ?to_ ?cmp (set : 'a t) =
  let cmp = Option.value ~default:set.cmp cmp in
  match set.data with
  | Empty -> []
  | Tree root -> slice_tree set.set_storage cmp from_ to_ root
  | Deferred { address } ->
      slice_deferred (storage_required set.set_storage) cmp from_ to_ address

let rslice ?from_ ?to_ ?cmp (set : 'a t) =
  let cmp = Option.value ~default:set.cmp cmp in
  match set.data with
  | Empty -> []
  | Tree root -> reverse_slice_tree set.set_storage cmp from_ to_ root
  | Deferred { address } ->
      reverse_slice_deferred
        (storage_required set.set_storage)
        cmp from_ to_ address

let seq_source_of_set set =
  match set.data with
  | Empty -> Seq_empty
  | Tree root -> Seq_tree { storage = set.set_storage; root }
  | Deferred { address } ->
      Seq_deferred { storage = storage_required set.set_storage; address }

let seq (set : 'a t) =
  {
    set_cmp = set.cmp;
    direction = Asc;
    source = seq_source_of_set set;
    lower = None;
    upper = None;
  }

let rseq (set : 'a t) =
  {
    set_cmp = set.cmp;
    direction = Desc;
    source = seq_source_of_set set;
    lower = None;
    upper = None;
  }

let seq_to_list (seq : 'a seq) =
  match (seq.source, seq.direction) with
  | Seq_empty, (Asc | Desc) -> []
  | Seq_tree { storage; root }, Asc ->
      slice_tree storage seq.set_cmp seq.lower seq.upper root
  | Seq_tree { storage; root }, Desc ->
      reverse_slice_tree storage seq.set_cmp seq.upper seq.lower root
  | Seq_deferred { storage; address }, Asc ->
      slice_deferred storage seq.set_cmp seq.lower seq.upper address
  | Seq_deferred { storage; address }, Desc ->
      reverse_slice_deferred storage seq.set_cmp seq.upper seq.lower address

let to_seq (seq : 'a seq) =
  match (seq.source, seq.direction) with
  | Seq_empty, (Asc | Desc) -> Seq.empty
  | Seq_tree { storage; root }, Asc ->
      cursor_seq storage seq.set_cmp Asc seq.lower seq.upper (Cursor_node root)
  | Seq_tree { storage; root }, Desc ->
      cursor_seq storage seq.set_cmp Desc seq.lower seq.upper (Cursor_node root)
  | Seq_deferred { storage; address }, Asc ->
      cursor_seq (Some storage) seq.set_cmp Asc seq.lower seq.upper
        (Cursor_address address)
  | Seq_deferred { storage; address }, Desc ->
      cursor_seq (Some storage) seq.set_cmp Desc seq.lower seq.upper
        (Cursor_address address)

let seq_reverse (seq : 'a seq) =
  let direction = match seq.direction with Asc -> Desc | Desc -> Asc in
  { seq with direction }

let fold_seq f init (seq : 'a seq) = Seq.fold_left f init (to_seq seq)

let slice_seq ?from_ ?to_ ?cmp (set : 'a t) =
  let set_cmp = Option.value ~default:set.cmp cmp |> normalize_cmp in
  {
    set_cmp;
    direction = Asc;
    source = seq_source_of_set set;
    lower = from_;
    upper = to_;
  }

let rslice_seq ?from_ ?to_ ?cmp (set : 'a t) =
  let set_cmp = Option.value ~default:set.cmp cmp |> normalize_cmp in
  {
    set_cmp;
    direction = Desc;
    source = seq_source_of_set set;
    lower = to_;
    upper = from_;
  }

let max_bound cmp left right =
  match (left, right) with
  | None, bound | bound, None -> bound
  | Some left, Some right -> Some (if cmp left right >= 0 then left else right)

let min_bound cmp left right =
  match (left, right) with
  | None, bound | bound, None -> bound
  | Some left, Some right -> Some (if cmp left right <= 0 then left else right)

let seek key (seq : 'a seq) =
  let cmp = seq.set_cmp in
  let lower, upper =
    match seq.direction with
    | Asc -> (max_bound cmp seq.lower (Some key), seq.upper)
    | Desc -> (seq.lower, min_bound cmp seq.upper (Some key))
  in
  { seq with lower; upper }

let split_branch_refs refs = List.split refs

let branch_key refs =
  match last refs with
  | Some (key, _) -> key
  | None -> invalid_arg "branch requires at least one child"

let store_branch storage child_refs =
  let keys, child_addresses = split_branch_refs child_refs in
  storage.store_node (Branch (keys, child_addresses))

let rec store_branch_tree storage settings child_refs =
  if List.length child_refs <= settings.branching_factor then
    let address = store_branch storage child_refs in
    (address, [ address ])
  else
    let child_groups = chunks settings.branching_factor child_refs in
    let branch_addresses =
      child_groups
      |> List.map (fun child_group ->
          let address = store_branch storage child_group in
          (branch_key child_group, address))
    in
    let root, branch_tree_addresses =
      store_branch_tree storage settings branch_addresses
    in
    (root, branch_tree_addresses @ List.map snd branch_addresses)

let rec store_node_tree storage settings = function
  | Node.Ref { address; _ } -> (address, [ address ])
  | Node.Leaf { address = Some address; _ } -> (address, [ address ])
  | Node.Leaf { values; len; address = None; _ } ->
      let address =
        storage.store_node (Leaf (array_prefix_to_list values len))
      in
      (address, [ address ])
  | Node.Branch { address = Some address; _ } -> (address, [ address ])
  | Node.Branch { keys; children; address = None; _ } ->
      let child_refs = ref [] in
      let child_address_lists = ref [] in
      for index = 0 to Array.length children - 1 do
        let address, addresses =
          store_node_tree storage settings children.(index)
        in
        child_refs := (keys.(index), address) :: !child_refs;
        child_address_lists := addresses :: !child_address_lists
      done;
      let child_refs = List.rev !child_refs in
      let child_address_lists = List.rev !child_address_lists in
      let address, branch_addresses =
        store_branch_tree storage settings child_refs
      in
      (address, branch_addresses @ List.concat child_address_lists)

let storage_of_set set =
  match (set.set_storage, set.data) with
  | Some storage, _ -> storage
  | None, Empty | None, Tree _ | None, Deferred _ ->
      invalid_arg "store requires a storage-backed set"

let store set =
  let storage = storage_of_set set in
  let stored_set address =
    { set with set_storage = Some storage; data = Deferred { address } }
  in
  match set.data with
  | Deferred { address } -> (address, set)
  | Tree root ->
      let address, _addresses = store_node_tree storage set.set_settings root in
      (address, stored_set address)
  | Empty ->
      let address = storage.store_node (Leaf []) in
      (address, stored_set address)

let restore ?(cmp = default_cmp) ?(settings = default_settings) storage address
    =
  let settings = validate_settings settings in
  Some
    {
      cmp = normalize_cmp cmp;
      set_settings = settings;
      set_storage = Some (cache_storage settings storage);
      data = Deferred { address };
      count_cache = None;
    }
