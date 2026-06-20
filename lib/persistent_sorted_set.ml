type 'a comparator = 'a -> 'a -> int
type settings = { branching_factor : int }
type 'a stored_node = Leaf of 'a list | Branch of 'a list * string list

type 'a storage = {
  store_node : 'a stored_node -> string;
  restore_node : string -> 'a stored_node option;
  accessed : string -> unit;
}

module Node = struct
  type 'a t =
    | Ref of { max_key : 'a; address : string }
    | Leaf of { values : 'a array; address : string option }
    | Branch of {
        keys : 'a array;
        children : 'a t array;
        address : string option;
      }
end

type 'a node = 'a Node.t

type 'a data =
  | Empty
  | Tree of { root : 'a node; has_stored_address : bool }
  | Deferred of { address : string }

type 'a t = {
  cmp : 'a comparator;
  set_settings : settings;
  set_storage : 'a storage option;
  data : 'a data;
}

type direction = Asc | Desc

type 'a seq_source =
  | Seq_values of 'a list
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
let default_settings = { branching_factor = 32 }

let validate_settings settings =
  if settings.branching_factor < 2 then
    invalid_arg "branching_factor must be at least 2";
  settings

let settings set = set.set_settings

let empty_with_cmp ?storage settings cmp =
  { cmp; set_settings = settings; set_storage = storage; data = Empty }

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

let prepend_array values init =
  let acc = ref init in
  for i = Array.length values - 1 downto 0 do
    acc := values.(i) :: !acc
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
  | Node.Leaf { values; _ } -> prepend_array values acc
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
  | Tree { root; _ } -> materialize_node set.set_storage root
  | Deferred { address } ->
      materialize_address (storage_required set.set_storage) address

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
      (values.(Array.length values - 1), Node.Leaf { values; address = None }))

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
  | Some root -> Tree { root; has_stored_address = false }

let data_of_changed_refs ~has_stored_address = function
  | [] -> Empty
  | [ (_, root) ] -> Tree { root; has_stored_address }
  | children -> Tree { root = node_branch_of_refs children; has_stored_address }

type 'a tree_edit_result =
  | Tree_edit_unchanged
  | Tree_edit_changed of ('a * 'a node) list

let array_insert values index value =
  let length = Array.length values in
  let result = Array.make (length + 1) value in
  Array.blit values 0 result 0 index;
  result.(index) <- value;
  Array.blit values index result (index + 1) (length - index);
  result

let array_remove values index =
  let length = Array.length values in
  if length = 1 then [||]
  else
    let first = if index = 0 then values.(1) else values.(0) in
    let result = Array.make (length - 1) first in
    Array.blit values 0 result 0 index;
    Array.blit values (index + 1) result index (length - index - 1);
    result

let array_split values =
  let length = Array.length values in
  let left_length = length / 2 in
  [
    Array.sub values 0 left_length;
    Array.sub values left_length (length - left_length);
  ]

let array_chunks size values =
  let length = Array.length values in
  let rec loop acc offset =
    if offset >= length then List.rev acc
    else
      let chunk_length = min size (length - offset) in
      loop (Array.sub values offset chunk_length :: acc) (offset + chunk_length)
  in
  loop [] 0

let tree_leaf_refs_of_arrays arrays =
  arrays
  |> List.map (fun values ->
      (values.(Array.length values - 1), Node.Leaf { values; address = None }))

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

let stored_branch_refs_of_arrays settings keys children =
  if Array.length keys <> Array.length children then
    invalid_arg "branch keys and children arity mismatch";
  let rec loop acc offset =
    if offset >= Array.length keys then List.rev acc
    else
      let chunk_length =
        min settings.branching_factor (Array.length keys - offset)
      in
      let key_chunk = Array.sub keys offset chunk_length in
      let child_chunk = Array.sub children offset chunk_length in
      let branch =
        Node.Branch { keys = key_chunk; children = child_chunk; address = None }
      in
      loop
        ((key_chunk.(chunk_length - 1), branch) :: acc)
        (offset + chunk_length)
  in
  loop [] 0

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

let total_cmp order_cmp equality_cmp left right =
  match order_cmp left right with 0 -> equality_cmp left right | n -> n

let find_insert_index order_cmp equality_cmp value values =
  let length = Array.length values in
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

let find_remove_index order_cmp equality_cmp value values =
  let length = Array.length values in
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

let find_index_by_cmp cmp value values =
  let length = Array.length values in
  let low = ref 0 in
  let high = ref (length - 1) in
  while !low <= !high do
    let middle = (!low + !high) / 2 in
    if cmp values.(middle) value < 0 then low := middle + 1
    else high := middle - 1
  done;
  let index = !low in
  if index < length && cmp values.(index) value = 0 then Some index else None

let array_mem_by_cmp cmp value values =
  match find_index_by_cmp cmp value values with Some _ -> true | None -> false

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
        Node.Ref { max_key = key; address = child_address })
      child_addresses
  in
  Node.Branch { keys = Array.copy keys; children; address = Some address }

type 'a node_edit_mode = Pure_tree | Stored_tree of 'a storage

let storage_of_edit_mode = function
  | Stored_tree storage -> storage
  | Pure_tree -> invalid_arg "storage-backed node requires storage-aware edit"

let add_leaf_refs settings mode inserted =
  match mode with
  | Pure_tree ->
      let changed =
        if Array.length inserted <= settings.branching_factor then [ inserted ]
        else array_split inserted
      in
      tree_leaf_refs_of_arrays changed
  | Stored_tree _ ->
      inserted
      |> array_chunks settings.branching_factor
      |> tree_leaf_refs_of_arrays

let branch_refs_of_arrays settings mode keys children =
  match mode with
  | Pure_tree -> tree_branch_refs_of_arrays settings keys children
  | Stored_tree _ -> stored_branch_refs_of_arrays settings keys children

let rec add_to_address storage settings order_cmp equality_cmp key_cmp value
    address =
  match restore_stored_node storage address with
  | Leaf values ->
      add_to_node (Stored_tree storage) settings order_cmp equality_cmp key_cmp
        value
        (Node.Leaf { values = Array.of_list values; address = Some address })
  | Branch (keys, child_addresses) ->
      add_to_node (Stored_tree storage) settings order_cmp equality_cmp key_cmp
        value
        (node_of_stored_branch keys child_addresses address)

and add_to_node mode settings order_cmp equality_cmp key_cmp value = function
  | Node.Ref { address; _ } ->
      let storage = storage_of_edit_mode mode in
      add_to_address storage settings order_cmp equality_cmp key_cmp value
        address
  | Node.Leaf { values; _ } -> (
      match find_insert_index order_cmp equality_cmp value values with
      | `Found _ -> Tree_edit_unchanged
      | `Insert index ->
          let inserted = array_insert values index value in
          Tree_edit_changed (add_leaf_refs settings mode inserted))
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
          branch_refs_of_arrays settings mode keys children |> fun changed ->
          Tree_edit_changed changed)

let rec remove_from_address storage settings order_cmp equality_cmp key_cmp
    value address =
  match restore_stored_node storage address with
  | Leaf values ->
      remove_from_node (Stored_tree storage) settings order_cmp equality_cmp
        key_cmp value
        (Node.Leaf { values = Array.of_list values; address = Some address })
  | Branch (keys, child_addresses) ->
      remove_from_node (Stored_tree storage) settings order_cmp equality_cmp
        key_cmp value
        (node_of_stored_branch keys child_addresses address)

and remove_from_node mode settings order_cmp equality_cmp key_cmp value =
  function
  | Node.Ref { address; _ } ->
      let storage = storage_of_edit_mode mode in
      remove_from_address storage settings order_cmp equality_cmp key_cmp value
        address
  | Node.Leaf { values; _ } -> (
      match find_remove_index order_cmp equality_cmp value values with
      | None -> Tree_edit_unchanged
      | Some index -> (
          array_remove values index |> function
          | [||] -> Tree_edit_changed []
          | values ->
              Tree_edit_changed
                [
                  ( values.(Array.length values - 1),
                    Node.Leaf { values; address = None } );
                ]))
  | Node.Branch { keys; children; _ } -> (
      let index = find_child_index key_cmp value keys in
      match
        remove_from_node mode settings order_cmp equality_cmp key_cmp value
          children.(index)
      with
      | Tree_edit_unchanged -> Tree_edit_unchanged
      | Tree_edit_changed [ (key, child) ] ->
          branch_replace_one settings keys children index key child
          |> fun changed -> Tree_edit_changed changed
      | Tree_edit_changed changed ->
          let keys, children = branch_splice_one keys children index changed in
          branch_refs_of_arrays settings mode keys children |> fun changed ->
          Tree_edit_changed changed)

let add value set =
  let equality_cmp = set.cmp in
  let key_cmp = set.cmp in
  match set.data with
  | Deferred { address } -> (
      let storage = storage_required set.set_storage in
      match
        add_to_address storage set.set_settings set.cmp equality_cmp key_cmp
          value address
      with
      | Tree_edit_unchanged -> set
      | Tree_edit_changed changed ->
          {
            set with
            data = data_of_changed_refs ~has_stored_address:true changed;
          })
  | Tree { root; has_stored_address = true } -> (
      let storage = storage_required set.set_storage in
      match
        add_to_node (Stored_tree storage) set.set_settings set.cmp equality_cmp
          key_cmp value root
      with
      | Tree_edit_unchanged -> set
      | Tree_edit_changed changed ->
          {
            set with
            data = data_of_changed_refs ~has_stored_address:true changed;
          })
  | Tree { root; has_stored_address = false } -> (
      match
        add_to_node Pure_tree set.set_settings set.cmp equality_cmp key_cmp
          value root
      with
      | Tree_edit_unchanged -> set
      | Tree_edit_changed changed ->
          {
            set with
            data =
              (match node_of_refs set.set_settings changed with
              | None -> Empty
              | Some root -> Tree { root; has_stored_address = false });
          })
  | Empty ->
      { set with data = data_of_sorted_values set.set_settings [ value ] }

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
  { set with data = data_of_sorted_values set.set_settings values }

let of_list values = of_list_by values

let of_sorted_array_by ?settings ?storage ?cmp values =
  let set = empty_by ?settings ?storage ?cmp () in
  let values_list = ref [] in
  for i = Array.length values - 1 downto 0 do
    values_list := values.(i) :: !values_list
  done;
  let values = distinct_sorted_values set.cmp !values_list in
  { set with data = data_of_sorted_values set.set_settings values }

let of_sorted_array values = of_sorted_array_by values

let remove value set =
  let equality_cmp = set.cmp in
  let key_cmp = set.cmp in
  match set.data with
  | Deferred { address } -> (
      let storage = storage_required set.set_storage in
      match
        remove_from_address storage set.set_settings set.cmp equality_cmp
          key_cmp value address
      with
      | Tree_edit_unchanged -> set
      | Tree_edit_changed changed ->
          {
            set with
            data = data_of_changed_refs ~has_stored_address:true changed;
          })
  | Tree { root; has_stored_address = true } -> (
      let storage = storage_required set.set_storage in
      match
        remove_from_node (Stored_tree storage) set.set_settings set.cmp
          equality_cmp key_cmp value root
      with
      | Tree_edit_unchanged -> set
      | Tree_edit_changed changed ->
          {
            set with
            data = data_of_changed_refs ~has_stored_address:true changed;
          })
  | Tree { root; has_stored_address = false } -> (
      match
        remove_from_node Pure_tree set.set_settings set.cmp equality_cmp key_cmp
          value root
      with
      | Tree_edit_unchanged -> set
      | Tree_edit_changed changed ->
          {
            set with
            data =
              (match node_of_refs set.set_settings changed with
              | None -> Empty
              | Some root -> Tree { root; has_stored_address = false });
          })
  | Empty -> set

type search_step = Found | Stop | Continue

let rec search_deferred storage order_cmp equality_cmp value address =
  storage.accessed address;
  match storage.restore_node address with
  | Some (Leaf values) -> (
      let rec mem_by = function
        | [] -> false
        | current :: rest -> (
            match order_cmp value current with
            | n when n < 0 -> false
            | 0 -> equality_cmp value current = 0 || mem_by rest
            | _ -> mem_by rest)
      in
      if mem_by values then Found
      else
        match last values with
        | Some last_value when order_cmp value last_value <= 0 -> Stop
        | _ -> Continue)
  | Some (Branch (keys, child_addresses)) -> (
      let rec choose_child keys child_addresses =
        match (keys, child_addresses) with
        | [], [] -> None
        | key :: _, child_address :: _
          when route_cmp order_cmp equality_cmp value key <= 0 ->
            Some child_address
        | _ :: keys, _ :: child_addresses -> choose_child keys child_addresses
        | [], _ :: _ | _ :: _, [] ->
            invalid_arg "branch keys and addresses arity mismatch"
      in
      match choose_child keys child_addresses with
      | Some child_address ->
          search_deferred storage order_cmp equality_cmp value child_address
      | None -> Continue)
  | None -> invalid_arg ("stored node not found: " ^ address)

let mem_in_deferred storage order_cmp equality_cmp value address =
  match search_deferred storage order_cmp equality_cmp value address with
  | Found -> true
  | Stop | Continue -> false

let rec search_node_by_cmp storage cmp value = function
  | Node.Ref { address; _ } ->
      search_deferred (storage_required storage) cmp cmp value address
  | Node.Leaf { values; _ } ->
      if array_mem_by_cmp cmp value values then Found
      else if
        Array.length values > 0
        && cmp value values.(Array.length values - 1) <= 0
      then Stop
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
  | Tree { root; _ } -> mem_in_node_by_cmp set.set_storage set.cmp value root
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
  | Node.Ref { address; _ } ->
      fold_address (storage_required storage) f init address
  | Node.Leaf { values; _ } -> Array.fold_left f init values
  | Node.Branch { children; _ } ->
      Array.fold_left
        (fun acc child -> fold_node storage f acc child)
        init children

let count (set : 'a t) =
  match set.data with
  | Tree { root; _ } ->
      fold_node set.set_storage (fun count _ -> count + 1) 0 root
  | Empty -> 0
  | Deferred { address } ->
      fold_address
        (storage_required set.set_storage)
        (fun count _ -> count + 1)
        0 address

let to_list (set : 'a t) = materialize set

let fold f init set =
  match set.data with
  | Empty -> init
  | Tree { root; _ } -> fold_node set.set_storage f init root
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

let rec list_seq_slice cmp from_ to_ values () =
  match values with
  | [] -> Seq.Nil
  | value :: rest ->
      if not (lower_ok cmp from_ value) then
        list_seq_slice cmp from_ to_ rest ()
      else if not (upper_ok cmp to_ value) then Seq.Nil
      else Seq.Cons (value, list_seq_slice cmp from_ to_ rest)

let reverse_slice_values cmp from_ to_ values =
  values |> List.rev
  |> List.filter (fun value ->
      match (from_, to_) with
      | None, None -> true
      | Some from_, None -> cmp value from_ <= 0
      | None, Some to_ -> cmp value to_ >= 0
      | Some from_, Some to_ -> cmp value from_ <= 0 && cmp value to_ >= 0)

let rec list_seq_reverse_slice cmp from_ to_ values () =
  match values with
  | [] -> Seq.Nil
  | value :: rest -> (
      let in_range =
        match (from_, to_) with
        | None, None -> true
        | Some from_, None -> cmp value from_ <= 0
        | None, Some to_ -> cmp value to_ >= 0
        | Some from_, Some to_ -> cmp value from_ <= 0 && cmp value to_ >= 0
      in
      if in_range then
        Seq.Cons (value, list_seq_reverse_slice cmp from_ to_ rest)
      else
        match from_ with
        | Some lower when cmp value lower > 0 ->
            list_seq_reverse_slice cmp from_ to_ rest ()
        | _ -> Seq.Nil)

let slice_array_into cmp from_ to_ values acc =
  let rec loop acc index =
    if index >= Array.length values then acc
    else
      let value = values.(index) in
      if not (lower_ok cmp from_ value) then loop acc (index + 1)
      else if not (upper_ok cmp to_ value) then acc
      else loop (value :: acc) (index + 1)
  in
  loop acc 0

let reverse_slice_array_into cmp from_ to_ values acc =
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
  loop acc (Array.length values - 1)

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
      let rec annotate previous_key acc keys child_addresses =
        match (keys, child_addresses) with
        | [], [] -> acc
        | key :: keys, child_address :: child_addresses ->
            annotate (Some key)
              ((previous_key, key, child_address) :: acc)
              keys child_addresses
        | [], _ :: _ | _ :: _, [] ->
            invalid_arg "branch keys and addresses arity mismatch"
      in
      let rec collect acc = function
        | [] -> acc
        | (previous_key, key, child_address) :: rest ->
            let child_above_range =
              match (from_, previous_key) with
              | Some from_, Some previous_key -> cmp previous_key from_ > 0
              | _ -> false
            in
            let child_below_range =
              match to_ with Some to_ -> cmp key to_ < 0 | None -> false
            in
            if child_above_range then collect acc rest
            else if child_below_range then acc
            else
              collect
                (reverse_slice_deferred_into storage cmp from_ to_ child_address
                   acc)
                rest
      in
      collect acc (annotate None [] keys child_addresses)
  | None -> invalid_arg ("stored node not found: " ^ address)

let reverse_slice_deferred storage cmp from_ to_ address =
  List.rev (reverse_slice_deferred_into storage cmp from_ to_ address [])

let rec slice_array_seq cmp from_ to_ values index () =
  if index >= Array.length values then Seq.Nil
  else
    let value = values.(index) in
    if not (lower_ok cmp from_ value) then
      slice_array_seq cmp from_ to_ values (index + 1) ()
    else if not (upper_ok cmp to_ value) then Seq.Nil
    else Seq.Cons (value, slice_array_seq cmp from_ to_ values (index + 1))

let rec reverse_slice_array_seq cmp from_ to_ values index () =
  if index < 0 then Seq.Nil
  else
    let value = values.(index) in
    let in_range =
      match (from_, to_) with
      | None, None -> true
      | Some from_, None -> cmp value from_ <= 0
      | None, Some to_ -> cmp value to_ >= 0
      | Some from_, Some to_ -> cmp value from_ <= 0 && cmp value to_ >= 0
    in
    if in_range then
      Seq.Cons (value, reverse_slice_array_seq cmp from_ to_ values (index - 1))
    else
      match from_ with
      | Some lower when cmp value lower > 0 ->
          reverse_slice_array_seq cmp from_ to_ values (index - 1) ()
      | _ -> Seq.Nil

let rec slice_deferred_seq storage cmp from_ to_ address () =
  storage.accessed address;
  match storage.restore_node address with
  | Some (Leaf values) -> list_seq_slice cmp from_ to_ values ()
  | Some (Branch (keys, child_addresses)) ->
      let rec collect previous_key keys child_addresses () =
        match (keys, child_addresses) with
        | [], [] -> Seq.Nil
        | key :: keys, child_address :: child_addresses ->
            if child_after_range cmp to_ previous_key then Seq.Nil
            else if child_before_range cmp from_ key then
              collect (Some key) keys child_addresses ()
            else
              Seq.append
                (slice_deferred_seq storage cmp from_ to_ child_address)
                (collect (Some key) keys child_addresses)
                ()
        | [], _ :: _ | _ :: _, [] ->
            invalid_arg "branch keys and addresses arity mismatch"
      in
      collect None keys child_addresses ()
  | None -> invalid_arg ("stored node not found: " ^ address)

let rec reverse_slice_deferred_seq storage cmp from_ to_ address () =
  storage.accessed address;
  match storage.restore_node address with
  | Some (Leaf values) ->
      list_seq_reverse_slice cmp from_ to_ (List.rev values) ()
  | Some (Branch (keys, child_addresses)) ->
      let rec annotate previous_key acc keys child_addresses =
        match (keys, child_addresses) with
        | [], [] -> acc
        | key :: keys, child_address :: child_addresses ->
            annotate (Some key)
              ((previous_key, key, child_address) :: acc)
              keys child_addresses
        | [], _ :: _ | _ :: _, [] ->
            invalid_arg "branch keys and addresses arity mismatch"
      in
      let rec collect refs () =
        match refs with
        | [] -> Seq.Nil
        | (previous_key, key, child_address) :: rest ->
            let child_above_range =
              match (from_, previous_key) with
              | Some from_, Some previous_key -> cmp previous_key from_ > 0
              | _ -> false
            in
            let child_below_range =
              match to_ with Some to_ -> cmp key to_ < 0 | None -> false
            in
            if child_above_range then collect rest ()
            else if child_below_range then Seq.Nil
            else
              Seq.append
                (reverse_slice_deferred_seq storage cmp from_ to_ child_address)
                (collect rest) ()
      in
      collect (annotate None [] keys child_addresses) ()
  | None -> invalid_arg ("stored node not found: " ^ address)

let rec slice_tree_into storage cmp from_ to_ node acc =
  match node with
  | Node.Ref { address; _ } ->
      slice_deferred_into (storage_required storage) cmp from_ to_ address acc
  | Node.Leaf { values; _ } -> slice_array_into cmp from_ to_ values acc
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

let rec slice_tree_seq storage cmp from_ to_ node =
  match node with
  | Node.Ref { address; _ } ->
      slice_deferred_seq (storage_required storage) cmp from_ to_ address
  | Node.Leaf { values; _ } -> slice_array_seq cmp from_ to_ values 0
  | Node.Branch { keys; children; _ } ->
      let rec collect previous_key index () =
        if index >= Array.length children then Seq.Nil
        else
          let key = keys.(index) in
          if child_after_range cmp to_ previous_key then Seq.Nil
          else if child_before_range cmp from_ key then
            collect (Some key) (index + 1) ()
          else
            Seq.append
              (slice_tree_seq storage cmp from_ to_ children.(index))
              (collect (Some key) (index + 1))
              ()
      in
      collect None 0

let rec reverse_slice_tree_into storage cmp from_ to_ node acc =
  match node with
  | Node.Ref { address; _ } ->
      reverse_slice_deferred_into (storage_required storage) cmp from_ to_
        address acc
  | Node.Leaf { values; _ } -> reverse_slice_array_into cmp from_ to_ values acc
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

let rec reverse_slice_tree_seq storage cmp from_ to_ node =
  match node with
  | Node.Ref { address; _ } ->
      reverse_slice_deferred_seq (storage_required storage) cmp from_ to_
        address
  | Node.Leaf { values; _ } ->
      reverse_slice_array_seq cmp from_ to_ values (Array.length values - 1)
  | Node.Branch { keys; children; _ } ->
      let rec collect index () =
        if index < 0 then Seq.Nil
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
          if child_above_range then collect (index - 1) ()
          else if child_below_range then Seq.Nil
          else
            Seq.append
              (reverse_slice_tree_seq storage cmp from_ to_ children.(index))
              (collect (index - 1))
              ()
      in
      collect (Array.length children - 1)

let slice ?from_ ?to_ ?cmp (set : 'a t) =
  let cmp = Option.value ~default:set.cmp cmp in
  match set.data with
  | Empty -> []
  | Tree { root; _ } -> slice_tree set.set_storage cmp from_ to_ root
  | Deferred { address } ->
      slice_deferred (storage_required set.set_storage) cmp from_ to_ address

let rslice ?from_ ?to_ ?cmp (set : 'a t) =
  let cmp = Option.value ~default:set.cmp cmp in
  match set.data with
  | Empty -> []
  | Tree { root; _ } -> reverse_slice_tree set.set_storage cmp from_ to_ root
  | Deferred { address } ->
      reverse_slice_deferred
        (storage_required set.set_storage)
        cmp from_ to_ address

let seq_source_of_set set =
  match set.data with
  | Empty -> Seq_values []
  | Tree { root; _ } -> Seq_tree { storage = set.set_storage; root }
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

let seq_to_list seq =
  match (seq.source, seq.direction) with
  | Seq_values values, Asc ->
      slice_values seq.set_cmp seq.lower seq.upper values
  | Seq_values values, Desc ->
      reverse_slice_values seq.set_cmp seq.upper seq.lower values
  | Seq_tree { storage; root }, Asc ->
      slice_tree storage seq.set_cmp seq.lower seq.upper root
  | Seq_tree { storage; root }, Desc ->
      reverse_slice_tree storage seq.set_cmp seq.upper seq.lower root
  | Seq_deferred { storage; address }, Asc ->
      slice_deferred storage seq.set_cmp seq.lower seq.upper address
  | Seq_deferred { storage; address }, Desc ->
      reverse_slice_deferred storage seq.set_cmp seq.upper seq.lower address

let to_seq seq =
  match (seq.source, seq.direction) with
  | Seq_values values, Asc ->
      list_seq_slice seq.set_cmp seq.lower seq.upper values
  | Seq_values values, Desc ->
      list_seq_reverse_slice seq.set_cmp seq.upper seq.lower (List.rev values)
  | Seq_tree { storage; root }, Asc ->
      slice_tree_seq storage seq.set_cmp seq.lower seq.upper root
  | Seq_tree { storage; root }, Desc ->
      reverse_slice_tree_seq storage seq.set_cmp seq.upper seq.lower root
  | Seq_deferred { storage; address }, Asc ->
      slice_deferred_seq storage seq.set_cmp seq.lower seq.upper address
  | Seq_deferred { storage; address }, Desc ->
      reverse_slice_deferred_seq storage seq.set_cmp seq.upper seq.lower address

let seq_reverse seq =
  let direction = match seq.direction with Asc -> Desc | Desc -> Asc in
  { seq with direction }

let fold_seq f init seq = Seq.fold_left f init (to_seq seq)

let slice_seq ?from_ ?to_ ?cmp set =
  let set_cmp = Option.value ~default:set.cmp cmp |> normalize_cmp in
  {
    set_cmp;
    direction = Asc;
    source = seq_source_of_set set;
    lower = from_;
    upper = to_;
  }

let rslice_seq ?from_ ?to_ ?cmp set =
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

let seek key seq =
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
  | Node.Leaf { values; address = None } ->
      let address = storage.store_node (Leaf (Array.to_list values)) in
      (address, [ address ])
  | Node.Branch { address = Some address; _ } -> (address, [ address ])
  | Node.Branch { keys; children; address = None } ->
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

let store_ordered_values storage settings iter =
  let chunk_values = ref [] in
  let chunk_length = ref 0 in
  let child_refs = ref [] in
  let flush_chunk () =
    match !chunk_values with
    | [] -> ()
    | values ->
        let values = List.rev values in
        let address = storage.store_node (Leaf values) in
        let key =
          match last values with
          | Some key -> key
          | None -> invalid_arg "leaf chunk cannot be empty"
        in
        child_refs := (key, address) :: !child_refs;
        chunk_values := [];
        chunk_length := 0
  in
  let add_value value =
    chunk_values := value :: !chunk_values;
    incr chunk_length;
    if !chunk_length = settings.branching_factor then flush_chunk ()
  in
  iter add_value;
  flush_chunk ();
  match List.rev !child_refs with
  | [] -> storage.store_node (Leaf [])
  | [ (_, address) ] -> address
  | child_refs ->
      let address, _branch_addresses =
        store_branch_tree storage settings child_refs
      in
      address

let store set =
  let storage = storage_of_set set in
  let stored_set address =
    { set with set_storage = Some storage; data = Deferred { address } }
  in
  match set.data with
  | Deferred { address } -> (address, set)
  | Tree { root; has_stored_address = true } ->
      let address, _addresses = store_node_tree storage set.set_settings root in
      (address, stored_set address)
  | Empty ->
      let address =
        store_ordered_values storage set.set_settings (fun _add_value -> ())
      in
      (address, stored_set address)
  | Tree { root; has_stored_address = false } ->
      let address =
        store_ordered_values storage set.set_settings (fun add_value ->
            ignore
              (fold_node set.set_storage
                 (fun () value -> add_value value)
                 () root))
      in
      (address, stored_set address)

let restore ?(cmp = default_cmp) ?(settings = default_settings) storage address
    =
  let settings = validate_settings settings in
  Some
    {
      cmp = normalize_cmp cmp;
      set_settings = settings;
      set_storage = Some storage;
      data = Deferred { address };
    }
