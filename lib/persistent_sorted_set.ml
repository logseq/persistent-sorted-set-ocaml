type 'a comparator = 'a -> 'a -> int

type settings =
  { branching_factor : int
  }

type 'a stored_node =
  | Leaf of 'a list
  | Branch of 'a list * string list

type 'a storage =
  { store_node : 'a stored_node -> string
  ; restore_node : string -> 'a stored_node option
  ; accessed : string -> unit
  }

type 'a data =
  | Loaded of 'a list
  | Tree of 'a tree
  | Deferred of
      { storage : 'a storage
      ; address : string
      }
  | Edited of
      { storage : 'a storage
      ; tree : 'a edited_tree
      }

and 'a edited_tree =
  | Edited_ref of 'a * string
  | Edited_leaf of 'a list * string option
  | Edited_branch of ('a * 'a edited_tree) list * string option

and 'a tree =
  | Tree_leaf of 'a array
  | Tree_branch of 'a array * 'a tree array

type 'a t =
  { cmp : 'a comparator
  ; set_settings : settings
  ; data : 'a data
  ; root_address : string option
  ; stored_addresses : string list option
  ; stored_chunks : ('a list * string option) list
  ; stored_branch_chunks : (('a * string) list * string option) list
  }

type direction =
  | Asc
  | Desc

type 'a seq_source =
  | Seq_values of 'a list
  | Seq_deferred of
      { storage : 'a storage
      ; address : string
      }

type 'a seq =
  { set_cmp : 'a comparator
  ; direction : direction
  ; source : 'a seq_source
  ; lower : 'a option
  ; upper : 'a option
  }

let normalize_cmp cmp left right =
  match cmp left right with
  | n when n < 0 -> -1
  | 0 -> 0
  | _ -> 1

let default_cmp left right = Stdlib.compare left right

let default_settings = { branching_factor = 32 }

let validate_settings settings =
  if settings.branching_factor < 2 then invalid_arg "branching_factor must be at least 2";
  settings

let settings set = set.set_settings

let empty_with_cmp settings cmp =
  { cmp
  ; set_settings = settings
  ; data = Loaded []
  ; root_address = None
  ; stored_addresses = None
  ; stored_chunks = []
  ; stored_branch_chunks = []
  }

let empty_by ?(settings = default_settings) cmp =
  let settings = validate_settings settings in
  empty_with_cmp settings (normalize_cmp cmp)

let empty () = empty_with_cmp (validate_settings default_settings) default_cmp

let chunks size values =
  let rec loop acc = function
    | [] -> List.rev acc
    | values ->
    let rec take count acc rest =
      if count = 0 then List.rev acc, rest
      else
        match rest with
        | [] -> List.rev acc, []
        | value :: rest -> take (count - 1) (value :: acc) rest
    in
    let chunk, rest = take size [] values in
      loop (chunk :: acc) rest
  in
  loop [] values

let rec materialize_address storage address =
  storage.accessed address;
  match storage.restore_node address with
  | Some (Leaf values) ->
    values
  | Some (Branch (_, child_addresses)) ->
    child_addresses |> List.concat_map (materialize_address storage)
  | None -> invalid_arg ("stored node not found: " ^ address)

let rec materialize_edited_tree storage = function
  | Edited_ref (_, address) -> materialize_address storage address
  | Edited_leaf (values, _) -> values
  | Edited_branch (children, _) ->
    children |> List.concat_map (fun (_, child) -> materialize_edited_tree storage child)

let array_fold_right f values init =
  let acc = ref init in
  for i = Array.length values - 1 downto 0 do
    acc := f values.(i) !acc
  done;
  !acc

let rec materialize_tree tree =
  match tree with
  | Tree_leaf values -> array_fold_right (fun value acc -> value :: acc) values []
  | Tree_branch (_, children) ->
    array_fold_right (fun child acc -> materialize_tree child @ acc) children []

let materialize set =
  match set.data with
  | Loaded values -> values
  | Tree tree -> materialize_tree tree
  | Deferred { storage; address } -> materialize_address storage address
  | Edited { storage; tree } -> materialize_edited_tree storage tree

let rec insert_unique order_cmp equality_cmp value = function
  | [] -> [ value ]
  | current :: rest as values ->
    (match order_cmp value current with
     | 0 ->
       (match equality_cmp value current with
        | 0 -> values
        | n when n < 0 -> value :: values
        | _ -> current :: insert_unique order_cmp equality_cmp value rest)
     | n when n < 0 -> value :: values
     | _ -> current :: insert_unique order_cmp equality_cmp value rest)

let rec remove_by order_cmp equality_cmp value = function
  | [] -> []
  | current :: rest ->
    (match order_cmp value current with
     | n when n < 0 -> current :: rest
     | 0 ->
       if equality_cmp value current = 0 then rest else current :: remove_by order_cmp equality_cmp value rest
     | _ -> current :: remove_by order_cmp equality_cmp value rest)

let rec last = function
  | [] -> None
  | [ value ] -> Some value
  | _ :: rest -> last rest

let last_exn values =
  match last values with
  | Some value -> value
  | None -> invalid_arg "tree node cannot be empty"

let route_cmp order_cmp equality_cmp value key =
  match order_cmp value key with
  | 0 -> equality_cmp value key
  | n -> n

type 'a edit_result =
  | Edit_unchanged
  | Edit_changed of ('a * 'a edited_tree) list

let branch_child_refs keys child_addresses =
  let rec loop acc keys child_addresses =
    match keys, child_addresses with
    | [], [] -> List.rev acc
    | key :: keys, address :: child_addresses ->
      loop ((key, Edited_ref (key, address)) :: acc) keys child_addresses
    | [], _ :: _ | _ :: _, [] -> invalid_arg "branch keys and addresses arity mismatch"
  in
  loop [] keys child_addresses

let restore_stored_node storage address =
  storage.accessed address;
  match storage.restore_node address with
  | Some node -> node
  | None -> invalid_arg ("stored node not found: " ^ address)

let refs_of_leaf_chunks chunks =
  chunks |> List.map (fun chunk -> last_exn chunk, Edited_leaf (chunk, None))

let edited_branch_key children =
  match last children with
  | Some (key, _) -> key
  | None -> invalid_arg "branch requires at least one child"

let refs_of_branch_chunks settings children =
  children
  |> chunks settings.branching_factor
  |> List.map (fun children -> edited_branch_key children, Edited_branch (children, None))

let tree_ref_key refs =
  match last refs with
  | Some (key, _) -> key
  | None -> invalid_arg "tree branch requires at least one child"

let tree_branch_of_refs refs =
  let keys, children = List.split refs in
  Tree_branch (Array.of_list keys, Array.of_list children)

let tree_leaf_refs_of_chunks chunks =
  chunks
  |> List.map (fun chunk ->
    let values = Array.of_list chunk in
    values.(Array.length values - 1), Tree_leaf values)

let rec tree_of_refs settings = function
  | [] -> None
  | [ _, tree ] -> Some tree
  | refs ->
    refs
    |> chunks settings.branching_factor
    |> List.map (fun refs -> tree_ref_key refs, tree_branch_of_refs refs)
    |> tree_of_refs settings

let data_of_sorted_values settings values =
  match values |> chunks settings.branching_factor |> tree_leaf_refs_of_chunks |> tree_of_refs settings with
  | None -> Loaded []
  | Some tree -> Tree tree

let rec add_to_address storage settings order_cmp equality_cmp value address =
  match restore_stored_node storage address with
  | Leaf values ->
    let inserted = insert_unique order_cmp equality_cmp value values in
    if inserted = values then Edit_unchanged
    else Edit_changed (refs_of_leaf_chunks (chunks settings.branching_factor inserted))
  | Branch (keys, child_addresses) ->
    let children = branch_child_refs keys child_addresses in
    let rec loop prefix = function
      | [] -> Edit_unchanged
      | (key, Edited_ref (_, child_address)) :: rest
        when route_cmp order_cmp equality_cmp value key <= 0 || rest = [] ->
        (match add_to_address storage settings order_cmp equality_cmp value child_address with
         | Edit_unchanged -> Edit_unchanged
         | Edit_changed changed ->
           List.rev_append prefix (changed @ rest)
           |> refs_of_branch_chunks settings
           |> fun children -> Edit_changed children)
      | child :: rest -> loop (child :: prefix) rest
    in
    loop [] children

let rec add_to_edited_tree storage settings order_cmp equality_cmp value = function
  | Edited_ref (_, address) -> add_to_address storage settings order_cmp equality_cmp value address
  | Edited_leaf (values, _) ->
    let inserted = insert_unique order_cmp equality_cmp value values in
    if inserted = values then Edit_unchanged
    else Edit_changed (refs_of_leaf_chunks (chunks settings.branching_factor inserted))
  | Edited_branch (children, _) ->
    let rec loop prefix = function
      | [] -> Edit_unchanged
      | (key, child) :: rest when route_cmp order_cmp equality_cmp value key <= 0 || rest = [] ->
        (match add_to_edited_tree storage settings order_cmp equality_cmp value child with
         | Edit_unchanged -> Edit_unchanged
         | Edit_changed changed ->
           List.rev_append prefix (changed @ rest)
           |> refs_of_branch_chunks settings
           |> fun children -> Edit_changed children)
      | child :: rest -> loop (child :: prefix) rest
    in
    loop [] children

let rec remove_from_address storage settings order_cmp equality_cmp value address =
  match restore_stored_node storage address with
  | Leaf values ->
    let removed = remove_by order_cmp equality_cmp value values in
    if removed = values then Edit_unchanged
    else Edit_changed (refs_of_leaf_chunks (chunks settings.branching_factor removed))
  | Branch (keys, child_addresses) ->
    let children = branch_child_refs keys child_addresses in
    let rec loop prefix = function
      | [] -> Edit_unchanged
      | (key, Edited_ref (_, child_address)) :: rest
        when route_cmp order_cmp equality_cmp value key <= 0 || rest = [] ->
        (match remove_from_address storage settings order_cmp equality_cmp value child_address with
         | Edit_unchanged -> Edit_unchanged
         | Edit_changed changed ->
           List.rev_append prefix (changed @ rest)
           |> refs_of_branch_chunks settings
           |> fun children -> Edit_changed children)
      | child :: rest -> loop (child :: prefix) rest
    in
    loop [] children

let rec remove_from_edited_tree storage settings order_cmp equality_cmp value = function
  | Edited_ref (_, address) -> remove_from_address storage settings order_cmp equality_cmp value address
  | Edited_leaf (values, _) ->
    let removed = remove_by order_cmp equality_cmp value values in
    if removed = values then Edit_unchanged
    else Edit_changed (refs_of_leaf_chunks (chunks settings.branching_factor removed))
  | Edited_branch (children, _) ->
    let rec loop prefix = function
      | [] -> Edit_unchanged
      | (key, child) :: rest when route_cmp order_cmp equality_cmp value key <= 0 || rest = [] ->
        (match remove_from_edited_tree storage settings order_cmp equality_cmp value child with
         | Edit_unchanged -> Edit_unchanged
         | Edit_changed changed ->
           List.rev_append prefix (changed @ rest)
           |> refs_of_branch_chunks settings
           |> fun children -> Edit_changed children)
      | child :: rest -> loop (child :: prefix) rest
    in
    loop [] children

let edited_data_of_changed_refs storage = function
  | [] -> Loaded []
  | [ _, tree ] -> Edited { storage; tree }
  | children -> Edited { storage; tree = Edited_branch (children, None) }

type 'a tree_edit_result =
  | Tree_edit_unchanged
  | Tree_edit_changed of ('a * 'a tree) list

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
  else (
    let first = if index = 0 then values.(1) else values.(0) in
    let result = Array.make (length - 1) first in
    Array.blit values 0 result 0 index;
    Array.blit values (index + 1) result index (length - index - 1);
    result)

let array_split values =
  let length = Array.length values in
  let left_length = length / 2 in
  [ Array.sub values 0 left_length; Array.sub values left_length (length - left_length) ]

let tree_leaf_refs_of_arrays arrays =
  arrays |> List.map (fun values -> values.(Array.length values - 1), Tree_leaf values)

let tree_branch_refs_of_arrays settings keys children =
  let length = Array.length keys in
  if length = 0 then []
  else if length <= settings.branching_factor then [ keys.(length - 1), Tree_branch (keys, children) ]
  else
    let key_chunks = array_split keys in
    let child_chunks = array_split children in
    List.map2
      (fun keys children -> keys.(Array.length keys - 1), Tree_branch (keys, children))
      key_chunks
      child_chunks

let ref_arrays_of_list refs =
  let keys, children = List.split refs in
  Array.of_list keys, Array.of_list children

let branch_splice_one keys children index replacement =
  let length = Array.length keys in
  let replacement_keys, replacement_children = ref_arrays_of_list replacement in
  let replacement_length = Array.length replacement_keys in
  let result_length = length - 1 + replacement_length in
  if result_length = 0 then [||], [||]
  else (
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
    Array.blit keys (index + 1) result_keys (index + replacement_length) (length - index - 1);
    Array.blit children (index + 1) result_children (index + replacement_length) (length - index - 1);
    result_keys, result_children)

let branch_replace_one settings keys children index key child =
  let keys = Array.copy keys in
  let children = Array.copy children in
  keys.(index) <- key;
  children.(index) <- child;
  tree_branch_refs_of_arrays settings keys children

let total_cmp order_cmp equality_cmp left right =
  match order_cmp left right with
  | 0 -> equality_cmp left right
  | n -> n

let find_insert_index order_cmp equality_cmp value values =
  let length = Array.length values in
  let low = ref 0 in
  let high = ref (length - 1) in
  while !low <= !high do
    let middle = (!low + !high) / 2 in
    if total_cmp order_cmp equality_cmp values.(middle) value < 0 then low := middle + 1
    else high := middle - 1
  done;
  let index = !low in
  if index < length && total_cmp order_cmp equality_cmp values.(index) value = 0 then `Found index
  else `Insert index

let find_remove_index order_cmp equality_cmp value values =
  let length = Array.length values in
  let low = ref 0 in
  let high = ref (length - 1) in
  while !low <= !high do
    let middle = (!low + !high) / 2 in
    if total_cmp order_cmp equality_cmp values.(middle) value < 0 then low := middle + 1
    else high := middle - 1
  done;
  let index = !low in
  if index < length && total_cmp order_cmp equality_cmp values.(index) value = 0 then Some index else None

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

let array_mem_by order_cmp equality_cmp value values =
  match find_remove_index order_cmp equality_cmp value values with
  | Some _ -> true
  | None -> false

let array_mem_by_cmp cmp value values =
  match find_index_by_cmp cmp value values with
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

let rec add_to_tree settings order_cmp equality_cmp key_cmp value = function
  | Tree_leaf values ->
    (match find_insert_index order_cmp equality_cmp value values with
     | `Found _ -> Tree_edit_unchanged
     | `Insert index ->
       let inserted = array_insert values index value in
       let changed =
         if Array.length inserted <= settings.branching_factor then [ inserted ]
         else array_split inserted
       in
       Tree_edit_changed (tree_leaf_refs_of_arrays changed))
  | Tree_branch (keys, children) ->
    let index = find_child_index key_cmp value keys in
    (match add_to_tree settings order_cmp equality_cmp key_cmp value children.(index) with
     | Tree_edit_unchanged -> Tree_edit_unchanged
     | Tree_edit_changed [ key, child ] ->
       branch_replace_one settings keys children index key child
       |> fun changed -> Tree_edit_changed changed
     | Tree_edit_changed changed ->
       let keys, children = branch_splice_one keys children index changed in
       tree_branch_refs_of_arrays settings keys children
       |> fun changed -> Tree_edit_changed changed)

let rec remove_from_tree settings order_cmp equality_cmp key_cmp value = function
  | Tree_leaf values ->
    (match find_remove_index order_cmp equality_cmp value values with
     | None -> Tree_edit_unchanged
     | Some index ->
       array_remove values index
       |> (function
         | [||] -> Tree_edit_changed []
         | values -> Tree_edit_changed [ values.(Array.length values - 1), Tree_leaf values ]))
  | Tree_branch (keys, children) ->
    let index = find_child_index key_cmp value keys in
    (match remove_from_tree settings order_cmp equality_cmp key_cmp value children.(index) with
     | Tree_edit_unchanged -> Tree_edit_unchanged
     | Tree_edit_changed [ key, child ] ->
       branch_replace_one settings keys children index key child
       |> fun changed -> Tree_edit_changed changed
     | Tree_edit_changed changed ->
       let keys, children = branch_splice_one keys children index changed in
       tree_branch_refs_of_arrays settings keys children
       |> fun changed -> Tree_edit_changed changed)

let add_to_stored_chunks settings order_cmp equality_cmp value stored_chunks =
  let rec loop acc = function
    | [] -> List.rev acc
    | (chunk, address) :: rest ->
      let target =
        match last chunk with
        | None -> rest = []
        | Some last_value -> order_cmp value last_value <= 0 || rest = []
      in
      if target then
        let inserted = insert_unique order_cmp equality_cmp value chunk in
        let changed_chunks =
          inserted
          |> chunks settings.branching_factor
          |> List.map (fun chunk -> chunk, None)
        in
        List.rev_append acc (changed_chunks @ rest)
      else loop ((chunk, address) :: acc) rest
  in
  loop [] stored_chunks

let add ?cmp value set =
  let equality_cmp, key_cmp =
    match cmp with
    | Some cmp ->
      let equality_cmp = normalize_cmp cmp in
      equality_cmp, (fun value key -> route_cmp set.cmp equality_cmp value key)
    | None -> set.cmp, set.cmp
  in
  match set.data with
  | Deferred { storage; address } ->
    (match add_to_address storage set.set_settings set.cmp equality_cmp value address with
     | Edit_unchanged -> set
     | Edit_changed changed ->
       { set with
         data = edited_data_of_changed_refs storage changed
       ; root_address = None
       ; stored_addresses = None
       ; stored_chunks = []
       ; stored_branch_chunks = []
       })
  | Tree tree ->
    if set.stored_chunks <> [] then (
      let previous_values = materialize_tree tree in
      let values = insert_unique set.cmp equality_cmp value previous_values in
      let stored_chunks =
        if values = previous_values then set.stored_chunks
        else add_to_stored_chunks set.set_settings set.cmp equality_cmp value set.stored_chunks
      in
      { set with
        data = data_of_sorted_values set.set_settings values
      ; root_address = (if values = previous_values then set.root_address else None)
      ; stored_addresses = (if values = previous_values then set.stored_addresses else None)
      ; stored_chunks
      })
    else
      (match add_to_tree set.set_settings set.cmp equality_cmp key_cmp value tree with
       | Tree_edit_unchanged -> set
       | Tree_edit_changed changed ->
         { set with
           data =
             (match tree_of_refs set.set_settings changed with
              | None -> Loaded []
              | Some tree -> Tree tree)
         ; root_address = None
         ; stored_addresses = None
         ; stored_chunks = []
         ; stored_branch_chunks = []
         })
  | Edited { storage; tree } ->
    (match add_to_edited_tree storage set.set_settings set.cmp equality_cmp value tree with
     | Edit_unchanged -> set
     | Edit_changed changed ->
      { set with
        data = edited_data_of_changed_refs storage changed
       ; root_address = None
       ; stored_addresses = None
       ; stored_chunks = []
       ; stored_branch_chunks = []
       })
  | Loaded _ ->
    let previous_values = materialize set in
    let values = insert_unique set.cmp equality_cmp value previous_values in
    let stored_chunks =
      if values = previous_values then set.stored_chunks
      else add_to_stored_chunks set.set_settings set.cmp equality_cmp value set.stored_chunks
    in
    { set with
      data = data_of_sorted_values set.set_settings values
    ; root_address = (if values = previous_values then set.root_address else None)
    ; stored_addresses = (if values = previous_values then set.stored_addresses else None)
    ; stored_chunks
    }

let of_list_by ?settings cmp values =
  List.fold_left (fun set value -> add value set) (empty_by ?settings cmp) values

let of_list values = of_list_by default_cmp values

let distinct_sorted_values cmp values =
  let rec loop acc = function
    | [] -> List.rev acc
    | value :: rest ->
      (match acc with
       | previous :: _ when cmp previous value = 0 -> loop acc rest
       | _ -> loop (value :: acc) rest)
  in
  loop [] values

let of_sorted_array_by ?settings cmp values =
  let set = empty_by ?settings cmp in
  let values_list = ref [] in
  for i = Array.length values - 1 downto 0 do
    values_list := values.(i) :: !values_list
  done;
  let values = distinct_sorted_values set.cmp !values_list in
  { set with data = data_of_sorted_values set.set_settings values }

let of_sorted_array values =
  let set = empty () in
  let values_list = ref [] in
  for i = Array.length values - 1 downto 0 do
    values_list := values.(i) :: !values_list
  done;
  let values = distinct_sorted_values set.cmp !values_list in
  { set with data = data_of_sorted_values set.set_settings values }

let remove_from_stored_chunks order_cmp equality_cmp value stored_chunks =
  let rec loop acc = function
    | [] -> List.rev acc
    | (chunk, address) :: rest ->
      let chunk' = remove_by order_cmp equality_cmp value chunk in
      if chunk' = chunk then loop ((chunk, address) :: acc) rest
      else
        let acc =
          match chunk' with
          | [] -> acc
          | _ :: _ -> (chunk', None) :: acc
        in
        List.rev_append acc rest
  in
  loop [] stored_chunks

let remove ?cmp value set =
  let equality_cmp, key_cmp =
    match cmp with
    | Some cmp ->
      let equality_cmp = normalize_cmp cmp in
      equality_cmp, (fun value key -> route_cmp set.cmp equality_cmp value key)
    | None -> set.cmp, set.cmp
  in
  match set.data with
  | Deferred { storage; address } ->
    (match remove_from_address storage set.set_settings set.cmp equality_cmp value address with
     | Edit_unchanged -> set
     | Edit_changed changed ->
       { set with
         data = edited_data_of_changed_refs storage changed
       ; root_address = None
       ; stored_addresses = None
       ; stored_chunks = []
       ; stored_branch_chunks = []
       })
  | Tree tree ->
    if set.stored_chunks <> [] then (
      let previous_values = materialize_tree tree in
      let values = remove_by set.cmp equality_cmp value previous_values in
      let stored_chunks =
        if values = previous_values then set.stored_chunks
        else remove_from_stored_chunks set.cmp equality_cmp value set.stored_chunks
      in
      { set with
        data = data_of_sorted_values set.set_settings values
      ; root_address = (if values = previous_values then set.root_address else None)
      ; stored_addresses = (if values = previous_values then set.stored_addresses else None)
      ; stored_chunks
      })
    else
      (match remove_from_tree set.set_settings set.cmp equality_cmp key_cmp value tree with
       | Tree_edit_unchanged -> set
       | Tree_edit_changed changed ->
         { set with
           data =
             (match tree_of_refs set.set_settings changed with
              | None -> Loaded []
              | Some tree -> Tree tree)
         ; root_address = None
         ; stored_addresses = None
         ; stored_chunks = []
         ; stored_branch_chunks = []
         })
  | Edited { storage; tree } ->
    (match remove_from_edited_tree storage set.set_settings set.cmp equality_cmp value tree with
     | Edit_unchanged -> set
     | Edit_changed changed ->
       { set with
         data = edited_data_of_changed_refs storage changed
       ; root_address = None
       ; stored_addresses = None
       ; stored_chunks = []
       ; stored_branch_chunks = []
       })
  | Loaded _ ->
    let previous_values = materialize set in
    let values = remove_by set.cmp equality_cmp value previous_values in
    let stored_chunks =
      if values = previous_values then set.stored_chunks
      else remove_from_stored_chunks set.cmp equality_cmp value set.stored_chunks
    in
    { set with
      data = data_of_sorted_values set.set_settings values
    ; root_address = (if values = previous_values then set.root_address else None)
    ; stored_addresses = (if values = previous_values then set.stored_addresses else None)
    ; stored_chunks
    }

let rec mem_by order_cmp equality_cmp value = function
  | [] -> false
  | current :: rest ->
    (match order_cmp value current with
     | n when n < 0 -> false
     | 0 -> equality_cmp value current = 0 || mem_by order_cmp equality_cmp value rest
     | _ -> mem_by order_cmp equality_cmp value rest)

type search_step =
  | Found
  | Stop
  | Continue

let rec search_deferred storage order_cmp equality_cmp value address =
  storage.accessed address;
  match storage.restore_node address with
  | Some (Leaf values) ->
    if mem_by order_cmp equality_cmp value values then Found
    else
      (match last values with
       | Some last_value when order_cmp value last_value <= 0 -> Stop
       | _ -> Continue)
  | Some (Branch (keys, child_addresses)) ->
    let rec choose_child keys child_addresses =
      match keys, child_addresses with
      | [], [] -> None
      | key :: _, child_address :: _ when route_cmp order_cmp equality_cmp value key <= 0 -> Some child_address
      | _ :: keys, _ :: child_addresses -> choose_child keys child_addresses
      | [], _ :: _ | _ :: _, [] -> invalid_arg "branch keys and addresses arity mismatch"
    in
    (match choose_child keys child_addresses with
     | Some child_address -> search_deferred storage order_cmp equality_cmp value child_address
     | None -> Continue)
  | None -> invalid_arg ("stored node not found: " ^ address)

let mem_in_deferred storage order_cmp equality_cmp value address =
  match search_deferred storage order_cmp equality_cmp value address with
  | Found -> true
  | Stop | Continue -> false

let rec search_edited_tree storage order_cmp equality_cmp value = function
  | Edited_ref (_, address) -> search_deferred storage order_cmp equality_cmp value address
  | Edited_leaf (values, _) ->
    if mem_by order_cmp equality_cmp value values then Found
    else
      (match last values with
       | Some last_value when order_cmp value last_value <= 0 -> Stop
       | _ -> Continue)
  | Edited_branch (children, _) ->
    let rec choose_child = function
      | [] -> None
      | (key, child) :: _ when route_cmp order_cmp equality_cmp value key <= 0 -> Some child
      | _ :: rest -> choose_child rest
    in
    (match choose_child children with
     | Some child -> search_edited_tree storage order_cmp equality_cmp value child
     | None -> Continue)

let mem_in_edited_tree storage order_cmp equality_cmp value tree =
  match search_edited_tree storage order_cmp equality_cmp value tree with
  | Found -> true
  | Stop | Continue -> false

let rec search_tree order_cmp equality_cmp key_cmp value = function
  | Tree_leaf values ->
    if array_mem_by order_cmp equality_cmp value values then Found
    else if Array.length values > 0 && order_cmp value values.(Array.length values - 1) <= 0 then Stop
    else Continue
  | Tree_branch (keys, children) ->
    let index = find_child_index key_cmp value keys in
    search_tree order_cmp equality_cmp key_cmp value children.(index)

let mem_in_tree order_cmp equality_cmp key_cmp value tree =
  match search_tree order_cmp equality_cmp key_cmp value tree with
  | Found -> true
  | Stop | Continue -> false

let rec search_tree_by_cmp cmp value = function
  | Tree_leaf values ->
    if array_mem_by_cmp cmp value values then Found
    else if Array.length values > 0 && cmp value values.(Array.length values - 1) <= 0 then Stop
    else Continue
  | Tree_branch (keys, children) ->
    let index = find_child_index cmp value keys in
    search_tree_by_cmp cmp value children.(index)

let mem_in_tree_by_cmp cmp value tree =
  match search_tree_by_cmp cmp value tree with
  | Found -> true
  | Stop | Continue -> false

let mem ?cmp value set =
  match cmp, set.data with
  | None, Loaded values -> mem_by set.cmp set.cmp value values
  | None, Tree tree -> mem_in_tree_by_cmp set.cmp value tree
  | None, Deferred { storage; address } -> mem_in_deferred storage set.cmp set.cmp value address
  | None, Edited { storage; tree } -> mem_in_edited_tree storage set.cmp set.cmp value tree
  | Some cmp, data ->
    let equality_cmp = normalize_cmp cmp in
    let key_cmp value key = route_cmp set.cmp equality_cmp value key in
    (match data with
     | Loaded values -> mem_by set.cmp equality_cmp value values
     | Tree tree -> mem_in_tree set.cmp equality_cmp key_cmp value tree
     | Deferred { storage; address } -> mem_in_deferred storage set.cmp equality_cmp value address
     | Edited { storage; tree } -> mem_in_edited_tree storage set.cmp equality_cmp value tree)

let rec fold_edited_tree storage f init = function
  | Edited_ref (_, address) -> List.fold_left f init (materialize_address storage address)
  | Edited_leaf (values, _) -> List.fold_left f init values
  | Edited_branch (children, _) ->
    List.fold_left
      (fun acc (_, child) -> fold_edited_tree storage f acc child)
      init
      children

let rec fold_tree f init = function
  | Tree_leaf values -> Array.fold_left f init values
  | Tree_branch (_, children) ->
    Array.fold_left (fun acc child -> fold_tree f acc child) init children

let count (set : 'a t) =
  match set.data with
  | Tree tree -> fold_tree (fun count _ -> count + 1) 0 tree
  | Edited { storage; tree } -> fold_edited_tree storage (fun count _ -> count + 1) 0 tree
  | Loaded _ | Deferred _ -> List.length (materialize set)

let to_list (set : 'a t) = materialize set

let fold f init set =
  match set.data with
  | Tree tree -> fold_tree f init tree
  | Edited { storage; tree } -> fold_edited_tree storage f init tree
  | Loaded _ | Deferred _ -> List.fold_left f init (materialize set)

let fold_list f init values = List.fold_left f init values

let lower_ok cmp from_ value =
  match from_ with
  | None -> true
  | Some from_ -> cmp value from_ >= 0

let upper_ok cmp to_ value =
  match to_ with
  | None -> true
  | Some to_ -> cmp value to_ <= 0

let child_before_range cmp from_ child_max =
  match from_ with
  | None -> false
  | Some from_ -> cmp child_max from_ < 0

let child_after_range cmp to_ previous_child_max =
  match to_, previous_child_max with
  | Some to_, Some previous_child_max -> cmp previous_child_max to_ > 0
  | _ -> false

let slice_values cmp from_ to_ values =
  values
  |> List.filter (fun value -> lower_ok cmp from_ value && upper_ok cmp to_ value)

let reverse_slice_values cmp from_ to_ values =
  values
  |> List.rev
  |> List.filter (fun value ->
    match from_, to_ with
    | None, None -> true
    | Some from_, None -> cmp value from_ <= 0
    | None, Some to_ -> cmp value to_ >= 0
    | Some from_, Some to_ -> cmp value from_ <= 0 && cmp value to_ >= 0)

let rec slice_deferred storage cmp from_ to_ address =
  storage.accessed address;
  match storage.restore_node address with
  | Some (Leaf values) ->
    slice_values cmp from_ to_ values
  | Some (Branch (keys, child_addresses)) ->
    let rec collect previous_key acc keys child_addresses =
      match keys, child_addresses with
      | [], [] -> List.rev acc |> List.concat
      | key :: keys, child_address :: child_addresses ->
        if child_after_range cmp to_ previous_key then List.rev acc |> List.concat
        else if child_before_range cmp from_ key then collect (Some key) acc keys child_addresses
        else
          let values = slice_deferred storage cmp from_ to_ child_address in
          collect (Some key) (values :: acc) keys child_addresses
      | [], _ :: _ | _ :: _, [] -> invalid_arg "branch keys and addresses arity mismatch"
    in
    collect None [] keys child_addresses
  | None -> invalid_arg ("stored node not found: " ^ address)

let rec reverse_slice_deferred storage cmp from_ to_ address =
  storage.accessed address;
  match storage.restore_node address with
  | Some (Leaf values) ->
    reverse_slice_values cmp from_ to_ values
  | Some (Branch (keys, child_addresses)) ->
    let rec annotate previous_key acc keys child_addresses =
      match keys, child_addresses with
      | [], [] -> acc
      | key :: keys, child_address :: child_addresses ->
        annotate (Some key) ((previous_key, key, child_address) :: acc) keys child_addresses
      | [], _ :: _ | _ :: _, [] -> invalid_arg "branch keys and addresses arity mismatch"
    in
    let child_refs = annotate None [] keys child_addresses in
    let rec collect acc = function
      | [] -> List.rev acc |> List.concat
      | (previous_key, key, child_address) :: rest ->
        let child_above_range =
          match from_, previous_key with
          | Some from_, Some previous_key -> cmp previous_key from_ > 0
          | _ -> false
        in
        let child_below_range =
          match to_ with
          | Some to_ -> cmp key to_ < 0
          | None -> false
        in
        if child_above_range then collect acc rest
        else if child_below_range then List.rev acc |> List.concat
        else
          let values = reverse_slice_deferred storage cmp from_ to_ child_address in
          collect (values :: acc) rest
    in
    collect [] child_refs
  | None -> invalid_arg ("stored node not found: " ^ address)

let slice ?from_ ?to_ ?cmp (set : 'a t) =
  let cmp = Option.value ~default:set.cmp cmp in
  match set.data with
  | Loaded values -> slice_values cmp from_ to_ values
  | Tree tree -> slice_values cmp from_ to_ (materialize_tree tree)
  | Deferred { storage; address } -> slice_deferred storage cmp from_ to_ address
  | Edited _ -> slice_values cmp from_ to_ (materialize set)

let rslice ?from_ ?to_ ?cmp (set : 'a t) =
  let cmp = Option.value ~default:set.cmp cmp in
  match set.data with
  | Loaded values -> reverse_slice_values cmp from_ to_ values
  | Tree tree -> reverse_slice_values cmp from_ to_ (materialize_tree tree)
  | Deferred { storage; address } -> reverse_slice_deferred storage cmp from_ to_ address
  | Edited _ -> reverse_slice_values cmp from_ to_ (materialize set)

let seq_source_of_set set =
  match set.data with
  | Loaded values -> Seq_values values
  | Tree tree -> Seq_values (materialize_tree tree)
  | Deferred { storage; address } -> Seq_deferred { storage; address }
  | Edited _ -> Seq_values (materialize set)

let seq (set : 'a t) =
  { set_cmp = set.cmp
  ; direction = Asc
  ; source = seq_source_of_set set
  ; lower = None
  ; upper = None
  }

let rseq (set : 'a t) =
  { set_cmp = set.cmp
  ; direction = Desc
  ; source = seq_source_of_set set
  ; lower = None
  ; upper = None
  }

let seq_to_list seq =
  match seq.source, seq.direction with
  | Seq_values values, Asc -> slice_values seq.set_cmp seq.lower seq.upper values
  | Seq_values values, Desc -> reverse_slice_values seq.set_cmp seq.upper seq.lower values
  | Seq_deferred { storage; address }, Asc ->
    slice_deferred storage seq.set_cmp seq.lower seq.upper address
  | Seq_deferred { storage; address }, Desc ->
    reverse_slice_deferred storage seq.set_cmp seq.upper seq.lower address

let seq_reverse seq =
  let direction =
    match seq.direction with
    | Asc -> Desc
    | Desc -> Asc
  in
  { seq with direction }

let fold_seq f init seq = List.fold_left f init (seq_to_list seq)

let slice_seq ?from_ ?to_ ?cmp set =
  let set_cmp = Option.value ~default:set.cmp cmp |> normalize_cmp in
  { set_cmp
  ; direction = Asc
  ; source = seq_source_of_set set
  ; lower = from_
  ; upper = to_
  }

let rslice_seq ?from_ ?to_ ?cmp set =
  let set_cmp = Option.value ~default:set.cmp cmp |> normalize_cmp in
  { set_cmp
  ; direction = Desc
  ; source = seq_source_of_set set
  ; lower = to_
  ; upper = from_
  }

let max_bound cmp left right =
  match left, right with
  | None, bound | bound, None -> bound
  | Some left, Some right -> Some (if cmp left right >= 0 then left else right)

let min_bound cmp left right =
  match left, right with
  | None, bound | bound, None -> bound
  | Some left, Some right -> Some (if cmp left right <= 0 then left else right)

let seek ?cmp key seq =
  let cmp = Option.value ~default:seq.set_cmp cmp in
  let lower, upper =
    match seq.direction with
    | Asc -> max_bound cmp seq.lower (Some key), seq.upper
    | Desc -> seq.lower, min_bound cmp seq.upper (Some key)
  in
  { seq with lower; upper }

let split_branch_refs refs =
  List.split refs

let branch_key refs =
  match last refs with
  | Some (key, _) -> key
  | None -> invalid_arg "branch requires at least one child"

let store_branch storage reusable_branch_chunks child_refs =
  match List.assoc_opt child_refs reusable_branch_chunks with
  | Some (Some address) -> address
  | Some None
  | None ->
    let keys, child_addresses = split_branch_refs child_refs in
    storage.store_node (Branch (keys, child_addresses))

let rec store_branch_tree storage settings reusable_branch_chunks child_refs =
  if List.length child_refs <= settings.branching_factor then
    let address = store_branch storage reusable_branch_chunks child_refs in
    address, [ address ], [ child_refs, Some address ]
  else
    let child_groups = chunks settings.branching_factor child_refs in
    let branch_addresses =
      child_groups
      |> List.map (fun child_group ->
        let address = store_branch storage reusable_branch_chunks child_group in
        branch_key child_group, address)
    in
    let root_address, branch_tree_addresses, branch_chunks =
      store_branch_tree storage settings reusable_branch_chunks branch_addresses
    in
    ( root_address
    , branch_tree_addresses @ List.map snd branch_addresses
    , branch_chunks
      @ List.map2
          (fun child_group (_, address) -> child_group, Some address)
          child_groups
          branch_addresses )

let rec store_edited_tree storage settings = function
  | Edited_ref (_, address) -> address, [ address ]
  | Edited_leaf (_, Some address) -> address, [ address ]
  | Edited_leaf (values, None) ->
    let address = storage.store_node (Leaf values) in
    address, [ address ]
  | Edited_branch (_, Some address) -> address, [ address ]
  | Edited_branch (children, None) ->
    let stored_children =
      children
      |> List.map (fun (key, child) ->
        let address, addresses = store_edited_tree storage settings child in
        (key, address), addresses)
    in
    let child_refs, child_address_lists = List.split stored_children in
    let address, branch_addresses, _ = store_branch_tree storage settings [] child_refs in
    address, branch_addresses @ List.concat child_address_lists

let store storage set =
  match set.root_address with
  | Some address -> address, set
  | None ->
    (match set.data with
     | Edited { storage = source_storage; tree } when source_storage == storage ->
       let address, addresses = store_edited_tree storage set.set_settings tree in
       address,
       { set with
         root_address = Some address
       ; stored_addresses = Some addresses
       ; stored_chunks = []
       ; stored_branch_chunks = []
       }
     | Loaded _ | Tree _ | Deferred _ | Edited _ ->
       let values = materialize set in
       let preferred_chunks = List.map fst set.stored_chunks in
       let leaf_chunks =
         if preferred_chunks <> [] && List.concat preferred_chunks = values then preferred_chunks
         else chunks set.set_settings.branching_factor values
       in
       let store_chunk chunk =
         match List.assoc_opt chunk set.stored_chunks with
         | Some (Some address) -> address
         | Some None
         | None -> storage.store_node (Leaf chunk)
       in
       (match leaf_chunks with
        | [] | [ _ ] ->
          let address = storage.store_node (Leaf values) in
          address,
          { set with
            root_address = Some address
          ; stored_addresses = Some [ address ]
          ; stored_chunks = [ values, Some address ]
          ; stored_branch_chunks = []
          }
        | chunks ->
          let child_addresses = chunks |> List.map store_chunk in
          let child_refs =
            List.map2
              (fun chunk address ->
                 match last chunk with
                 | Some key -> key, address
                 | None -> invalid_arg "leaf chunk cannot be empty")
              chunks
              child_addresses
          in
          let address, branch_addresses, stored_branch_chunks =
            store_branch_tree storage set.set_settings set.stored_branch_chunks child_refs
          in
          address,
          { set with
            root_address = Some address
          ; stored_addresses = Some (branch_addresses @ child_addresses)
          ; stored_chunks = List.combine chunks (List.map (fun address -> Some address) child_addresses)
          ; stored_branch_chunks
          }))

let restore ~cmp ?(settings = default_settings) storage address =
  let settings = validate_settings settings in
  Some
    { cmp = normalize_cmp cmp
    ; set_settings = settings
    ; data = Deferred { storage; address }
    ; root_address = Some address
    ; stored_addresses = Some [ address ]
    ; stored_chunks = []
    ; stored_branch_chunks = []
    }

let rec walk_storage_addresses storage address =
  storage.accessed address;
  match storage.restore_node address with
  | Some (Branch (_, child_addresses)) ->
    address :: List.concat_map (walk_storage_addresses storage) child_addresses
  | Some (Leaf _) ->
    [ address ]
  | None -> invalid_arg ("stored node not found: " ^ address)

let rec walk_branch_chunk_addresses branch_chunks address =
  let child_refs =
    branch_chunks
    |> List.find_map (fun (child_refs, stored_address) ->
      match stored_address with
      | Some stored_address when stored_address = address -> Some child_refs
      | Some _ | None -> None)
  in
  match child_refs with
  | None -> [ address ]
  | Some child_refs ->
    address
    :: (child_refs
        |> List.concat_map (fun (_, child_address) -> walk_branch_chunk_addresses branch_chunks child_address))

let walk_addresses set =
  match set.root_address with
  | None -> []
  | Some address ->
    (match set.data with
     | Deferred { storage; address } -> walk_storage_addresses storage address
     | Loaded _ | Tree _ | Edited _ ->
       if set.stored_branch_chunks <> [] then
         walk_branch_chunk_addresses set.stored_branch_chunks address
       else
         match set.stored_addresses with
         | Some addresses -> addresses
         | None -> [ address ])
