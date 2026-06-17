type 'a comparator = 'a -> 'a -> int

type settings =
  { branching_factor : int
  }

type 'a stored_node =
  | Values of 'a list
  | Leaf of 'a list
  | Branch of 'a list * string list

type 'a storage =
  { store_node : 'a stored_node -> string
  ; restore_node : string -> 'a stored_node option
  ; accessed : string -> unit
  }

type 'a data =
  | Loaded of 'a list
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

let default_cmp left right = normalize_cmp Stdlib.compare left right

let default_settings = { branching_factor = 32 }

let validate_settings settings =
  if settings.branching_factor < 2 then invalid_arg "branching_factor must be at least 2";
  settings

let settings set = set.set_settings

let empty_by ?(settings = default_settings) cmp =
  let settings = validate_settings settings in
  { cmp = normalize_cmp cmp
  ; set_settings = settings
  ; data = Loaded []
  ; root_address = None
  ; stored_addresses = None
  ; stored_chunks = []
  ; stored_branch_chunks = []
  }

let empty () = empty_by default_cmp

let rec chunks size values =
  match values with
  | [] -> []
  | values ->
    let rec take count acc rest =
      if count = 0 then List.rev acc, rest
      else
        match rest with
        | [] -> List.rev acc, []
        | value :: rest -> take (count - 1) (value :: acc) rest
    in
    let chunk, rest = take size [] values in
    chunk :: chunks size rest

let rec materialize_address storage address =
  storage.accessed address;
  match storage.restore_node address with
  | Some (Values values)
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

let materialize set =
  match set.data with
  | Loaded values -> values
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

let rec add_to_address storage settings order_cmp equality_cmp value address =
  match restore_stored_node storage address with
  | Values values
  | Leaf values ->
    let inserted = insert_unique order_cmp equality_cmp value values in
    if inserted = values then Edit_unchanged
    else Edit_changed (refs_of_leaf_chunks (chunks settings.branching_factor inserted))
  | Branch (keys, child_addresses) ->
    let children = branch_child_refs keys child_addresses in
    let rec loop prefix = function
      | [] -> Edit_unchanged
      | (key, Edited_ref (_, child_address)) :: rest
        when order_cmp value key <= 0 || rest = [] ->
        (match add_to_address storage settings order_cmp equality_cmp value child_address with
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
  | Values values
  | Leaf values ->
    let removed = remove_by order_cmp equality_cmp value values in
    if removed = values then Edit_unchanged
    else Edit_changed (refs_of_leaf_chunks (chunks settings.branching_factor removed))
  | Branch (keys, child_addresses) ->
    let children = branch_child_refs keys child_addresses in
    let rec loop prefix = function
      | [] -> Edit_unchanged
      | (key, Edited_ref (_, child_address)) :: rest
        when order_cmp value key <= 0 || rest = [] ->
        (match remove_from_address storage settings order_cmp equality_cmp value child_address with
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
  let equality_cmp =
    match cmp with
    | Some cmp -> normalize_cmp cmp
    | None -> set.cmp
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
  | Loaded _ | Edited _ ->
    let previous_values = materialize set in
    let values = insert_unique set.cmp equality_cmp value previous_values in
    let stored_chunks =
      if values = previous_values then set.stored_chunks
      else add_to_stored_chunks set.set_settings set.cmp equality_cmp value set.stored_chunks
    in
    { set with
      data = Loaded values
    ; root_address = (if values = previous_values then set.root_address else None)
    ; stored_addresses = (if values = previous_values then set.stored_addresses else None)
    ; stored_chunks
    }

let of_list_by ?settings cmp values =
  List.fold_left (fun set value -> add value set) (empty_by ?settings cmp) values

let of_list values = of_list_by default_cmp values

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
  let equality_cmp =
    match cmp with
    | Some cmp -> normalize_cmp cmp
    | None -> set.cmp
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
  | Loaded _ | Edited _ ->
    let previous_values = materialize set in
    let values = remove_by set.cmp equality_cmp value previous_values in
    let stored_chunks =
      if values = previous_values then set.stored_chunks
      else remove_from_stored_chunks set.cmp equality_cmp value set.stored_chunks
    in
    { set with
      data = Loaded values
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
  | Some (Values values)
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
      | key :: _, child_address :: _ when order_cmp value key <= 0 -> Some child_address
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

let mem ?cmp value set =
  let equality_cmp =
    match cmp with
    | Some cmp -> normalize_cmp cmp
    | None -> set.cmp
  in
  match set.data with
  | Loaded values -> mem_by set.cmp equality_cmp value values
  | Deferred { storage; address } -> mem_in_deferred storage set.cmp equality_cmp value address
  | Edited _ -> mem_by set.cmp equality_cmp value (materialize set)

let count (set : 'a t) = List.length (materialize set)

let to_list (set : 'a t) = materialize set

let fold f init set = List.fold_left f init (materialize set)

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
  | Some (Values values)
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
  | Some (Values values)
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
          | Some from_, Some previous_key -> cmp previous_key from_ >= 0
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
  | Deferred { storage; address } -> slice_deferred storage cmp from_ to_ address
  | Edited _ -> slice_values cmp from_ to_ (materialize set)

let rslice ?from_ ?to_ ?cmp (set : 'a t) =
  let cmp = Option.value ~default:set.cmp cmp in
  match set.data with
  | Loaded values -> reverse_slice_values cmp from_ to_ values
  | Deferred { storage; address } -> reverse_slice_deferred storage cmp from_ to_ address
  | Edited _ -> reverse_slice_values cmp from_ to_ (materialize set)

let seq_source_of_set set =
  match set.data with
  | Loaded values -> Seq_values values
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
     | Loaded _ | Deferred _ | Edited _ ->
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

let walk_addresses set =
  match set.stored_addresses, set.root_address with
  | Some addresses, _ -> addresses
  | None, Some address -> [ address ]
  | None, None -> []
