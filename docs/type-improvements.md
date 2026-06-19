# Type Improvement Notes

This document focuses on type-system improvements for the OCaml implementation.
The goal is not to make the API clever. The goal is to make invalid states
harder to represent while keeping the module simple to use.

## Current Type Shape

The public interface currently exposes:

```ocaml
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

type 'a t
type 'a seq
```

This is small and easy to call, but several invariants are represented only by
runtime checks or convention.

## Main Type Gaps

### 1. Settings Can Be Constructed Invalidly

`branching_factor` must be at least 2, but callers can construct an invalid
record and only discover the problem when passing it to a constructor.

Recommendation:

- Make `settings` private.
- Add `make_settings : branching_factor:int -> settings option` and, if needed,
  `make_settings_exn : branching_factor:int -> settings`.

Example:

```ocaml
type settings = private
  { branching_factor : int
  }

val make_settings : branching_factor:int -> settings option
val make_settings_exn : branching_factor:int -> settings
```

Benefit:

- Invalid settings cannot move through the program unnoticed.

### 2. Storage Address Is Hard-Coded to `string`

The current storage API fixes addresses to strings. That is convenient for
tests, but some callers may naturally use integers, hashes, structured keys, or
content-addressed values.

Simple option:

- Keep string addresses for now, because it keeps the public API simple.
- Introduce a named alias:

```ocaml
type address = string
```

Better long-term option:

```ocaml
type ('a, 'address) storage =
  { store_node : 'a stored_node -> 'address
  ; restore_node : 'address -> 'a stored_node option
  ; accessed : 'address -> unit
  }

type ('a, 'address) t
```

Tradeoff:

- Generic addresses improve type precision.
- They also spread an extra type parameter through the whole API.

Recommendation:

- Add `type address = string` now.
- Only move to `('a, 'address) t` if a real caller needs non-string addresses.

### 3. Stored Branch Shape Is Too Loose

`Branch of 'a list * string list` requires the two lists to have the same length,
and each key must be the max key of the matching child. The type does not express
either invariant.

Recommendation:

Use named records:

```ocaml
type 'a stored_node =
  | Leaf of
      { values : 'a list
      }
  | Branch of
      { keys : 'a list
      ; children : address list
      }
```

Then add a validation helper:

```ocaml
val validate_stored_node : 'a stored_node -> (unit, string) result
```

Benefit:

- Call sites become clearer.
- Error messages can name `keys` and `children`.
- Future metadata can be added without changing tuple order.

### 4. Missing Nodes Are Only `option`

`restore_node : address -> stored_node option` cannot distinguish:

- address not found
- decode error
- storage unavailable
- malformed node

Recommendation:

Keep the simple callback for compatibility, but consider a richer variant for a
new API:

```ocaml
type storage_error =
  | Missing_address of address
  | Decode_error of string
  | Invalid_node of string
  | Storage_error of string

type 'a storage_v2 =
  { store_node : 'a stored_node -> (address, storage_error) result
  ; restore_node : address -> ('a stored_node, storage_error) result
  ; accessed : address -> unit
  }
```

Benefit:

- Better diagnostics.
- Easier integration with real storage backends.

### 5. Count and Depth Metadata Are Missing From Types

Upstream restored sets can carry count and depth metadata. OCaml restored sets
currently only know root address, comparator, settings, and storage.

Recommendation:

Add a metadata type:

```ocaml
type metadata =
  { root : address
  ; count : int
  ; depth : int
  ; settings : settings
  }
```

Use it in new APIs:

```ocaml
val store_with_metadata : 'a storage -> 'a t -> metadata * 'a t
val restore_with_metadata :
  cmp:'a comparator -> 'a storage -> metadata -> 'a t option
```

Benefit:

- O(1) count for restored sets.
- Depth can be known without reading the root.
- Better parity with upstream `BTSet` fields.

### 6. Comparator Is a Raw Function

The type alias is convenient, but it does not state whether the function is
normalized, total, or stable. The implementation normalizes custom comparators
at construction and some operation boundaries.

Recommendation:

Use an internal comparator record first:

```ocaml
type 'a normalized_comparator = private
  { compare : 'a -> 'a -> int
  }
```

Public API can still accept raw functions:

```ocaml
val empty_by : ?settings:settings -> 'a comparator -> 'a t
```

Internally, store only `normalized_comparator`.

Benefit:

- Avoids accidental use of unnormalized comparators internally.
- Keeps public API unchanged.

Avoid for now:

- Phantom comparator identity types. They are possible, but they would make the
  API heavier without preventing all runtime comparator mistakes.

### 7. Bounds Should Have a Named Type Internally

The implementation uses option values directly for bounds:

```ocaml
lower : 'a option
upper : 'a option
```

That is compact, but range direction and `rslice` bound reversal are easy to
misread.

Recommendation:

Use internal names:

```ocaml
type 'a bound =
  | Unbounded
  | Included of 'a

type 'a range =
  { lower : 'a bound
  ; upper : 'a bound
  }
```

Public optional arguments can remain unchanged.

Benefit:

- Clearer code in slice, reverse slice, and seek.
- Easier to add exclusive bounds later if needed.

### 8. Sequence Direction Could Be More Explicit

Current sequence type stores:

```ocaml
type direction = Asc | Desc
```

This is fine publicly because `'a seq` is opaque. Internally, it allows bound
mixups.

Recommendation:

Keep the public `'a seq`, but introduce internal helpers:

```ocaml
type asc
type desc

type ('a, 'dir) cursor
```

Only use phantom direction if the deferred cursor refactor happens. Do not add
it only for style.

Benefit:

- Direction-specific cursor code can avoid accidental lower/upper reversal.

### 9. Internal Nodes Need Non-Empty Invariants

Many helpers assume leaves and branches are non-empty:

- `last_exn`
- `tree_ref_key`
- `edited_branch_key`
- branch key/address arity checks

Recommendation:

Use small internal constructors:

```ocaml
module Non_empty_array : sig
  type 'a t
  val of_array : 'a array -> 'a t option
  val to_array : 'a t -> 'a array
  val last : 'a t -> 'a
end
```

Use it only internally. Do not expose it unless callers need it.

Benefit:

- Fewer `invalid_arg "tree node cannot be empty"` paths.
- More invariants checked at construction boundaries.

### 10. Storage Clean/Dirty State Should Be Typed

Current internal nodes use address options and separate chunk maps. A clearer
internal type would be:

```ocaml
type address_state =
  | Fresh
  | Stored of address
```

or:

```ocaml
type dirty =
  | Clean of address
  | Dirty
```

Benefit:

- Makes store behavior clearer.
- Removes ambiguity between "never stored" and "stored but address forgotten".

## Suggested Type Migration Order

1. Add `type address = string`.
2. Make `settings` private and add constructors.
3. Convert public `stored_node` variants to records in a breaking-version branch,
   or add new `stored_node_v2` APIs first.
4. Add metadata APIs while keeping existing `store` and `restore`.
5. Internally wrap normalized comparators.
6. Introduce internal bound/range types.
7. Refactor internal node storage state when the tree/address unification work
   starts.

This order keeps the early changes small and useful, while leaving the larger
type redesign tied to implementation work that actually benefits from it.
