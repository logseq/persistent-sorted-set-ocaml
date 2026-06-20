# Upstream Differences Report

This report compares the OCaml implementation in this repository with the local
upstream ClojureScript implementation at:

- Upstream repository: `/Users/tiensonqin/Codes/projects/persistent-sorted-set`
- Upstream commit inspected: `efc3add9af192abc64711bb58c3d1d2352097129`
- Upstream file: `src-clojure/me/tonsky/persistent_sorted_set.cljs`
- OCaml commit inspected: `f4ecbe720de97e08210a538c122bfdd59b98163d`
- OCaml files: `lib/persistent_sorted_set.ml`,
  `lib/persistent_sorted_set.mli`

The upstream test-name coverage helper reports:

```text
Upstream tests: 15
Covered by exact name or alias: 15
Missing name coverage: 0
Stale aliases: 0
```

That means every upstream test name has a local test or an explicit alias. It
does not mean every upstream runtime feature is implemented, because the OCaml
port intentionally exposes a smaller API and has additional OCaml-specific
storage and performance tests.

## Executive Summary

The OCaml implementation is behaviorally aligned with the upstream sorted-set
surface that matters for sorted DataScript-style index use: ordered insertion,
removal, membership, range slicing, reverse slicing, seeking, storing, restoring,
and lazy access to restored storage paths.

It is not a literal port. The upstream ClojureScript implementation is a
Clojure collection with protocols, metadata, hashing, chunked sequences, mutable
address caches, weak references, and path-encoded iterators. The OCaml
implementation is a standalone module with opaque set and sequence types,
explicit storage callbacks, optional comparator overrides, and a simpler
tree/edit representation.

The largest implementation differences are:

- Upstream keeps a rolling `cnt` and returns count in O(1). OCaml computes count
  by traversal and fully materializes restored trees for `count`.
- Upstream delete handling rebalances underfull B+ tree nodes with sibling
  rotation and merge. OCaml removes empty children and splits overflow nodes, but
  does not enforce the upstream 16..32 occupancy invariant after deletions.
- Upstream iterators are cursor/path based and can advance lazily through stored
  trees. OCaml `seq_to_list` can use pruned restored traversal, but `to_seq` for
  deferred and edited sets materializes before producing the first element.
- Upstream stores mutable node address caches and can call storage `delete`.
  OCaml storage has no delete operation and tracks reusable chunks outside the
  main in-memory tree.
- Upstream implements Clojure protocols. OCaml exposes a focused module API.

## Data Structure Shape

### Upstream ClojureScript

Upstream is a B+ tree:

- `Leaf` stores a JS array of values.
- `Node` stores parallel arrays:
  - `keys`: the maximum key for each child subtree.
  - `children`: child node references.
  - `_addresses`: stored child addresses.
- `BTSet` stores:
  - `storage`
  - `_root`
  - `shift`, the tree depth minus one
  - `cnt`, the set size
  - `comparator`
  - `meta`
  - `_hash`
  - `_address`

Upstream fixes the ClojureScript branching factor at `max-len = 32` and
`min-len = 16`. Insertions split full nodes. Deletions rotate or merge nodes so
non-root nodes stay within the intended B+ tree occupancy range.

### OCaml

OCaml uses several representations:

- `Tree` for fully in-memory sets:
  - `Tree_leaf of 'a array`
  - `Tree_branch of 'a array * 'a tree array`
- `Deferred` for a restored root address.
- `Edited` for a restored tree with local edits:
  - `Edited_ref` points to a stored node address.
  - `Edited_leaf` and `Edited_branch` hold changed nodes with optional stored
    addresses.
- Public storage nodes:
  - `Leaf of 'a list`
  - `Branch of 'a list * string list`

OCaml supports configurable `branching_factor` and `ref_type`, validated at
construction and restore time. The default branching factor is 32 and the
default ref type is `Weak`.

## Public API Differences

### Upstream Features Not Exposed in OCaml

The upstream ClojureScript set participates in Clojure collection protocols:

- metadata
- equality with other sets
- unordered hash
- function invocation as lookup
- sequence, reverse sequence, chunked sequence, and reduce protocols
- print protocols

The OCaml module does not implement equivalents for metadata, hash, equality
against other set types or Clojure protocol integration. This is a reasonable
simplification for an OCaml library, but it is a compatibility difference.

### OCaml Features Not Present in the Same Form Upstream

OCaml exposes:

- `settings : 'a t -> settings`
- `empty : unit -> 'a t`
- explicit `of_list`, `of_list_by`, `of_sorted_array`, and
  `of_sorted_array_by`
- `to_list`
- `fold_list`
- `seq_to_list`
- `to_seq`
- `walk_addresses : 'a t -> string list`
- public storage callbacks as an OCaml record

The shape is simpler for OCaml callers, but less transparent to the Clojure
collection model.

## Comparator and Nil/Option Semantics

Upstream cannot store `nil`, but it uses `nil` slice bounds to mean unbounded
ranges. Its tests also rely on comparator behavior where `nil` inside composite
keys can act as a wildcard.

OCaml uses optional arguments for range bounds:

- no argument means unbounded
- `Some value` is an inclusive bound

This lets OCaml sets store option values, such as `(string option * string
option)`, while still using omitted arguments for open range bounds. The local
tests cover this with the pair comparator wildcard cases.

OCaml also normalizes custom comparator results to `-1`, `0`, or `1` at the API
boundary. Upstream comparators only need to be negative, zero, or positive and
are used directly.

## Construction Differences

Upstream `from-sequential` sorts the full input array, removes adjacent
duplicates, and bulk-builds a tree with approximate 24-element chunks. This gives
good construction performance for large unordered inputs.

OCaml `of_list_by` currently folds `add` over the input. This preserves behavior
but is less direct than the upstream bulk build path. OCaml `of_sorted_array_by`
does bulk construction, but it converts the array to a list, removes adjacent
duplicates, chunks by `branching_factor`, and builds the tree from those chunks.

Main consequence:

- `of_sorted_array_by` is close to the upstream fast path.
- `of_list_by` is simpler but slower for large inputs than upstream
  `from-sequential`.

## Insert and Remove Differences

### Insert

Both implementations route by branch max keys and split overflowing nodes.

Upstream:

- inserts into a leaf by binary search
- returns one or two replacement nodes
- creates a new root when the old root splits
- keeps address arrays attached to nodes

OCaml:

- inserts into arrays by binary search for `Tree`
- inserts into lists for `Edited` and stored-node paths
- uses `tree_of_refs` or `refs_of_branch_chunks` to rebuild changed levels
- clears `root_address` and reusable address metadata after most normal edits

### Remove

Upstream:

- deletes by exact lookup
- rotates or merges with siblings when nodes become too small
- can shrink the root when it has only one child
- can notify storage to delete replaced addresses

OCaml:

- deletes from the target leaf
- drops empty leaves
- rebuilds changed ancestor refs
- can collapse the root through `tree_of_refs`
- does not rebalance underfull internal nodes
- does not have a storage delete callback

The OCaml behavior is simpler and passes current parity tests. The structural
difference matters for long-lived workloads with many deletes, because tree shape
can become less balanced than upstream expects.

## Storage and Restore Differences

### Upstream

Upstream stores nodes incrementally. Nodes remember `_address` and `_dirty`.
Stored child references may be replaced by weak references. Restoring a set
stores only root metadata initially; child nodes are fetched when traversed.

The storage protocol includes:

- `store`
- `restore`
- `accessed`
- `delete`

Upstream `restore-by` accepts metadata such as count and shift, which allows a
restored set to know count and depth without loading the whole tree.

### OCaml

OCaml storage includes:

- `store_node`
- `restore_node`
- `accessed`

It does not include `delete`, count metadata, or depth metadata. A restored set
is represented as a `Deferred` root address. Later edits use `Edited_ref` nodes
to preserve laziness and address reuse on the changed path.

OCaml has strong tests for restored-path laziness and address reuse. For example,
restored add/remove tests assert that only the root and target leaf are read for
shallow trees, and only root, intermediate branch, and leaf are read for nested
trees.

Main differences:

- Upstream stores address state inside the tree nodes.
- OCaml stores some reusable state in `stored_chunks` and
  `stored_branch_chunks` outside the tree.
- Upstream can delete obsolete storage addresses.
- OCaml never deletes old addresses.
- Upstream can restore with count/depth metadata.
- OCaml has to traverse or materialize for `count` on restored sets.

## Iteration, Slicing, and Seek Differences

### Upstream

Upstream iterators use encoded numeric paths. Iteration caches the current leaf
array and index. Advancing within the same leaf is cheap, and moving to another
leaf uses `next-path` or `prev-path`.

Slicing uses:

- `-seek*` for first element greater than or equal to lower bound
- `-rseek*` for first element greater than upper bound
- `Iter` and `ReverseIter` for lazy traversal within bounds

The iterator also supports chunked sequence operations and efficient reduce.

### OCaml

OCaml `seq` stores:

- source
- direction
- lower bound
- upper bound
- comparator

For in-memory trees, `slice_tree_seq` and `reverse_slice_tree_seq` produce
standard OCaml `Seq.t` values. For restored trees, `seq_to_list` and slices use
specialized deferred traversal that prunes non-overlapping branches.

Important difference:

- `seq_to_list` for `Seq_deferred` uses `slice_deferred` and can avoid
  irrelevant stored leaves.
- `to_seq` for `Seq_deferred` calls `materialize_address` before producing the
  sequence, so it loads the entire restored tree.

This means the OCaml sequence abstraction is only partially lazy. It is lazy at
construction and for some list-producing range operations, but not for `to_seq`
over restored sources.

## Count Differences

Upstream `BTSet` has a rolling `cnt`, so `count` is O(1), including restored
sets when metadata is provided.

OCaml `count` traverses in-memory and edited trees. For `Deferred`, it
materializes the whole restored tree and returns `List.length`.

This is correct but can be expensive for large restored indexes.

## Settings Differences

Upstream ClojureScript reports:

```clojure
{:branching-factor max-len
 :ref-type :strong}
```

In the inspected ClojureScript file, `max-len` is fixed at 32.

OCaml exposes:

```ocaml
type ref_type = Strong | Weak
type settings = { branching_factor : int; ref_type : ref_type }
```

and allows any branching factor >= 2. `Strong` keeps restored nodes cached in
memory. `Weak` allows restored nodes to be released and fetched again.

## Test Coverage Differences

The local tests map every upstream test name through
`test/upstream_test_aliases.tsv`. The OCaml test suite also adds implementation
specific checks:

- configurable branching factor
- restored settings
- `of_sorted_array`
- storage address reuse after add/remove
- nested branch storage
- restored nested add/remove laziness
- `to_seq` seeking behavior for tree slices
- js_of_ocaml smoke behavior

Known upstream behavior that is not directly represented:

- Clojure protocol behavior
- hashing and equality semantics
- metadata
- chunked sequence protocol details
- storage `delete`
- durable and loaded ratio helpers from JVM storage tests

## Compatibility Risk Areas

1. Delete-heavy workloads can create underfull OCaml nodes because the port does
   not implement upstream rotate/merge rebalancing.
2. Restored `count` and `to_seq` can load much more storage than upstream.
3. The external `stored_chunks` metadata path can force full materialization on
   edits to already stored in-memory trees.
4. Lack of storage `delete` means obsolete addresses accumulate unless the
   embedding storage layer handles garbage collection externally.
5. Count/depth metadata is missing from restore, so OCaml cannot preserve the
   upstream O(1) count behavior for restored sets.

## Suggested Compatibility Checks

Keep the existing upstream name coverage script. Add targeted parity tests for:

- repeated delete workloads that would trigger upstream sibling rotation
- restored `to_seq` reading only the needed path
- restored `count` behavior once metadata is added
- storage deletion or explicit no-delete semantics
- construction from large unordered lists compared with upstream
