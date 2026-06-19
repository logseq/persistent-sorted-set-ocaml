# OCaml Simplicity and Performance Improvement Report

This report proposes improvements that fit the OCaml implementation rather than
copying upstream ClojureScript literally. The priority is:

1. preserve current behavior and upstream parity
2. simplify internal representations
3. improve asymptotic costs where current code materializes full trees
4. keep the public API small

## Current Strengths

The current implementation already has several good OCaml-specific choices:

- The public API is small and idiomatic enough for OCaml callers.
- The main in-memory tree uses arrays and binary search, which is a good fit for
  the small B+ tree node size.
- Restored add/remove preserve lazy access to the changed path.
- Storage is explicit and easy to test.
- js_of_ocaml stack-safety issues are already considered in construction and
  traversal paths.
- Benchmarks in `docs/perf.md` show native OCaml and js_of_ocaml are competitive
  with upstream CLJS/JS on the shared benchmark names.

## Highest Priority Improvements

### 1. Bulk-build `of_list_by`

Current `of_list_by` repeatedly calls `add`.

That is simple, but for large unordered inputs it does much more work than
necessary. Upstream sorts once, deduplicates adjacent values, and bulk-builds the
tree.

Recommended change:

- Convert the input list to an array.
- Sort with the normalized set comparator.
- Deduplicate adjacent comparator-equal values.
- Build with `data_of_sorted_values` or a new array-based bulk builder.

Expected result:

- Faster construction for large sets.
- Less allocation than repeated persistent updates.
- Simpler performance story: `of_list_by` becomes the normal bulk constructor,
  and `add` remains the incremental constructor.

Risk:

- Comparator override behavior for later `add ~cmp` must remain unchanged.
- Tests with comparator-equal values should be kept.

### 2. Add Count Metadata

Current `count` traverses the tree. For restored `Deferred` sets it materializes
the full stored tree.

Recommended change:

- Add `count : int option` or `count : int` to `'a t`.
- Maintain it on `add` and `remove`.
- Return O(1) count when known.
- Add storage metadata so restore can receive count without reading the root.

Minimal public API option:

```ocaml
type metadata =
  { root : string
  ; count : int
  ; settings : settings
  }
```

Then add a new API without breaking the existing one:

```ocaml
val store_with_metadata : 'a storage -> 'a t -> metadata * 'a t
val restore_with_metadata : cmp:'a comparator -> 'a storage -> metadata -> 'a t option
```

Keep `store` and `restore` as compatibility wrappers.

Expected result:

- O(1) count for normal and restored sets.
- Less accidental storage loading.
- Better parity with upstream `cnt`.

### 3. Make Restored `to_seq` Truly Lazy

Current `to_seq` for `Seq_deferred` calls `materialize_address`, which loads the
entire stored tree before the first value is yielded.

Recommended change:

- Replace list-based deferred sequence generation with a cursor/frame iterator.
- Store traversal frames such as:

```ocaml
type 'a frame =
  | Leaf_frame of
      { values : 'a array
      ; index : int
      ; stop : int
      }
  | Branch_frame of branch_state
```

- Advance one leaf segment at a time.
- Call `restore_node` only for branches/leaves required by the current bounds.

Expected result:

- `Seq.uncons (to_seq (seq restored))` reads only the first path.
- `fold_seq` over a bounded restored slice reads only overlapping leaves.
- Behavior matches the upstream iterator design more closely.

### 4. Unify Stored Address State With Tree Nodes

The current implementation has separate concepts:

- `Tree` for in-memory arrays
- `Edited` for restored edits
- `stored_chunks` and `stored_branch_chunks` for address reuse after storing

This works, but it creates full-materialization paths. In particular, editing a
stored in-memory `Tree` with non-empty `stored_chunks` materializes the whole
tree to update chunk metadata.

Recommended direction:

- Give tree nodes optional addresses:

```ocaml
type 'a node =
  | Leaf of
      { values : 'a array
      ; address : string option
      }
  | Branch of
      { keys : 'a array
      ; children : 'a node array
      ; address : string option
      }
  | Ref of
      { key : 'a
      ; address : string
      }
```

- Use one representation for in-memory, restored, and edited trees.
- Store can skip addressed clean nodes and write only changed nodes.

Expected result:

- Less code duplication across `Tree` and `Edited`.
- No need for external chunk caches.
- More local updates after storing a set.
- Easier reasoning about address reuse.

Risk:

- This is a larger refactor. It should follow smaller wins like bulk build and
  count metadata.

## Medium Priority Improvements

### 5. Rebalance Underfull Nodes on Remove

Upstream maintains B+ tree occupancy with rotate/merge logic. OCaml currently
does not. This is simpler but can leave underfull nodes after many deletions.

Recommended change:

- Add sibling-aware remove for array trees.
- If a child becomes too small, borrow from the smaller sibling when useful or
  merge when combined size fits.
- Keep the root-collapse behavior.

Expected result:

- More stable tree height and memory use under delete-heavy workloads.
- Better structural parity with upstream.

Tradeoff:

- More code complexity. This should be justified with a benchmark that shows
  delete-heavy workloads degrade today.

### 6. Use Arrays in Stored Nodes Internally

Public stored nodes use lists:

```ocaml
type 'a stored_node =
  | Leaf of 'a list
  | Branch of 'a list * string list
```

That is convenient for test storage and serialization, but internal operations
on restored leaves use list insertion/removal.

Recommended change:

- Keep the public storage format for compatibility.
- Convert restored leaf values to arrays once when editing.
- Store edited leaves from arrays back to lists only at the storage boundary.

Expected result:

- Less list recursion on restored edits.
- Reuse the same binary search helpers as in-memory tree updates.

### 7. Add a Mutable Builder

OCaml does not need Clojure transients, but a builder is useful and idiomatic:

```ocaml
module Builder : sig
  type 'a t
  val create : ?settings:settings -> 'a comparator -> 'a t
  val add : 'a -> 'a t -> unit
  val freeze : 'a t -> 'a Persistent_sorted_set.t
end
```

This can be a thin wrapper around sort-and-bulk-build for batch input, or a
mutable B+ tree if profiling later justifies it.

Expected result:

- Faster large index construction.
- Clearer API than exposing transient-style mutation on the set itself.

### 8. Optimize Range Scans Inside Leaves

Leaf size is small by default, so scanning from index 0 is usually fine.
However, with custom larger branching factors, leaf-local binary search can save
comparisons.

Recommended change:

- In `slice_array_into` and `slice_array_seq`, find the lower bound index with
  binary search when `from_` is present.
- In reverse slice, find the upper starting index when possible.

Expected result:

- Better behavior for large custom branching factors.
- No semantic change.

## Simplicity Improvements

### 9. Name the Two Comparator Roles

Several functions take `order_cmp`, `equality_cmp`, and `key_cmp`. This is
correct but easy to misread.

Recommended change:

- Introduce a small internal record:

```ocaml
type 'a comparators =
  { order : 'a comparator
  ; equality : 'a comparator
  ; route : 'a comparator
  }
```

- Build it once per operation.

Expected result:

- Fewer argument-order mistakes.
- Clearer semantics for `add ~cmp` and `remove ~cmp`.

### 10. Replace Repeated List Concatenation in Materialization

`materialize_tree` currently uses `materialize_tree child @ acc` under
`array_fold_right`. This is correct but can allocate more than needed.

Recommended change:

- Use accumulator-passing traversal throughout.
- Convert to a list once at the end.

Expected result:

- Less allocation for `to_list`, `store`, and fallback materialization paths.

### 11. Reduce `Invalid_argument` for Expected Storage Failures

Missing storage nodes currently raise `Invalid_argument`. That is acceptable for
internal invariant violations, but storage misses are often environmental
failures.

Recommended change:

- Keep existing API behavior for now.
- Internally use `result` in restore/traversal helpers.
- Convert to `Invalid_argument` only at compatibility boundaries.

Expected result:

- Easier testing and future error reporting.

## Benchmark Plan

Before changing implementation strategy, add benchmarks for:

- `of-list-100K`
- `of-sorted-array-100K`
- delete-heavy churn: build 100K, remove 50K random values
- restored `count`
- restored `Seq.uncons`
- restored bounded `fold_seq`
- store after editing an already stored in-memory tree

Keep benchmark categories separate:

- native OCaml
- js_of_ocaml
- upstream CLJS/JS

Do not optimize by changing ordering, uniqueness, restored laziness, or storage
address reuse semantics.

## Suggested Implementation Order

1. Add benchmarks for current behavior.
2. Bulk-build `of_list_by`.
3. Add count metadata to `'a t`.
4. Add `store_with_metadata` and `restore_with_metadata`.
5. Make deferred `to_seq` lazy.
6. Refactor tree nodes to carry addresses directly.
7. Consider remove rebalancing if delete-heavy benchmarks show degradation.

This order gives useful wins early while delaying the risky representation
refactor until there are tests and benchmarks that make the target behavior
obvious.
