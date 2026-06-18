# Performance Notes

This document records performance work for the OCaml port of
`persistent-sorted-set`. Semantic parity with the ClojureScript implementation is
the first constraint: optimizations must preserve sorted-set behavior, storage
address reuse, and lazy restored-tree access.

## Benchmark Harness

The cross-runtime benchmark entrypoint is:

```sh
script/benchmark_vs_cljs.sh
```

It compares:

- native OCaml
- the same OCaml code compiled with `js_of_ocaml`
- upstream ClojureScript/JavaScript from `../persistent-sorted-set`

The common benchmark names match upstream:

- `conj-10K`
- `disj-10K`
- `contains-10K`
- `doseq-300K`
- `next-300K`
- `reduce-300K`

The OCaml harness also includes `restored-chained-add-10K`, which tracks repeated
adds against a restored stored tree. That benchmark is OCaml-only because the
upstream CLJS benchmark does not expose the storage benchmark path.

## Latest Verified Results

Command:

```sh
script/benchmark_vs_cljs.sh
```

Lower is better. Units are ms/op.

| Benchmark | OCaml native | js_of_ocaml | upstream CLJS/JS |
| --- | ---: | ---: | ---: |
| conj-10K | 10.4 | 23.7 | 6.0 |
| disj-10K | 7.0 | 16.9 | 25.1 |
| contains-10K | 3.4 | 6.4 | 1.9 |
| doseq-300K | 0.894 | 2.2 | 2.1 |
| next-300K | 0.896 | 2.6 | 8.0 |
| reduce-300K | 0.896 | 2.1 | 2.4 |

Current status:

- Native OCaml is faster than CLJS on `disj-10K` and the 300K traversal
  benchmarks.
- `js_of_ocaml` is faster than CLJS on `disj-10K`, `next-300K`, and
  `reduce-300K`, and roughly tied on `doseq-300K`.
- The performance goal is not fully satisfied yet: `conj-10K` and
  `contains-10K` are still slower than upstream CLJS, especially under
  `js_of_ocaml`.

## Optimizations Applied

### Edited Tree Add

Repeated `add` calls on a restored stored set now update the in-memory edited
tree directly. The previous path materialized the edited tree back to a loaded
list before inserting, which forced reads of unchanged stored siblings on the
next write.

The new path preserves the existing persistent B+ tree shape:

- `Edited_ref` still reads only the addressed stored node when it is the target.
- `Edited_leaf` inserts into the leaf and splits with the existing chunking
  rules.
- `Edited_branch` routes by child max key and reuses unchanged sibling refs.
- duplicate adds remain no-ops without reading stored siblings.

Regression coverage checks that chained restored adds read only the initial root
and target leaf, then perform later in-memory edits without touching stored
siblings.

### In-Memory Tree Updates

Unstored in-memory sets now use the same B+ tree shape for add, remove,
membership, count, fold, and store boundaries. The previous in-memory path kept a
loaded list and made repeated add/remove/mem operations linear over the full set.

Sets that have already been stored still preserve stored chunk metadata so later
updates can reuse unchanged leaf and branch addresses. This keeps storage
behavior consistent with the previous tests while avoiding full-list updates for
normal in-memory add-one-by-one construction.

### Comparator Routing

Branch routing now uses the total ordering implied by the set comparator plus the
per-operation equality comparator. This preserves the upstream-compatible
behavior where `add ~cmp` can keep values that are equal under the set comparator
but distinct under the override comparator.

Reverse restored slices also use strict upper pruning so comparator-equal range
boundaries are not skipped.

### JS-Safe Construction

Large sorted-array construction and chunking avoid non-tail recursive conversion
paths that overflow the JavaScript stack after `js_of_ocaml` compilation. The
benchmark traversal paths also avoid materializing 300K-element OCaml lists just
to measure tree iteration.

### Default Comparator Fast Path

The default comparator now calls `Stdlib.compare` directly instead of normalizing
its result on every comparison. Custom comparators are still normalized at the
API boundary.

## Semantics Constraints

- Public ordering and uniqueness must match upstream.
- Lazy restored `slice`, `rslice`, `seq`, and `seek` behavior must be preserved.
- Storage updates must reuse unchanged leaf and branch addresses.
- Performance work must not make public operations eager where upstream is lazy.

## Remaining Work

The clear remaining gap is the node representation. Upstream CLJS stores node
keys and children in JavaScript arrays and uses binary search inside nodes. The
current OCaml implementation still represents pure tree leaves and child refs as
lists, so hot `conj` and `contains` paths allocate and scan more than upstream.

The next readable optimization is to introduce array-backed pure in-memory nodes
while keeping the public storage format as lists. That should target
`conj-10K` and `contains-10K` before adding more specialized benchmark tricks.

## Verification

Latest full test command:

```sh
rtk dune runtest
```

It passed after the current optimization set. The run emitted a linker
warning about a missing `/opt/homebrew/opt/node@22/lib` search path, but no test
failed.
