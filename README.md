# Persistent Sorted Set OCaml

An OCaml port of Logseq's persistent sorted set implementation.

The library provides a persistent ordered set data structure intended for DataScript-style indexes, where ordered iteration, range access, and immutable updates are core operations.

## Goals

- Preserve the behavior expected by the ClojureScript persistent sorted set implementation.
- Provide a small OCaml API that can be used from native OCaml and js_of_ocaml.
- Keep the implementation readable and predictable for DataScript index workloads.

## Development

Requirements:

- OCaml 5.2.1 or newer
- Dune 3.17 or newer
- Node.js for js_of_ocaml smoke tests

Common commands:

```sh
dune build
dune runtest
```

Benchmark and upstream comparison helpers live in `script/`:

```sh
bash script/benchmark_vs_cljs.sh
bash script/diff_upstream_tests.sh
```

## Repository Layout

- `lib/`: library implementation and interface
- `test/`: native and js_of_ocaml tests
- `bench/`: benchmark entry points
- `script/`: benchmark and upstream comparison helpers
- `docs/`: performance notes

## Credits

This project is an OCaml port of Logseq's ClojureScript persistent sorted set work.

Primary credit goes to the upstream ClojureScript repository:

- https://github.com/logseq/persistent-sorted-set

The OCaml implementation is written independently, with upstream behavior used as the compatibility reference.
