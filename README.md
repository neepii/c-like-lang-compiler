# Pascal-like Language compiler

This compiler generates code from 'Pascal-like' language to RISC-V 64 instructions. Written in Ocaml using libraries only for tests.

## Requirements

- Ocaml
- Dune
- RISCV64 toolchain (riscv64-gnu-linux-ld, riscv64-gnu-linux-as)

## Build and run

To build and launch compiler, run these commands:

```bash
	dune build
	dune exec compiler <input_file> <output_path>
```

## Test
Run these commands:

```bash
	opam install qcheck
	opam install ppx_expect
	dune build
	dune runtest
```

Also, as said earlier, you need RISCV64 toolchain
