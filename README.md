YATCP
=====

![YATCP testing pipeline](https://github.com/Valtis/YATCP/workflows/YATCP%20testing%20pipeline/badge.svg?branch=master)  

Yet another toy compiler project, written in Rust.

A compiler for a language with Rust-like syntax and C-like semantics. Currently generates object files in ELF format, using System V AMD 64 ABI. This makes it compatible with C compilers running under Linux. Windows support is not currently planned.

# Tooling dependencies

Some test cases compile a binary and run it, asserting the output is the expected one. To do this, and to handle failure cases, following programs need to be installed to run the tests:

* GCC - for compiling a helper .c file, and linking the object file this compiler produces against this helper.
* Objdump - used to dump and print the generated assembly in case of failure

Note that all the necessary temporary files are generated under /tmp/

# Basic usage:

### All command line switches available using:
```
cargo run -- --help
```

## Compile:

```
cargo run -- source_file_name
```
* Default file name will be source\_file\_name.out, if none provided

## Compile and give output file name:

```
cargo run -- source_file_name -o output_file_name
```

## Compile with optimizations:

```
cargo run -- source_file_name -O
```
* Optimizations are currently disabled due to AST/TAC changing so much that keeping optimizations up-to-date was slowing everything.

# Link using GCC:

```
gcc support.c output_file_name 
```
* Here support.c is assumed to contain any helper functions the program may need; currently support for I/O is very limited and generally needs to be handled through C stdlib.


