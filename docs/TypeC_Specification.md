# The TypeC Language

C like low-level system programming language which has ML like type system.  

## Features

- Strongly and Statically typed
- Low-level memory access (raw-pointer)
- RAII
- Smart Pointer
- Pattern Matching
- Algebraic Data Types
- Optional & Named Arguments
- Type Inference
- Generics
- First class function
- Multi-stage programming(Eg: meta-OCaml, string-mixin(D's feature))
- Multiple compile target: C(to replace C, like altJS), Native(LLVM), VM(to support many architecture)
- ML and D like module system
- Type Class or trait
- Compile Time Function Execution

## Primitive Types

- int, int!(Size) - 1, 2, 3
- uint, uint!(Size) - 1u, 2u, 3u
- int!(64) - 1L, 2L, 3L
- uint!(64) - 1UL, 2UL
- byte - 1b, 2b, 3b
- ubyte - 1ub, 2ub, 3ub
- float - 1.2
- list - [1; 2; 3]
- array - [|1; 2; 3|]
- string - "Hello"
- function - fun (arglist) -> ...
- variant
- record
- pointer(expressed by object, parametrized object) - Pointer!(int), syntax sugar *int

## Grammar Example

```tc:Hello, World
import std.stdio;

fn main(args: string list): void {
  println("Hello, world");
}
```

```tc: list
import std.stdio;

let lst = [1; 2; 3; 4]; // typed as int list
let lst2 = [1; 2; 3; 4] as int!(64) list;

let mapped = lst |> List.map ~f:(fun <T>(x: T) -> x * x);
```

```tc: fun
// int -> int -> int
let f = fun (x: int, y: int) -> x + y;
let x = 10;
let y = f(x, x);
```

```tc: ADT
type ADT_Test =
  | A
  | B
  | C

let ADT_Test = A();

type Option(T) =
  | Some of T
  | None;

let int_opt: Option!(int) = Some(10);

let x = 
  match int_opt with
  | Some x -> x
  | None -> -1;
```

```tc: Record
type Record_Test = {
  name: string;
  age: int;
}

let record_value = {
  name = "alphaKAI";
  age  = 22;
}
```