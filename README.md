# Craft lang

- ongoing implementation of <https://craftinginterpreters.com/>.

### Features:
    - Implemented in OCaml (Interpreter) and Rust (VM)
    - No OOP
    - Closures use resolver (but still buggy in some edge cases)

to run execute: 

```bash
To run the interpreter on a file
> dune exec craft ./res/sample.craft 

To run the vm
> cargo run 
```
