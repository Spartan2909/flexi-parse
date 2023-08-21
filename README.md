# flexi-parse

`flexi-parse` is a parsing library intended to be more flexible than parser generators and parser combinators, while still being simple to use.

## Features

- **Flexible.** The parsing machinery supports many diverse grammars, with
support for semantic whitespace, single- and double-quoted strings, custom
delimiters, and more.
- **Simplicity.** Built-in types for common symbols and helper macros for
punctuation and keywords mean very little hacking is required to parse complex
grammars.
- **Familiarity.** The API is very similar to that of `syn`, making transfer
from `syn` very simple.

## Examples

Examples of this library in use can be found under the `examples` directory.
[`calc.rs`](examples/calc.rs) is a very simple command line calculator in under
100 lines of code, and [`lox.rs`](examples/lox.rs) is a parser for the Lox
language from [Crafting Interpreters](https://craftinginterpreters.com) with
full error recovery.

## License

Licensed under either of

* Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE) or
http://www.apache.org/licenses/LICENSE-2.0)
* MIT license ([LICENSE-MIT](LICENSE-MIT) or
http://opensource.org/licenses/MIT)

at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall
be dual licensed as above, without any additional terms or conditions.
