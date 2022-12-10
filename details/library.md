# The extintcode library

This shows which predefined modules come with the compiler and which functions you can use from them.

This also contains functions from the pseudo module `builtin`. These are no real functions but compile to IntCode statements directly.

### builtin

| Function  | Description                           |
|:----------|:--------------------------------------|
| `ichr()`  | Reads one character. (Opcode 3)       |
| `ochr(.)` | Prints a single character. (Opcode 4) |
| `exit()`  | Halts the program. (Opcode 99)        |

### stdlib

| Function     | Description                                                                                                              |
|:-------------|:-------------------------------------------------------------------------------------------------------------------------|
| `div(..)`    | Divides two values. Result is truncated. Dividing by zero causes a crash. Available as `/` Operator via implicit import. |
| `mod(..)`    | Calculates th modulus of two numbers. Dividing by zero causes a crash. Available as `%` Operator via implicit import.    |
| `iint()`     | As extintcode programs are meant to run in ASCII mode, this reads an int by parsing the unicode characters entered.      |
| `oint(.)`    | As extintcode programs are meant to run in ASCII mode, this prints an int by printing unicode characters.                |
| `ointnnl(.)` | Same as `oint` but without a trailing newline.                                                                           |
| `&istr()`    | Reads a line and gives a pointer to it.                                                                                  |
| `ostr(&)`    | Writes a string and appends a newline.                                                                                   |
| `ostrnnl(.)` | Same as `ostr` but without a trailing newline.                                                                           |
| `pow(..)`    | Exponentiation of two values. Negative exponents always give 0. Available as `^` Operator via implicit import.           |
| `obool(.)`   | Prints a boolean value (`false` for `0` and `true` for any other value)                                                  |
| `olist(.)`   | Prints a list of integers                                                                                                |
| `parse(&)`   | Parses an integer contained in a string.                                                                                 |

### stdmath

| Function     | Description                                                                                                                                    |
|:-------------|:-----------------------------------------------------------------------------------------------------------------------------------------------|
| `max(..)`    | Returns the greater one of the two arguments.                                                                                                  |
| `min(..)`    | Returns the lower one of the two arguments.                                                                                                    |
| `fib(.)`     | Gets an element from the fibonacci sequence (starting at 1)                                                                                    |
| `abs(.)`     | Gets tha absolute of a value.                                                                                                                  |
| `clamp(...)` | Gets the first value if it's between the other two values or if it's not, the one from the other two values that is closer to the first value. |
| `gcd(..)`    | Gets the greatest common divisor of two values.                                                                                                |
| `sgn(.)`     | Gets the sign of a value (-1, 0 or 1)                                                                                                          |
| `faculty(.)` | Gets the faculty of a number                                                                                                                   |

### stdlist

| Function        | Description                                                                                                               |
|:----------------|:--------------------------------------------------------------------------------------------------------------------------|
| `&indices(&.)`  | Gets a list of positions where the list given as first argument contains the element given as second argument.            |
| `&sublist(&..)` | Gets a sublist of the list given as first argument from the second argument (inclusive) to the third argument (exclusive) |
