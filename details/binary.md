# Binary IntCode

This file covers what a Binary IntCode file is. A binary IntCode file stores the ints of an IntCode program in binary form. The file extension is `ic`.

*A n-bit integer* means an n-bit signed integer in big-endian.

*A n-bit unsigned integer* means an n-bit unsigned integer in big-endian.

Each binary IntCode file starts with a 64-bit unsigned integer with the value `0xAD4E570FC0DE2019`.

Then a 16-bit unsigned integer follows that defines the major IntCode version. This must be 2.

After that, a 64-bit unsigned integer follows, that specifies the flags. A flag not listed in the table here must be zero.

| Bit (0-63) | Flag | Meaning |
| :---: | :---: | :--- |
| 0 | ASCII | The vm will run in ASCII mode. See below. |
| 1 | DEBUG | The vm will output debug information. This is an optional flag and may be ignored. |
| 2 | EXD | (Exit Dump) whenever a `99` instruction is executed instead of terminating the program normally, the vm will crash and provide a memory dump. This is an optional flag and may be ignored. |
| 3 | NOP | (No-op) Instead of failing on opcode `0` interpret it as a no operation opcode that takes 0 arguments. |

### ASCII Mode

Unlike in regular IntCode in extintcode ASCII mode does **not** operate on ASCII characters but on Unicode Code Points. This means each program that only uses ASCII characters will still be compatible, but it allows for many more characters to be printed.

### The ints

After the flags value, the following step is repeated:

Read an unsigned 8-bit integer. This must be one from the table below. Then perform the action from the table.

| 8-bit int | Action |
| :---: | :--- |
| 0 | Add an int with value 0 to the memory. |
| 1 | Add an int with value 1 to the memory. |
| 2 | Add an int with value 2 to the memory. |
| 3 | Add an int with value 3 to the memory. |
| 4 | Add an int with value 4 to the memory. |
| 5 | Add an int with value 5 to the memory. |
| 6 | Add an int with value 10 to the memory. |
| 7 | Add an int with value -1 to the memory. |
| 8 | Read an 8-bit integer and add it to the memory. |
| 9 | Read an unsigned 16-bit integer and that as many zeros to the memory. |
| 16 | Read an 16-bit integer and add it to the memory. |
| 32 | Read an 32-bit integer and add it to the memory. |
| 64 | Read an 64-bit integer and add it to the memory. |
| 255 | End of the file reached. |