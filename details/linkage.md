# Linkage

### IntCode Relocatable

This covers what an *IntCode Relocatable* file is.

An IntCode Relocatable file is a text file that starts with a version specifier, followed by any number of dependency specifiers and a function specifier. See [misc](misc.md).

Then all ints follow, separated by commas or newlines. Each int is a signed integer in plain notation (no scientific notation).

Each int may be prefixed with a module name followed by a percent sign (%). This indicates that the base memory address of that module is added to the int on linkage. The module name should be empty when the base address of the module represented by the containing file should be added to the int.

The relocatable code may not end with an exit value as this is appended on linkage.

The code will run on startup. The order in which modules run is unspecified. If functions are defined, you need to jump over these.

Only modules that are defined as a dependency may be used for relocation.

Line comments start with a pipe ('|')

### The Linker

The linker takes multiple Relocatable IntCode files and generates a new file in either Binary or Plain IntCode format. To do so, it needs to create a runtime header and an exit instruction at the end. See [runtime](runtime.md).