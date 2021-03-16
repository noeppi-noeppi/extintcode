# File Types

extintcode defines 6 different file type to work with IntCode:

### Plain IntCode

Extension: `ints`

This just contains many ints separated by commas that can be executed.

### Binary IntCode

Extension: `ic`

This contains the ints of an IntCode program in executable form and is also able to  store flags. See [here](binary.md).

### Relocatable IntCode

Extension: `intr`

This just contains many ints separated by commas. However, these are not executable themselves and can have a module name attached to them, that will be added to them at linkage. See [linkage](linkage.md).

### IntCode Header

Extension: `inth`

This contains exported method and field signatures, and their address relative to the modules base address. See [modules](modules.md)

### IntCode Assembler

Extension: `inta`

An assembly-like language that can be converted to relocatable IntCode and optionally an IntCode Header. See [assembly](assembly.md)

### ExtIntCode Language

Extension: `intx`

A more or less readable language that compiles to IntCode Assembler. See [compilation](compilation.md).