# extintcode Modules

An extintcode module is a unique part of software that can be used by other modules. Modules are joined to one program by the linker.

A module is uniquely identified by its name. A module name must match the regex `^[A-Za-z][A-Za-z_0-9]*$` and may not equal `builtin` or `local` as there two names are reserved as pseudo modules for the extintcode language.

Also, a module name should not start with `std` if it's not a module bundled in the compiler (like `stdlib`).

A module name is always the file name without the extension. A module way be present as an extintcode language file, an IntCode Assembler file, or a Relocatable IntCode file and optionally an IntCode header.

Relocatable IntCode is described in [linkage](linkage.md). The IntCode Header is described here, but it might be better if you read [runtime](runtime.md) before.

### IntCode Header

An IntCode Header starts with a [version specifier](misc.md). After that, function and field signatures may follow.

A field signature looks like this:

```
<signature><name>+<address>
```

where `name` is the name of the field. The `mode` of te field is either an empty string ('') which means the field is in direct mode, or an ampersand ('&') which means the field is in pointer mode.
Each field must be uniquely defined by its name inside a module. `address` is the memory address of that field relative to this module.

A function signature looks like this:

```
<pure><return><name>(<params>)+<address>
```

where `name` is the name of the function. The return of the function is either an empty string, which means the function returns a value in direct mode, or an ampersand (`&`) which means the function returns a value in pointer mode. `pure` is either a question mark or empty. A question mark means the function is pure, which means it does not use the stack. So a push and pop ist not required before/after the call.
`params` is a series of dots (`.`) and ampersands (`&`), one for each parameter. A dot means the parameter is in direct mode, an `&` means it's in pointer mode.
Each function must be uniquely defined by its name, and its parameter count inside a module. Address is the memory address of that function relative to this module.
