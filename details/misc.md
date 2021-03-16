# Miscellaneous

### Version specifier

A version specifier must be located at the start of a file and looks like this:

```
$V 2.x
```

The `2` is the major IntCode version, which must be `2`. The x is the modules version. tHis must be a non-negative integer.

### Dependency specifier

A dependency specifier must be located after a version specifier and before a dependency specifier there may only be a version specifier, a function specifier and other dependency specifiers.

A dependency specifier tells the assembler and linker, that a module requires another module.
 It looks like this:

```
$D name.2.x
```

The `2` is the major IntCode version, which must be `2`. `name` and `x` are a module name and module version that this module requires.

### Function specifier

In Relocatable IntCode this specifies the maximum amount of function parameters, a function of a module may take. This is important, so the linker can allocate enough space to pass all parameters.

### String literals

Whenever string literals are mentioned, they must be enclosed in quotes (`"`) and allow for the same escape codes as in Java.