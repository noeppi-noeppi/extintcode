# IntCode Assembler

This specifies IntCode assembler file format.

Empty lines are ignored. A semicolon (';') starts a line comment.

The file must start with a version specifier and any number of dependency specifiers. See [misc](misc.md)

Lines starting with a colon mark a label. A label may look like this:

```
:label
```

Label names must match the same pattern as module names. However, they may aso be named `builtin` or `local`.  If a label also marks a function entry, or a field, it may look like this:

```
:label#signature
```

where signature is the same as defined in [modules](modules.md) but without the address. If this is the case, the lable name may also be left empty. Field labels may only be used in the data section. All other labels may only be used in the text section. That means you can not create field labels with names as names are only allowed in the text section.

IntCode Assembly consists of two sections: text and data. Text defines runnable code and data defines memory that can then be used at runtime. A section is started with

```
.section name
```

The text section must be present. The data section can be omitted. The text section must be located before the data section.

An `int` is any value that is a valid relocatable IntCode int. See [linkage](linkage.md)

An `ovalue` is defined to be one of the following:

```
[int]      The value at that address.
[*int]     The value at that address added to the relative base register.
[&label]   The value at the address of that label
[!ref]     The value at the address of a value defined in the data section
header     One of the header fields (including PARAM*). Must be all caps. This will reference the
           memory at that address, not the address itself.
```

An `ivalue` is defined to be one of the following:

```
ovalue     See above
int        The Integer defined by that int.
&label     Memory address of a label
!ref       Memory address of a value defined in the data section
\header    One of the header fields (including PARAM*). Must be all caps. This is the address
           of the header field, not the memory at that address.
```

Each line  in the text section must be a statement, where a statement is one of these:

```
add ovalue, ivalue, ivalue        IntCode OpCode  1 but with input and output swapped
mul ovalue, ivalue, ivalue        IntCode OpCode  2 but with input and output swapped
inp ovalue                        IntCode OpCode  3
outp ivalue                       IntCode OpCode  4
jnz ivalue, ivalue                IntCode OpCode  5 but with arguments swapped
jz ivalue, ivalue                 IntCode OpCode  6 but with arguments swapped
lt ovalue, ivalue, ivalue         IntCode OpCode  7 but with input and output swapped (it's still first ivalue < second ivalue)
eq ovalue, ivalue, ivalue         IntCode OpCode  8 but with input and output swapped
rel ivalue                        IntCode OpCode  9
ret                               IntCode OpCode 99

mov ovalue, ivalue                Set param 2 to param 1
jmp ivalue                        Unconditional Jump
push ivalue                       Add to relative base register and to header field CALLSTACK
pop ivalue                        Remove from relative base register and from header field CALLSTACK
dyn  ovalue, ivalue               Increase header field NEXTDYN by ivalue and store first usable address in ovalue.
                                  ivalue and ovalue may NOT reference the same meory
load ovalue, ivalue               Read ivalue as a memory address  and store the value in oavlue (Pointer resolution)
store ivalue, ivalue              Read second param and store it at the memory value found in first param (Pointer creation)
call ivalue                       Sets the header value BACKJUMP to the instruction right after the call instruction and jumps to ivalue
raw int<,int>*                    Inserts these ints into the code as they are. They can still be relocated.
```

Each line in the data section must look like this:

```
name(size)
name datadef
name(size) datadef
```

`size` marks the size that should be allocated for this data element. Size must be greater than zero. If the `datadef` is not present or shorter that size, zeros are padded on the right to match the given size.

Size may not be lower that the length of the `datadef`

A `datadef` must be one of the following:

```
Multiple datadef s separated by a pipe ('|')
Multiple ints separated by comma.
An int array: Multiple ints separated by comma enclosed in braces ('{}'). Here the length is prepended
A string literal: Strings are int arrays from unicode code points.
A string literal prefixed with r: Here the length is not prepeded.
!ref  Memory of another datadef
```
