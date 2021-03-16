# extintcode Runtime

### Modes

Everything works in two modes. This has nothing to do with IntCodes parameter modes. The two modes are `pointer` and `direct`.

In direct mode, a value is interpreted as that value. So when a function takes an argument in direct mode, and you pass a `1` the function should treat this as a `1`.

In pointer mode, a value is interpreted as a memory address. So when you pass a `976` to a function that requires an argument in pointer mode, the function should look at address `976` and read the value there and optionally some other values relative to `976` (for example for arrays.)

### Arrays

Arrays are commonly represented as a pointer which points to the array in memory. Suppose you have an array with five elements, it'd look like this:

```
+---+----+----+----+----+----+
| 5 | V0 | V1 | V2 | V3 | V4 |
+---+----+----+----+----+----+
```

The pointer points at the memory with the `5` which is the length of the array. After that the next five elements store the array's values from `0` to `4`.

Strings should be represented as arrays of unicode code points.

### Dynamic Memory

Behind the last int there's always empty space. This space is called the dynamic memory. The next free value is stored in the header field `NEXTDYN`. You can always expect zeroes after it. When using the dynamic memory, increment that value by the amount of ints you use up. You may not write anything behind the address of this value.

### Runtime Header

The runtime header is created by the linker and is always located at int 0. It has the following structure:

```
0-15       Reserved for intialisation
16     NEXTDYN     Contains the address of the next int after the whole program, where only zeros follow. Wh nthis space is used, it must be increased.
17     CALLSTACK   Contains the value at which the relative base register is. This is required as the relative base register sometimes needs to be moved out the way temporarily (for example to resolve pointers.)
18     BACKJUMP    When a function is called it gets the address to jump back after it executed in here.
19     RETURN      Stores the return value of a function when it's done.
20-21  RESERVED    Reserved for future use
22     X1          Temporary value used by assembler statements.
23     X2          Temporary value used by assembler statements.
24-31  GLOBAL1-8   Values that may store anything but they also may be changed by everyyone so as soon as your code gives control to anything else, you may not expect the same values again there.
32 onward          These values store the parameters for a function call.
```

### The CallStack

The callstack is memory that can be used that persists after you code loses control. You can use the callstack (which starts at rel0). When your code loses control (on a function call), you increase the relative base register by the amount of ints you've used and also increase the `CALLSTACK` value. After the function call you can then decrease the relative base register and the `CALLSTACK` value.

### Calling Functions

To call a function, you need to set the `BACKJUMP` and `PARAM*` header fields and then jump to the functions address. after the function terminates it'll jump back to the value you set in `BACKJUMP` and you get the result in `RETURN`.