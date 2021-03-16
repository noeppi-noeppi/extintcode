# extintcode

Tools to deal with IntCode

*extintcode stands for extended IntCode which was just me adding some more opcodes to IntCode. However, when I tried to create a compiler for it, I failed. Then I realised hat everything, my new opcodes could do can also be done in normal IntCode. So I created a new, better compiler that allows you to write IntCode program bigger, better, and less painful to read. (This is also the reason why the major version is 2 and not 1...)*

For more details see [here](details/README.md).

### Running on own IntCode computer

**extintcode program are meant to run in ASCII mode**. More specifically they're meant to operate on unicode code points. As long as only ASCII characters are used, normal ASCII mode works just well.
To get a regular IntCode file, add the option `-fplain` to `intcode link`. If you don't do so, you'll get binary IntCode.

### Now back to the language

I'll briefly go through the extintcode language here.


First: Hello World

Create a file called `hello.intx` and put the following in:

```
$V 2.0

import stdlib.ostr;

ostr("Hello World");
```

Now run the following commands (one after another) to run the program:

```
intcode compile hello.intx
intcode asm hello.inta
intcode link hello.intr
intcode vm hello.ic
```

That should print `Hello World`

Let's take a look at the program in detail:

`$V 2.0` is the version specifier. The 2 is the major version and must always be `2`. The `0` is the version of your module. You should increment this whenever you change a method signature or an exported variable.

`import stdlib.ostr;` imports the function `ostr` from the bundled module `stdlib`, so you can use this. The whole module `stdlib` will then be added to the program when running `intcode link`.

`ostr("Hello World");` call the function `stdlib.ostr` with a pointer to the string `Hello World`.

You should now read the section *Modes* [here](details/runtime.md) and then return here.

So now let's take a look at another program:

```
$V 2.0
import stdlib.oint;
oint(10/5);
```

`oint` is a function that outputs an integer. When you run this, it gives `2`. So now let the user enter the two numbers to divide:

```
$V 2.0
import stdlib.*;
oint(iint()/iint());
```

First you can see, that we can import everything from a module by using an asterisk. Second, you'll notice that the program fails to compile with the message `Division Operator requires implicit import of function stdlib.div which is not imported.`.

As IntCode provides no way for division, there's a function defined in `stdlib.div` which divides two numbers. However, to use this as an operator, you need to add an implicit import:

```
$V 2.0
import stdlib.*;
implicit stdlib.div;
oint(iint()/iint());
```

SO now you might be wondering: *Why did it compile the first example then?* This is because the compiler will resolve compile time constants whenever possible. So the first example is equivalent to:

```
$V 2.0
import stdlib.oint;
oint(2);
```

In most cases you'll just have the following at the top of you file:

```
import builtin.*;
import stdlib.*;
implicit stdlib.*;
```

Now you should take a look at [this](details/library.md).

I'll make two more examples:

Let's say we want to have a program which lets a user enter some numbers and then outputs them reversed. So first the user should enter, how many numbers follow, then he enters the numbers, and we display them in the reverse order. The program looks like this:

```
$V 2.0

import builtin.*;
import stdlib.*;
implicit stdlib.*;

let &nums = array[iint()];
let idx = 0;
while (idx < *nums) {
  nums[idx] = iint();
  idx = idx + 1;
}
idx = *nums - 1;
while (idx >= 0) {
  oint(nums[idx]);
  idx = idx - 1;
}
```

I'll go over this in detail:

`let &nums = array[iint()];` creates an array with a size of the value entered and stores it in the pointer variable `nums`

`let idx = 0;` initialises the direct variable `idx` to 0.

`while (idx < *nums) {` A loop that runs as long as `idx` is smaller than the size of the array.
The size of an array is retrieved by de-referencing the pointer: `*nums`

`nums[idx] = iint();` read an integer and store it at the position `idx` of the array. Indices start at 0.

`idx = idx + 1;` increments `idx`

`idx = *nums - 1;` sets `idx` to the array length minus one.

`while (idx >= 0) { ... }` is another loop that outputs the integers and decrements `idx`.

Ok time for the last example:

```
$V 2.0

import builtin.*;
import stdlib.*;
implicit stdlib.*;

oint(fib(10));

def fib(n) {
  if (n <= 2) {
    return 1;
  } else {
    return fib(n-1) + fib(n-2);
  }
}
```

This program defines a function which computes the n-th element in the fibonacci sequence. This is a terribly bad implementation, but it shows how recursive functions are possible.