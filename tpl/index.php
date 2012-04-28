<?php
$homePath = ".."; 
include "$homePath/head.php";
?>

<link rel="stylesheet" type="text/css" href="css/tpl.css" />

<title>
  TPL
</title>

</head>

<body>

<?php
include "$homePath/header.php";
?>

<div class="tpl content" id="main">
  <h1 class="tpl header"> tpl </h1>

  <p>
    TPL is a simple dynamically typed scripting language. The core
    idea is to combine minimalism with flexibility&mdash;the language
    has a small core flexible enough to delegate things like control
    structures to libraries. This also means you are free to add your
    own control structures to the language if you feel some are
    missing.
  </p>

  <p>
    I'm still playing around with the design of the language and
    adding features; even core parts may change drastically in the
    near future.
  </p>
</div>

<div class="tpl content">
  <h1 class="header"> Installation </h1>

  <p>
    The language interpreter requires Haskell and Cabal. If you do not
    have Haskell installed on your system, I suggest
    the <a href="http://hackage.haskell.org/platform/">Haskell
    Platform</a>. This is packaged for every major operating system,
    comes with Cabal and the correct version of Haskell.
  </p>

  <p>
    When you have Haskell installed, you need to download the TPL
    code. If you have <code>git</code> installed, run:
  </p>

<pre>git clone git://github.com/TikhonJelvis/TPL.git
</pre>

  <p>
    If you do not want to use <code>git</code>, you can also download
    the code as
    a <a href="https://github.com/TikhonJelvis/TPL/zipball/master">zip
    file</a>.
  </p>

  <p>
    Once you have the code, enter the its directory:
  </p>

<pre>cd TPL
</pre>

  <p>
    and run:
  </p>

<pre>cabal install
</pre>

  <p>
    This will install the TPL interpreter and its dependencies.
  </p>
</div>

<div class="tpl content">
  <h1 class="header"> Language </h1>

  <h2 class="header"> Literals </h2>

  <p>
    Numbers are just themselves. So <code>1</code> is
    a <code>1</code>. Floating point numbers are not currently
    supported, but they're overrated anyhow ;).
  </p>

  <p>
    Strings can be surround by either <code>&quot;</code>
    or <code>'</code>. So <code>"abc"</code> and <code>'abc'</code>
    are the same.
  </p>

  <p>
    Null and boolean literals are all
    lower-case: <code>null</code>, <code>true</code>, <code>false</code>.
  </p>

  <p>
    Lists look like array literals from JavaScript and can be
    nested: <code>[1,2,"abc","def",[5,6,7]]</code>.
  </p>

  <h2 class="header"> Variables </h2>

  <p>
    Each program is a series of statements, much like other scripting
    languages (Python, Ruby, JavaScript...). You can put each
    statement on a newline or use semi-colons to separate multiple
    statements on the same line.
  </p>

  <p>
    You can declare a variable with <code>:=</code>:
  </p>

<pre>x := 10
</pre>

  <p>
    You can change the value of a variable
    with <code>&lt;-</code>.
  </p>

<pre>x &lt;- 11
</pre>

  <p>
    If <code>x</code> has not been declared yet, this will result in
    an error.
  </p>

  <h2 class="header"> Functions </h2>

  <p>
    You can write a function as follows:
  </p>

<pre>\ a b -&gt; a + b
</pre>

  <p>
    This lets you define a named function:
  </p>

<pre>fn := \ a b -&gt; a + b
</pre>

  <p>
    As this is a very common operation, there is some special syntax
    for it:
  </p>

<pre>fn a b := a + b
</pre>

  <p>
    Each function creates a new scope and forms a closure. In essence,
    this means scoping behaves exactly like in Scheme or
    JavaScript. Unlike Python, you can reassign the value of a
    variable declared outside of a function:
  </p>

<pre>x := 10
fn a := x &lt;- a
</pre>

  <p>
    Here <code>fn</code> will set <code>x</code> to the value
    of <code>a</code>. Had <code>:=</code> been used in place
    of <code>&lt;-</code>, it would have created a new variable
    called <code>x</code> in the scope of <code>fn</code> without
    affecting the <code>x</code> in the outer scope.
  </p>

  <p>
    Functions are applied using Haskell-style syntax. This means
    that <code>f a b c</code> is like <code>f(a, b, c)</code> in
    JavaScript/Python or <code>(f a b c)</code> in Scheme. Parentheses
    are only used for grouping, so <code>f (a b)</code> is the same
    as <code>f(a(b))</code> in JavaScript rather than <code>f(a,
    b)</code>.
  </p>

  <p>
    Functions can be partially applied. That is, given:
  </p>

<pre>f a b := a + b
</pre>

  <p>
    the expression <code>f 1</code> is equal to <code>\ α -&gt; f 1
    α</code>.
  </p>

  <p>
    You can define a function that has a body containing multiple
    statements. This is done by surrounding the body in
    parentheses. Just like in the body of the program, you can
    separate statements by semi-colons or newlines.
  </p>

<pre>f x := (
  a := x + 10
  a + 11
)
</pre>

  <p>
    The value of the last statement executed will be returned.
  </p>

  <p>
    You can actually put "blocks" of code like this anywhere you could
    put a normal expression.
  </p>

  <p>
    How you indent code like this is, naturally, completely up to
    you.
  </p>

  <h2 class="header"> Laziness </h2>

  <p>
    If you declare a function taking <em>no</em> arguments, it isn't
    immediately clear how to use it. Other functions get called when
    you pass parameters into them (<code>f 1 2 3</code>), but how do
    you call one without any parameters?
  </p>

  <p>
    In fact, you call a function like that just by referencing
    it. This means that a function of no arguments acts like a
    deferred expression rather than normal function. Also, from the
    perspective of the caller, you cannot tell a function of no
    arguments from a normal value.
  </p>

  <p>
    For convenience, you can declare a function like this (or,
    equivalently, defer a statement) using a <code>$</code>. So,
    given:
  </p>

<pre>x := $(y + z)
</pre>

  <p>
    <code>x</code> will be the sum of the <em>current</em> value
    of <code>y</code> and <code>z</code> each time you reference
    it. This makes basic reactive programming very easy, and lets you
    control when a statement gets executed.
  </p>

  <p>
    Functions can also control which of their parameters to
    execute. If you use <code>$</code> on a function's parameter, it
    will not get evaluated when the function is called. So:
  </p>

<pre>f a $b := a
</pre>

  <p>
    will <em>never</em> evaluate <code>b</code>. If you call <code>f a
    (print "blarg")</code> nothing will be printed.
  </p>

  <p>
    This also works on patterns (like <code>f $[a, b]</code>).
  </p>

  <h2 class="header"> Custom Scope </h2>

  <p>
    You can add bindings to the environment of a closure using
    the <code>with</code> native function. Given a
    closure <code>f</code>, we can write:
  </p>

<pre>g := with ["x" -&gt; 11] f
</pre>

  <p>
    Now <code>g</code> is the same function as <code>f</code> except
    that <code>x</code> will be equal to <code>11</code> inside
    it.
  </p>

  <p>
    If <code>with</code> is given a deferred statement, that statement
    is executed immediately. So:
  </p>

<pre>with ["x" -&gt; 11] $(x^2)
</pre>

  <p>
    will return 121.
  </p>

  <p>
    If you pass <code>with</code> a deferred variable name to bind to,
    it unpacks this. So:
  </p>

<pre>name := $x
with [name -&gt; 11] $(x^11)
</pre>

  <p>
    is the same as the previous example (121). This is useful for
    control structures like the <code>for .. in</code> loop.
  </p>

  <p>
    This <code>with</code> function may seem a little arbitrary. And
    it is. In fact, this is just a placeholder until I get objects
    working; see the end of this guide.
  </p>

  <h2 class="header"> Operators </h2>

  <p>
    Operators are just normal functions which are in infix position by
    default. You can define your own operators:
  </p>

<pre>x ~ y := if (x &gt; y) x else y
</pre>

  <p>
    You can also partially apply operators. <code>(1 ~)</code> is
    equal to <code>\ α -&gt; 1 ~ α</code>.
  </p>

  <p>
    You can use operators in prefix position by surrounding them in
    parentheses. So <code>1 + 2</code> and <code>(+) 1 2</code> are
    identical. This is just like partially applying the operator to no
    arguments.
  </p>

  <p>
    You can also use normal functions in infix position by surrounding
    them with backticks. So <code>f a b</code> could be written
    as <code>a `f` b</code>. You can partially apply these the same
    way as operators.
  </p>

  <p>
    When you define an operator, you can set its precedence as
    follows:
  </p>

<pre>precedence (~) 2
</pre>

  <p>
    The precedence should be a number between 1 and 11. You can get an
    operators precedence with the <code>precedenceOf</code>
    function:
  </p>

<pre>precedenceOf (~)
</pre>

<h2 class="header"> Destructuring Assignment </h2>

  <p>
    You can destructure lists just like in newer version of
    JavaScript. For example, <code>[x, y] := [1, 2]</code>
    sets <code>x</code> to <code>1</code> and <code>y</code>
    to <code>2</code>. This works with both <code>:=</code>
    and <code>&lt;-</code>.
  </p>

  <p>
    These patterns can be nested as well:
  </p>

<pre>[a, [b, c], d] := [1, [2, 3], 4]
</pre>

  <p>
    You can match the rest of the list with <code>...</code>:
  </p>

<pre>[x, xs...] := [1, 2, 3, 4, 5]
</pre>

  <p>
    This also works on nested patterns.
  </p>

  <p>
    These patterns can also be used in function declarations.
  </p>

<pre>head [x, xs...] := x
</pre>

  <p>
    This defines <code>head</code> to return the first element of a
    list. If the list is empty, <code>x</code> would
    be <code>null</code>.
  </p>
</div>

<div class="tpl content">
  <h1 class="header"> Standard Library </h1>

  <h2 class="header"> Basic Operators </h2>

  <p>
    The language comes with the arithmetic operators you would expect
    from a language like JavaScript. The main differences
    are <code>=</code> for equality, <code>/=</code> for
    inequality, <code>//</code> for mod and <code>^</code> for
    power. Bitwise operations are not supported.
  </p>

  <p>
    The <code>%</code> operator is used for string formatting, like in
    Python. However, only <code>%s</code> and <code>%d</code> patterns
    are supported right now, so you can only really show things as
    strings or numbers. The following are equivalent:
  </p>

<pre>"A number: %d and a string: %s." % [1, "blarg"]
"A number: 1 and a string: blarg."
</pre>

  <p>
    For lists, <code>:</code> is cons and <code>++</code> is
    append. So <code>1:[2,3,4]</code> gives <code>[1,2,3,4]</code> as
    does <code>[1,2] ++ [3, 4]</code>. You can get a range
    with <code>..</code>. So <code>[1,2,3,4,5]</code>
    is <code>1..5</code>.
  </p>

  <p>
    Logic operators are simple: <code>&amp;</code> is and
    and <code>|</code> is or.
  </p>

  <p>
    <code>@</code> is the function application operator. This lets you
    write fewer parentheses: <code>f (a b c)</code> can be written
    as <code>f @ a b c</code>.
  </p>

  <p>
    We can combine this with <code>?</code> to get a ternary
    conditional operator:
  </p>

<pre> x := a &gt; b ? 10 * a @ 12 * b
</pre>

  <p>
    There is also an "implication"
    operator <code>--&gt;</code>. <code>a --&gt; b</code> is
    equivalent to <code>not a | b</code>.
  </p>

  <p>
    Finally, there is a very useful composition
    operator <code>.</code>. <code>f . g</code> is equivalent to:
  </p>

<pre>\ x -&gt; f (g x)
</pre>

  <p>
    This is just like function composition in math (f ∘ g).
  </p>

  <h2 class="header"> Control Structures </h2>

  <p>
    Control structures are also part of the standard library. There
    are several defined.
  </p>

  <p>
    When using control structures like this, it is often useful to
    pass in blocks of code. You can do this the same way you define
    complex functions.
  </p>

  <p>
    The simplest control structure is the <code>if</code>
    statement. This statement <em>always</em> has to have
    an <code>else</code> clause. If you do not want
    an <code>else</code> clause, you can use the <code>--&gt;</code>
    operator discussed earlier.
  </p>

<pre>x := if (a &gt; b) (
       a * 100
     ) else (
       b * 100
     )
</pre>

  <p>
    More complicated conditions could benefit from a <code>cond</code>
    statement. This is a list of conditions and statements; the
    statement corresponding to the first condition will be
    executed.
  </p>

<pre>cond [
  a &gt; b -&gt; "greater",
  a = b -&gt; "equal",
  a &lt; b -&gt; "lesser"
]
</pre>

  <p>
    If none of the conditions are true, it will
    return <code>false</code>.
  </p>

  <p>
    You can also use a <code>switch</code> statement.
  </p>

<pre>switch x [
  1 -&gt; "one",
  2 -&gt; "two",
  3 -&gt; "many"
]
</pre>

  <p>
    This executes the statement matching the first value passed in
    (<code>x</code> in this case).
  </p>

  <p>
    There are also several loops. You can use a
    simple <code>while</code> loop:
  </p>

<pre>while (x &gt; 0) (
  x &lt;- x - 10
  print x
)
</pre>

  <p>
    or a do-while loop:
  </p>

<pre>do (
  x &lt;- x - 10
  print x
) while (x &gt; 0)
</pre>

  <p>
    You can also use a for loop to iterate over a list:
  </p>

<pre>for x in (1..10) (
  y := x^2 + 2*x + 10
  print ("(%d, %d)" % [x, y])
)
</pre>

<h2 class="header"> Useful Functions </h2>

  <p>
    There are also a bunch of fairly standard but useful functions for
    list manipulation like <code>map</code>
    and <code>fold</code>. They all work as expected.
  </p>
</div>

<div class="tpl content">
  <h1 class="header"> Objects </h1>

  <p>
    The one main feature still missing from the interpreter is support
    for objects.
  </p>

  <p>
    I want to add objects that behave much like objects in JavaScript
    or Lua. I want them to be as simple and as flexible as possible;
    particularly, I will not add classes to the language and I will
    let programmers have full control of how values are set and looked
    up in objects (much like proxies do in JavaScript).
  </p>

  <p>
    However, I actually want to go one step further than JavaScript or
    Lua: I want to unify the idea of scopes (environments) and
    objects. I want to be able to treat a function's environment as an
    object and to set an arbitrary object as a function's
    environment. This will give the programmer full control over how
    variables behave inside a function, making the <code>with</code>
    function for custom scoping redundant. You will be able to do
    anything you want (including introducing arbitrary variables) with
    a function, so you would be able to implement <code>with</code> in
    terms of objects and environments rather than needing it to be
    provided by the interpreter.
  </p>
</div>

<?php
include "$homePath/footer.php";
?>

</body>

</html>
