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
  TPL is a dynamically typed, interpreted scripting language. It
  borrows ideas from Haskell, JavaScript and Scheme. 
</p>
<p>
  You can get the code off 
  <a href="http://www.github.com/TikhonJelvis/tpl">Github</a>!
</p>

<h2 class="header"> Future Features: </h2>

<p>
  In the near future, I want to add:
</p>

<ul>
  <li>
    more extensive and useful pattern matching (not just destructuring
    assignment for lists).
  </li>
  <li>
    better error handling and reporting
  </li>
  <li>
    more standard functions (IO is particularly lacking right now)
  </li>
  <li>
    laziness on demand--I want functions to be able to control which
    of their parameters gets evaluated
  </li>
  <li>
    maybe an OOP system of some kind--most likely prototype-based (no
    classes)
  </li>
</ul>

</div>

<div class="tpl content">

<h1 class="header"> Guide: </h1>

<h2 class="header"> Basics </h2>

<p>
  <code>:=</code> lets you define variables and functions. Defining a
  variable is easy:
</p>

<pre>x := 0;
</pre>

<p>
  You can define functions in two different ways:
</p>

<pre>f := \ n -&gt; n + 1;
f n := n + 1;
</pre>

<p>Functions are called without parentheses; <code>f a b c</code> is equal to <code>f(a, b, c)</code> in C-like languages.</p>

<p>If you do not pass enough arguments to a function, it will just <em>curry</em> the arguments you did pass in. Given</p>

<pre>f a b c := a + b * c;
</pre>

<p>calling <code>f 1 2</code> results in:</p>

<pre>λ α1 → {λ a b c → {a + b * c} 1 2 α1}
</pre>

<p>This also means that functions with <em>no</em> arguments (often called "thunks") are called just by their name:</p>

<pre>f := \ -&gt; 1 + 2;
f = 3;
</pre>

<p>There is currently no way to reference a thunk without calling it. While I think this feature is not as necessary as it seems, I will probably add some way to do it in the future (maybe along with the "laziness-on-demand" stuff I want to add).</p>

<p><code>\ a b c -&gt; ...</code> is a lambda expression and can be used anywhere a normal expression can. You can also call a lambda directly:</p>

<pre>(\ x -&gt; x + 10) 100 = 110;
</pre>

<p>The <code>=</code> tests for equality; any time I have a statement like this by itself in the docs, it means the statement is true.</p>

<p><code>:=</code> will create a new variable in the current scope. The only way to create a new scope is in a function. </p>

<p>Later on, you can change the value of <code>x</code> using <code>&lt;-</code>. </p>

<pre>x &lt;- x + 1;
</pre>

<p>This will change the current binding of <code>x</code> no matter which scope you are currently in. If you use <code>:=</code> instead, it will create a new variable unless it already exists in the <em>current</em> scope.</p>

<pre>x := 0;
(\ n -&gt; x := n) 10;
x = 0;
(\ n -&gt; x &lt;- n) 10;
x = 10;
</pre>

<p>All expressions have values; practically anything can be an expression. Particularly, a block of code (between { and }) is an expression with the value of its last expression; it can be put anywhere a normal expression could be. So:</p>

<pre>blarg := \ n -&gt; if ({a := n; n &lt;- n + 1; a + n} = 101) "blarg" 
              else "not blarg";
</pre>

<p>is valid and <code>blarg 50</code> will return the string <code>blarg</code>.</p>

<h2 class="header">Operators</h2>

<p>You can define custom operators. For example, <code>&lt;&lt;</code> is equal to <code>map</code> in the standard library:</p>

<pre>ls &lt;&lt; fn := map fn ls;
</pre>

<p>You can also use "operator slices" like in Haskell:</p>

<pre>load 'base';
map (+ 2) (1..10);
</pre>

<p>This code will add 2 to each of the list's items. The <code>+ 2</code> expression actually evaluates to a function in the form <code>λ α → α + 2</code>; the greek letter α is used because it cannot currently be used as a normal identifier. The opposite expression would have worked too: <code>2 +</code> results in <code>λ α -&gt; 2 + α</code>. I suspect bad things might happen if you try to nest slices, so don't.</p>

<p>You can set the precedence of an operator--whether or not you've defined it--using the built-in <code>precedence</code> function:</p>

<pre>1 + 2 * 3 = 7;
precedence (+) 3;
1 + 2 * 3 = 9;
</pre>

<p>The precedence should be a number between 1 and 11; 10 is the default.</p>

<p>You can get the current precedence of an operator using <code>precedenceOf</code>.</p>

<pre>precedenceOf (*) = 4
</pre>

<p>Note how you need to wrap the operators in parentheses in these cases. Without the parentheses, the expressions will not be parsed correctly.</p>

<h2 class="header">Pattern Matching (Destructuring Assignment)</h2>

<p>There is <em>very basic</em> pattern matching, which is really like "destructuring assignment" from JavaScript. I want to have proper pattern matching, but that comes later (if at all :)).</p>

<pre>[a, b] := [37, 42];
[a, b] &lt;- (1..10);
[c, d] := 1;
</pre>

<p>Note how the two size-mismatched cases are both valid. In <code>[a, b] &lt;- (1..10)</code>, <code>a</code> is <code>1</code> and b is <code>2</code>; the rest is thrown away. In <code>[c, d] := 1</code>, <code>c</code> is 1 and <code>d</code> is not defined.</p>

<p>Finally, you can also nest these patterns:</p>

<pre>[a, [b, c], d] := [1, [2, 3], 4];
</pre>

<p>You can match the rest of the list using <code>...</code>:</p>

<pre>[x, xs...] := (1..10);
</pre>

<p>Here <code>x = 1</code> and <code>xs = [2,3,4,5,6,7,8,9,10]</code>.</p>

<p>The <code>...</code> also works on nested patterns:</p>

<pre>[[a, b], [c, d]...] := zip (1..10) (11..20);
</pre>

<p>Here <code>a = 1</code>, <code>b = 11</code>, <code>c = 2..10</code> and <code>d = 12..10</code>.</p>

<p>You can also use these patterns in functions:</p>

<pre>f [a, b] := a + b;
f [1, 2];
</pre>

<p>You cannot have multiple declarations of a function with different patterns yet. This is part of the "proper" pattern matching I want to add in the future. Maybe.</p>

<p>You can use these patterns when declaring an operator:</p>

<pre>[a, b] ~ [c, d] := a * c + b * d;
</pre>

<h2 class="header">Files and IO</h2>

<p>You can also load other files. This will run those files in your current environment (so if you load a file inside a function, things from that file will only be available in <em>that</em> function!).</p>

<pre>load 'base';
</pre>

<p>You do <em>not</em> include the .tpl extension when loading files. <code>base.tpl</code> is the standard library and contains things like <code>map</code> that don't need to be native.</p>

<p>Since <code>load</code> is just a (native) function, you can pass it around like normal:</p>

<pre>map load ['base', 'example', 'blarg', 'stuff '];
</pre>

<p>You can also get the contents of a file as a string with <code>open</code>. This <em>does</em> need the extension:</p>

<pre>str := open 'blarg.txt';
</pre>

<p>You can also print to STDOUT with <code>print</code>:</p>

<pre>print "blarg";
print 5;
print (+);
</pre>

<p>This should work on <em>all</em> values including functions.</p></div>

</div>

<?php
include "$homePath/footer.php";
?>

</body>

</html>
