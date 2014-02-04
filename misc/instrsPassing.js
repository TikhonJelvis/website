var MEM_SIZE = 10; // What value should this be?
var BIT_SIZE = 18;

function to_int_real(n) {
  var res = 0;
  var i_n = 1;
  for (var i = 0; i < BIT_SIZE; i++) {
    if (n[i]) {
      res += i_n;
    }
    i_n *= 2;
  }

  return res;
}

function to_int(n) { return n; }

function Stack(ptr, body) {
  return { ptr : ptr, body : body };
}

function empty() { return new Stack(0, [0,0,0,0,0,0,0,0]); }

/* Whee, what ugly code! */
function push_s(stack,  n) {
  stack.ptr = stack.ptr + 1;
  if (to_int(stack.ptr) == 8) {
    stack.ptr = 0;
  }
  stack.body[to_int(stack.ptr)] = n;
}

/* Ditto */
function pop_s(stack) {
  var top = stack.body[to_int(stack.ptr)];
  stack.ptr = stack.ptr + 1;
  if (to_int(stack.ptr) == 8) {
    stack.ptr = 0;
  }
  return top;
}

/* Maybe not terribly efficient. Meh. */
function State(a, b, p, r, s, t, data, ret, memory) {
  return { a : a,
           b : b, 
           p : p,
           r : r,
           s : s,
           t : t,
           data : data,
           ret : ret, memory : memory
         };
}

function start() {
  return State(0, 0, 0, 0, 0, 0, empty(), empty(), []);
}

function reset() {
  s = start();
}

function pop_d(s) {
  var temp = s.t;
  var res = pop_s(s.data);
  s.t = s.s;
  s.s = res;
  return temp;
}

function push_d(s, value) {
  push_s(s.data, s.s);
  s.s = s.t;
  s.t = value;
}

function pop_r(s) {
  var temp = s.r;
  s.r = pop_s(s.ret);
  return temp;
}

function push_r(s, value) {
  push_s(s.ret, s.r);
  s.r = value;
}

function ret(s) {
  s.p = s.r;
  return pop_r(s);
}

function exec(s) {
  var temp = s.r;
  s.r = s.p;
  s.p = temp;
  return 0;
}

function unext(s) {
  if (s.r == 0) {
    pop_r(s);
  } else {
    s.r--;
    s.p--;
  }
  return 0;
}

function fetchP(s) {
  push_d(s, s.memory[to_int(s.p)]);
  s.p++;
  return 0;
}

function fetchPlus(s) {
  push_d(s, s.memory[to_int(s.a)]);
  s.a++;
  return 0;
}

function fetchB(s) {
  push_d(s, s.memory[to_int(s.b)]);
  return 0;
}

function fetch(s) {
  push_d(s, s.memory[to_int(s.a)]);
  return 0;
}

function storeP(s) {
  var temp = pop_d(s);
  s.memory[to_int(s.p)] = temp;
  s.p++;
  return 0;
}

function storePlus( s) {
  var temp = pop_d(s);
  s.memory[to_int(s.a)] = temp;
  s.a++;
  return 0;
}

function storeB(s) {
  var temp = pop_d(s);
  s.memory[to_int(s.b)] = temp;
  return 0;
}

function store( s) {
  var temp = pop_d(s);
  s.memory[to_int(s.a)] = temp;
  return 0;
}

function multiplyStep( s) {
  return 0; // I'm too lazy to implement multiply step at the moment.
}

function times2( s) {
  s.t = s.t >> 1;
  return 0;
}

function div2( s) {
  s.t = s.t << 1;
  return 0;
}

function not( s) {
  s.t = ~s.t;
  return 0;
}

function plus( s) {
  var temp = pop_d(s);
  s.t += temp;
  return 0;
}

function and( s) {
  var temp = pop_d(s);
  s.t = s.t & temp;
  return 0;
}

function or(s) {
  var temp = pop_d(s);
  s.t = s.t ^ temp;
  return 0;
}

function drop( s) {
  pop_d(s);
  return 0;
}

function dup( s) {
  push_d(s, s.t);
  return 0;
}

function pop( s) {
  push_d(s, pop_r(s));
  return 0;
}

function over( s) {
  push_d(s, s.s);
  return 0;
}

function readA( s) {
  push_d(s, s.a);
  return 0;
}

function nop( s) {
  return 0;
}

function push( s) {
  push_r(s, pop_d(s));
  return 0;
}

function setB( s) {
  s.b = pop_d(s);
  return 0;
}

function setA( s) {
  s.a = pop_d(s);
  return 0;
}

function loadLiteral( s,  literal) {
  push_d(s, literal);
  return 0;
}
