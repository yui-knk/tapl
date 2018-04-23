tru  = t => f => { return t };
fls  = t => f => { return f };
test = l => m => n => { return l(m)(n) };
and  = b => c => { return b(c)(fls) };

// Practice 5.2.1
or   = b => c => { return b(tru)(c) };
not  = b => { return b(fls)(tru) };

pair = f => s => b => { return b(f)(s) };
fst  = p => { return p(tru) };
snd  = p => { return p(fls) };

c0   = s => z => { return z };
scc  = n => s => z => { return s(n(s)(z)) };
c1   = scc(c0);
c2   = scc(c1);
c3   = scc(c2);

// A function for debugging
church2Int = n => n(i => { return i + 1 })(0);

// Practice 5.2.2
scc2  = n => s => z => { return n(s)(s(z)) };

plus  = m => n => s => z => { return m(s)(n(s)(z)) };
times = m => n => { return m(plus(n))(c0) };

// Practice 5.2.3
times2 = m => n => s => z => { return m(n(s))(z) };

// Practice 5.2.4
pow = m => n => { return n(times(m))(c1) };


iszro = m => { return m(_ => { return fls })(tru) };

zz = pair(c0)(c0);
ss = p => { return pair(snd(p))(plus(c1)(snd(p))) };
prd = m => { return fst(m(ss)(zz)) };


// Practice 5.2.5
minus = m => n => { return n(prd)(m) };

// Practice 5.2.6


// Practice 5.2.7
equal = m => n => { return and(iszro(minus(m)(n)))(iszro(minus(n)(m))) };

// Practice 5.2.8
// `[x, y, z]` is `c => n => c x (c y (c z n))`
nil   = c => n => { return n };
cons  = h => t => c => n => { return c(h)(t(c)(n)) };
isnil = l => { return l(_ => _ => { return fls })(tru) };
head  = l => { return l(e => acc => { return e })(nil) };
l_zz  = pair(nil)(nil);
l_ss  = e => p => { return pair(snd(p))(cons(e)(snd(p))) };
tails = l => { return fst(l(l_ss)(l_zz)) };


// From church bool to JS bool
realbool = b => { return b(true)(false) };

// From JS bool to church bool
churchbool = b => { return (b ? tru : fls) };

// Compare church and return JS bool
realeq = m => n => { return (equal(m)(n))(true)(false) };

// Fix
// fix = λf.(λx.f(λy.xxy))(λx.f(λy.xxy));
fix = f => { return (x => { return f(y => { return x(x)(y) }) })(x => { return f(y => { return x(x)(y) }) }) };

g = fct => n => { return realeq(n)(c0) ? c1 : times(n)(fct(prd(n))) };
factorial = fix(g);

// Practice 5.2.9
// 
// bad_g2のケースでは、equalが実行されるまえに、(fct(prd(n)))が実行され
// fctの呼び出しが延々に続いてしまう。
bad_g2 = fct => n => { return test(equal(n)(c0))(c1)(times(n)(fct(prd(n)))) };
bad_factorial2 = fix(bad_g2);

g2     = fct => n => { return test(equal(n)(c0))(c1)(x => { return times(n)(fct(prd(n)))(x) }) };
factorial2 = fix(g2);


// Practice 5.2.10
// From JS int to church nat
churchnat = n => {
  c = c0;
  for(i=0; i<n; i++) {
    c = plus(c)(c1);
  }
  return c;
};


// Practice 5.2.11
// Sum of a list
g_sum = sum => l => { return test(isnil(l))(c0)(x => { return (plus(head(l))(sum(tails(l))))(x) }) };
sum = fix(g_sum);



