def bounce(val):
    while callable(val):
        val = val()
    return val

def seq(source):
    return source if isinstance(source, Seq) else Seq.from_array(list(source))


class Seq:
    def __init__(self, *args):
        if len(args) == 2 and callable(args[1]):
            self.first  = args[0]
            self.__rest = args[1]
        else:
            if len(args) == 0:
                raise ArgumentError("Use None for empty sequences.")

            if callable(args[-1]):
                if len(args) == 1:
                    s = args[0]()
                else:
                    s = seq(args[:-2]).concat(Seq(*args[-2:]))
            else:
                s = seq(args)
                
            self.first = s.first
            self.__rest = lambda : s.rest

    @property
    def rest(self):
        self.rest = self.__rest and self.__rest()
        return self.rest

    def forced(self):
        step = lambda s: s and (lambda : step(s.rest))
        bounce(step(self.rest))
        return self

    def __iter__(self):
        s = self
        while s:
            yield s.first
            s = s.rest

    @classmethod
    def from_array(cls, a, i = 0):
        if i < len(a):
            return Seq(a[i], lambda : cls.from_array(a, i + 1))

    @classmethod
    def up_from(cls, start):
        return Seq(start, lambda : cls.up_from(start + 1))

    @classmethod
    def down_from(cls, start):
        return Seq(start, lambda : cls.down_from(start - 1))

    @classmethod
    def range(cls, start, limit):
        if limit >= start:
            return Seq.up_from(start).take_while(lambda x: x <= limit)
        else:
            return Seq.down_from(start).take_while(lambda x: x >= limit)

    @classmethod
    def constant(cls, val):
        return Seq(val, lambda : cls.constant(val))

    @classmethod
    def iterate(cls, x, f):
        return Seq(x, lambda : cls.iterate(f(x), f))
    
    def __str__(self):
        return " -> ".join(self.map(str))

    def size(self):
        def step(s, n):
            return (lambda : step(s.rest, n + 1)) if s else n
        return bounce(step(self, 0))

    def last(self):
        def step(s):
            return (lambda : step(s.rest)) if s.rest else s.first
        return bounce(step(self))

    def reverse(self):
        def step(rev, s):
            if s:
                return lambda : step(Seq(s.first, lambda : rev), s.rest)
            else:
                return rev
        return bounce(step(None, self))

    def take(self, n):
        if n > 0:
            return Seq(self.first,
                       lambda : self.rest and self.rest.take(n-1))

    def take_while(self, pred):
        if pred(self.first):
            return Seq(self.first,
                       lambda : self.rest and self.rest.take_while(pred))

    def drop(self, n):
        def step(s, n):
            if s and n > 0:
                return lambda : step(s.rest, n-1)
            else:
                return s
        return bounce(step(self, n))

    def drop_until(self, pred):
        def step(s):
            if s and not pred(s.first):
                return lambda : step(s.rest)
            else:
                return s
        return bounce(step(self))

    def pick(self, n):
        return self.drop(n).first

    def cycle(self):
        return self.cycle_from(self)

    def cycle_from(self, s):
        if s:
            return Seq(s.first, lambda : self.cycle_from(s.rest))
        else:
            return self.cycle()

    def select(self, pred):
        if pred(self.first):
            return Seq(self.first, lambda : self.rest and self.rest.select(pred))
        elif self.rest:
            r = self.rest.drop_until(pred)
            return r and r.select(pred)

    def distinct(self, back = None):
        if back and back.contains(self.first):
            r = self.drop_until(lambda x: not back.contains(x))
            return r and r.distinct(back)
        else:
            return Seq(self.first,
                       lambda : self.rest and self.rest.distinct(
                           Seq(self.first, lambda : back)))

    def find(self, pred):
        good = self.drop_until(pred)
        return good and good.first

    def contains(self, val):
        return self.drop_until(lambda x: x == val) is not None

    def forall(self, pred):
        return not self.drop_until(lambda x: not pred(x))

    def map(self, f):
        return Seq(f(self.first), lambda : self.rest and self.rest.map(f))

    def reduce(self, start, op):
        def step(val, s):
            if s:
                return lambda : step(op(val, s.first), s.rest)
            else:
                return val
        return bounce(step(start, self))

    def sum(self):
        return self.reduce(0, lambda a, b: a + b)

    def product(self):
        return self.reduce(1, lambda a, b: a * b)

    def fold(self, op):
        if self.rest:
            return self.rest.reduce(self.first, op)
        else:
            return self.first

    def min(self):
        return self.fold(lambda a, b: b if b < a else a)

    def max(self):
        return self.fold(lambda a, b: b if b > a else a)

    def zip_seq(self):
        firsts = self.map(lambda s: s and s.first)
        if not firsts.forall(lambda s: s is None):
            return Seq(firsts,
                       lambda : self.map(lambda s: s and s.rest).zip_seq())

    def sequentialize_with(self, *args):
        return Seq(self, lambda: args and seq(args).map(seq))

    def zip(self, *others):
        return self.sequentialize_with(*others).zip_seq()

    def combine(self, op, *others):
        return self.zip(*others).map(lambda s: s and s.fold(op))

    def add(self, *others):
        return self.combine((lambda a, b: a + b), *others)

    def __add__(self, other):
        return self.add(other)

    def sub(self, *others):
        return self.combine((lambda a, b: a - b), *others)

    def __sub__(self, other):
        return self.sub(other)

    def mul(self, *others):
        return self.combine((lambda a, b: a * b), *others)

    def __mul__(self, other):
        return self.mul(other)

    def div(self, *others):
        return self.combine((lambda a, b: a / b), *others)

    def __div__(self, other):
        return self.div(other)

    def eq(self, *others):
        return self.zip(*others).forall(
            lambda s: s.forall(lambda x: x == s.first) if s else true)

    def __eq__(self, other):
        try:
            return self.eq(other)
        except TypeError:
            False

    def __ne__(self, other):
        return not self.eq(other)

    def lazy_concat(self, s):
        return Seq(self.first,
                   lambda : self.rest.lazy_concat(s) if self.rest else s())

    def flatten(self):
        if self.rest:
            return self.first.lazy_concat(lambda : self.rest.flatten())
        else:
            return self.first

    def flat_map(self, fun):
        return self.map(fun).flatten()

    def concat(self, *others):
        return self.sequentialize_with(*others).flatten()

    def interleave_seq(self):
        head = lambda s: s.map(lambda t: t.first)
        tail = lambda s: s.map(lambda t: t.rest).interleave_seq()
        
        alive = self.select(lambda s: s is not None)
        if alive:
            return head(alive).lazy_concat(lambda : tail(alive))

    def interleave(self, *others):
        return self.sequentialize_with(*others).interleave_seq()

    def cartesian_seq(self):
        if self.rest:
            return self.first.flat_map(
                lambda s: self.rest.cartesian_seq().map(
                    lambda t: Seq(s, lambda : t)))
        else:
            return self.first.map(lambda s: Seq(s))

    def cartesian(self, *others):
        return self.sequentialize_with(*others).cartesian_seq()

    def cantor_fold(self, back, remaining):
        if remaining:
            t = Seq(remaining.first, lambda : back)
            z = self.zip(t).take_while(lambda x: x and x.pick(1)).flat_map(
                lambda x: x.pick(1).map(lambda y: Seq(x.first, lambda : y)))
            return Seq(z, lambda : self.cantor_fold(t, remaining.rest))

    def cantor_runs(self):
        if self.rest:
            return self.first.cantor_fold(None, self.rest.cantor_runs())
        else:
            return self.first.map(lambda x: Seq(Seq(x)))

    def cantor(self, *others):
        return self.sequentialize_with(*others).cantor_runs().flatten()

    def subseqs(self):
        return Seq(self, lambda : self.rest and self.rest.subseqs())

    def consec(self, n):
        return self.subseqs().map(lambda s: list(s.take(n)))


if __name__ == "__main__":
    s = seq("the quick brown fox jumps over".split())
    print "Sequence:     ", s
    print "Forced:       ", s.forced()
    print "Mangle last:  ",
    print s.subseqs().map(
        lambda sub: sub.first if sub.rest else sub.first.upper())
    print "Size:         ", s.size()
    print "Last:         ", s.last()
    print "Runs of 3:    ", s.consec(3).drop(3)
    print "Letter counts:", dict(s.map(lambda w: (w, len(w))).take(4))
    print "Repeat third: ", Seq.constant(s.pick(2)).take(5)
    print "Cycle:        ", s.cycle().take(8)
    print "Start at fox: ", s.drop_until(lambda x: x == "fox")
    print "Five letters: ", s.select(lambda x: len(x) == 5)
    print "First with r: ", s.find(lambda x: 'r' in x)
    print "All 3 letters:", s.forall(lambda x: len(x) == 3)
    print "Reverse:      ", s.reverse()
    print "Min and max:  ", "%s, %s" % (s.min(), s.max())
    print "Zip with ints:", s.zip('abcdefg').map(list).drop(3)
    print
    print "Number range: ", s.range(10, 20)
    print "Its sum:      ", s.range(10, 20).sum()
    print "Its product:  ", s.range(10, 20).product()
    print "flat_map:     ", Seq.range(4, 1).flat_map(lambda n: Seq.range(1, n))
    print "Iterate:      ", Seq.iterate(1, lambda x: 2 * x).take(10)
    print
    fib = Seq(0, 1, lambda : fib.rest + fib)
    print "Fibonacci:    ", fib.take(12)
    print "Compare:      ", (fib.take(10) == (0, 1, 1, 2, 3, 5, 8, 13, 21, 34))
    print "Compare:      ", (fib.take(10) == (0, 1, 1, 2, 3, 5, 8.2, 13, 21, 34))
    print
    print "No first:     ", Seq(lambda : s)
    print "One first:    ", Seq(1, lambda : s)
    print "Two firsts:   ", Seq(1, 2, lambda : s)
    print "Three firsts: ", Seq(1, 2, 3, lambda : s)
    print
    primes = Seq.up_from(2).select(
        lambda n: n < 4 or (primes.take_while(lambda m: m * m <= n).
                            forall(lambda m: n % m)))
    print "Prime numbers:", primes.take(10)
    print
    print "Concatenation:", s.take(3).concat(fib.take(2), primes.take(3))
    print "Interleave:   ", s.take(3).interleave(fib.take(2), primes.take(3))
    print "Cartesian:    ", fib.take(2).cartesian(primes.take(2), [0]).map(list)
    print "Cantor:       ", primes.cantor(primes, primes).take(5).map(list)
    print "Distinct:     ", fib.interleave(primes).distinct().take(10)
