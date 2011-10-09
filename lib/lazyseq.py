def bounce(val):
    while callable(val):
        val = val()
    return val

def seq(source):
    return source if isinstance(source, Seq) else Seq.from_array(list(source))


class Seq:
    def __init__(self, first, rest):
        self.first = first
        self.__rest = rest

    @property
    def rest(self):
        self.rest = self.__rest()
        return self.rest

    def forced(self):
        step = lambda seq: seq and (lambda : step(seq.rest))
        bounce(step(self.rest))
        return self

    def __iter__(self):
        seq = self
        while seq:
            yield seq.first
            seq = seq.rest

    @classmethod
    def from_array(cls, a, i = 0):
        if i < len(a):
            return Seq(a[i], lambda : cls.from_array(a, i + 1))

    @classmethod
    def up_from(cls, start):
        return Seq(start, lambda : cls.up_from(start + 1))

    @classmethod
    def down_from(cls, start):
        return Seq(start, lambda : cls.up_from(start - 1))

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
    
    def map(self, f):
        return Seq(f(self.first), lambda : self.rest and self.rest.map(f))

    def __str__(self):
        return " -> ".join(self.map(str))

    def size(self):
        def step(seq, n):
            return (lambda : step(seq.rest, n + 1)) if seq else n
        return bounce(step(self, 0))

    def take(self, n):
        if n > 0:
            return Seq(self.first,
                       lambda : self.rest and self.rest.take(n-1))

    def take_while(self, pred):
        if pred(self.first):
            return Seq(self.first,
                       lambda : self.rest and self.rest.take_while(pred))

    def drop(self, n):
        def step(seq, n):
            if seq and n > 0:
                return lambda : step(seq.rest, n-1)
            else:
                return seq
        return bounce(step(self, n))

    def drop_until(self, pred):
        def step(seq):
            if seq and not pred(seq.first):
                return lambda : step(seq.rest)
            else:
                return seq
        return bounce(step(self))

    def subseqs(self):
        return Seq(self, lambda : self.rest and self.rest.subseqs())

if __name__ == "__main__":
    s = seq("the quick brown fox jumps over".split())
    print "Sequence:     ", s
    print "Forced:       ", s.forced()
    print "Mangle last:  ",
    print s.subseqs().map(
        lambda sub: sub.first if sub.rest else sub.first.upper())
    print "Size:         ", s.size()
