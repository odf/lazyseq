bounce := method(val, while(val type == "Block", val = val call); val)


Seq := Object clone do(
    new    := method(first, rest,
        seq := Seq clone
    	seq first := first
	seq rest_ := rest
	seq)

    rest   := method(rest = rest_ call)

    asList := method(
	a := List clone
        step := method(s, if(s, a append(s first); step(s rest), a))
	bounce(step(self)))

    asString := method(asList join(" -> "))

    fromList := method(a, i_,
    	i := if(i_, i_, 0)
    	if(i < a size, new(a at(i), block(fromList(a, i + 1)))))

    upFrom := method(start, new(start, block(upFrom(start + 1))))

    range  := method(start, limit, upFrom(start) take(limit - start + 1))

    size   := method(
    	 step := method(s, n, if(s, block(step(s rest, n + 1)), n))
	 bounce(step(self, 0)))

    take   := method(n, if(n > 0, new(first, block(rest take(n-1)))))
)

Seq range(10, 20) size println
s := Seq fromList("the quick brown fox jumps over" split)
s println
s size println
s first println
s asList println
