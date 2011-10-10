bounce := method(val, while(val type == "Block", val = val call); val)


Seq := Object clone do(
    new    := method(first, rest,
        seq := Seq clone
    	seq first := first
	seq rest_ := rest
	seq)

    rest   := method(rest = rest_ call)

    upFrom := method(start, new(start, block(upFrom(start + 1))))

    range  := method(start, limit, upFrom(start) take(limit - start + 1))

    size   := method(
    	 step := method(s, n, if(s, block(step(s rest, n + 1)), n))
	 bounce(step(self, 0)))

    take   := method(n, if(n > 0, new(first, block(rest take(n-1)))))
)

Seq range(10, 20) size println
