identity =           (x) -> x
defined  =           (x) -> x?
compose  = (f, g) -> (x) -> f g x
option   = (f)    -> (x) -> f x if x?
curry    = (f, x) -> (y) -> f x, y
rcurry   = (f, y) -> (x) -> f x, y
twice    = (f)    -> (x) -> f f x


bounce = (val) -> val = val() while typeof val == 'function'; val


class Seq
  constructor: (@__first, @__rest) ->

  first: -> @__first

  rest:  -> val = @__rest(); (@rest = -> val)()

  forced: ->
    step = (s) -> if s then -> step s.rest()
    bounce step @rest()
    this

  toSeq: -> this

  toArray: ->
    a = []
    step = (s) -> if s then a.push s.first(); -> step s.rest()
    bounce step this
    a

  toString: -> @toArray().join ' -> '

  size: ->
    step = (s, n) -> if s then -> step s.rest(), n + 1 else n
    bounce step this, 0

  last: ->
    step = (s) -> if s.rest() then -> step s.rest() else s.first()
    bounce step this

  reverse: ->
    step = (rev, s) ->
      if s
        -> step seq.build(s.first(), -> rev), s.rest()
      else
        rev
    bounce step null, this

  take: (n) -> if n > 0 then seq.build @first(), => @rest()?.take n-1

  takeWhile: (pred) ->
    if pred @first() then seq.build @first(), => @rest()?.takeWhile pred

  drop: (n) ->
    step = (s, n) -> if s and n > 0 then -> step s.rest(), n-1 else s
    bounce step this, n

  dropUntil: (pred) ->
    step = (s) -> if s and not pred(s.first()) then -> step s.rest() else s
    bounce step this

  pick: (n) -> @drop(n)?.first()

  cycle: -> @cycleFrom this

  cycleFrom: (s) ->
    if s then seq.build s.first(), => @cycleFrom s.rest() else @cycle()

  select: (pred) ->
    if pred @first()
      seq.build @first(), => @rest()?.select(pred)
    else if @rest()
      @rest().dropUntil(pred)?.select(pred)

  distinct: (back = null) ->
    if back?.contains @first()
      @dropUntil((x) -> not back.contains x)?.distinct(back)
    else
      seq.build @first(), => @rest()?.distinct seq.build @first(), -> back

  find: (pred) -> @dropUntil(pred)?.first()

  contains: (val) -> @dropUntil((x) -> x == val)?

  forall: (pred) -> not @dropUntil (x) -> not pred(x)

  map: (f) -> seq.build f(@first()), => @rest()?.map(f)

  reduce: (start, op) ->
    step = (val, s) -> if s then -> step op(val, s.first()), s.rest() else val
    bounce step start, this

  sum: -> @reduce 0, (a, b) -> a + b

  product: -> @reduce 1, (a, b) -> a * b

  fold: (op) -> if @rest() then @rest().reduce @first(), op else @first()

  min: -> @fold (a, b) -> if b < a then b else a

  max: -> @fold (a, b) -> if b > a then b else a

  zipSeq: ->
    firsts = @map seq.first
    if firsts.dropUntil defined
      seq.build firsts, => @map(option(seq.rest)).zipSeq()

  sequentializeWith: (args...) -> seq.build this, -> seq(args).map(seq) if args

  zip: (others...) -> @sequentializeWith(others...).zipSeq()

  combine: (op, others...) -> @zip(others...).map (s) -> s?.fold(op)

  add: (others...) -> @combine ((a, b) -> a + b), others...

  sub: (others...) -> @combine ((a, b) -> a - b), others...

  mul: (others...) -> @combine ((a, b) -> a * b), others...

  div: (others...) -> @combine ((a, b) -> a / b), others...

  equals: (others...) ->
    @zip(others...).forall (s) ->
      if s then s.forall (x) -> x == s.first() else true

  lazyConcat: (s) ->
    seq.build @first(), => if @rest() then @rest().lazyConcat s else s()

  flatten: ->
    if @rest() then @first().lazyConcat => @rest().flatten() else @first()

  flatMap: (fun) -> @map(fun).flatten()

  concat: (others...) -> @sequentializeWith(others...).flatten()

  interleaveSeq: ->
    alive = @select defined
    alive?.map(seq.first).lazyConcat -> alive.map(seq.rest).interleaveSeq()

  interleave: (others...) -> @sequentializeWith(others...).interleaveSeq()

  cartesianSeq: ->
    if @rest()
      @first().flatMap (s) => @rest().cartesianSeq().map (t) -> seq.build s, -> t
    else
      @first().map (s) -> seq.build s

  cartesian: (others...) -> @sequentializeWith(others...).cartesianSeq()

  cantorFold: (back, remaining) ->
    if remaining
      t = seq.build remaining.first(), -> back
      z = @zip(t).takeWhile((x) -> x?.pick(1)?).flatMap (x) ->
        a = x.first()
        x.pick(1).map (y) -> seq.build a, -> y
      seq.build z, => @cantorFold t, remaining.rest()

  cantorRuns: ->
    if @rest()
      @first().cantorFold null, @rest().cantorRuns()
    else
      @first().map (s) -> seq.build(seq.build(s))

  cantor: (others...) -> @sequentializeWith(others...).cantorRuns().flatten()

  subseqs: -> seq.build this, => @rest()?.subseqs()

  consec: (n) -> @subseqs().map rcurry seq.take, n


seq = (source) ->
  if typeof source.toSeq == 'function'
    source.toSeq()
  else
    seq.fromArray source

seq.build = (args...) ->
  n = args.length
  throw new Error("Use null for empty sequences") if n == 0

  if typeof args[n-1] == 'function'
    if n == 1
      args[0]()
    else if n == 2
      new Seq args...
    else
      seq(args[...n-2]).concat(new Seq(args[n-2...]...))
  else
    seq args

seq.fromArray = (a, i = 0) ->
  if i < a.length then seq.build a[i], -> seq.fromArray a, i+1

seq.upFrom = (start) -> seq.build start, -> seq.upFrom start + 1

seq.downFrom = (start) -> seq.build start, -> seq.downFrom start - 1

seq.range = (start, limit) ->
  if limit >= start
    seq.upFrom(start).takeWhile (x) -> x <= limit
  else
    seq.downFrom(start).takeWhile (x) -> x >= limit

seq.constant = (val) -> seq.build val, -> seq.constant val

seq.iterate = (x, f) -> seq.build x, -> seq.iterate f(x), f

seq.treeWalk = (root, nextLevel) ->
  seq.build root, -> nextLevel(root)?.flatMap (s) -> seq.treeWalk s, nextLevel


for k, v of Seq.prototype
  do ->
    key = k
    seq[key] = (s, args...) -> Seq::[key].apply(s, args)


(exports &= this.lazyseq &= {}).seq = seq


if module? and not module.parent
  print = console.log

  s = seq "the quick brown fox jumps over".split(/\s+/)
  print "Sequence:      #{s}"
  print "Forced:        #{s.forced()}"
  print "Mangle last:   " + s.subseqs().map (sub) ->
    if sub.rest() then sub.first() else sub.first().toUpperCase()
  print "Size:          #{s.size()}"
  print "Last:          #{s.last()}"
  print "Runs of 3:     #{s.consec(3).map(seq.toArray).drop(3)}"
  print "Letter counts: #{s.map((w) -> [w, w.length]).take(4)}"
  print "Repeat third:  #{seq.constant(s.pick 2).take(5)}"
  print "Cycle:         #{s.cycle().take(8)}"
  print "Start at fox:  #{s.dropUntil (x) -> x == 'fox'}"
  print "Five letters:  #{s.select (x) -> x.length == 5}"
  print "First with r:  #{s.find (x) -> 'r' in x}"
  print "All 3 letters: #{s.forall (x) -> x.length == 3}"
  print "Reverse:       #{s.reverse()}"
  print "Min and max:   #{s.min()}, #{s.max()}"
  print "With indexes:  #{s.zip('abcdefg').map(seq.toArray).drop(3)}"
  print ""
  print "Number range:  #{seq.range 10, 20}"
  print "Its sum:       #{seq.range(10, 20).sum()}"
  print "Its product:   #{seq.range(10, 20).product()}"
  print "flat_map:      #{seq.range(4, 1).flatMap (n) -> seq.range(1, n)}"
  print "Iterate:       #{seq.iterate(1, (n) -> 2 * n).take(10)}"
  print ""

  fib = seq.build 0, 1, -> fib.rest().add fib
  print "Fibonacci:     #{fib.take(12)}"
  print "Compare:       " +
    fib.take(10).equals [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]
  print "Compare:       " +
    fib.take(10).equals [0, 1, 1, 2, 3, 5, 8.2, 13, 21, 34]
  print ""
  print "No first:      #{seq.build -> s}"
  print "One first:     #{seq.build 1, -> s}"
  print "Two firsts:    #{seq.build 1, 2, -> s}"
  print "Three firsts:  #{seq.build 1, 2, 3, -> s}"
  print ""

  primes = seq.upFrom(2).select (n) ->
    n < 4 or primes.takeWhile((m) -> m * m <= n).forall((m) -> n % m > 0)
  print "Prime numbers: #{primes.take(10)}"
  print ""
  print "Concatenation: #{s.take(3).concat fib.take(2), primes.take(3)}"
  print "Interleave:    #{s.take(3).interleave fib.take(2), primes.take(3)}"
  print "Cartesian:     " +
    fib.take(2).cartesian(primes.take(2), [0]).map(seq.toArray)
  print "Cantor:        " +
    primes.cantor(primes, primes).take(5).map(seq.toArray)
  print "Distinct:      #{fib.interleave(primes).distinct().take(10)}"
  print ""

  permutations = (degree) ->
    choices = (p) ->
      i = p.indexOf 0
      unless i < 0
        seq.range(1, degree).select((n) -> n not in p).
          map (n) -> p[...i].concat [n], p[i+1...]

    seq.treeWalk(0 for i in [1..degree], choices).select (p) -> 0 not in p

  print "Permutations:  #{permutations 4}"
