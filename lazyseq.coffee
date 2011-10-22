identity =           (x) -> x
defined  =           (x) -> x?
compose  = (f, g) -> (x) -> f g x
option   = (f)    -> (x) -> f x if x?
curry    = (f, x) -> (y) -> f x, y
rcurry   = (f, y) -> (x) -> f x, y
twice    = (f)    -> (x) -> f f x


bounce = (val) -> val = val() while typeof val == 'function'; val

seq = (source) ->
  if source.constructor == Seq
    source
  else
    Seq.fromArray source


class Seq
  constructor: (args...) ->
    if args.length == 2 and typeof args[1] == 'function'
      [@__first, @__rest] = args
    else
      throw new Error("Use null for empty sequences") if args.length == 0

      n = args.length

      if typeof args[n-1] == 'function'
        if n == 1
          s = args[0]()
        else
          s = seq(args[...n-2]).concat(new Seq(args[n-2...]...))
      else
        s = seq args

      @__first = s.first()
      @__rest = -> s.rest()

  first: -> @__first

  rest:  -> val = @__rest(); (@rest = -> val)()

  forced: ->
    step = (s) -> if s then -> step s.rest()
    bounce step @rest()
    this

  toArray: ->
    a = []
    step = (s) -> if s then a.push s.first(); -> step s.rest()
    bounce step this
    a

  @fromArray: (a, i = 0) ->
    if i < a.length then new Seq a[i], => @fromArray a, i+1

  @upFrom: (start) -> new Seq start, => @upFrom start + 1

  @downFrom: (start) -> new Seq start, => @downFrom start - 1

  @range: (start, limit) ->
    if limit >= start
      @upFrom(start).takeWhile (x) -> x <= limit
    else
      @downFrom(start).takeWhile (x) -> x >= limit

  @constant: (val) -> new Seq val, => @constant val

  @iterate: (x, f) -> new Seq x, => @iterate f(x), f

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
        -> step new Seq(s.first(), -> rev), s.rest()
      else
        rev
    bounce step null, this

  take: (n) -> if n > 0 then new Seq @first(), => @rest()?.take n-1

  takeWhile: (pred) ->
    if pred @first() then new Seq @first(), => @rest()?.takeWhile pred

  drop: (n) ->
    step = (s, n) -> if s and n > 0 then -> step s.rest(), n-1 else s
    bounce step this, n

  dropUntil: (pred) ->
    step = (s) -> if s and not pred(s.first()) then -> step s.rest() else s
    bounce step this

  pick: (n) -> @drop(n)?.first()

  cycle: -> @cycleFrom this

  cycleFrom: (s) ->
    if s then new Seq s.first(), => @cycleFrom s.rest() else @cycle()

  select: (pred) ->
    if pred @first()
      new Seq @first(), => @rest()?.select(pred)
    else if @rest()
      @rest().dropUntil(pred)?.select(pred)

  distinct: (back = null) ->
    if back?.contains @first()
      @dropUntil((x) -> not back.contains x)?.distinct(back)
    else
      new Seq @first(), => @rest()?.distinct new Seq @first(), -> back

  find: (pred) -> @dropUntil(pred)?.first()

  contains: (val) -> @dropUntil((x) -> x == val)?

  forall: (pred) -> not @dropUntil (x) -> not pred(x)

  map: (f) -> new Seq f(@first()), => @rest()?.map(f)

  reduce: (start, op) ->
    step = (val, s) -> if s then -> step op(val, s.first()), s.rest() else val
    bounce step start, this

  sum: -> @reduce 0, (a, b) -> a + b

  product: -> @reduce 1, (a, b) -> a * b

  fold: (op) -> if @rest() then @rest().reduce @first(), op else @first()

  min: -> @fold (a, b) -> if b < a then b else a

  max: -> @fold (a, b) -> if b > a then b else a

  zipSeq: ->
    firsts = @map Seq.first
    if firsts.dropUntil defined
      new Seq firsts, => @map(option(Seq.rest)).zipSeq()

  sequentializeWith: (args...) -> new Seq this, -> seq(args).map(seq) if args

  zip: (others...) -> @sequentializeWith(others...).zipSeq()

  combine: (op, others...) -> @zip(others...).map (s) -> s?.fold(op)

  add: (others...) -> @combine ((a, b) -> a + b), others...

  sub: (others...) -> @combine ((a, b) -> a - b), others...

  mul: (others...) -> @combine ((a, b) -> a * b), others...

  div: (others...) -> @combine ((a, b) -> a / b), others...

  eq: (others...) ->
    @zip(others...).forall (s) ->
      if s then s.forall (x) -> x == s.first() else true

  lazyConcat: (s) ->
    new Seq @first(), => if @rest() then @rest().lazyConcat s else s()

  flatten: ->
    if @rest() then @first().lazyConcat => @rest().flatten() else @first()

  flatMap: (fun) -> @map(fun).flatten()

  concat: (others...) -> @sequentializeWith(others...).flatten()

  interleaveSeq: ->
    alive = @select defined
    alive?.map(Seq.first).lazyConcat -> alive.map(Seq.rest).interleaveSeq()

  interleave: (others...) -> @sequentializeWith(others...).interleaveSeq()

  cartesianSeq: ->
    if @rest()
      @first().flatMap (s) => @rest().cartesianSeq().map (t) -> new Seq s, -> t
    else
      @first().map (s) -> new Seq s

  cartesian: (others...) -> @sequentializeWith(others...).cartesianSeq()

  cantorFold: (back, remaining) ->
    if remaining
      t = new Seq remaining.first(), -> back
      z = @zip(t).takeWhile((x) -> x?.pick(1)?).flatMap (x) ->
        a = x.first()
        x.pick(1).map (y) -> new Seq a, -> y
      new Seq z, => @cantorFold t, remaining.rest()

  cantorRuns: ->
    if @rest()
      @first().cantorFold null, @rest().cantorRuns()
    else
      @first().map (s) -> new Seq(new Seq(s))

  cantor: (others...) -> @sequentializeWith(others...).cantorRuns().flatten()

  subseqs: -> new Seq this, => @rest()?.subseqs()

  consec: (n) -> @subseqs().map (s) -> s.take(n).toArray()

  @treeWalk: (root, nextLevel) ->
    nextStep = (path) ->
      if path
        top = path.first()
        s = nextLevel top.first()
        if s
          new Seq s, -> path
        else if top.rest()
          new Seq top.rest(), -> path.rest()
        else if path.rest()
          backtrack = path.rest().dropUntil(Seq.rest)
          new Seq(backtrack.first().rest(), -> backtrack.rest()) if backtrack

    Seq.iterate(new Seq(new Seq(root)), nextStep).takeWhile((x) -> x?).
      map twice Seq.first


for k, v of Seq.prototype
  do ->
    key = k
    Seq[key] = (s, args...) -> Seq::[key].apply(s, args)


if exports
  exports.seq = seq
  exports.Seq = Seq


if module? and not module.parent
  print = console.log

  s = seq "the quick brown fox jumps over".split(/\s+/)
  print "Sequence:      #{s}"
  print "Forced:        #{s.forced()}"
  print "Mangle last:   " + s.subseqs().map (sub) ->
    if sub.rest() then sub.first() else sub.first().toUpperCase()
  print "Size:          #{s.size()}"
  print "Last:          #{s.last()}"
  print "Runs of 3:     #{s.consec(3).drop(3)}"
  print "Letter counts: #{s.map((w) -> [w, w.length]).take(4)}"
  print "Repeat third:  #{Seq.constant(s.pick 2).take(5)}"
  print "Cycle:         #{s.cycle().take(8)}"
  print "Start at fox:  #{s.dropUntil (x) -> x == 'fox'}"
  print "Five letters:  #{s.select (x) -> x.length == 5}"
  print "First with r:  #{s.find (x) -> 'r' in x}"
  print "All 3 letters: #{s.forall (x) -> x.length == 3}"
  print "Reverse:       #{s.reverse()}"
  print "Min and max:   #{s.min()}, #{s.max()}"
  print "With indexes:  #{s.zip('abcdefg').map(Seq.toArray).drop(3)}"
  print ""
  print "Number range:  #{Seq.range 10, 20}"
  print "Its sum:       #{Seq.range(10, 20).sum()}"
  print "Its product:   #{Seq.range(10, 20).product()}"
  print "flat_map:      #{Seq.range(4, 1).flatMap (n) -> Seq.range(1, n)}"
  print "Iterate:       #{Seq.iterate(1, (n) -> 2 * n).take(10)}"
  print ""

  fib = new Seq 0, 1, -> fib.rest().add fib
  print "Fibonacci:     #{fib.take(12)}"
  print "Compare:       #{fib.take(10).eq [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]}"
  print "Compare:       #{fib.take(10).eq [0, 1, 1, 2, 3, 5, 8.2, 13, 21, 34]}"
  print ""
  print "No first:      #{new Seq -> s}"
  print "One first:     #{new Seq 1, -> s}"
  print "Two firsts:    #{new Seq 1, 2, -> s}"
  print "Three firsts:  #{new Seq 1, 2, 3, -> s}"
  print ""

  primes = Seq.upFrom(2).select (n) ->
    n < 4 or primes.takeWhile((m) -> m * m <= n).forall((m) -> n % m > 0)
  print "Prime numbers: #{primes.take(10)}"
  print ""
  print "Concatenation: #{s.take(3).concat fib.take(2), primes.take(3)}"
  print "Interleave:    #{s.take(3).interleave fib.take(2), primes.take(3)}"
  print "Cartesian:     " +
    fib.take(2).cartesian(primes.take(2), [0]).map(Seq.toArray)
  print "Cantor:        " +
    primes.cantor(primes, primes).take(5).map(Seq.toArray)
  print "Distinct:      #{fib.interleave(primes).distinct().take(10)}"
  print ""

  permutations = (degree) ->
    nextLevel = (perm) ->
      i = perm.indexOf null
      unless i < 0
        Seq.range(1, degree).select((n) -> n not in perm).
          map (to) -> perm[...i].concat [to], perm[i+1...]

    Seq.treeWalk([0].concat(null for i in [1..degree]), nextLevel).
      map((p) -> p[1...]).select (p) -> null not in p

  print "Permutations:  #{permutations 4}"
