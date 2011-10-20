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
          s = seq(args[...n-2]).concat(new Seq(args[n-2...]))
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

  toString: -> @toArray().join ' '

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

  find: (pred) -> @dropUntil(pred)?.first()

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
    firsts = @map (s) -> s?.first()
    if firsts.dropUntil((s) -> s?)
      new Seq firsts, => @map((s) -> s?.rest()).zipSeq()

  sequentializeWith: (args...) -> new Seq this, -> seq(args).map(seq) if args

  zip: (others...) -> @sequentializeWith(others...).zipSeq()

  subseqs: -> new Seq this, => @rest()?.subseqs()

  consec: (n) -> @subseqs().map (s) -> s.take(n).toArray()


if exports
  exports.seq = seq
  exports.Seq = Seq


if module? and not module.parent
  s = seq "the quick brown fox jumps over".split(/\s+/)
  console.log "Sequence:      #{s}"
  console.log "Forced:        #{s.forced()}"
  console.log "Mangled last:  " + (s.subseqs().map((sub) ->
    if sub.rest() then sub.first() else sub.first().toUpperCase()))
  console.log "Size:          #{s.size()}"
  console.log "Last:          #{s.last()}"
  console.log "Runs of 3:     #{s.consec(3).drop(3)}"
  console.log "Letter counts: #{s.map((w) -> [w, w.length]).take(4)}"
  console.log "Repeat third:  #{Seq.constant(s.pick 2).take(5)}"
  console.log "Cycle:         #{s.cycle().take(8)}"
  console.log "Start at fox:  #{s.dropUntil (x) -> x == 'fox'}"
  console.log "Five letters:  #{s.select (x) -> x.length == 5}"
  console.log "First with r:  #{s.find (x) -> 'r' in x}"
  console.log "All 3 letters: #{s.forall (x) -> x.length == 3}"
  console.log "Reverse:       #{s.reverse()}"
  console.log "Min and max:   #{s.min()}, #{s.max()}"
  console.log "With indexes:  #{s.zip('abcdef').map((s) -> s.toArray()).drop(3)}"
  console.log
  console.log "Number range:  #{Seq.range 10, 20}"
  console.log "Its sum:       #{Seq.range(10, 20).sum()}"
  console.log "Its product:   #{Seq.range(10, 20).product()}"
