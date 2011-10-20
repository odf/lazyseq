identity = (x)    -> x
compose  = (f, g) -> (x) -> f g x
option   = (f)    -> (x) -> f x if x
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

  @constant: (val) -> new Seq val, => @constant val

  toString: -> @toArray().join ' '

  size: ->
    step = (s, n) -> if s then -> step s.rest(), n + 1 else n
    bounce step this, 0

  last: ->
    step = (s) -> if s.rest() then -> step s.rest() else s.first()
    bounce step this

  take: (n) -> if n > 0 then new Seq @first(), => @rest()?.take n-1

  drop: (n) ->
    step = (s, n) -> if s and n > 0 then -> step s.rest(), n-1 else s
    bounce step this, n

  pick: (n) -> @drop(n)?.first()

  cycle: -> @cycleFrom this

  cycleFrom: (s) ->
    if s then new Seq s.first(), => @cycleFrom s.rest() else @cycle()

  map: (f) -> new Seq f(@first()), => @rest()?.map(f)

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
