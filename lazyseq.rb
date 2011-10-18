def bounce(val)
  while val.is_a? Proc
    val = val.call
  end
  val
end


class Array
  def to_seq
    Seq.from_array self
  end
end


module Enumerable
  def to_seq
    to_a.to_seq
  end
end


class Seq
  attr_reader :first

  def initialize(*elems, &rest)
    case elems.length
    when 0
      seq = rest.call()
      @first = seq.first
      @rest  = lambda { seq.rest }
    when 1
      @first = elems[0]
      @rest  = rest
    when 2
      @first = elems[0]
      @rest  = lambda { Seq.new elems[1], &rest }
    else
      @first = elems[0]
      @rest  = lambda { elems[1..-2].to_seq.concat Seq.new(elems[-1], &rest) }
    end
  end

  def rest
    @rest = @rest && @rest.call()
    class << self; def rest; @rest end end
    freeze
    @rest
  end

  def forced
    step = lambda { |seq| lambda { step.call seq.rest } if seq }
    bounce step.call(self.rest)
    self
  end

  def to_enum
    Enumerator.new do |yielder|
      s = self
      while s
        yielder << s.first
        s = s.rest
      end
    end
  end

  def to_seq
    self
  end

  def to_a
    to_enum.to_a
  end

  def to_hash
    hash = {}
    to_enum.each { |k, v| hash[k] = v }
    hash
  end

  def self.from_array(array, i = 0)
    Seq.new(array[i]) { from_array array, i+1 } if i < array.length
  end

  def self.up_from(start)
    Seq.new(start) { up_from start.next }
  end

  def self.down_from(start)
    Seq.new(start) { down_from start.pred }
  end

  def self.range(start, limit)
    if limit >= start
      Seq.up_from(start).take_while { |x| x <= limit }
    else
      Seq.down_from(start).take_while { |x| x >= limit }
    end
  end

  def self.constant(val)
    Seq.new(val) { constant val }
  end

  def self.iterate(x, &f)
    Seq.new(x) { iterate f.call(x), &f }
  end

  def to_s
    map(&:to_s).to_a.join ' -> '
  end

  def size
    step = lambda { |seq, n|
      if seq then lambda { step.call(seq.rest, n + 1) }  else n end
    }
    bounce step.call(self, 0)
  end

  def last
    step = lambda { |seq|
      if seq.rest then lambda { step.call seq.rest } else seq.first end
    }
    bounce step.call(self)
  end

  def reverse
    step = lambda { |rev, seq|
      if seq
        lambda { step.call Seq.new(seq.first) { rev }, seq.rest }
      else
        rev
      end
    }
    bounce step.call(nil, self)
  end

  def take(n)
    Seq.new(self.first) { rest.take n-1 if rest } if n > 0
  end

  def take_while(&pred)
    Seq.new(first) { rest.take_while &pred if rest } if pred.call(first)
  end

  def drop(n)
    step = lambda { |seq, n|
      if seq and n > 0 then lambda { step.call seq.rest, n-1 } else seq end
    }
    bounce step.call(self, n)
  end

  def drop_until(&pred)
    step = lambda { |seq|
      if seq and not pred.call seq.first then
        lambda { step.call seq.rest }
      else
        seq
      end
    }
    bounce step.call(self)
  end

  def pick(n)
    drop(n).first
  end

  def cycle
    cycle_from self
  end

  def cycle_from(seq)
    if seq then Seq.new(seq.first) { cycle_from(seq.rest) } else cycle end
  end

  def select(&pred)
    if pred.call first
      Seq.new(first) { rest.select &pred if rest }
    elsif rest
      r = rest.drop_until &pred
      r.select &pred if r
    end
  end

  def distinct(back = nil)
    if back and back.contains? first
      r = drop_until { |x| not back.contains? x }
      r.distinct(back) if r
    else
      Seq.new(first) { rest.distinct Seq.new(first) { back } if rest }
    end
  end

  def find(&pred)
    if good = drop_until(&pred) then good.first end
  end

  def contains?(val)
    not drop_until { |x| x == val }.nil?
  end

  def forall?(&pred)
    not drop_until { |x| not pred.call x }
  end

  def map(&f)
    Seq.new(f.call self.first) { rest.map(&f) if rest }
  end

  def reduce(start = nil, &op)
    step = lambda do |val, seq|
      if seq
        lambda { step.call op.call(val, seq.first), seq.rest }
      else
        val
      end
    end
    bounce step.call(start, self)
  end

  def sum
    reduce(0) { |a, b| a + b }
  end

  def product
    reduce(1) { |a, b| a * b }
  end

  def fold(&op)
    if rest then rest.reduce first, &op else first end
  end

  def min
    fold { |a, b| b < a ? b : a }
  end

  def max
    fold { |a, b| b > a ? b : a }
  end

  def zip_seq
    firsts = map { |s| s.first if s }
    unless firsts.forall? &:nil?
      Seq.new(firsts) { map { |s| s.rest if s }.zip_seq }
    end
  end

  def sequentialize_with(*args)
    Seq.new(self) { args.to_seq.map { |s| s.to_seq } if args.length > 0 }
  end

  def zip(*others)
    sequentialize_with(*others).zip_seq
  end

  def combine(*others, &op)
    zip(*others).map { |seq| seq.fold &op if seq }
  end

  def +(*others)
    combine(*others) { |a, b| a + b }
  end

  def -(*others)
    combine(*others) { |a, b| a - b }
  end

  def *(*others)
    combine(*others) { |a, b| a * b }
  end

  def /(*others)
    combine(*others) { |a, b| a / b }
  end

  def ==(*others)
    zip(*others).forall? do |seq|
      if seq then seq.forall? { |x| x == seq.first } else true end
    end
  end

  def lazy_concat(&seq)
    Seq.new(first) { if rest then rest.lazy_concat(&seq) else seq.call end }
  end

  def flatten
    if rest then first.lazy_concat { rest.flatten } else first end
  end

  def flat_map(&fun)
    map(&fun).flatten
  end

  def concat(*others)
    sequentialize_with(*others).flatten
  end

  def interleave_seq
    alive = select { |seq| not seq.nil? }
    alive.map(&:first).lazy_concat { alive.map(&:rest).interleave_seq } if alive
  end

  def interleave(*others)
    sequentialize_with(*others).interleave_seq
  end

  def cartesian_seq
    if rest
      first.flat_map { |s| rest.cartesian_seq.map { |t| Seq.new(s) { t } } }
    else
      first.map { |s| Seq.new s }
    end
  end

  def cartesian(*others)
    sequentialize_with(*others).cartesian_seq
  end

  def cantor_fold(back, remaining)
    if remaining
      t = Seq.new(remaining.first) { back }
      z = zip(t).take_while { |x| x and x.pick 1 }.flat_map do |x|
        a = x.first
        x.pick(1).map { |y| Seq.new(a) { y } }
      end
      Seq.new(z) { cantor_fold t, remaining.rest }
    end
  end

  def cantor_runs
    if rest
      first.cantor_fold nil, rest.cantor_runs
    else
      first.map { |x| Seq.new Seq.new x }
    end
  end

  def cantor(*others)
    sequentialize_with(*others).cantor_runs.flatten
  end

  def subseqs
    Seq.new(self) { rest.subseqs if rest }
  end

  def consec(n)
    subseqs.map { |s| s.take(n).to_a }
  end

  def self.tree_walk(root, next_level)
    next_step = lambda { |path|
      if path
        top = path.first
        s = next_level.call top.first
        if s
          Seq.new(s) { path }
        elsif top.rest
          Seq.new(top.rest) { path.rest }
        elsif path.rest
          backtrack = path.rest.drop_until { |s| s.rest }
          Seq.new(backtrack.first.rest) { backtrack.rest } if backtrack
        end
      end
    }

    Seq.iterate(Seq.new(Seq.new(root)), &next_step).take_while { |x|
      x }.map { |x| x.first.first }
  end
end


if __FILE__ == $0
  seq = %w{the quick brown fox jumps over}.to_seq
  puts "Sequence:      #{seq}"
  puts "Forced:        #{seq.forced}"
  puts "Mangle last:   " +
    seq.subseqs.map { |sub| sub.rest ? sub.first : sub.first.upcase }.to_s
  puts "Size:          #{seq.size}"
  puts "Last:          #{seq.last}"
  puts "Runs of 3:     #{seq.consec(3).drop(3)}"
  puts "Letter counts: #{seq.map { |w| [w, w.length] }.take(4).to_hash}"
  puts "Repeat third:  #{Seq.constant(seq.pick(2)).take 5}"
  puts "Cycle:         #{seq.cycle.take 8}"
  puts "Start at fox:  #{seq.drop_until { |x| x == 'fox' }}"
  puts "Five letters:  #{seq.select { |x| x.length == 5 }}"
  puts "First with r:  #{seq.find { |x| x.include? 'r' }}"
  puts "All 3 letters: #{seq.forall? { |x| x.length == 3 }}"
  puts "Reverse:       #{seq.reverse}"
  puts "Min and max:   #{seq.min}, #{seq.max}"
  puts "With indexes:  #{seq.zip('abcdefg'.chars).map(&:to_a).drop 3}"
  puts
  puts "Number range:  #{Seq.range 10, 20}"
  puts "Its sum:       #{Seq.range(10, 20).sum}"
  puts "Its product:   #{Seq.range(10, 20).product}"
  puts "flat_map:      #{Seq.range(4, 1).flat_map { |n| Seq.range(1, n) }}" 
  puts "String range:  #{Seq.range "ady", "aeg"}"
  puts "Iterate:       #{Seq.iterate(1) { |x| 2 * x }.take(10)}"
  puts
  fib = Seq.new(0, 1) { fib.rest + fib }
  puts "Fibonacci:     #{fib.take 12}"
  puts "Compare:       #{fib.take(10) == [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]}"
  puts "Compare:       #{fib.take(10) == [0, 1, 1, 2, 3, 5, 8.2, 13, 21, 34]}"
  puts
  puts "No first:      #{Seq.new() { seq }}"
  puts "One first:     #{Seq.new(1) { seq }}"
  puts "Two firsts:    #{Seq.new(1, 2) { seq }}"
  puts "Three firsts:  #{Seq.new(1, 2, 3) { seq }}"
  puts
  primes = Seq.up_from(2).select do |n|
    n < 4 or primes.take_while { |m| m * m <= n }.forall? { |m| n % m > 0 }
  end
  puts "Prime numbers: #{primes.take(10)}"
  puts
  puts "Concatenation: #{seq.take(3).concat(fib.take(2), primes.take(3))}"
  puts "Interleave:    #{seq.take(3).interleave(fib.take(2), primes.take(3))}"
  puts "Cartesian:     #{fib.take(2).cartesian(primes.take(2), [0]).map &:to_a}"
  puts "Cantor:        #{primes.cantor(primes, primes).take(5).map &:to_a}"
  puts "Distinct:      #{fib.interleave(primes).distinct.take(10)}"
  puts

  def permutations(degree)
    next_level = lambda { |perm|
      i = perm.index nil
      unless i.nil?
        Seq.range(1, degree).select { |n| not perm.include? n }.map { |to|
          perm[0..i-1] + [to] + perm[i+1..-1] }
      end
    }

    Seq.tree_walk([0] + Array.new(degree), next_level).map { |p|
      p[1..-1] }.select { |p| not p.include? nil }
  end

  puts "Permutations:  #{permutations 4}"
end
