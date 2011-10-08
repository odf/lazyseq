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


def bounce(val)
  while val.is_a? Proc
    val = val.call
  end
  val
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
      @rest  = lambda { elems[1..-2].to_seq.before Seq.new(elems[-1], &rest) }
    end
  end

  def rest
    @rest = @rest.call()
    class << self; def rest; @rest end end
    freeze
    @rest
  end

  def forced
    step = lambda { |seq| lambda { step.call seq.rest } if seq }
    bounce step.call(self.rest)
    self
  end

  def self.from_array(array, i = 0)
    Seq.new(array[i]) { from_array array, i+1 } if i < array.length
  end

  def self.from(start)
    Seq.new(start) { from start.next }
  end

  def self.range(start, limit)
    Seq.from(start).take_while { |x| x <= limit }
  end

  def self.constant(val)
    Seq.new(val) { constant val }
  end

  def each(&fun)
    step = lambda { |seq|
      if seq
        fun.call seq.first
        lambda { step.call seq.rest }
      end
    }
    bounce step.call(self)
  end

  def to_seq
    self
  end

  def to_a
    array = []
    each { |x| array.push x }
    array
  end

  def to_hash
    hash = {}
    each { |k, v| hash[k] = v }
    hash
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

  def select(&pred)
    if pred.call first
      Seq.new(first) { rest.select &pred }
    elsif rest
      rest.drop_until(&pred).select &pred
    end
  end

  def find(&pred)
    if good = drop_until(&pred) then good.first end
  end

  def forall(&pred)
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
    rest.reduce first, &op
  end

  def min
    fold { |a, b| b < a ? b : a }
  end

  def max
    fold { |a, b| b > a ? b : a }
  end

  def zip_seq
    firsts = map { |s| s.first if s }
    if firsts.find { |x| not x.nil? }
      Seq.new(firsts) { map { |s| s.rest if s }.zip_seq }
    end
  end

  def zip(*others)
    tail = others.to_seq.map { |s| s.to_seq } if others.length > 0
    Seq.new(self) { tail }.zip_seq
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
    zip(*others).forall do |seq|
      if seq and seq.rest
        x = seq.first
        seq.rest.forall { |y| y == x }
      else
        true
      end
    end
  end

  def before(seq)
    Seq.new(first) { if rest then rest.before seq else seq end }
  end

  def concat_seq
    if rest then first.before rest.concat_seq else first end
  end

  def concat(*others)
    tail = others.to_seq.map { |s| s.to_seq } if others.length > 0
    Seq.new(self) { tail }.concat_seq
  end

  def subseqs
    Seq.new(self) { rest.subseqs if rest }
  end

  def consec(n)
    subseqs.map { |s| s.take(n).to_a }
  end

  private

  def cycle_from(seq)
    if seq then Seq.new(seq.first) { cycle_from(seq.rest) } else cycle end
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
  puts "All 3 letters: #{seq.forall { |x| x.length == 3 }}"
  puts "Reverse:       #{seq.reverse}"
  puts "Min and max:   #{seq.min}, #{seq.max}"
  puts "Zip with ints: #{seq.zip('abcdefg'.chars).map(&:to_a).drop 3}"
  puts
  puts "Number range:  #{Seq.range 10, 20}"
  puts "Its sum:       #{Seq.range(10, 20).sum}"
  puts "Its product:   #{Seq.range(10, 20).product}"
  puts "String range:  #{Seq.range "ady", "aeg"}"
  puts
  fib = Seq.new(0, 1) { fib.rest + fib }
  puts "Fibonacci:     #{fib.take 12}"
  puts "Compare:       #{fib.take(10) == [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]}"
  puts "Compare:       #{fib.take(10) == [0, 1, 1, 2, 3, 5, 8.2, 13, 21, 34]}"
  puts
  primes = Seq.from(2).select do |n|
    n < 4 or primes.take_while { |m| m * m <= n }.forall { |m| n % m > 0 }
  end
  puts "Prime numbers: #{primes.take(10)}"
  puts
  puts "Concatenation: #{seq.take(3).concat(fib.take(2), primes.take(3))}"
  puts
  puts "No first:      #{Seq.new() { seq }}"
  puts "One first:     #{Seq.new(1) { seq }}"
  puts "Two first:     #{Seq.new(1, 2) { seq }}"
  puts "Three firsts:  #{Seq.new(1, 2, 3) { seq }}"
end
