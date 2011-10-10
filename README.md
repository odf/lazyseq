Fun with lazy sequences in Ruby and Python.

Defines a class `Seq` which allows on to do things like this:

    fib = Seq.new(0, 1) { fib.rest + fib }
    puts "Fibonacci numbers: #{fib.take 10} ..."

    primes = Seq.from(2).select do |n|
      n < 4 or primes.take_while { |m| m * m <= n }.forall { |m| n % m > 0}
    end
    puts "Prime numbers: #{primes.take 10} ..."

or in Python:

    fib = Seq(0, 1, lambda : fib.rest + fib)
    print "Fibonacci numbers: ", fib.take(10), "..."

    primes = Seq.up_from(2).select(
        lambda n: n < 4 or (primes.take_while(lambda m: m * m <= n).
                            forall(lambda m: n % m)))
    print "Prime numbers:", primes.take(10), "..."
