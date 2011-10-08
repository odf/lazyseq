Fun with lazy sequences in Ruby.

Defines a class `Seq` with which one can do things like this:

    primes = Seq.from(2).select do |n|
      n < 4 or primes.take_while { |m| m * m <= n }.forall { |m| n % m > 0}
    end
    puts "Prime numbers: #{primes.take(10)}"
