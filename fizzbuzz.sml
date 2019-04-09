fun fizzbuzz n =
    if n mod 15 = 0 then print "FizzBuzz\n" else
    if n mod 3 = 0 then print "Fizz\n" else
    if n mod 5 = 0 then print "Buzz\n" else
    print (concat [(Int.toString n), "\n"])

fun main() = map fizzbuzz (List.tabulate (100, (fn x => x * 1)))
