This file demonstrates how to guarantee correctness of input via the use of
module interfaces to construct abstract data types. Only true natural numbers
can be created with the interface to the "Natural" module. An attempt to create
a negative natural number will yield a Nothing value. The resultant Maybe types
can be handled expressively and correctly once the result of construction is
generated.

There is some debate over what a Natural number is...
  See http://en.wikipedia.org/wiki/Natural_number

These discussions are irrelevant to the demonstration, as it only deals with
validating input for all occurrences of value creation.

It is worth noting that the program will crash if you supply a non-integer.
This is expected.
