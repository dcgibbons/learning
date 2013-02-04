Programming Pearls - Exercise 1.6
=================================

If each phone number appeared more than once in the input file, up to a maximum 
of 10 times, we would need to allocate multiple bits for each number. To 
represent 10, we'd need 4 bits (from the formula ceil(log2(10))) per number to 
represent the count.

Given that we are still limited by our heap size (either 1MB or 1.25MB - it 
makes no difference given 4 * 10,000,000 will exceed this size) we have to 
determine how many numbers we can store in our bit vector at one time. The math 
for this would be:

    ((1024 * 1024 / sizeof(unsigned char)) * CHAR_BITS) / 4

This results in a value of 2,097,152 on most platforms that have 8-bit wide 
unsigned char types. This limitation means we will require 5 total passes 
through the file.

The order this solution would still be O(n) (more specifically, O(5n)), but 
total run-time would likely not be 5x that of the exercise 1.3 solution, 
depending upon the speed of the storage and how much core is available for file 
system cache. That said, if 1MB of heap is truly a limitation, then chances are 
file system caches aren't plentiful, either.

You would also want to ask the business case of why you'd need up to 10 copies 
of the same number in the output file, rather than just a single copy.  
Duplicates do not actually matter in our current exercise 1.3 and 1.5 
implementations.

Using More Core
---------------

If we are using a system with at least 5MB of core available, then we can 
return to a single pass algorithm using a bit vector, with 4 bits per value.

If we are on a system with at least 10MB of core available, then we can use a 
full byte (on systems with 8-bit characters) with room remaining. We could then 
return to a single pass through the file without any bit manipulation, 
resulting in a very fast run-time, similar to exercise 1.3's solution.
