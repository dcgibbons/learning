Programming Pearls - Exericse 1.8
=================================

Dealing with multiple toll-free area codes (as of today, 800, 888, 877, 866, 
and 855) could be done a few ways. The most straight-forward to me would be to 
store the sorted list of numbers in a separate file per area code.

This technique would still allow all of the files to be sorted in 1 or 2 passes 
(depending upon available core). Since the scope of all the numbers are unique 
within an area code, using the area code as a file separate provides an 
immediate partitioning of the dataset into a naturally searchable set.

Searching for the matching number very quickly would require these sorted files 
and then a binary search being performed on the file. Given that our records 
are fixed-length, we can easily compute record positions in a disk file without 
having to read the entire file into memory. A binary search of each sorted file 
would be the typical O(log(n)) that every binary search is, with a very small 
number of actual I/O's performed.
