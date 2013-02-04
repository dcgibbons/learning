Programming Pearls - Exercise 1.7
=================================

Duplicate Numbers
-----------------

If a phone number appears more than once in the input file the current 
implementations will silently ignore the error and remove the duplicate in the 
output file. For a general-purpose problem, this is probably the desired 
result, but it might not be in the siutation given in the chapter.

Handling errors with duplicate numbers is easy. The function to set a bit could 
be modified like this:

```C
    static void store_bit(const int n, 
                          unsigned char* bits, 
                          const int offset, 
                          (void *)error_fn(const int n))
    {
        assert (n > 0 && n < MAX_VALUE);

        int byte = (n / CHAR_BIT) + offset;
        int bit = n % CHAR_BIT;
        if (bits[byte] & 1 << bit)
        {
            if (error_fn) error_fn(n);
        }
        else
        {
            bits[byte] |= 1 << bit;
        }
    }
```

Invalid Values
--------------

In my initial implementation, I perform an assertion check to ensure the value 
is > 0 and < MAX_VALUE. This aborts the program and prevent any invalid output 
data from being generate. This same check will also prevent non-numeric values 
from being processed.

If we wanted the program to be more resilent, it could either fail in a more 
graceful way (rather than SIGABRT), or call an error function that perhaps logs 
an exception but continues to process the remainder of the data.

Other Error Conditions
----------------------

The biggest risk of errors in these sets of exercises, besides invalid data, 
would be memory allocation and I/O errors. These errors could be handled 
gracefully and allow for a program exit without corrupting any data.

In particular, if this program is run using stdin and stdout as I've intended, 
then the shell environment should check if the program was succesful (exit code 
0) before it moves the new output file into place.
