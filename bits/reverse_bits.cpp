#include <iostream>

using namespace std;

int main() 
{
  unsigned n = 0xF0000000;
  unsigned t = 0;
  for (int bit = sizeof(n) * 8 - 1; bit > 0; bit--)
  {
    t |= n & 1;
    t <<= 1;
    n >>= 1;
  }
  t |= n & 1;


  return 0;
}

