#include <iostream>

using namespace std;

uint32_t count_bits(uint32_t n) {
  uint32_t nbits = 0;
  for (unsigned bit = 0; bit < 32; bit++) {
    nbits += (n & (1 << bit)) != 0 ? 1 : 0;
  }
  return nbits;
}

uint32_t count_bits_faster(uint32_t n) {
  uint32_t nbits = 0;
  while (n > 0) {
    nbits += (n & 1);
    n >>= 1;
  }
  return nbits;
}

uint32_t count_bits_fastest(uint32_t n) {
  uint32_t nbits = 0;
  while (n > 0) {
    n = n & (n - 1);
    nbits++;
  }
  return nbits;
}

int main() {
  uint32_t n = 0xfeedbeef;
  count_bits(n);
  count_bits_faster(n);
  count_bits_fastest(n);
  return 0;
}
