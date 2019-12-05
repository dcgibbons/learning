#include <iostream>

using namespace std;

int main()
{
  int* a = new int[10000];
  ptrdiff_t mydiff = &a[50] - &a[25];

  int*b = new int[500];
  ptrdiff_t mydiff2 = &b[0] - &a[9999];

  cout << "mydiff=" << mydiff << " mydiff2=" << mydiff2 << endl;
}

