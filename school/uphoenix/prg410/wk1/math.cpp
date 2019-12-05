#include <iostream>
#include <limits>
#include <cmath>

using namespace std;

int main()
{
  signed int x = numeric_limits<signed int>::max();
  cout << "x=" << x << endl;
  x++;
  cout << "x=" << x << endl;

  unsigned int y = numeric_limits<unsigned int>::min();
  cout << "y=" << y << endl;
  y--;
  cout << "y=" << y << endl;

  int z = -99;
  int a = z&7;
  cout << "z=" << z << "z%8=" << z%8 << "z&7=" << a<< endl;
}

