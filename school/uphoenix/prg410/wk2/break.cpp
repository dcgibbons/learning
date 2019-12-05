#include <iostream>

using namespace std;

int main()
{
  int x = 0;

  while (true)
  {
    x++;

    if (x == 1)
    {
      cout << "hello!" << endl;
    }
    else if (x == 4)
    {
      break;
    }

    cout << "x=" << x << endl;
  }

  cout << "goodbye!" << endl;
}

