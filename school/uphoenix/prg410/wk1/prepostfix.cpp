#include <iostream>

using namespace std;

int main()
{
  int i = 5;
  if (i++ == 6) cout << "it's six!" << endl;
  cout << "i=" << i << endl;
  return 0;
}

