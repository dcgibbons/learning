
#include <string>
#include <iostream>

using namespace std;

int main()
{
  string lineBuffer;
  getline(cin, lineBuffer);

  size_t n = lineBuffer.find_first_not_of("0123456789.", 0);
  if (n != string::npos)
  {
    cout << "invalid numeric characters entered" << endl;
  }
}
