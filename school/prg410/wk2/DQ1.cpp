#include <iostream>
#include <string>

using namespace std;

int main()
{
  cout << "while loop:" << endl;
  string s;
  while (cin >> s)
  {
    cout << "You entered: " << s << endl;
  }

  cout << endl << "do-while loop:" << endl;
  do
  {
    string s;
    cin >> s;
    cout << "You entered: " << s << endl;
  }
  while (cin);
}

