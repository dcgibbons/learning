#include <iostream>
#include <string>

using namespace std;

int main()
{
  bool timeToExit = false;
  while (!timeToExit)
  {
    string input;
    cin >> input;
    if (input.substr(0, 4) == "quit")
    {
      timeToExit = true;
    }
    else
    {
      cout << "You entered: " << input << endl;
    }
  }

  for (;;)
  {
    string input;
    cin >> input;
    if (input.substr(0, 4) == "quit")
    {
      break;
    }
    else
    {
      cout << "You entered: " << input << endl;
    }
  }
}
