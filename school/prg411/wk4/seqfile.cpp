#include <fstream>
#include <iostream>

using namespace std;

int main()
{
  unsigned long sum = 0;

  ifstream inputFile;
  inputFile.open("myfile.txt", ios::in);
  if (!inputFile)
  {
    cerr << "unable to open file" << endl;
  }
  else
  {
    while (!inputFile.eof())
    {
      unsigned char byte;
      inputFile >> byte;
      sum += byte;
    }
    inputFile.close();

    cout << "total sum of all bytes in file: " << sum << endl;
  }

}

