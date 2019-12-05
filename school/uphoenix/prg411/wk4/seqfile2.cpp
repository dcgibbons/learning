#include <fstream>
#include <iostream>

using namespace std;

int main(int argc, char** argv)
{
  ifstream inputFile;
  inputFile.open(argv[1], ios::in);
  if (!inputFile)
  {
    cerr << "unable to open file" << endl;
  }
  else
  {
    //s = sum of all bytes;
    //r = s % 2^16 + (s % 2^32) / 2^16;
    //cksum = (r % 2^16) + r / 2^16;

    unsigned long sum = 0;

    while (!inputFile.eof())
    {
      unsigned char byte;
      inputFile >> byte;
      sum += byte;
    }
    inputFile.close();

    unsigned long r = sum % 2^16 + (sum % 2^32) / 2^16;
    unsigned long cksum = (r % 2^16 ) + r / 2^16;
    cout << "total sum of all bytes in file: " << sum << endl;
    cout << "r=" << r << endl;
    cout << "cksum=" << cksum << endl;
  }

}

