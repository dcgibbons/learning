#include <iostream>
#include <cctype>

using namespace std;

int int2String(const char* s) {
  int n = 0;
  bool is_neg = false;

  if (*s == '-') {
    is_neg = true;
    s++;
  }

  while (*s != '\0') {
    if (!isdigit(*s)) {
      break;
    }

    if (n > 0) n *= 10;
    n += (*s) - '0';
    s++;
  }

  n *= (is_neg) ? -1 : 1;
  return n;
}

int main() {
  int n = int2String("363");
  cout << "n=" << n << endl;

  n = int2String("-365");
  cout << "n=" << n << endl;

  n = int2String("0");
  cout << "n=" << n << endl;

  n = int2String("1");
  cout << "n=" << n << endl;

  n = int2String("-1");
  cout << "n=" << n << endl;

  n = int2String("-56.32");
  cout << "n=" << n << endl;

  return 0;
}

