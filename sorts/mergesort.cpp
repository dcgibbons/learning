#include <iostream>

using namespace std;

void sort(int* a, int n) {


}

int main() {
  int a[] = { 1, 9, 3, 8, 6, 7, 11, 99, 32 };
  int n = sizeof(a)/sizeof(a[0]);

  sort(a, n);

  for (int i = 0; i < n; i++) {
    cout << "a[" << i << "]=" << a[i] << endl;
  }

  return 0;
}

