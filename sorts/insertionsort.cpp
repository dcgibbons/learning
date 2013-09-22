#include <iostream>

using namespace std;

void sort(int* a, int n) {
  for (int j = 1; j < n; j++) {
    int k = a[j];
    // insert key into the sorted sequence A[1..j - 1];
    int i = j - 1;
    while (i >= 0 && a[i] > k) {
      a[i + 1] = a[i];
      i--;
    }
    a[i + 1] = k;
  }
}

int main() {
  int a[] = { 5, 2, 4, 6, 1, 3 };
  const int n = sizeof(a)/sizeof(a[0]);

  sort(a, n);
  for (int i = 0; i < n; i++) {
    cout << "a[" << i << "]=" << a[i] << endl;
  }

  return 0;
}

