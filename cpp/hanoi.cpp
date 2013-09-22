#include <iostream>
#include <cmath>

using namespace std;

const int HEIGHT = 3;
int discs[HEIGHT];
int moves;

void shift(const int disc, const int peg) {
  cout << "moving disc " << disc << " from peg " << discs[disc] << " to peg " << peg << endl;
  discs[disc] = peg;
  moves++;
}

void hanoi(const int disc, const int to_peg) {
  cout << "hanoi disc " << disc << " to peg " << to_peg << endl;

  // don't continue past the bottom disc!
  if (disc > 0) {
    const int from_peg = discs[disc - 1];
    const int dest_peg = 3 - from_peg - to_peg;

    hanoi(disc - 1, dest_peg);
    shift(disc - 1, to_peg);
    hanoi(disc - 1, to_peg);
  }
}

int main() {
  int n = HEIGHT;

  cout << "Moving tower of hanoi, where N = " << n << endl;

  hanoi(n, 1);

  cout << "Total moves: " << moves << endl;
  cout << "Predicted moves: " << (pow(2.0, n) - 1.0) << endl;

  for (int i = 0; i < n; i++) {
    assert(discs[i] == 1);
  }

  return 0;
}
