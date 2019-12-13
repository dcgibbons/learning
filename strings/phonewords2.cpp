#include <cassert>
#include <cstring>
#include <iostream>

using namespace std;

void permute_phone(const char* phone_num);
char getCharKey(int telephoneKey, int place);

int main() {
  const char* phone_num = "4971927";
  size_t length = strlen(phone_num);
  char* buffer = new char[length + 1];

  permute_phone(phone_num);

  delete[] buffer;
  return 0;
}

void permute_phone(const char* phone_num) {
  const size_t length = strlen(phone_num);
  char* outbuf = new char[length + 1];

  // seed the phone number by starting with the baseline
  for (size_t digit = 0; digit < length; digit++) {
    outbuf[digit] = getCharKey(phone_num[digit] - '0', 0);
  }
  outbuf[length] = '\0';

  // loop until we've reset the first digit
  do {
    cout << outbuf << endl;

    int digit;
    for (digit = length - 1; digit >= 0; digit--) {

      int n = phone_num[digit] - '0';

      if (n == 0 || n == 1 || getCharKey(n, 2) == outbuf[digit]) {
        outbuf[digit] = getCharKey(n, 1);
        continue;
      } else if (getCharKey(n, 0) == outbuf[digit]) {
        outbuf[digit] = getCharKey(n, 1);
        break;
      } else if (getCharKey(n, 1) == outbuf[digit]) {
        outbuf[digit] = getCharKey(n, 2);
        break;
      }
    }

    if (digit == -1) {
      break;
    }

  } while (true);

  delete[] outbuf;
}

char getCharKey(int telephone_key, int place) {
  assert(telephone_key >= 0 && telephone_key <= 9);
  assert(place >= 0 && place <= 2);

  static char keypad[][3] = {
    { '0', '0', '0' }, // 0
    { '1', '1', '1' }, // 1
    { 'A', 'B', 'C' }, // 2
    { 'D', 'E', 'F' }, // 3
    { 'G', 'H', 'I' }, // 4
    { 'J', 'K', 'L' }, // 5
    { 'M', 'N', 'O' }, // 6
    { 'P', 'R', 'S' }, // 7
    { 'T', 'U', 'V' }, // 8
    { 'W', 'X', 'Y' }  // 9
  };

  return keypad[telephone_key][place];
}

