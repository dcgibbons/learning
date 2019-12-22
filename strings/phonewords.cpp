#include <cassert>
#include <cstring>
#include <iostream>

using namespace std;

void permute_phone(const char* phone_num, char* outbuf, const size_t length, size_t digit);
char getCharKey(int telephoneKey, int place);

const int PHONE_LENGTH = 7;

int main() {
  const char* phone_num = "4971927";
  size_t length = strlen(phone_num);
  char* buffer = new char[length + 1];

  permute_phone(phone_num, buffer, length, 0);

  delete[] buffer;
  return 0;
}

void permute_phone(const char* phone_num, char* outbuf, const size_t length, size_t digit) {
  if (digit == length) {
    outbuf[length] = '\0';
    cout << outbuf << endl;
  } else {
    for (int place = 0; place < 3; place++) {
      int n = phone_num[digit] - '0';
      outbuf[digit] = getCharKey(n, place);
      permute_phone(phone_num, outbuf, length, digit + 1);
      if (n == 0 || n == 1) {
        break;
      }
    }
  }
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

