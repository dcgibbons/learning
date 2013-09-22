#include <iostream>
#include <cstring>

using namespace std;

void combine(const char* str, const size_t len, int start_pos, int end_pos, char* out) {
  for (size_t i = start_pos; i < len; i++) {
    out[end_pos++] = str[i];
    out[end_pos] = '\0';
    cout << out << endl;

    if (i < len) {
      combine(str, len, i + 1, end_pos, out);
    }

    end_pos--;
  }
}

int main() {
  //const char* str = "wxyz";
  const char* str = "AAABB";
  const size_t len = strlen(str);

  char* out = new char[len + 1];

  cout << "str=" << str << " len=" << len << endl;

  combine(str, len, 0, 0, out);

  return 0;
}
