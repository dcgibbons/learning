#include <iostream>
#include <cstring>

using namespace std;

void combine(const char* str, const size_t len, int start_pos, int end_pos, char* out) {
  for (size_t i = start_pos; i < len - 1; i++) {
    out[end_pos++] = str[i];
    out[end_pos] = '\0';
    cout << out << endl;

    combine(str, len, i + 1, end_pos, out);

    end_pos--;
  }

  // do final loop iteration
  out[end_pos] = str[end_pos];
  out[end_pos + 1] = '\0';
  cout << out << endl;
}

int main() {
  const char* str = "wxyz";
  const size_t len = strlen(str);
  char* out = new char[len + 1];
  cout << "str=" << str << " len=" << len << endl;
  combine(str, len, 0, 0, out);
  delete[] out;
  return 0;
}
