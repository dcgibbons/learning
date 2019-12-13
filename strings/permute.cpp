#include <iostream>

using namespace std;

void permute(const char* instr, char* outstr, bool* used, int len, int level);

int main() {
  const char* const str = "abcd";
  int len = strlen(str);
  char* out = new char[len + 1];
  bool* used = new bool[len]; 

  for (int i = 0; i < len; i++) {
    used[i] = false;
  }

  permute(str, out, used, len, 0);

  return 0;
}

void permute(const char* instr, char* outstr, bool* used, int len, int level) {
  if (level == len) {
    outstr[len] = '\0';
  } else {
    for (int i = 0; i < len; i++) {
      if (!used[i]) {
        outstr[level] = instr[i];
        used[i] = true;
        permute(instr, outstr, used, len, level + 1);
        used[i] = false;
      }
    }
  }
}

