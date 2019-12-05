#ifndef VLIWENCODER_H
#define VLIWENCODER_H

#include <string>
#include <algorithm>
#include <vector>

using namespace std;

typedef enum _Op_type {
    None = -1,
    NOP,
    ACTION
} Op_type;

Op_type get_OpType(string &Syllable);

#endif // VLIWENCODER_H

