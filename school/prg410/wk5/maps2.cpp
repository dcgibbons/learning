#include <iostream>
#include <map>

using namespace std;

int main() {
  map<string, string> dictionary;

  dictionary["abash"] = "cause to feel embarassed";
  dictionary["abate"] = "become less intense or widespread";
  dictionary["abbatoir"] = "a slaughterhouse";
  dictionary["abaxial"] = "facing away from the stem of a plant";
  // etc...

  string word = "abate";
  cout << "Definition for " << word
       << " is : " << dictionary[word]
       << endl;
}

