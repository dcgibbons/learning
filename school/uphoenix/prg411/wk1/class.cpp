#include <iostream>
#include <string>

using namespace std;

class Feline {
  public:
    Feline(const string& name) { myname = name; }
    string& getName() { return myname; }
  private:
    string myname;
};

int main() {
  string catName = "felix";
  Feline f(catName);
  cout << "f.name=" << f.getName() << endl;
}

