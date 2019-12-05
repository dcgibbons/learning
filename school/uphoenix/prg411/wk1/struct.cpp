#include <iostream>

using namespace std;

struct person
{
  person() { name = "bob"; }
  string& getName() { return name; }
  string name;
};

class person2 : public person {
  public:
  person2() { name = "fred"; }
};

int main() {
  person p = person();
  cout << "p.name=" << p.getName() << endl;

  person2 p2 = person2();
  cout << "p2.name=" << p2.getName() << endl;
}
