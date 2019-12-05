#include <iostream>
#include <string>

using namespace std;

class Person {
  public:
    Person(const string& name) : m_name(name) {}
    string getName() const { return m_name; }
  private:
    string m_name;
};

class Student : private Person {
  public:
    Student(const string& name) : Person(name) {}
    string getStudentName() const { return getName(); }
};

void dance(const Person& p) {
  cout << p.getName() << " is now dancing" << endl;
}

void study(const Student& s) {
  cout << s.getStudentName() << " is now studying" << endl;
}

int main() {
  Person bob("Bob");
  Student fred("Fred");
  Student veronica("Veronica");

  dance(bob);

  study(fred);
  study(veronica);
}

