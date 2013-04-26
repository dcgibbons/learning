#include <iostream>
#include <string>

using namespace std;

class Person {
  public:
    virtual ~Person() {}

    virtual void setLastName(const string& lastName) { m_lastName = lastName; }
    virtual const string getLastName() { return m_lastName; }

    virtual void setFirstName(const string& firstName) { m_firstName = firstName; }
    virtual const string getFirstName() { return m_firstName; }

  private:
    string m_lastName;
    string m_firstName;
};

class PersonBuilder {
  public:
    virtual Person* build(const string& firstName, const string& lastName) {
      Person* p = new Person();;
      p->setFirstName(firstName);
      p->setLastName(lastName);
      return p;
    }

    virtual Person* build(const string& name) {
      Person* p = new Person();
      string::size_type n = name.find_first_of(',');
      if (n != string::npos) {
        p->setLastName(name.substr(0, n));
        p->setFirstName(name.substr(n + 1));
      }
      return p;
    }
};

int main() {
  PersonBuilder builder;

  Person* p1 = builder.build("Robert", "Smith");
  Person* p2 = builder.build("Robert, Smith");

  cout << "p1: " << p1->getLastName() << ", " << p1->getFirstName() << endl;
  cout << "p2: " << p2->getLastName() << ", " << p2->getFirstName() << endl;

  delete p1;
  delete p2;
}

