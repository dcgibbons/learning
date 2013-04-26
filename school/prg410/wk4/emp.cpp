#include <string>

using namespace std;

class Employee1 {
  void setName(string& lastName, string& firstName) {
    this->lastName = lastName;
    this->firstName = firstName;
  }

private:
  string lastName;
  string firstName;
};

class Employee2 {
  void setName(string& lastName, string& firstName) {
    m_lastName = lastName;
    m_firstName = firstName;
  }

private:
  string m_lastName;
  string m_firstName;
};

