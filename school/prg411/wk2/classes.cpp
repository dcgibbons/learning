#include <string>
#include <list>

using namespace std;

class Employee {
  public:
    virtual int getEmpNo() = 0;
    virtual string getName() = 0;
};

class Supervisor : public Employee {
  public:
    virtual list<Employee*> getDirectReports() = 0;
};

class EmployeeImpl : public Employee {
  public:
    EmployeeImpl(int empNo, const string& lastName, const string& firstName) 
      : m_empNo(empNo), m_lastName(lastName), m_firstName(firstName) {
      m_fullName = m_lastName + ", " + m_firstName;
    }
    virtual int getEmpNo() { return m_empNo; }
    virtual string getName() { return m_fullName; }

  private:
    int m_empNo;
    string m_lastName;
    string m_firstName;
    string m_fullName;
};

class SupervisorImpl : public EmployeeImpl, Supervisor {
  public:
    SupervisorImpl(int empNo, const string& lastName, const string& firstName, list<Employee*> directReports) 
      : EmployeeImpl(empNo, lastName, firstName) {
      m_directReports = directReports;
    }

  private:
    list<Employee*> m_directReports;
};

int main() {
  Employee* rob = new EmployeeImpl(100, "Bach", "Robert");
  Employee* kevin = new EmployeeImpl(101, "Johnson", "Kevin");
  Employee* jeffry = new EmployeeImpl(102, "Raikes", "Jeffry");

  list<Employee*> reports;
  reports.push_front(rob);
  reports.push_front(kevin);
  reports.push_front(jeffry);
  Supervisor* ceo = new SupervisorImpl(1, "Ballmer", "Steven", reports);
};

