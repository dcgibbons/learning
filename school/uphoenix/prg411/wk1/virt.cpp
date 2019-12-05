#include <iostream>

using namespace std;

class A
{
  public:
    virtual ~A() {}
    virtual const char* getName() { return "class A"; }
    double getSalary() { return 1000.75; }
};

class B : public A
{
  public:
    virtual ~B() {}
    virtual const char* getName() { return "class B"; }
    double getSalary() { return 9999.55; }
};

int main()
{
  A* a = new A();
  A* b = new B();
  B* c = new B(); 
  cout << "a name=" << a->getName() << " salary=" << a->getSalary() << endl;
  cout << "b name=" << b->getName() << " salary=" << b->getSalary() << endl;
  cout << "c name=" << c->getName() << " salary=" << c->getSalary() << endl;

  return 0;
}

