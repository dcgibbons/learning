#include <iostream>
#include <string>

using namespace std;

class Fruit {
  public:
    virtual ~Fruit() { cout << "Destructing a fruit of color " << m_color << endl; }
  protected:
    Fruit(const string& color) : m_color(color) {}
    string m_color;
};

class Apple : public Fruit {
  public:
    Apple() : Fruit("red") {}
    virtual ~Apple() { cout << "Destructing an Apple" << endl; }
};

class Banana : public Fruit {
  public:
    Banana() : Fruit("yellow") {}
    virtual ~Banana() { cout << "Destructing a Banana" << endl; }
};

int main() {
  Fruit* fruit1 = new Apple();
  Fruit* fruit2 = new Banana();

  delete fruit1;
  delete fruit2;
};

