#include <iostream>
#include <string>

using namespace std;

class Fruit {
  public:
    virtual string& getColor() { return m_color; }
  protected:
    Fruit(const string& color) : m_color(color) {}
    string m_color;
};

class Apple : public Fruit {
  public:
    Apple() : Fruit("red") {}
};

class Banana : public Fruit {
  public:
    Banana() : Fruit("yellow") {}
};

int main() {
  Fruit fruit1 = Apple();
  Fruit fruit2 = Banana();

  cout << "fruit1.color=" << fruit1.getColor() << endl;
  cout << "fruit2.color=" << fruit2.getColor() << endl;
};

