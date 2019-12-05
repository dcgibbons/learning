#include <iostream>
#include <string>

using namespace std;

class Movable {
  public:
    enum DIRECTION { NORTH, EAST, WEST, SOUTH };
    virtual bool move(DIRECTION dir) = 0; 
    virtual ~Movable() {}
};

class Touchable {
  public:
    virtual string getTexture() = 0;
    virtual ~Touchable() {}
};

class Visible {
  public:
    virtual string getColor() = 0;
    virtual ~Visible() {}
};

class Tank : public Movable, public Touchable, public Visible {
  public:
    virtual bool move(DIRECTION dir) { return false; }
    virtual string getTexture() { return "metallic"; }
    virtual string getColor() { return "camouflage"; }
};

void move(Movable& m) { m.move(Movable::NORTH); }
void touch(Touchable& t) { cout << "It feels " << t.getTexture() << endl; }
void look(Visible& v) { cout << "It looks " << v.getColor() << endl; }

int main() {
  Tank mytank;
  move(mytank);
  touch(mytank);
  look(mytank);

  Movable& m = mytank;
}


