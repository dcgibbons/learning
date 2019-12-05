#include <string>
using namespace std;

class myClass
{
	int myInteger; //this variable would be private by default

public:
float myFloat; //this variable would be publicly accessible

protected:
string myString; //this variable would be accessible by friend’s of this class
};
