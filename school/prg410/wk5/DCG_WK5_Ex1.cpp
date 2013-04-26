//
// DCG_WK5_Ex1.cpp
// Week 5, Exercise 1
// David C. Gibbons
// PRG/410 - C++ Programming I
// Chip Dickson
// October 11, 2007
//
// Implement a simple string class in native C++ that holds a char* and an 
// integer length as private data members. Provide a constructor which takes
// an argument of type const char*, and implement the copy constructor, 
// assignment operator and destructor functions. Verify that your class works. 
// You will find it easiest to use the string functions from the <cstring> header
// file. Create a "main" to test your class. All methods should be exercised 
// (including the copy constructor and destructor (never called directly)).
//

#include <cstring>
#include <iostream>
#include <ostream>

using namespace std;

// declare our simple string class
class mystring
{
  public:
    // default constructor - an empty string
    mystring();

    // constructor from a C string
    mystring(const char* const s);

    // copy constructor
    mystring(const mystring& s);

    // destructor, virtual to allow proper delete behavior when subclassed
    virtual ~mystring();

    // assignment operators
    mystring& operator =(const char* const s);
    mystring& operator =(const mystring& s);

    // ostream output operator, friend by convention
    friend ostream& operator <<(ostream& out, const mystring& s);

  private:
    void copycstr(const char* cstr, size_t n);

    size_t m_len; // length of current C string 
    char* m_str;  // a pointer to the C string wrapped
};

mystring::mystring()
{
  // by default construct an empty, but valid, string
  copycstr("", 0);
}

mystring::mystring(const char* const s)
{
  copycstr(s, strlen(s));
}

mystring::mystring(const mystring& s)
{
  copycstr(s.m_str, s.m_len);
}

mystring::~mystring()
{
  // delete the C string
  delete[] m_str;
}

// assign this string directly from a C string (saves an extra construction)
mystring& mystring::operator =(const char* const s)
{
  // save our current m_str pointer so it can be freed
  const char* const old_cstr = m_str;

  // attempt to allocate & copy the other C string
  copycstr(s, strlen(s));

  // free our current C string memory
  delete[] old_cstr;

  return *this;
}

// assign this string from another 
mystring& mystring::operator =(const mystring& s)
{
  // prevent self-assignment from being harmful
  if (this != &s)
  {
    // save our current m_str pointer so it can be freed
    const char* const old_cstr = m_str;

    // attempt to allocate & copy the other C string
    copycstr(s.m_str, s.m_len);

    // free our current C string memory
    delete[] old_cstr;
  }

  return *this;
}

// helper method to allocate and a copy a C string to our internal state
void mystring::copycstr(const char* const cstr, size_t n)
{
  // create a copy of the other string's internal C string
  // (used instead of strcpy since we already know the length)
  char* new_cstr = new char[n + 1]; // might throw an exception!
  memcpy(new_cstr, cstr, n);
  new_cstr[n] = '\0';

  // update our internal state to point to the new C string
  m_len = n;
  m_str = new_cstr;
}

// helper method to dump our string's state to the provided ostream
ostream& operator <<(ostream& out, const mystring& s)
{
  return out << "mystring(m_len=" << s.m_len << ","
             << "m_str=\"" << s.m_str << "\","
             << "m_str(ptr)=" << static_cast<void*>(s.m_str)
             << ")";
}

// main entry point to help unit test class
int main()
{
  // verify that we can create a new mystring from a C string
  mystring s("hello, world!");
  cout << "s=" << s << endl;

  // verify that we can create a new mystring from another mystring
  mystring t = s;
  cout << "t=" << t << endl;

  // verify that the assignment operator works in a variety of ways
  mystring u, v, w, x;
  u = "football!";
  v = "rugby!";
  w = u;
  x = t;

  cout << "u=" << u << endl
       << "v=" << v << endl
       << "w=" << w << endl
       << "x=" << x << endl;

  // verify that we can new and delete a mystring
  mystring* y = new mystring("that's all, folks!");
  cout << "y=" << y << " *y=" << *y << endl;
  delete y;

  return 0;
}

