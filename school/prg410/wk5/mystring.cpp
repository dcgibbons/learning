#include <iostream>

class MyString {
  public:
    MyString(const char* s) {
      m_str = strdup(s);
      m_len = strlen(s);
    }

    friend std::ostream& operator <<(std::ostream& out, MyString& str) {
      out << "MyString("
          << "m_str=\"" << str.m_str << "\","
          << "m_len=" << str.m_len << ")";
    }

  private:
    char* m_str;
    size_t m_len;
};
  

int main() {
  MyString mystr("hello, world!");
  std::cout << mystr << std::endl;
}
