#include <iostream>
#include <string>

using namespace std;

template <class T> class stack {
  public:
    stack() : stack_list(0) {
      cout << "constructing a stack" << endl;
    }

    virtual ~stack() {
      cout << "destructing a stack" << endl;
      struct list_node* p;
      while (stack_list) {
        p = stack_list;
        stack_list = stack_list->next;
        cout << "deleting " << p->data << endl;
        delete p;
      }
    }

    virtual bool peek(T& data) {
      bool success = false;
      if (stack_list) {
        data = stack_list->data;
        success = true;
      }
      return success;
    }

    virtual bool pop(T& data) {
      bool success = false;
      if (stack_list) {
        struct list_node* old_head = stack_list;
        data = old_head->data;
        stack_list = old_head->next;
        delete old_head;
        success = true;
      }
      return success;
    }

    virtual bool push(const T& data) {
      struct list_node* new_head = new struct list_node;
      new_head->data = data;
      new_head->next = stack_list;
      stack_list = new_head;
      return true;
    }


  private:
    struct list_node {
      struct list_node* next;
      T data;
    }* stack_list;
};

int main() {
  stack<string>* s = new stack<string>();;
  s->push(string("hello"));
  s->push(string("world"));

  string str;
  while (s->pop(str)) {
    cout << "popped: " << str << endl;
  }
  delete s;

  stack<string>* p = new stack<string>();
  p->push("foo");
  p->push("bar");
  p->push("fred");
  delete p;

  sleep(100);

  return 0;
}
