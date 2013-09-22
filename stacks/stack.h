template<class T> class stack {
  public:
    stack();
    virtual ~stack();

    virtual bool peek(T& data);
    virtual bool pop(T& data);
    virtual bool push(const T& data);

  protected:
    struct list_node {
      struct list_node* next;
      T data;
    };
};
