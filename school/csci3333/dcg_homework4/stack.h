/*
 * PURPOSE:             Provide a template based Stack data structure
 * ALGORITHM:           This implementation uses a fixed-size array to provide
 *                      a simple stack. This algorithm is space and time
 *                      efficient with the drawback of the stack being a fixed
 *                      size.
 */

#ifndef STACK_H_DEFINED
#define STACK_H_DEFINED

#include <stdexcept>

template <class T> 
class Stack
{
  public:
    /**
     * PURPOSE:             Constructs a new Stack object.
     * PARAMETERS:          size - maximum size of the stack (optional)
     */
    Stack(int size = 10) : m_size(size), m_top(-1)
    {
      m_elements = new T[m_size];
    }

    /**
     * PURPOSE:             Destructs a new Stack object.
     */
    virtual ~Stack()
    {
      delete[] m_elements;
    }

    /**
     * PURPOSE:             Pushes an object onto the top of the stack.
     * PARAMETERS:          object - the object that will be added to the stack
     * ACTION:              The provided object will be added to the stack if
     *                      there is room, otherwise a logic_error will be
     *                      thrown.
     * ALGORITHM:           if the stack is full then
     *                        throw
     *                      else
     *                        top_pos <- top_pos + 1
     *                        stack[top_pos] <- object
     *                      endif
     */
    void push(const T& object)
    {
      if (size() == m_size)
      {
        throw new std::logic_error("stack is full");
      }
      else
      {
        m_elements[++m_top] = object;
      }
    }

    /**
     * PURPOSE:             Pops the top object off the stack.
     * RETURNS:             the top object of the stack
     * ACTION:              The object at the top of the stack will be removed
     *                      and returned, unless the stack is empty in which
     *                      case a logic_error will be thrown.
     * ALGORITHM:           if the stack is empty then
     *                        throw
     *                      else
     *                        object = stack[top_pos]
     *                        top_pos <- top_pos - 1
     *                        return object
     *                      end if
     */
    const T& pop()
    {
      if (isEmpty())
      {
        throw new std::logic_error("stack is empty");
      }
      else
      {
        const T& object = m_elements[m_top--];
        return object;
      }
    }

    /**
     * PURPOSE:             Determines the current number of objects on the
     *                      stack.
     * RETURNS:             the number of objects on the stack
     */
    const int size() const
    {
      return (m_top + 1);
    }

    /**
     * PURPOSE:             Determines if the stack is empty.
     * RETURNS:             true if the stack is empty, false otherwise
     */
    const bool isEmpty() const
    {
      return (m_top < 0);
    }

  private:
    const int m_size;
    int m_top;
    T* m_elements;

};

#endif
