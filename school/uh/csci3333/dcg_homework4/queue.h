/*
 * PURPOSE:             Provide a template based Queue data structure
 * ALGORITHM:           This implementation uses a fixed-size array to provide
 *                      a circular queue. This algorithm is space and time
 *                      efficient with the drawback of the queue being a fixed
 *                      size.
 */

#ifndef QUEUE_H_DEFINED
#define QUEUE_H_DEFINED

#include <stdexcept>

template <class T> 
class Queue
{
  public:

    /**
     * PURPOSE:             Constructs a new Queue object.
     * PARAMETERS:          size - maximum size of the queue (optional)
     */
    Queue(int size = 10) : m_size(size), m_front(0), m_rear(0)
    {
      m_elements = new T[m_size];
    }

    /**
     * PURPOSE:             Destructs a queue object.
     */
    virtual ~Queue()
    {
      delete[] m_elements;
    }

    /**
     * PURPOSE:             Places a new object into the queue.
     * PARAMETERS:          object - the object that will be copied & added
     * ACTIONS:             The provided object will be added to the queue if
     *                      space is available, otherwise a logic_error
     *                      exception will be thrown
     * ALGORITHM:           if the queue is full then
     *                        throw
     *                      else
     *                        queue[rear_index] <- object
     *                        rear_index <- (rear_index + 1) % queue_size
     *                      endif                     
     */
    void enqueue(const T& object)
    {
      if (size() == m_size - 1)
      {
        throw new std::logic_error("queue is full");
      }
      else
      {
        m_elements[m_rear] = object;
        m_rear = (m_rear + 1) % m_size;
      }
    }

    /**
     * PURPOSE:             Removes any object from the head of the queue.
     * RETURNS:             the object at the head of the queue
     * ACTIONS:             an available object will be removed from the queue
     *                      if one is avaialble, otherwise a logic_error will be
     *                      thrown
     * ALGORITHM:           if the queue is empty then
     *                        throw
     *                      else
     *                        object <- queue[front_index]
     *                        front_index <- (front_index + 1) % queue_size
     *                      endif                     
     */
    const T& dequeue()
    {
      if (isEmpty())
      {
        throw new std::logic_error("queue is empty");
      }
      else
      {
        const T& element = m_elements[m_front];
        m_front = (m_front + 1) % m_size;
        return element;
      }
    }

    /**
     * PURPOSE:             Determines if the queue object is empty or not.
     * RETURNS:             true if the queue is empty, false otherwise
     */
    bool isEmpty()
    {
      return (m_front == m_rear);
    }

    /**
     * PURPOSE:             Determines the number of elements currently in the
     *                      queue.
     * RETURNS:             returns the current queue size
     */
    int size()
    {
      return (m_size - m_front + m_rear) % m_size;
    }

  private:
    const int m_size;
    int m_front;
    int m_rear;
    T* m_elements;
};


#endif
