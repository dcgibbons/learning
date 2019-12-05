/*
 * PURPOSE:             Provide a simple Customer object.
 */

#ifndef CUSTOMER_H_LOADED
#define CUSTOMER_H_LOADED

#include <string>

class Customer
{
  public:
    /*
     * PURPOSE:             Provide the default constructor so empty objects
     *                      can be added to data structures.
     */
    Customer()
    {
    }

    /*
     * PURPOSE:             Construct a new Customer object.
     * PARAMETERS:          lastName - the last name of the customer
     *                      firstName - the first name of the customer
     *                      ssn - the social security # of the customer
     */
    Customer(const std::string& lastName,
             const std::string& firstName,
             const std::string& ssn)
      : m_lastName(lastName), m_firstName(firstName), m_ssn(ssn)
    {
    }

    /*
     * PURPOSE:             Dumps the Customer object to an output stream.
     * PARAMETERS:          out - the output stream
     *                      customer - the customer object to dump
     */
    friend std::ostream& operator<<(std::ostream& out, const Customer& customer)
    {
      out << "Customer("
          << "last=" << customer.m_lastName
          << ", first=" << customer.m_firstName
          << ", ssn=" << customer.m_ssn
          << ")";
      return out;
    }

  private:
    std::string m_lastName;
    std::string m_firstName;
    std::string m_ssn;
};

#endif
