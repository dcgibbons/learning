/**
 * PROGRAMMER:          David C. Gibbons
 * COURSE:              CSCI 3333.01
 * DATE:                November 19, 2008
 * ASSIGNMENT:          Homework # 4
 * ENVIRONMENT:         Any ANSI C++ capable environment
 * FILES INCLUDED:      dcg_homework4.cpp, customer.h, queue.h, stack.h
 * PURPOSE:             Simulate a queue of people waiting in a bank. The data 
 *                      fields at minimum include first name, last name, and 
 *                      social security. Use a queue implementation.  Your queue
 *                      simulation at minimum must perform "enqueue" and 
 *                      "dequeue" operation.
 *
 *                      The second part of your program reads a line, prints the
 *                      line as it was read, and then prints the line with its 
 *                      text reversed.
 */

#include <cctype>
#include <iomanip>
#include <iostream>
#include <string>

#include "customer.h"
#include "queue.h"
#include "stack.h"

using namespace std;

/**
 * PURPOSE:             Retrieves customer information from the user
 * RETURNS:             a valid Customer object based upon the user's input
 * ACTION:              Prompts the user for various customer information via
 *                      standard output and retrieves their response via
 *                      standard input.
 */
Customer getCustomerInput()
{
  cout << "Last Name: " << flush;
  string lastName;
  getline(cin, lastName);

  cout << "First Name: " << flush;
  string firstName;
  getline(cin, firstName);

  cout << "SSN: " << flush;
  string ssn;
  getline(cin, ssn);

  return Customer(lastName, firstName, ssn);
}

/**
 * PURPOSE:             Run a bank simulation using a queue object.
 * ACTION:              Customer data is retrieved from the user, added to a 
 *                      queue to simulate a bank line, and then removed from the
 *                      queue to simulate a teller processing each customer.
 */
void bankSimulation()
{
  cout << "Bank Simulation" << endl << endl;

  Queue<Customer> customerQueue;

  // keep reading customers until the user wants to stop
  while (true)
  {
    Customer customer = getCustomerInput();
    customerQueue.enqueue(customer);

    cout << endl << "Add another customer? [Y/n] " << flush;
    string answer;
    getline(cin, answer);
    if (answer.length() > 0 && tolower(answer[0]) == 'n')
    {
      break;
    }
  }

  // process the queue of customers and display each one as they are processed
  cout << endl << "Now serving customers:" << endl;
  int customerNumber = 0;
  while (!customerQueue.isEmpty())
  {
    Customer customer = customerQueue.dequeue();
    customerNumber++;
    cout << "\tServing customer #" << customerNumber
         << ", " << customer << endl;
  }
  
  cout << endl << endl;
}

/**
 * PURPOSE:             Reverses a string using a stack data structure.
 * ACTION:              Retrieves a line of input from the user and places each
 *                      character on a stack. The stack is then processed and
 *                      the resulting reversed string is displayed.
 */
void reverseString()
{
  cout << "Reverse a String using a Stack" << endl << endl;

  cout << "Enter a line of text that will be reversed: " << flush;
  string line;
  getline(cin, line);

  Stack<char> stack(255);
  for (int i = 0, n = line.length(); i < n; i++)
  {
    stack.push(line[i]);
  }

  line.clear();
  while (!stack.isEmpty()) 
  {
    char c = stack.pop();
    line.append(1, c);
  }

  cout << "The line reversed: " << line << endl
       << endl;
}

/**
 * PURPOSE:             Main program entry point.
 */
int main()
{
   cout << "David C. Gibbons" << endl
        << "Homework Assignment # 4" << endl
        << "CSCI 3333 - Data Structures" << endl
        << "Fall 2008" << endl
        << "Bindra Shrestha" << endl
        << setfill('=') << setw(38) << " "
        << endl << endl;

  bankSimulation();

  cout << setfill('=') << setw(38) << " " << endl << endl;

  reverseString();
}

