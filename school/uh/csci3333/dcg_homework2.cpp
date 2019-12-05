/**
 * PROGRAMMER:          David C. Gibbons
 * COURSE:              CSCI 3333.01
 * DATE:                October 15, 2008
 * ASSIGNMENT:          Homework # 2
 * ENVIRONMENT:         Any ANSI C++ capable environment
 * FILES INCLUDED:      dcg_homework2.cpp
 * PURPOSE:             1. A function that that uses arrays and pointers 
 *                         to allows users to enter three students 
 *                         information and print what they have entered. 
 *                         Each student should have a last name, first 
 *                         name, age, and graduation year.
 *
 *                      2. The second part of the program uses class to do 
 *                         the same thing that is, it allows users to enter
 *                         three students information and print what they 
 *                         have entered. Each student should have a last 
 *                         name, first name, age, and graduation year.
 */

// include standard C++ I/O and string header files
#include <iostream>
#include <iomanip>
#include <string>
#include <sstream>

using namespace std;

// maximum number of students allowed at once
const int MAX_STUDENTS = 3; 


/*
 * FUNCTION:            get_value
 * PURPOSE:             retrieves user input, string or numeric
 * INPUT:               v - value variable of type T
 *                      numericOnly - if true, only allow digits
 * PRECONDITIONS:       N/A
 * OUTPUT:              true if the input was valid, false otherwise
 * POSTCONDITIONS:      N/A
 * ALGORITHM:           retrieves a string line from the user and then
 *                      attempts to convert it to the appropriate type
 */
template <class T> const bool get_value(T& v, const bool numericOnly)
{
  // read an entire line from the user first before trying to extract a
  // value. this allows any error in the input to be more easily handled
  string line_buffer;
  getline(cin, line_buffer);

  bool is_valid;
  if (line_buffer.empty())
  {
    is_valid = false;
  }
  else if (numericOnly &&
    line_buffer.find_first_not_of("0123456789.", 0) != string::npos)
  {
    is_valid = false;
  }
  else
  {
    // create a stringstream around the line that was just read and try and
    // read a value of the provided data type from it
    istringstream iss(line_buffer);
    iss >> v;

    // determine if the input value was read successfully
    is_valid = !iss.fail();
  }

  return is_valid;
}

/*
 * FUNCTION:            input_stupid
 * PURPOSE:             retrieves user input for a single student
 * INPUT:               last_name, first_name, age, graduation_year -
 *                      variables to receive individual fields read
 * PRECONDITIONS:       N/A
 * OUTPUT:              true if the input was valid, false otherwise
 * POSTCONDITIONS:      N/A
 * ALGORITHM:           N/A
 */
bool input_student(string& last_name, string& first_name, 
                   int& age, int& graduation_year)
{
  cout << endl << "Provide the following student information:" << endl;
  
  cout << "\tLast Name: " << flush;
  if (!get_value(last_name, false))
  {
    return false;
  }

  cout << "\tFirst Name: " << flush;
  if (!get_value(first_name, false))
  {
    return false;
  }

  cout << "\tAge: " << flush;
  if (!get_value(age, true) || (age < 0 || age > 99))
  {
    return false;
  }

  cout << "\tGraduation Year: " << flush;
  if (!get_value(graduation_year, true) || 
      (graduation_year < 2000 || graduation_year > 2050))
  {
    return false;
  }

  cout << endl;

  return true;
}

/*
 * FUNCTION:            array_test
 * PURPOSE:             test case that uses arrays to store student info
 * INPUT:               N/A
 * PRECONDITIONS:       N/A
 * OUTPUT:              N/A
 * POSTCONDITIONS:      N/A
 * ALGORITHM:           N/A
 */
void array_test()
{
  cout << endl << endl
       << "Test Case # 1 - Using Arrays to Represent Data" << endl
       << setfill('=') << setw(78) << " "
       << endl;

  // allocate array storage on the stack for the student data
  string last_names[MAX_STUDENTS];
  string first_names[MAX_STUDENTS];
  int ages[MAX_STUDENTS];
  int graduation_years[MAX_STUDENTS];

  // retrieve all the student data from the user
  cout << "Provide 3 students worth of data:" << endl << endl;
  for (int student = 0; student < MAX_STUDENTS; )
  {
    string last_name, first_name;
    int age, graduation_year;

    if (input_student(last_name, first_name, age, graduation_year))
    {
      last_names[student] = last_name;
      first_names[student] = first_name;
      ages[student] = age;
      graduation_years[student] = graduation_year;
      student++;
    }
    else
    {
      cout << "Invalid input, try again..." << endl;
      continue;
    }
  }

  // display all of the students to the output
  cout << "Displaying Student Data:" << endl 
       << endl;
  for (int student = 0; student < MAX_STUDENTS; student++)
  {
    cout << "Student #" << (student + 1) << endl
         << "\tName:" << last_names[student] 
         << ", " << first_names[student] << endl
         << "\tAge:" << ages[student] 
         << ", Graduation Year: " << graduation_years[student] << endl
         << endl;
  }
}

/*
 * CLASS:               Student
 * PURPOSE:             represents a single student object
 */
class Student
{
  public:
    Student(const string& last_name, 
            const string& first_name, 
            const int age, 
            const int graduation_year)
      : m_last_name(last_name), 
        m_first_name(first_name), 
        m_age(age), 
        m_graduation_year(graduation_year) 
    {
      // NO-OP
    } 

    virtual ~Student() 
    {
      // NO-OP
    }

    // allow this class to be sent to an ostream for display
    friend ostream& operator<<(ostream& output, const Student& s) 
    {
      output << "\tName:" << s.m_last_name << ", " << s.m_first_name << endl
             << "\tAge:" << s.m_age 
             << ", Graduation Year: " << s.m_graduation_year 
             << endl;
      return output;
    }

  private:
    string m_last_name;
    string m_first_name;
    int m_age;
    int m_graduation_year;
};

/*
 * FUNCTION:            class_test
 * PURPOSE:             test case that uses classes & pointers to store 
 *                      student info
 * INPUT:               N/A
 * PRECONDITIONS:       N/A
 * OUTPUT:              N/A
 * POSTCONDITIONS:      N/A
 * ALGORITHM:           N/A
 */
void class_test()
{
  cout << endl << endl
       << "Test Case # 2 - Using C++ Classes to Represent Data" << endl
       << setfill('=') << setw(78) << " "
       << endl;

  // allocate an array of pointers to Student objects on the stack
  Student* students[MAX_STUDENTS];

  // retrieve all the student data from the user
  cout << "Provide 3 students worth of data:" << endl << endl;
  for (int student = 0; student < MAX_STUDENTS; )
  {
    string last_name, first_name;
    int age, graduation_year;
    if (input_student(last_name, first_name, age, graduation_year))
    {
      Student* s = new Student(last_name, first_name, age, graduation_year);
      students[student] = s;
      student++;
    }
    else
    {
      cout << "Invalid input, try again..." << endl;
      continue;
    }
  }

  // print each student and destroy it afterwards
  cout << "Displaying Student Data:" << endl 
       << endl;
  for (int student = 0; student < MAX_STUDENTS; student++)
  {
    cout << "Student #" << (student + 1) << endl
         << *students[student] << endl;

    delete students[student];
    students[student] = 0;
  }
}

/*
 * FUNCTION:            main
 * PURPOSE:             runs the program with several test cases
 * INPUT:               N/A
 * PRECONDITIONS:       N/A
 * OUTPUT:              N/A
 * POSTCONDITIONS:      N/A
 * ALGORITHM:           N/A
 */
int main(void)
{
  cout << "David C. Gibbons" << endl
       << "Homework Assignment # 2" << endl
       << "CSCI 3333 - Data Structures" << endl
       << "Fall 2008" << endl
       << "Bindra Shrestha" << endl
       << setfill('=') << setw(38) << " "
       << endl << endl;

  // run the test suite that uses arrays for storage
  array_test();

  // run the test suite that uses classes & pointers for storage
  class_test();

  // return a successful status to the operating system
  return 0;
}
