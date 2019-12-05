//
// DCG_WK4_Ex1.cpp
// Week 4, Exercise 1
// David C. Gibbons
// PRG/410 - C++ Programming I
// Chip Dickson
// October 7, 2007
//
// 1. Write a program that reads students' names followed by their test scores. The program
// should output each students' name followed by the test scores and the relevant grade. It 
// should also find and print the highest test score and the name of the students having
// the highest test score.
//
// Student data should be stored in a struct variable of the type studentType, which
// has four components: studentFName and studentLName of the type string,
// testScore of the type int (testScore is between 0 and 100), and grade of the
// type string. Suppose that the class has 10 students. Use an array of 10 components of
// the type studentType.
//
// Your program must contain at least the following functions:
// a. A function to read the students' data into the array. (file or user input, prefer file)
// b. A function to assign the relevant grade to each student. (Calculate the grade)
// c. A function to find the highest test score.
// d. A function to print the names of the students having the highest test score.
//
// Your program must output each students' name in this form: last name followed by
// a comma, followed by a space, followed by the first name, and the name must be left justified.
// Moreover, other than declaring the variables and opening the input and output
// files, the function main should only be a collection of function calls.

#include <iostream>
#include <fstream>
#include <string>
#include <cstdlib>

using namespace std;

typedef struct 
{
  string studentFName;
  string studentLName;
  int testScore;
  string grade;
} 
studentType;

const int MAX_STUDENTS = 10;
const char* STUDENTS_FILE = "students.dat";

// forward reference for internal functions
const int readStudents(istream& input, studentType students[], const int maxStudents);
void calculateGrades(studentType students[], const int nStudents);
const int findHighestScore(const studentType students[], const int nStudents);
void reportTopScores(const int highestScore, const studentType students[], const int nStudents);
const char getLetterGrade(const int grade);
void trim(string& str);

// main program entry point
int main()
{
  int exitStatus = 0;

  fstream inputFile(STUDENTS_FILE, fstream::in);
  if (!inputFile.is_open())
  {
    cerr << "unable to open file" << endl;
    exitStatus = 1;
  }
  else
  {
    studentType students[MAX_STUDENTS]; 
    const int nStudents = readStudents(inputFile, students, MAX_STUDENTS);
    inputFile.close();

    calculateGrades(students, nStudents);
  
    const int highestScore = findHighestScore(students, nStudents);
    reportTopScores(highestScore, students, nStudents);
  }

  return exitStatus;
}

//
// Attempts to read student data from the provided istream and into the provided
// students array.
//
// input - the input stream to read from
// students - the array of student data that will receive the file data
// maxStudents - the maximum size of the students array
//
// returns the actual number of students read from the input stream
//
const int readStudents(istream& input, studentType students[], const int maxStudents)
{
  int student = 0;
  int lineNo = 0;

  do
  {
    string lineBuffer;
    getline(input, lineBuffer);
    lineNo++;

    if (lineBuffer.empty())
    {
      continue;
    }
    else
    {
      // look for the comma that separates last name from the first name
      string::size_type n = 0;
      string::size_type found = lineBuffer.find_first_of(',');
      if (found == string::npos)
      {
        cerr << "invalid student data at line #" << lineNo << " : " << lineBuffer << endl;
        continue;
      }
      else
      {
        // extract out the last name
        students[student].studentFName = lineBuffer.substr(n, found);
        trim(students[student].studentFName);
      }

      // look for the comma that separates the first name from the test score
      n = found + 1;
      found = lineBuffer.find_first_of(',', n);
      if (found == string::npos)
      {
        cerr << "invalid student data at line #" << lineNo << " : " << lineBuffer << endl;
        continue;
      }
      else
      {
        // extract out the first name
        students[student].studentLName = lineBuffer.substr(n, found - n);
        trim(students[student].studentLName);
      }

      // extract out the test score from the remaining text
      n = found + 1;
      students[student].testScore = atoi(lineBuffer.substr(n).c_str());

      student++;
    }
  }
  while (!input.eof() && student < maxStudents);
  
  return student;
}

//
// Calculates the letter grade for all provided student data.
//
// students - the student data to process
// nStudents - the number of students in the students array
//
void calculateGrades(studentType students[], const int nStudents)
{
  for (int student = 0; student < nStudents; student++)
  {
    students[student].grade = getLetterGrade(students[student].testScore);
  }
}

//
// Finds the highest test score of all the students.
//
// students - the student data to process
// nStudents - the number of students in the students array
//
// returns the highest test score of all the students
//
const int findHighestScore(const studentType students[], const int nStudents)
{
  int highestScore = 0;
  for (int student = 0; student < nStudents; student++)
  {
    highestScore = max(highestScore, students[student].testScore);
  }
  return highestScore;
}

//
// Reports all of the students with the highest test score in the class.
//
// highestScore - the highest test score calculated for this class
// students - the student data to process
// nStudents - the number of students in the students array
//
void reportTopScores(const int highestScore, const studentType students[], const int nStudents)
{
  cout << endl
       << "Top Score for Class of " << nStudents << " Students is " 
       << highestScore << " (" << getLetterGrade(highestScore) << ")"
       << endl;

  for (int student = 0; student < nStudents; student++)
  {
    if (students[student].testScore == highestScore)
    {
      cout << students[student].studentLName << ", "
           << students[student].studentFName
           << endl;
    }
  }
}

//
// Calculates the letter grade of a test score.
//
// grade - the test score to calculate the letter grade for
//
// return the letter grade of the test score
//
const char getLetterGrade(const int grade)
{
  char letterGrade;
  if (grade >= 90)
  {
    letterGrade = 'A';
  }
  else if (grade >= 80)
  {
    letterGrade = 'B';
  }
  else if (grade >= 70)
  {
    letterGrade = 'C';
  }
  else if (grade >= 60)
  {
    letterGrade = 'D';
  }
  else
  {
    letterGrade = 'F';
  }
  return letterGrade;
}

//
// Trims leading and trailing whitespace from a string.
//
// str - the string that wil be trimmed in place
//
void trim(string& str)
{
  // first find any non-whitespace on the right side of the string
  string::size_type pos = str.find_last_not_of(" \r\n\t");
  if (pos != string::npos)
  {
    str.erase(pos + 1);

    // then try and find any non-whitespace on the left side of the string
    pos = str.find_first_not_of(" \r\n\t");
    if (pos != string::npos)
    {
      str.erase(0, pos);
    }
  }
  else
  {
    // nothing but whitespace, erase it all!
    str.erase(str.begin(), str.end());
  }
}

