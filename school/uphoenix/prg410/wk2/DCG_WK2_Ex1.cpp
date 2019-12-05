//
// DCG_WK2_Ex1.cpp
// Week 2, Exercise 1
// David C. Gibbons
// PRG/410 - C++ Programming I
// Chip Dickson
// September 20, 2007
//
// 1. Write a short program to calculate the grades for 10 students. Each 
//    student should have 4 scores. Output the scores, numeric grade, and
//    letter grade for each student.
//

#include <iostream>
#include <iomanip>
#include <limits>

using namespace std;

const int NUM_STUDENTS = 10;
const int NUM_GRADES = 4;
const int GRADES[NUM_STUDENTS][NUM_GRADES] =
{
  /* student  1 */ { 72, 80, 84, 86 },
  /* student  2 */ { 88, 92, 94, 87 },
  /* student  3 */ { 94, 99, 100, 100 },
  /* student  4 */ { 45, 74, 64, 68 },
  /* student  5 */ { 89, 90, 90, 100 },
  /* student  6 */ { 64, 82, 88, 92 },
  /* student  7 */ { 99, 100, 99, 97 },
  /* student  8 */ { 80, 82, 88, 91 },
  /* student  9 */ { 91, 92, 94, 67 },
  /* student 10 */ { 80, 90, 80, 84 }
};

/* forward reference */
const char getLetterGrade(const int grade);

int main()
{
  // report header
  cout << "Student" 
       << "\tTest 1" 
       << "\tTest 2" 
       << "\tTest 3" 
       << "\tTest 4" 
       << "\tFinal Grade" 
       << endl;

  for (int student = 0; student < NUM_STUDENTS; student++)
  {
    cout << setw(2) << student + 1 << "\t"; // add 1 to hide zero-reference array

    int gradeTotal = 0;
    for (int grade = 0; grade < NUM_GRADES; grade++)
    {
      int currentGrade = GRADES[student][grade];
      gradeTotal += currentGrade; 

      // display current score and letter grade
      cout << setw(3) << currentGrade << " " << getLetterGrade(currentGrade) << "\t";
    }
    // calculate and dipslay final score and letter grade
    const int finalGrade = gradeTotal / NUM_GRADES;
    cout << setw(3) << finalGrade << " " << getLetterGrade(finalGrade) << endl;
  }

  return 0;
}

/**
 * Determines the letter grade for a numeric score using a standard
 * 10-point scoring window.
 */
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

