#include <stdio.h>
#include <string.h>

struct employee 
{
  char id[6];
  char last_name[10];
  char first_name[10];
  char dob[8];
};

int main()
{
  FILE* fp = fopen("employees.dat", "wb+");

  struct employee emp;

  memcpy(emp.id, "000001", sizeof(emp.id));
  memcpy(emp.last_name, "SMITH     ", sizeof(emp.last_name));
  memcpy(emp.first_name, "DAVID     ", sizeof(emp.first_name));
  memcpy(emp.dob, "19680402", sizeof(emp.dob));
  fwrite(&emp, sizeof(emp), 1, fp);

  memcpy(emp.id, "000002", sizeof(emp.id));
  memcpy(emp.last_name, "JONES     ", sizeof(emp.last_name));
  memcpy(emp.first_name, "JANET     ", sizeof(emp.first_name));
  memcpy(emp.dob, "19540810", sizeof(emp.dob));
  fwrite(&emp, sizeof(emp), 1, fp);

  memcpy(emp.id, "000003", sizeof(emp.id));
  memcpy(emp.last_name, "CLARKSON  ", sizeof(emp.last_name));
  memcpy(emp.first_name, "FREDERICK ", sizeof(emp.first_name));
  memcpy(emp.dob, "19330102", sizeof(emp.dob));
  fwrite(&emp, sizeof(emp), 1, fp);

  long recnum = 2; // the record number would come from the index file
  long offset = (recnum - 1) * sizeof(emp); // calculate file position
  fseek(fp, offset, SEEK_SET); // move to record position in file

  struct employee emp2;
  fread(&emp2, sizeof(emp2), 1, fp);

  printf("Employee #: %6.6s\n", emp2.id);
  printf("Last Name:  %10.10s\n", emp2.last_name);
  printf("First Name: %10.10s\n", emp2.first_name);
  printf("Birthdate:  %4.4s-%2.2s-%2.2s\n", &emp2.dob[0], &emp2.dob[4], &emp2.dob[6]);
}

