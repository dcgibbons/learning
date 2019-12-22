#include <errno.h>
#include <stdlib.h>
#include <stdio.h>

#define BUFFER_SIZE (1024 * 1024)

int count_bits_in_int(unsigned int n)
{
  int nbits = 0;

  for (int i = 0; i < 32; i++)
  {
    nbits += n & 0x01;
    n >>= 1;
  }

  return nbits;
}

int count_bits(unsigned char byte)
{
  int nbits = 0;

  for (int i = 0; i < 8; i++)
  {
    nbits += byte & 0x01;
    byte >>= 1;
  }

  return nbits;
}

int main(int argc, char** argv)
{
  const char* filename = (argc != 2) ? "foo" : argv[1];
  FILE* fp = fopen(filename, "r");
  if (fp == NULL)
  {
    perror(filename);
  }
  else
  {
    unsigned int nbytes = 0;
    unsigned long nbits = 0;
    while (!feof(fp)) 
    {
      char buffer[BUFFER_SIZE];
      size_t nread = fread(buffer, sizeof(char), BUFFER_SIZE, fp);
      nbytes += nread;
      int i;
      for (i = 0; i + sizeof(unsigned int) < nread; i += sizeof(unsigned int))
      {
        unsigned int* p = (unsigned int*) &buffer[i];
        nbits += count_bits_in_int(*p);
      }
      for ( ; i < nread; i++)
      {
        nbits += count_bits(buffer[i]);
      }
    }
    fclose(fp);

    printf("nbytes=%u\n", nbytes);
    printf("nbits=%lu\n", nbits);
  }

  return errno;
}

