#include <errno.h>
#include <stdlib.h>
#include <stdio.h>

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
    int ch;
    while ((ch = fgetc(fp)) != EOF)
    {
      nbytes++;
      nbits += count_bits(ch);
    }
    fclose(fp);

    printf("nbytes=%u\n", nbytes);
    printf("nbits=%lu\n", nbits);
  }

  return errno;
}

