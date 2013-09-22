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


void build_cache(int bit_cache[256])
{
  for (int i = 0; i < 256; i++)
  {
    bit_cache[i] = count_bits(i);
  }
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
    int bit_cache[256];
    build_cache(bit_cache);

    unsigned int nbytes = 0;
    unsigned long nbits = 0;
    while (!feof(fp)) 
    {
      char buffer[BUFSIZ];
      size_t nread = fread(buffer, sizeof(char), BUFSIZ, fp);
      nbytes += nread;
      for (int i = 0; i < nread; i++)
      {
        nbits += bit_cache[(unsigned char)buffer[i]];
      }
    }
    fclose(fp);

    printf("nbytes=%u\n", nbytes);
    printf("nbits=%lu\n", nbits);
  }

  return errno;
}

