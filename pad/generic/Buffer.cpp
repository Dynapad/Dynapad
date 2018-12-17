#include "Buffer.h"
#include <ctype.h>
#include <stdio.h>

/* 
 *	fake file reading funcs
 */

void 
Buffer::fopen(unsigned char *buf, int length)
{
  max_ = 0;			// So we don't free the data later.
  pos_ = 0;
  data_ = buf;
  count_ = length;
}

#define NL	(0x0a)
#define CR	(0x0d)

void 
Buffer::gettoken(char *str)
{
  int c;
  unsigned char *cptr;

  cptr = data_ + pos_;
  c = *cptr++;
  pos_++;
  if (c != NL && c != CR) {
    while(isspace(c)) {
      c = *cptr++;
      pos_++;
    }
    while (pos_ < count_ && !isspace(c)) {
      *str++ = c;
      c = *cptr++;
      pos_++;
    }
    if (c == NL || c == CR)
      pos_--;
  }
  *str = 0;
  if (pos_ > count_) {
    fprintf(stderr, "fromtype1: unexpected eof\n");
    exit(1);
  }
}

int 
Buffer::fgets(char *buf, int max)
{
  unsigned char *cptr;

  cptr = (unsigned char *)(data_ + pos_);
  while (max--) {
    *buf++ = *cptr;
    pos_++;
    if (*cptr == NL || *cptr == CR) {
	if (max > 0) {
	    *buf = '\0';
	    return 1;
	} else {
	    return 0;
	}
    }
    cptr++;
    if (pos_ > count_) return 0;
  }
  return 0;
}

unsigned char *
Buffer::fread(int n)
{
  pos_ += n;
  return data_ + pos_ - n;
}
