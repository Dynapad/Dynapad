#ifndef IMAGEMAGICK_H
#define IMAGEMAGICK_H 1

#include <cstdio>

void Pad_InitIM();
unsigned long *Pad_ReadIM(FILE *fp, int* ret_width, int* ret_height);
unsigned long *Pad_LoadIM(const char *filename, int *ret_width, int *ret_height);
unsigned long *Pad_LoadIM(const unsigned char *formattedData, int len);
Pad_Bool Pad_Imagep(char *filename);

#endif
