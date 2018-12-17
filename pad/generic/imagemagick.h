#ifndef IMAGEMAGICK_H
#define IMAGEMAGICK_H 1

#include <stdio.h>

void Pad_InitIM();
unsigned long *Pad_ReadIM(FILE *fp, int* ret_width, int* ret_height);
unsigned long *Pad_LoadIM(char *filename, int* ret_width, int* ret_height);
unsigned long *Pad_LoadIM(unsigned char *formattedData, int len);
Pad_Bool Pad_Imagep(char *filename);

#endif
