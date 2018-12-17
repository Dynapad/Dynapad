#include <stdio.h>
#include <fontconfig/fontconfig.h>

main()
{
  char *p;
  FcFontSet *FcFontSet;

  if (!(p = FcConfigFilename(NULL))) {
    fprintf(stderr, "FcConfigFile failed\n");
  }
  printf("%s\n", p);

  if (FcInit() != FcTrue) {
    fprintf(stderr, "FcInit failed\n");
    exit(-1);
  }
  if (!(FcFontSet = FcConfigGetFonts(NULL, FcSetSystem))) {
    fprintf(stderr, "FcConfigGetFonts failed\n");
  }
  printf("nfont sfont %d %d\n", FcFontSet->nfont, FcFontSet->sfont);
}
