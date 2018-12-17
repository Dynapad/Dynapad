#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>

#include <fontconfig/fontconfig.h>

int main(int argc, char *argv[])
{
    FcFontSet       *fonts;
    int             i, print_detail = 0;
    FcChar8         *name;
    FcPattern       *pattern = NULL;
    FcObjectSet     *objects;

    if (argc >= 2 && strcmp(argv[1], "-d") == 0)
        print_detail = 1;
    
    if (FcInit() != FcTrue)
    {
        perror("FcInit");
        exit(__LINE__);
    }
    
    if (argc >= 2 + print_detail)
    {
        pattern = FcNameParse(argv[1 + print_detail]);
        if (pattern == NULL)
        {
            perror("FcNameParse");
            exit(__LINE__);
        }
        
        objects = FcObjectSetBuild(FC_FAMILY, FC_STYLE, FC_SLANT, FC_WEIGHT,
                FC_SIZE, FC_ASPECT, FC_PIXEL_SIZE, FC_SPACING, FC_FOUNDRY,
                FC_ANTIALIAS, FC_HINTING, FC_VERTICAL_LAYOUT, FC_AUTOHINT,
                FC_GLOBAL_ADVANCE, FC_FILE, FC_INDEX, FC_FT_FACE, FC_RASTERIZER,
                FC_OUTLINE, FC_SCALABLE, FC_SCALE, FC_DPI, FC_RGBA, FC_MINSPACE,
                NULL);
        
        fonts = FcFontList(NULL, pattern, objects);
        if (fonts == NULL)
        {
            perror("FcFontList");
            exit(__LINE__);
        }
    }
    else
    {
        fonts = FcConfigGetFonts(NULL, FcSetSystem);
        if (fonts == NULL)
        {
            perror("FcConfigGetFonts");
            exit(__LINE__);
        }
    }
    
    for (i = 0; i < fonts->nfont; ++i)
    {
        if (print_detail)
        {
            FcPatternPrint(fonts->fonts[i]);
            continue;
        }
    
        name = FcNameUnparse(fonts->fonts[i]);
        if (name == NULL)
        {
            perror("FcNameUnparse");
            exit(__LINE__);
        }
        
        pattern = FcNameParse(name);
        if (pattern == NULL)
        {
            perror("FcNameParse");
            exit(__LINE__);
        }
        
        free(name);
    
        FcPatternDel(pattern, FC_CHARSET);
        FcPatternDel(pattern, FC_LANG);
        
        name = FcNameUnparse(pattern);
        if (name == NULL)
        {
            perror("FcNameUnparse");
            exit(__LINE__);
        }
        
        puts(name);
        
        FcPatternDestroy(pattern);
        free(name);
    }
    
    return 0;
}
