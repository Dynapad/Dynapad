/*
"(c) Copyright 1993-1997 Pad++ Consortium {University of New Mexico (UNM),
and New York University (NYU)}, All Rights Reserved."  
Licensee can not remove or obscure any of the
copyright or trademark notices in this software.

IN NO EVENT SHALL THE AUTHORS OR DISTRIBUTORS BE LIABLE TO ANY PARTY
FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES
ARISING OUT OF THE USE OF THIS SOFTWARE, ITS DOCUMENTATION, OR ANY
DERIVATIVES THEREOF, EVEN IF THE AUTHORS HAVE BEEN ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.

THE AUTHORS AND DISTRIBUTORS SPECIFICALLY DISCLAIM ANY WARRANTIES,
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE, AND NON-INFRINGEMENT.  THIS SOFTWARE
IS PROVIDED ON AN "AS IS" BASIS, AND THE AUTHORS AND DISTRIBUTORS HAVE
NO OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
MODIFICATIONS.

See the file "License" for general information on usage and
redistribution, and the file "LicenseTerms" for the specific license
agreement on usage and redistribution of this file, and the Pad++
software in general.
*/

#include "defs.h"
#include "misc.h"
#include "fontdata.h"
#include "font.h"
#include "ZFontType1.h"
#include "line-font.h"
#include "hashtab.h"
#include <string.h>
#include <dirent.h>

//
// Pad++ Font Data Manager
//

#define DEFAULT_FONT_PATH "/opt/X11/share/fonts/Type1 /usr/lib/X11/fonts/Type1 /usr/X11R6/lib/X11/fonts/Type1"

#define PATH_SEP_CHAR ' '

Pad_FontData::Pad_FontData(const char *filename, const char *name, int style) {
    this->filename = filename;
    this->name = name;
    this->style = style;

    loaded = FALSE;
    substituted = FALSE;
    useLineFont = FALSE;
    id = 0;
    zfont = NULL;
    lowResPoly = NULL;
    highResPoly = NULL;
    xftfont = NULL;
    previousmag = 0;
    xftfactor = 1.0;
    xftfactorchanged = FALSE;
}


Pad_FontData::Pad_FontData(const char *name) {
    this->filename = "";
    this->name = name;
    this->style = FONT_PLAIN;

    loaded = TRUE;
    substituted = FALSE;
    useLineFont = TRUE;
    id = 0;
    zfont = NULL;
    lowResPoly = NULL;
    highResPoly = NULL;
    xftfont = NULL;
    previousmag = 0;
    xftfactor = 1.0;
    xftfactorchanged = FALSE;
}

Pad_FontData::Pad_FontData(Pad_FontData *copy) {
    filename = copy->filename;
    name = copy->name;
    style = copy->style;
    loaded = copy->loaded;
    substituted = copy->substituted;
    useLineFont = copy->useLineFont;
    id = copy->id;
    zfont = copy->zfont;
    lowResPoly = copy->lowResPoly;
    highResPoly = copy->highResPoly;
    xftfont = NULL;
    previousmag = 0;
    xftfactor = 1.0;
    xftfactorchanged = FALSE;
}


//
// Makes data for line fonts
//
Pad_FontData Pad_FontData::lineFontData("Line");
Pad_FontData Pad_FontData::systemFontData("System");
Pad_Bool Pad_FontData::xftenable = TRUE;

Pad_FontData::~Pad_FontData() {
}


float
Pad_FontData::Char_height(char c) {
    return (useLineFont ? Pad_LineFont::CharHeight()
                        : (float) (highResPoly->char_height(c)));
}

float
Pad_FontData::Char_width(char c) {
    return (useLineFont ? Pad_LineFont::CharWidth(c)
                        : (float) (highResPoly->char_width(c)));
}

void
Pad_FontData::String_extents(const char *string, float &width, float &height) {

    if (useLineFont) {
        width = Pad_LineFont::StringWidth(string);
        height = Pad_LineFont::LineHeight();
        return;
    }

    const char *p;
    float char_height;
    int tabpos;

    width = 0.0;
    height = 0.0;
    tabpos = 0;
    for (p = string; *p; p++) {
        if (*p == '\t') {
            width += (8 - (tabpos % 8)) * highResPoly->char_width(' ');
            tabpos = 0;
        } else {
            width += (float) (highResPoly->char_width(*p));
            tabpos++;
        }
        char_height = (float) (highResPoly->char_height(*p));
        height = MAX(height, char_height);
    }
}

//
// Mechanism for mapping from a logical font name/style to its Pad_FontData
// info (loading its Postscript Type 1 font data if its not been loaded).
//


//
// Fonts are looked up in the font table based on their name and style.
//
typedef struct {
    const char *name;
    int style;
} FontKey;

//
// A hash table of all available fonts (both loaded and not yet loaded).
// This hash table is setup whenever the font search path is changed.
// It maps the font name/style to its Pad_FontData structure.
//
static Pad_HashTable fontTable(sizeof(FontKey) / sizeof(int));

static Pad_FontData *Substitute_font(FontKey key, Pad_Bool &isAlias);

static Pad_FontData *
Get_data(const char *name, int style, Pad_Bool warn) {
    FontKey key;
    Pad_FontData *font;

    if (name == Pad_FontData::lineFontData.name) {
        return &Pad_FontData::lineFontData;
    }
    if (name == Pad_FontData::systemFontData.name) {
        return &Pad_FontData::systemFontData;
    }

    key.name = name;
    key.style = style;

    font = (Pad_FontData *) fontTable.Get(&key);

    if (!font) {
        // attempted to load a font whose name wasn't recognized...

        Pad_Bool gotAlias;
        font = new Pad_FontData(Substitute_font(key, gotAlias));
        font->substituted = TRUE;
        fontTable.Set(&key, font);

        if (warn && !gotAlias) {
            // I am generating warnings, and this isn't a font alias,
            // check if this is a System font. If not, print a warning
            if (strcasecmp(name, "system") != 0 && strcasecmp(name, "line") != 0) {
                fprintf(stderr,
                        "Warning: Cannot find font %s. Using %s instead.\n",
                        name, font->name);
            }
        }
    }

    font->Load();
    return font;
}

void
Pad_FontData::Load() {
    ZFontType1 *zf;
    static int fid = 1;

    if (loaded) {
        return;
    }

    zf = new ZFontType1(filename.Get());

    if (!zf->Is_loaded()) {
        // something went wrong during loading
        delete zf;
        fprintf(stderr,
                "Error loading font file %s, using Line font instead.\n",
                filename.Get());

        loaded = TRUE;
        substituted = TRUE;
        useLineFont = TRUE;
        id = 0;
        return;
    }

    loaded = TRUE;
    useLineFont = FALSE;
    zfont = zf;
    lowResPoly = zf->GenFontPoly(20.0, 0);
    highResPoly = zf->GenFontPoly(1.0, 0);
    id = fid++;
}


//
// This is the core routine for replacing one font with another.
// 

typedef struct {
    const char *origName, *newName;
    Pad_Bool isAlias;  // is this an alias, or a substitution?
} FontSub;

static FontSub fontSubTable[] = {
    {"TimesRoman",  "Times",     TRUE},
    {"Dialog",      "Helvetica", TRUE},
    {"DialogInput", "Helvetica", TRUE},
    {"Times",       "Utopia",    TRUE},
    {"Helvetica",   "Utopia",    FALSE},
    {"default",     "Helvetica", TRUE},
    {NULL, NULL}
};

static Pad_FontData *
Substitute_font(FontKey key, Pad_Bool &isAlias) {
    Pad_FontData *font;
    isAlias = FALSE;

    Pad_HashTableIterator iter;
    Pad_FontData *f;

    // First, check if the fonts in the table, but using a caseless
    // comparison for the names.

    DOTABLE(iter, fontTable, Pad_FontData, f) {
        if (!(strcasecmp(key.name, f->name))) {
            // yes, they gave the case incorrectly - fix it up
            return Get_data(f->name, key.style, FALSE);
        }
    }

    // see if its in the substitution table
    FontSub *sub;
    for (sub = fontSubTable; sub->origName; sub++) {
        if (!strcasecmp(key.name, sub->origName)) {
            font = Get_data(Pad_GetUid(sub->newName), key.style, FALSE);
            if (font) {
                isAlias = sub->isAlias;
                return font;
            }
        }
    }

    // if we failed to load a bold/italic version, try the plain version
    if (key.style != FONT_PLAIN) {
        font = Get_data(key.name, FONT_PLAIN, FALSE);
        if (font) {
            return font;
        }
    }

    return &Pad_FontData::lineFontData;
}




//
// Font Table Management
//

// adds a font to fontTable
static void Add_to_font_table(char *filename, char *name, int style) {
    Pad_FontData *font;
    FontKey key;
    key.name = name;
    key.style = style;

    if ((font = (Pad_FontData *) fontTable.Get(&key))) {
        // know this font - but maybe I can do better...

        if (font->substituted) {
            // retry on this font
            font->name = name;
            font->style = style;
            font->filename = filename;
            font->substituted = FALSE;
            font->loaded = FALSE;
        }
    } else {
        // don't know this font yet - add it to the table
        fontTable.Set(&key, new Pad_FontData(filename, key.name, key.style));
    }
}

// Takes a full Postscript font name. Splits it into family and style.
// Shortens -name- so it only includes the family portion. Sets -style-
// to the font style.

static void Extract_style(char *name, int &style) {
    char *c;
    char *stylename = NULL;

    // at this point, name is set to the name of the font - we need to
    // parse this into the font name and style

    style = FONT_PLAIN;

    // split name into basename-stylename
    if ((c = strchr(name, '-'))) {
        stylename = c + 1;
        *c = 0;
    }

    // now go figure on the style name
    if (stylename) {
        if (strstr(stylename, "Bold"))
            style |= FONT_BOLD;

        if (strstr(stylename, "Oblique") || strstr(stylename, "Italic"))
            style |= FONT_ITALIC;
    }
}

//
// Takes the name of a font file and the first line in the file.
// Adds the font to fontTable. Returns the name of the font that was added,
// and its style.
//
char *Add_font(char *filename, char *firstline, int &styleReturn) {
    char *name, *c;

    if (firstline[0] != '%' || firstline[1] != '!')
        return NULL;

    if ((c = strchr(firstline, ' '))) {
        // name starts after first space
        name = c + 1;
    } else {
        // some fonts don't have names in their files - so use
        // the filename as the font name. (must remove path and extension)
        //
        name = NULL;

        // get the tail of the path
        if (filename == NULL || filename[0] == '\0')
            name = NULL;
        else if ((name = strrchr(filename, '/')))
            name++;
        else
            name = filename;

        if (!name || !*name) {
            // something went wrong getting the tail of the path...
            return NULL;
        }

        name = strdup(name);
        // remove any extension
        if ((c = strchr(name, '.'))) {
            *c = 0;
        }
    }

    if ((c = strchr(name, ' '))) {
        *c = 0;
    }
    int style;
    Extract_style(name, style);
    name = (char *) Pad_GetUid(name);
    Add_to_font_table(filename, name, style);

    styleReturn = style;
    return name;
}


//
// Opens the given font file, reads its first line, parses the name
// defined on the first line, and adds the filename/name/style information
// to fontTable. Returns the name of the font that was added, and its style.
//        
static char *Register_font_file(char *filename,
                                int &styleReturn) {
    char *result = NULL;
    FILE *fp;
    fp = fopen(filename, "r");

    if (fp) {
        char buf[1024];  // yes, someday it will be too small - ron
        char *p;

        if ((p = fgets(buf, sizeof(buf), fp))) {
            if ((p = strchr(buf, '\n')))
                *p = '\0';
            result = Add_font(filename, buf, styleReturn);
        }

        fclose(fp);
    }

    return result;
}

//
// This registers all the font files in a given directory.
//

static void Register_font_directory(char *dirname) {
    DIR *dirp;
    struct dirent *direntp;

    if ((dirp = opendir(dirname))) {
        while ((direntp = readdir(dirp))) {
            Pad_String name = dirname;
            int len;
            name += "/";
            name += direntp->d_name;
            len = name.Length();
            if (len > 4 && !strcmp(name.Get(len - 4), ".pfa")) {
                int styleReturn;
                Register_font_file(name.Get(), styleReturn);
            }
        }
        closedir(dirp);
    }
}


//
// This is called when the font path changes. It finds all the fonts
// in the font path and adds them to the fontTable.
//
static void Set_font_path(char *path) {
    Pad_HashTableIterator iter;
    Pad_Iterator i;
    Pad_FontData *f;
    FontKey *key;
    Pad_List tmp;

    // First, remove all unloaded fonts from the fontTable
    DOTABLE(iter, fontTable, Pad_FontData, f) {
        if (!f->loaded) {
            // not loaded yet - free up space
            delete f;
            // add key to tmp - so that entry can be removed from the table
            tmp.Push_last(iter.key);
        }
    }

    DOLIST(i, tmp, FontKey, key) {
        fontTable.Set(key, NULL);
    }

    // now break the path into directories separated by PATH_SEP_CHAR,
    // and locate the fonts in each directory.


    char *nextPath;

    do {
        nextPath = strchr(path, PATH_SEP_CHAR);

        if (nextPath) { // set end-of-string at nextPath
            *nextPath = 0;
        }
        if (path[0]) {
            Register_font_directory(path);
        }

        if (nextPath) { // reset nextPath
            *nextPath = PATH_SEP_CHAR;
            path = nextPath + 1;
        }
    } while (nextPath);
}

//
// Public API to the font manager
// ------------------------------
//


//
// For back-compatibility, this takes a font filename,
// and maps it into the corresponding logical name and style. It returns
// true if the font filename was recognized, false otherwise.
//

int
Pad_FontData::File_name_and_style(const char *filename,
                                  Pad_String &name, int &style) {
    Pad_HashTableIterator iter;
    Pad_FontData *f;

    if (strstr("AType1", filename)) {    // special case AType1Font
        name = "Utopia";
        style = FONT_PLAIN;
        return TRUE;
    }

    DOTABLE(iter, fontTable, Pad_FontData, f) {
        if (!strcmp(f->filename.Get(), filename)) {
            name = f->name;
            style = f->style;
            return TRUE;
        }
    }

    //
    // Must be a font I don't yet know about (one not on the font path).
    // Try to register it and then try the table again. 
    // Hopefully this won't happen too often...
    //
    Pad_String filenameCopy = filename;
    int fontStyle;
    char *fontName;

    fontName = Register_font_file(filenameCopy.Get(), fontStyle);


    if (fontName) {
        name = fontName;
        style = fontStyle;
        return TRUE;
    }

    return FALSE;
}

//
// Takes a font style, returns its symbolic name.
//
const char *
Pad_FontData::Style_as_string(int style) {
    switch (style) {
        case FONT_BOLD:
            return "bold";

        case FONT_ITALIC:
            return "italic";

        case FONT_BOLD | FONT_ITALIC:
            return "bolditalic";
    }

    return "plain";
}

//
// Returns a list of strings, each of which is a font name
// in the form "family-style"
//
void
Pad_FontData::Get_available_fonts(Pad_List &l) {
    Pad_HashTableIterator iter;
    Pad_FontData *f;

    Pad_String *s = new Pad_String("Line");
    l.Push_last(s);

    DOTABLE(iter, fontTable, Pad_FontData, f) {
        Pad_String *s = new Pad_String();
        *s += f->name;
        if (f->style != FONT_PLAIN) {
            *s += "-";
            *s += Pad_FontData::Style_as_string(f->style);
        }
        l.Push_last(s);
    }
}

static Pad_String fontPath;

void Pad_FontData::Get_path(Pad_String &path) {
    path = fontPath;
}

void Pad_FontData::Set_path(const char *path) {
    if (!strcmp(path, fontPath.Get()))
        return; /* do nothing if there is no change... */

    fontPath = path;
    Set_font_path(fontPath.Get());
}

void
Pad_FontData::Set_default_font_path() {
    // Sets default font path
    Pad_FontData::Set_path(DEFAULT_FONT_PATH);
}

//
// Adds directory to font path
// 
void
Pad_FontData::Append_to_path(char *dirname) {
    Register_font_directory(dirname);

    fontPath += " ";
    fontPath += dirname;
}


void *
Pad_FontData::Get_font_data(char *name, int style) {
    return (void *) Get_data(name, style, TRUE);
}
