#include "defs.h"
#include "ZFontType1.h"
#include "ZFontType1Interp.h"
#include <string.h>
#include <sys/stat.h>

#define LINELEN 	2048
#define SKIP 		4
#define NOTHEX		100
#define MAXTRIES 	30

Pad_Bool ZFontType1::hextab_initialized_ = FALSE;
unsigned char ZFontType1::hextab_[256];

/* the following characters are used by the SEAC command to make */
/* accented (composite) characters in text fonts                 */

const pschar ZFontType1::accentlist_[NACCENT] = {
    {"/grave",           0301},
    {"/acute",           0302},
    {"/circumflex",      0303},
    {"/tilde",           0304},
    {"/macron",          0305},
    {"/breve",           0306},
    {"/dotaccent",       0307},
    {"/dieresis",        0310},
    {"/ring",            0312},
    {"/cedilla",         0313},
    {"/hungarumlaut",    0315},
    {"/ogonek",          0316},
    {"/caron",           0317},
    {"/dotlessi",        0365}
};

/* ISO Latin1 Encoding of standard char set*/
const pschar ZFontType1::charlist_[NASCII] = {
				/* Missing a few characters!! */
    {"/space", 		040},
    {"/exclam", 		041},
    {"/quotedbl", 	042},
    {"/numbersign", 	043},
    {"/dollar", 		044},
    {"/percent", 	045},
    {"/ampersand", 	046},
    {"/quoteright", 	047},
    {"/parenleft", 	050},
    {"/parenright", 	051},
    {"/asterisk", 	052},
    {"/plus", 		053},
    {"/comma", 		054},
    {"/hyphen", 		055},
    {"/period", 		056},
    {"/slash", 		057},
    {"/zero", 		060},
    {"/one", 		061},
    {"/two", 		062},
    {"/three", 		063},
    {"/four", 		064},
    {"/five", 		065},
    {"/six", 		066},
    {"/seven", 		067},
    {"/eight", 		070},
    {"/nine", 		071},
    {"/colon", 		072},
    {"/semicolon", 	073},
    {"/less",	 	074},
    {"/equal",	 	075},
    {"/greater",	 	076},
    {"/question", 	077},
    {"/at",	 	0100},
    {"/A",	 	0101},
    {"/B",	 	0102},
    {"/C",	 	0103},
    {"/D",	 	0104},
    {"/E",	 	0105},
    {"/F",	 	0106},
    {"/G",	 	0107},
    {"/H",	 	0110},
    {"/I",	 	0111},
    {"/J",	 	0112},
    {"/K",	 	0113},
    {"/L",	 	0114},
    {"/M",	 	0115},
    {"/N",	 	0116},
    {"/O",	 	0117},
    {"/P",	 	0120},
    {"/Q",	 	0121},
    {"/R",	 	0122},
    {"/S",	 	0123},
    {"/T",	 	0124},
    {"/U",	 	0125},
    {"/V",	 	0126},
    {"/W",	 	0127},
    {"/X",	 	0130},
    {"/Y",	 	0131},
    {"/Z",	 	0132},
    {"/bracketleft", 	0133},
    {"/backslash",	0134},
    {"/bracketright", 	0135},
    {"/asciicircum",	0136},
    {"/underscore", 	0137},
    {"/quoteleft", 	0140},
    {"/a",	 	0141},
    {"/b",	 	0142},
    {"/c", 		0143},
    {"/d",		0144},
    {"/e", 		0145},
    {"/f",		0146},
    {"/g",	 	0147},
    {"/h",	 	0150},
    {"/i",	 	0151},
    {"/j",	 	0152},
    {"/k", 		0153},
    {"/l",		0154},
    {"/m", 		0155},
    {"/n",		0156},
    {"/o",	 	0157},
    {"/p",	 	0160},
    {"/q",	 	0161},
    {"/r",	 	0162},
    {"/s", 		0163},
    {"/t",		0164},
    {"/u",		0165},
    {"/v", 		0166},
    {"/w",		0167},
    {"/x",	 	0170},
    {"/y",	 	0171},
    {"/z",	 	0172},
    {"/braceleft", 	0173},
    {"/bar",		0174},
    {"/braceright",	0175},
    {"/asciitilde", 	0176},
    {NULL,		0177},
    {NULL,		0200},
    {NULL,		0201},
    {NULL,		0202},
    {NULL,		0203},
    {NULL,		0204},
    {NULL,		0205},
    {NULL,		0206},
    {NULL,		0207},
    {NULL,		0210},
    {NULL,		0211},
    {NULL,		0212},
    {NULL,		0213},
    {NULL,		0214},
    {NULL,		0215},
    {NULL,		0216},
    {NULL,		0217},
    {"/dotlessi",	0220},	/* added */
    {"/grave",		0221},	/* added */
    {"/acute",		0222},	/* added */
    {"/circumflex",	0223},	/* added */
    {"/tilde",		0224},	/* added */
    {NULL,		0225},	/* repeated macron */
    {"/breve",		0226},	/* added */
    {"/dotaccent",	0227},	/* added */
    {NULL,		0230},	/* repeated dieresis */
    {NULL,		0231},	
    {"/ring",		0232},	/* added */
    {NULL,		0233},	/* repeated cedilla */
    {NULL,		0234},
    {"/hungarumlaut",	0235},	/* added */
    {"/ogonek",		0236},	/* added */
    {"/caron",		0237},	/* added */
    {NULL,		0240},	/* repeated space */
    {"/exclamdown", 	0241},
    {"/cent",	 	0242},
    {"/sterling",	0243},
    {"/currency",	0244},
    {"/yen",		0245},
    {"/brokenbar",	0246},
    {"/section",		0247},
    {"/dieresis",	0250},
    {"/copyright",	0251},
    {"/ordfeminine",	0252},
    {"/guillemotleft",	0253},
    {"/logicalnot",	0254},
    {"/hyphen",		0255},
    {"/registered",	0256},
    {"/macron",		0257},
    {"/degree",		0260},
    {"/plusminus",	0261},
    {"/twosuperior",	0262},
    {"/threesuperior",	0263},
    {"/acute",		0264},
    {"/mu",		0265},
    {"/paragraph",	0266},
    {"/periodcentered",	0267},
    {"/cedilla",		0270},
    {"/onesuperior",	0271},
    {"/ordmasculine",	0272},
    {"/guillemotright",	0273},
    {"/onequarter",	0274},
    {"/onehalf",		0275},
    {"/threequarters",	0276},
    {"/questiondown",	0277},
    {"/Agrave",		0300},
    {"/Aacute",		0301},
    {"/Acircumflex",	0302},
    {"/Atilde",		0303},
    {"/Adieresis",	0304},
    {"/Aring",		0305},
    {"/AE",		0306},
    {"/Ccedilla",	0307},
    {"/Egrave",		0310},
    {"/Eacute",		0311},
    {"/Ecircumflex",	0312},
    {"/Edieresis",	0313},
    {"/Igrave",		0314},
    {"/Iacute",		0315},
    {"/Icircumflex",	0316},
    {"/Idieresis",	0317},
    {"/Eth",		0320},
    {"/Ntilde",		0321},
    {"/Ograve",		0322},
    {"/Oacute",		0323},
    {"/Ocircumflex",	0324},
    {"/Otilde",		0325},
    {"/Odieresis",	0326},
    {"/multiply",	0327},
    {"/Oslash",		0330},
    {"/Ugrave",		0331},
    {"/Uacute",		0332},
    {"/Ucircumflex",	0333},
    {"/Udieresis",	0334},
    {"/Yacute",		0335},
    {"/Thorn",		0336},
    {"/germandbls",	0337},
    {"/agrave",		0340},
    {"/aacute",		0341},
    {"/acircumflex",	0342},
    {"/atilde",		0343},
    {"/adieresis",	0344},
    {"/aring",		0345},
    {"/ae",		0346},
    {"/ccedilla",	0347},
    {"/egrave",		0350},
    {"/eacute",		0351},
    {"/ecircumflex",	0352},
    {"/edieresis",	0353},
    {"/igrave",		0354},
    {"/iacute",		0355},
    {"/icircumflex",	0356},
    {"/idieresis",	0357},
    {"/eth",		0360},
    {"/ntilde",		0361},
    {"/ograve",		0362},
    {"/oacute",		0363},
    {"/ocircumflex",	0364},
    {"/otilde",		0365},
    {"/odieresis",	0366},
    {"/divide",		0367},
    {"/oslash",		0370},
    {"/ugrave",		0371},
    {"/uacute",		0372},
    {"/ucircumflex",	0373},
    {"/udieresis",	0374},
    {"/yacute",		0375},
    {"/thorn",		0376},
    {"/ydieresis",	0377}
};

const pschar ZFontType1::scharlist_[NSYMBL] = {
    {"/space",		040},
    {"/exclam",		041},
    {"/universal",	042},
    {"/numbersign",	043},
    {"/existential",	044},
    {"/percent",		045},
    {"/ampersand",	046},
    {"/suchthat",	047},
    {"/parenleft",	050},
    {"/parenright",	051},
    {"/asteriskmath",	052},
    {"/plus",		053},
    {"/comma",		054},
    {"/minus",		055},
    {"/period",		056},
    {"/slash",		057},
    {"/zero",		060},
    {"/one",		061},
    {"/two",		062},
    {"/three",		063},
    {"/four",		064},
    {"/five",		065},
    {"/six",		066},
    {"/seven",		067},
    {"/eight",		070},
    {"/nine",		071},
    {"/colon",		072},
    {"/semicolon",	073},
    {"/less",		074},
    {"/equal",		075},
    {"/greater",		076},
    {"/question",	077},
    {"/congruent",	0100},
    {"/Alpha",		0101},
    {"/Beta",		0102},
    {"/Chi",		0103},
    {"/Delta",		0104},
    {"/Epsilon",		0105},
    {"/Phi",		0106},
    {"/Gamma",		0107},
    {"/Eta",		0110},
    {"/Iota",		0111},
    {"/theta1",		0112},
    {"/Kappa",		0113},
    {"/Lambda",		0114},
    {"/Mu",		0115},
    {"/Nu",		0116},
    {"/Omicron",		0117},
    {"/Pi",		0120},
    {"/Theta",		0121},
    {"/Rho",		0122},
    {"/Sigma",		0123},
    {"/Tau",		0124},
    {"/Upsilon",		0125},
    {"/sigma1",		0126},
    {"/Omega",		0127},
    {"/Xi",		0130},
    {"/Psi",		0131},
    {"/Zeta",		0132},
    {"/bracketleft",	0133},
    {"/therefore",	0134},
    {"/bracketright",	0135},
    {"/perpendicular",	0136},
    {"/underscore",	0137},
    {"/radicalex",	0140},
    {"/alpha",		0141},
    {"/beta",		0142},
    {"/chi",		0143},
    {"/delta",		0144},
    {"/epsilon",		0145},
    {"/phi",		0146},
    {"/gamma",		0147},
    {"/eta",		0150},
    {"/iota",		0151},
    {"/phi1",		0152},
    {"/kappa",		0153},
    {"/lambda",		0154},
    {"/mu",		0155},
    {"/nu",		0156},
    {"/omicron",		0157},
    {"/pi",		0160},
    {"/theta",		0161},
    {"/rho",		0162},
    {"/sigma",		0163},
    {"/tau",		0164},
    {"/upsilon",		0165},
    {"/omega1",		0166},
    {"/omega",		0167},
    {"/xi",		0170},
    {"/psi",		0171},
    {"/zeta",		0172},
    {"/braceleft",	0173},
    {"/bar",		0174},
    {"/braceright",	0175},
    {"/similar",		0176},
    {NULL,               0177},
    {NULL,               0200},
    {NULL,               0201},
    {NULL,               0202},
    {NULL,               0203},
    {NULL,               0204},
    {NULL,               0205},
    {NULL,               0206},
    {NULL,               0207},
    {NULL,               0210},
    {NULL,               0211},
    {NULL,               0212},
    {NULL,               0213},
    {NULL,               0214},
    {NULL,               0215},
    {NULL,               0216},
    {NULL,               0217},
    {NULL,               0220},
    {NULL,               0221},
    {NULL,               0222},
    {NULL,               0223},
    {NULL,               0224},
    {NULL,               0225},
    {NULL,               0226},
    {NULL,               0227},
    {NULL,               0230},
    {NULL,               0231},
    {NULL,               0232},
    {NULL,               0233},
    {NULL,               0234},
    {NULL,               0235},
    {NULL,               0236},
    {NULL,               0237},
    {NULL,               0240},
    {"/Upsilon1",	0241},
    {"/minute",		0242},
    {"/lessequal",	0243},
    {"/fraction",	0244},
    {"/infinity",	0245},
    {"/florin",		0246},
    {"/club",		0247},
    {"/diamond",		0250},
    {"/heart",		0251},
    {"/spade",		0252},
    {"/arrowboth",	0253},
    {"/arrowleft",	0254},
    {"/arrowup",		0255},
    {"/arrowright",	0256},
    {"/arrowdown",	0257},
    {"/degree",		0260},
    {"/plusminus",	0261},
    {"/second",		0262},
    {"/greaterequal",	0263},
    {"/multiply",	0264},
    {"/proportional",	0265},
    {"/partialdiff",	0266},
    {"/bullet",		0267},
    {"/divide",		0270},
    {"/notequal",	0271},
    {"/equivalence",	0272},
    {"/approxequal",	0273},
    {"/ellipsis",	0274},
    {"/arrowvertex",	0275},
    {"/arrowhorizex",	0276},
    {"/carriagereturn",	0277},
    {"/aleph",		0300},
    {"/Ifraktur",	0301},
    {"/Rfraktur",	0302},
    {"/weierstrass",	0303},
    {"/circlemultiply",	0304},
    {"/circleplus",	0305},
    {"/emptyset",	0306},
    {"/intersection",	0307},
    {"/union",		0310},
    {"/propersuperset",	0311},
    {"/reflexsuperset",	0312},
    {"/notsubset",	0313},
    {"/propersubset",	0314},
    {"/reflexsubset",	0315},
    {"/element",		0316},
    {"/notelement",	0317},
    {"/angle",		0320},
    {"/gradient",	0321},
    {"/registerserif",	0322},
    {"/copyrightserif",	0323},
    {"/trademarkserif",	0324},
    {"/product",		0325},
    {"/radical",		0326},
    {"/dotmath",		0327},
    {"/logicalnot",	0330},
    {"/logicaland",	0331},
    {"/logicalor",	0332},
    {"/arrowdblboth",	0333},
    {"/arrowdblleft",	0334},
    {"/arrowdblup",	0335},
    {"/arrowdblright",	0336},
    {"/arrowdbldown",	0337},
    {"/lozenge",		0340},
    {"/angleleft",	0341},
    {"/registersans",	0342},
    {"/copyrightsans",	0343},
    {"/trademarksans",	0344},
    {"/summation",	0345},
    {"/parenlefttp",	0346},
    {"/parenleftex",	0347},
    {"/parenleftbt",	0350},
    {"/bracketlefttp",	0351},
    {"/bracketleftex",	0352},
    {"/bracketleftbt",	0353},
    {"/bracelefttp",	0354},
    {"/braceleftmid",	0355},
    {"/braceleftbt",	0356},
    {"/braceex",		0357},
    {"/apple",           0360}, 
    {"/angleright",	0361},
    {"/integral",	0362},
    {"/integraltp",	0363},
    {"/integralex",	0364},
    {"/integralbt",	0365},
    {"/parenrighttp",	0366},
    {"/parenrightex",	0367},
    {"/parenrightbt",	0370},
    {"/bracketrighttp",	0371},
    {"/bracketrightex",	0372},
    {"/bracketrightbt",	0373},
    {"/bracerighttp",	0374},
    {"/bracerightmid",	0375},
    {"/bracerightbt",	0376},
    {NULL,               0377}
};

const pschar ZFontType1::zcharlist_[NZAPFD] = {
    {"/space",		040},
    {"/a1",		041},
    {"/a2",		042},
    {"/a202",		043},
    {"/a3",		044},
    {"/a4",		045},
    {"/a5",		046},
    {"/a119",		047},
    {"/a118",		050},
    {"/a117",		051},
    {"/a11",		052},
    {"/a12",		053},
    {"/a13",		054},
    {"/a14",		055},
    {"/a15",		056},
    {"/a16",		057},
    {"/a105",		060},
    {"/a17",		061},
    {"/a18",		062},
    {"/a19",		063},
    {"/a20",		064},
    {"/a21",		065},
    {"/a22",		066},
    {"/a23",		067},
    {"/a24",		070},
    {"/a25",		071},
    {"/a26",		072},
    {"/a27",		073},
    {"/a28",		074},
    {"/a6",		075},
    {"/a7",		076},
    {"/a8",		077},
    {"/a9",		0100},
    {"/a10",		0101},
    {"/a29",		0102},
    {"/a30",		0103},
    {"/a31",		0104},
    {"/a32",		0105},
    {"/a33",		0106},
    {"/a34",		0107},
    {"/a35",		0110},
    {"/a36",		0111},
    {"/a37",		0112},
    {"/a38",		0113},
    {"/a39",		0114},
    {"/a40",		0115},
    {"/a41",		0116},
    {"/a42",		0117},
    {"/a43",		0120},
    {"/a44",		0121},
    {"/a45",		0122},
    {"/a46",		0123},
    {"/a47",		0124},
    {"/a48",		0125},
    {"/a49",		0126},
    {"/a50",		0127},
    {"/a51",		0130},
    {"/a52",		0131},
    {"/a53",		0132},
    {"/a54",		0133},
    {"/a55",		0134},
    {"/a56",		0135},
    {"/a57",		0136},
    {"/a58",		0137},
    {"/a59",		0140},
    {"/a60",		0141},
    {"/a61",		0142},
    {"/a62",		0143},
    {"/a63",		0144},
    {"/a64",		0145},
    {"/a65",		0146},
    {"/a66",		0147},
    {"/a67",		0150},
    {"/a68",		0151},
    {"/a69",		0152},
    {"/a70",		0153},
    {"/a71",		0154},
    {"/a72",		0155},
    {"/a73",		0156},
    {"/a74",		0157},
    {"/a203",		0160},
    {"/a75",		0161},
    {"/a204",		0162},
    {"/a76",		0163},
    {"/a77",		0164},
    {"/a78",		0165},
    {"/a79",		0166},
    {"/a81",		0167},
    {"/a82",		0170},
    {"/a83",		0171},
    {"/a84",		0172},
    {"/a97",		0173},
    {"/a98",		0174},
    {"/a99",		0175},
    {"/a100",		0176},
    {NULL,               0177},
    {NULL,               0200},
    {NULL,               0201},
    {NULL,               0202},
    {NULL,               0203},
    {NULL,               0204},
    {NULL,               0205},
    {NULL,               0206},
    {NULL,               0207},
    {NULL,               0210},
    {NULL,               0211},
    {NULL,               0212},
    {NULL,               0213},
    {NULL,               0214},
    {NULL,               0215},
    {NULL,               0216},
    {NULL,               0217},
    {NULL,               0220},
    {NULL,               0221},
    {NULL,               0222},
    {NULL,               0223},
    {NULL,               0224},
    {NULL,               0225},
    {NULL,               0226},
    {NULL,               0227},
    {NULL,               0230},
    {NULL,               0231},
    {NULL,               0232},
    {NULL,               0233},
    {NULL,               0234},
    {NULL,               0235},
    {NULL,               0236},
    {NULL,               0237},
    {NULL,               0240},
    {"/a101",		0241},
    {"/a102",		0242},
    {"/a103",		0243},
    {"/a104",		0244},
    {"/a106",		0245},
    {"/a107",		0246},
    {"/a108",		0247},
    {"/a112",		0250},
    {"/a111",		0251},
    {"/a110",		0252},
    {"/a109",		0253},
    {"/a120",		0254},
    {"/a121",		0255},
    {"/a122",		0256},
    {"/a123",		0257},
    {"/a124",		0260},
    {"/a125",		0261},
    {"/a126",		0262},
    {"/a127",		0263},
    {"/a128",		0264},
    {"/a129",		0265},
    {"/a130",		0266},
    {"/a131",		0267},
    {"/a132",		0270},
    {"/a133",		0271},
    {"/a134",		0272},
    {"/a135",		0273},
    {"/a136",		0274},
    {"/a137",		0275},
    {"/a138",		0276},
    {"/a139",		0277},
    {"/a140",		0300},
    {"/a141",		0301},
    {"/a142",		0302},
    {"/a143",		0303},
    {"/a144",		0304},
    {"/a145",		0305},
    {"/a146",		0306},
    {"/a147",		0307},
    {"/a148",		0310},
    {"/a149",		0311},
    {"/a150",		0312},
    {"/a151",		0313},
    {"/a152",		0314},
    {"/a153",		0315},
    {"/a154",		0316},
    {"/a155",		0317},
    {"/a156",		0320},
    {"/a157",		0321},
    {"/a158",		0322},
    {"/a159",		0323},
    {"/a160",		0324},
    {"/a161",		0325},
    {"/a163",		0326},
    {"/a164",		0327},
    {"/a196",		0330},
    {"/a165",		0331},
    {"/a192",		0332},
    {"/a166",		0333},
    {"/a167",		0334},
    {"/a168",		0335},
    {"/a169",		0336},
    {"/a170",		0337},
    {"/a171",		0340},
    {"/a172",		0341},
    {"/a173",		0342},
    {"/a162",		0343},
    {"/a174",		0344},
    {"/a175",		0345},
    {"/a176",		0346},
    {"/a177",		0347},
    {"/a178",		0350},
    {"/a179",		0351},
    {"/a193",		0352},
    {"/a180",		0353},
    {"/a199",		0354},
    {"/a181",		0355},
    {"/a200",		0356},
    {"/a182",		0357},
    {NULL,               0360},
    {"/a201",		0361},
    {"/a183",		0362},
    {"/a184",		0363},
    {"/a197",		0364},
    {"/a185",		0365},
    {"/a194",		0366},
    {"/a198",		0367},
    {"/a186",		0370},
    {"/a195",		0371},
    {"/a187",		0372},
    {"/a188",		0373},
    {"/a189",		0374},
    {"/a190",		0375},
    {"/a191",		0376},
    {NULL,               0377}
};

ZFontType1::ZFontType1(const char *infont)
{
  int i;
  int k;

  /* Make the hex table. */
  if (!hextab_initialized_) {
    hextab_initialized_ = TRUE;
    for (i = 0; i < 256; i++) {
      if (i >= '0' && i <= '9') hextab_[i] = i - '0';
      else if (i >= 'a' && i <= 'f') hextab_[i] = 10 + i - 'a';
      else if (i >= 'A' && i <= 'F') hextab_[i] = 10 + i - 'A';
      else hextab_[i] = NOTHEX;
    }
  }

  bzero(charprog_, NASCII * sizeof(charprog_[0]));
  bzero(scharprog_, NSYMBL * sizeof(scharprog_[0]));
  bzero(zcharprog_, NZAPFD * sizeof(zcharprog_[0]));
  bzero(accentprog_, NACCENT * sizeof(accentprog_[0]));

  /* open the input file */
  FILE *inf = fopen(infont, "r");
  if (!inf) {
      fprintf(stderr, "Can't open Adobe Type 1 font file: %s\n", infont);
      _loaded = FALSE;
      return;
  }
  _loaded = TRUE;
 
  /* Read the font matrix and font name. */
  readfontmatrix(inf);

  /* Decode the font data and leave it in the decoded_ buffer. */
  decode(inf);
  fclose(inf);

  /* init the charname, charlen and sublen_ arrays */
  nchars_ = 0;
  for (int c = 0; c < MAXCHARS; c++) {
    charname_[c] = 0;
    charlen_[c] = 0;
  }
  for (int s = 0; s < MAXSUBRS; s++) sublen_[s] = 0;

  /* look for the /Subrs def and get nsubrs_ */
  while(1) {
    char oneline[LINELEN];
    if (!decoded_.fgets(oneline, LINELEN)) {
      fprintf(stderr, "fromtype1: no /Subrs found\n");
      exit(1);
    }
    char *cptr = strchr(oneline, '/');
    if (cptr) {
      if (strncmp(cptr, "/Subrs", 6) == 0) {
	nsubrs_ = atoi(cptr + 6);
	break;
      }
    }
  }

  /* read the Subrs in one by one */
  char tok[LINELEN];

  for (i=0; i<nsubrs_; i++) 
    sublen_[i] = 0;
  for (i=0; i<nsubrs_; i++) {
    for (k=0; k<MAXTRIES; k++) {
      decoded_.gettoken(tok);
      if (strcmp(tok, "dup") == 0)
	break;
    }
    if (k == MAXTRIES) {
      fprintf(stderr, "dup for subr %s not found in range\n", tok);
      exit(1);
    }

    /* get the Subr index here */
    decoded_.gettoken(tok);
    int index = atoi(tok);

    /* check to make sure it is in range */
    if (index < 0 || index > nsubrs_) {
      fprintf(stderr, "bad Subr index %d\n", index);
      exit(1);
    }

    /* get the number of bytes to read */
    decoded_.gettoken(tok);
    int nread = atoi(tok);
    decoded_.gettoken(tok);

    /* read in the subroutine */
    sublen_[index] = nread;
    subrs_[index] = decoded_.fread(nread);
    decoded_.gettoken(tok);
  }

  /* look for the CharStrings */
  while(1) {
    decoded_.gettoken(tok);
    char *cptr = strchr(tok, '/');
    if (cptr && strcmp(cptr, "/CharStrings") == 0)
      break;
  }

  decoded_.gettoken(tok);	/* skip ncharscrings */
  decoded_.gettoken(tok);	/* skip dict */
  decoded_.gettoken(tok);	/* skip dup */
  decoded_.gettoken(tok);	/* skip begin */
  decoded_.gettoken(tok);	/* skip newline */

  /* read the CharStrings one by one */
  for (i=0; i<MAXCHARS; i++) { 

    /* check for end */
    decoded_.gettoken(tok);
    if (strcmp(tok, "end") == 0) 
      break;

    /* get the char name and allocate space for it */
    charname_[i] = (char *)malloc(strlen(tok) + 1);
    strcpy(charname_[i], tok);

    /* get the number of bytes to read */
    decoded_.gettoken(tok);
    int nread = atoi(tok);
    decoded_.gettoken(tok);

    /* read in the char description */
    charlen_[i] = nread;
    chars_[i] = decoded_.fread(nread);

    /* skip the end of line */
    decoded_.gettoken(tok);
    decoded_.gettoken(tok);
    nchars_++;
  }

  /* decrypt the character descriptions */
  decryptall();
  setcharlist();
}

Pad_Bool
ZFontType1::Is_loaded(void)
{
    return _loaded;
}

ZFontPoly *
ZFontType1::GenFontPoly(double beztol, Pad_Bool fullset)
{
  int i, c;
  int maxchars;

  ZFontType1InterpPoly interp(*this, beztol);
  ZFontPoly *fnt;

  if (strncmp(fname_, "Symbol", 6) == 0) {
    fnt = new ZFontPoly(32, 32 + NSYMBL - 1, 1000);
    for (i = 0; i < NSYMBL; i++) {
      c = i + 32;
      if (scharprog_[i] >= 0) {
	interp.reset(fnt, c);
	interp.drawchar(scharprog_[i], c, *fnt);
      }
      else if (c == ' ') {
	printf("faking space %d\n", i);
	fnt->fakechar(' ', 400);
      }
    }
  }
  else {
    if (strncmp(fname_, "ZapfDingbats", 12) == 0) {
      fnt = new ZFontPoly(32, 32 + NZAPFD - 1, 1000);
      for (i=0; i<NZAPFD; i++) {
	c = i+32;
	if (zcharprog_[i] >= 0) {
	  interp.reset(fnt, c);
	  interp.drawchar(zcharprog_[i], c, *fnt);
	}
	else 
	  if (c == ' ') {
	    printf("faking space %d\n", i);
	    fnt->fakechar(' ', 400);
	  }
      }
    }
    else {

      maxchars = 0;
      for (i=0; i<NASCII; i++) {
	if (charprog_[i] >= 0)
	  maxchars = i;
      }
      maxchars++;

      if (maxchars > NASCII) maxchars = NASCII;
      if (!fullset) {
	if (maxchars > 95) maxchars = 95;
      }

      fnt = new ZFontPoly(32, 32 + maxchars - 1, 1000);
      for (i = 0; i < maxchars; i++) {
	c = i + 32;
	if (charprog_[i] >= 0) {
	  interp.reset(fnt, c);
	  interp.drawchar(charprog_[i], c, *fnt);
	}
	else if (c == ' ') {
	  printf("faking space %d\n", i);
	  fnt->fakechar(' ', 400);
	}
      }
    } 
  }
  fnt->calccharbboxes();
  return fnt;
}

ZFontSpline *
ZFontType1::GenFontSpline(double beztol, Pad_Bool fullset)
{
  int i, c;
  int maxchars;

  ZFontSpline *fnt;
  ZFontType1InterpSpline interp(*this, beztol);

  if (strncmp(fname_, "Symbol", 6) == 0) {
    fnt = new ZFontSpline(32, 32 + NSYMBL - 1, 1000);
    for (i = 0; i < NSYMBL; i++) {
      c = i + 32;
      if (scharprog_[i] >= 0) {
	interp.reset(fnt, c);
	interp.drawchar(scharprog_[i], c, *fnt);
      }
      else if (c == ' ') {
	printf("faking space %d\n", i);
	fnt->fakechar(' ', 400);
      }
    }
  }
  else {
    if (strncmp(fname_, "ZapfDingbats", 12) == 0) {
      fnt = new ZFontSpline(32, 32 + NZAPFD - 1, 1000);
      for (i=0; i<NZAPFD; i++) {
	c = i + 32;
	if (zcharprog_[i] >= 0) {
	  interp.reset(fnt, c);
	  interp.drawchar(zcharprog_[i], c, *fnt);
	}
	else if (c == ' ') {
	  printf("faking space %d\n", i);
	  fnt->fakechar(' ', 400);
	}
      }
    }
    else {

      maxchars = 0;
      for (i=0; i<NASCII; i++) {
	if (charprog_[i] >= 0)
	  maxchars = i;
      }
      maxchars++;

      if (maxchars > NASCII) maxchars = NASCII;
      if (!fullset) {
	if (maxchars > 95) maxchars = 95;
      }

      fnt = new ZFontSpline(32, 32 + maxchars - 1, 1000);
      for (i = 0; i<maxchars; i++) {
	c = i + 32;
	if (charprog_[i] >= 0) {
	  interp.reset(fnt, c);
	  interp.drawchar(charprog_[i], c, *fnt);
	}
	else if (c == ' ') {
	  printf("faking space %d\n", i);
	  fnt->fakechar(' ', 400);
	}
      }
    } 
  }
  fnt->calccharbboxes();
  return fnt;
}

/*
 *	read the font matrix out of the font file
 *	update global fname with the actual font name.
 */

void 
ZFontType1::readfontmatrix(FILE *inf)
{
  double a, b, c, d, e, f;

  *fname_ = '\0';
  /* look for the FontName and FontMatrix def */
  while(1) {
    char oneline[LINELEN];
    if (!fgets(oneline, LINELEN, inf)) {
      fprintf(stderr, "fromtype1: no FontMatrix found\n");
      exit(1);
    }
    char *cptr = strchr(oneline, '/');
    if (cptr) {
      if (strncmp(cptr, "/FontName", 9) == 0) {
	cptr = strchr(cptr+1, '/');
	if (!cptr) {
	  fprintf(stderr, "fromtype1: bad FontName line\n");
	  exit(1);
	}
	sscanf(cptr+1, "%s\n", fname_);
      } else {
	if (strncmp(cptr, "/FontMatrix", 11) == 0) {
	  cptr = strchr(cptr, '[');
	  if (!cptr) {
	    fprintf(stderr, "fromtype1: bad FontMatrix line\n");
	    exit(1);
	  }
	  sscanf(cptr+1, "%lf %lf %lf %lf %lf %lf\n", &a, &b, &c, &d, &e, &f);
	  if (*fname_)
	    break;
	}
      }
    }
  }
  if (!*fname_) {
    fprintf(stderr, "fromtype1: bad FontName\n");
    exit(1);
  }

  mat_[0][0] = 1000.0 * a; 
  mat_[0][1] = 1000.0 * c; 
  mat_[1][0] = 1000.0 * b; 
  mat_[1][1] = 1000.0 * d; 
}

static int sizeoffile(FILE *f)
{
  struct stat statbuf;

  if (fstat(fileno(f), &statbuf)) {
    fprintf(stderr, "sizeoffile: stat failed\n");
    return 0;
  }
  return statbuf.st_size;
}

void 
ZFontType1::decode(FILE *inf)
{
  /* allocate buffers */
  int totlen = sizeoffile(inf);
  Buffer hex;
  Buffer binary;
  hex.allocate(totlen);
  binary.allocate(totlen / 2);
  decoded_.allocate(totlen / 2);

  char oneline[LINELEN];
  /* look for eexec part of file */
  while(1) {
    if (!fgets(oneline, LINELEN, inf)) {
      fprintf(stderr, "fromtype1: no currentfile eexec found\n");
      exit(1);
    }
    // luxi fonts have \r rather than \n after eexec
    // postscript spec allows either \r or \n as newline
    // yes, this is a hack - gehngis
    if (strncmp(oneline, "currentfile eexec", 17) == 0) {
      if (oneline[17] == '\r') {
        long off = strlen(oneline) - 18;
        fseek(inf, -off, SEEK_CUR);
      }
      break;
    }
  }

  readhex(inf, hex);

  /* translate hex data to binary */
  for (int c = 0; c < hex.count(); c += 2)
    binary.append(hextab_[hex.get(c)]<<4 | hextab_[hex.get(c+1)]);

  /* decrypt the data */
  resetdecrypt(55665);
  decoded_.allocate(binary.count());
  decoded_.setcount(binary.count());
  decryptdata(binary.data(), decoded_.data(), binary.count());
#if 0
  if (debugtype1) {
    FILE *outf = fopen("debug.bin", "w");
    fwrite(decoded_.data() + 4, decoded_.count() - 4, 1, outf);
    fclose(outf);
  }
#endif
}

void 
ZFontType1::readhex(FILE *inf, Buffer &hexdat)
{
  char oneline[LINELEN];
  /* read all the hex bytes into the hex buffer */
  while(fgets(oneline, LINELEN, inf)) {
    for (unsigned char *hptr = (unsigned char *)oneline; *hptr; hptr++)
      if (hextab_[*hptr] != NOTHEX) hexdat.append(*hptr);
  }

  /* check number of hex bytes */
  if (hexdat.count() & 1) hexdat.pop();
}

void 
ZFontType1::decryptall()
{
  int i;

  for (i=0; i<nsubrs_; i++)
    sublen_[i] = decryptprogram(subrs_[i], sublen_[i]);
  for (i=0; i<nchars_; i++)
    charlen_[i] = decryptprogram(chars_[i], charlen_[i]);
}

int 
ZFontType1::decryptprogram(unsigned char *buf, int len)
{
  resetdecrypt(4330);
  if (len < 4) {
    printf("bad char program\n");
    exit(1);
  }
  decryptdata(buf, buf, SKIP);
  decryptdata(buf + SKIP, buf, len - SKIP);
  return len - SKIP;
}

void 
ZFontType1::resetdecrypt(int n)
{
  mr_ = n;
}

void 
ZFontType1::decryptdata(unsigned char *iptr, unsigned char *optr, int n)
{
  unsigned char in;

  unsigned short mymr = mr_;
  unsigned short c1 = 52845;
  unsigned short c2 = 22719;
  while (n > 8) {
    in = iptr[0];
    optr[0] = (in ^ (mymr >> 8));
    mymr = (in + mymr) * c1 + c2;
    in = iptr[1];
    optr[1] = (in ^ (mymr >> 8));
    mymr = (in + mymr) * c1 + c2;
    in = iptr[2];
    optr[2] = (in ^ (mymr >> 8));
    mymr = (in + mymr) * c1 + c2;
    in = iptr[3];
    optr[3] = (in ^ (mymr >> 8));
    mymr = (in + mymr) * c1 + c2;
    in = iptr[4];
    optr[4] = (in ^ (mymr >> 8));
    mymr = (in + mymr) * c1 + c2;
    in = iptr[5];
    optr[5] = (in ^ (mymr >> 8));
    mymr = (in + mymr) * c1 + c2;
    in = iptr[6];
    optr[6] = (in ^ (mymr >> 8));
    mymr = (in + mymr) * c1 + c2;
    in = iptr[7];
    optr[7] = (in ^ (mymr >> 8));
    mymr = (in + mymr) * c1 + c2;
    n -= 8;
    optr += 8;
    iptr += 8;
  }
  while(n--) {
    in = iptr[0];
    optr[0] = (in ^ (mymr >> 8));
    mymr = (in + mymr) * c1 + c2;
    optr++;
    iptr++;
  }
  mr_ = mymr;
}

void 
ZFontType1::setcharlist()
{
  char *name;
  int i, j;

  if (!strncmp(fname_, "Symbol", 6)) {
    for (i = 0; i < NSYMBL; i++) 
      scharprog_[i] = -1;
    for (i = 0; i < NSYMBL - 1; i++) {
      name = scharlist_[i].name;
      if (name) {
	for (j = 0; j < nchars_; j++) {
	  if (charname_[j] && (!strcmp(name, charname_[j])))
	    scharprog_[i] = j;
	}
      }
    }
  } else {
    if (strncmp(fname_, "ZapfDingbats", 12) == 0) {
      for (i=0; i<NZAPFD; i++) 
	zcharprog_[i] = -1;
      for (i=0; i<NZAPFD-1; i++) {
	name = zcharlist_[i].name;
	if (name) {
	  for (j=0; j<nchars_; j++) {
	    if (charname_[j] && (!strcmp(name, charname_[j])))
	      zcharprog_[i] = j;
	  }
	}
      }
    } else {
      for (i=0; i < ACCENTBASE + NACCENT; i++) 
	charprog_[i] = -1;
      for (i=0; i < NASCII; i++) {
	name = charlist_[i].name;
	if (name) {
	  for (j=0; j<nchars_; j++) {
	    if (charname_[j] && (!strcmp(name, charname_[j])))
	      charprog_[i] = j;
	  }
	}
      }
      for (i = 0; i < NACCENT; i++) {
	name = accentlist_[i].name;
	if (name) {
	  for (j = 0; j < nchars_; j++) {
	    if (charname_[j] && (!strcmp(name, charname_[j]))) {
	      charprog_[i + ACCENTBASE] = j;
	    }
	  }
	}
      }
    }
  }
}
