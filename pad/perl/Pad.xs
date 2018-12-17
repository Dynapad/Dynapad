#ifdef __cplusplus
extern "C" {
#endif
#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"
#ifdef __cplusplus
}
#endif
#include "../generic/pad-perl.h"
#include "boot.h"

MODULE = Pad		PACKAGE = Pad		

void
pad_foo()
	CODE:
	Pad_perl_foo();

BOOT:
	{
	   Boot_glue();
        }
