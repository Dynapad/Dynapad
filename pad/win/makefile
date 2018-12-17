#
# Makefile to build Pad++ for Windows95 and WindowsNT
# using Visual C++.  Tested with versions 4.0 and 4.2.
# This works only for Tcl/Tk version 7.6/4.2.
#
# Change the following variables to match your system
#
# (c) 1997 Pad++ Consortium
#
MSDEV    = \msdev
TCL_HOME = \tcl7.6
TK_HOME  = \tk4.2
PADHOME  = \pad-09

TCL_VERSION = tcl7.6
TK_VERSION  = tk4.2
TCL_LIBNAME = tcl76
TK_LIBNAME  = tk42

# If you are using Visual C++ Version 4.2, you must
# uncomment out the following line.  For version 4.0,
# it should remain commented:
MSDEV42_LIB = msvcirt.lib

# Comment the following line to compile with symbols for debugging
NODEBUG	= 1

# uncomment the following two lines to compile with TCL_MEM_DEBUG
#DEBUGDEFINES	= -DTCL_MEM_DEBUG


##########################################################
##########################################################
# The following part of the file should not need
# to be modified.
##########################################################
##########################################################

TOOLS32	    = $(MSDEV)
MFC         = $(MSDEV)\mfc
MFCSRC      = $(MSDEV)\mfc\src

# Flags to be passed on to Pad++ while compiling
#
PAD_FLAGS   = -DPAD_WIN -DPAD_TK

# KPL Files
#
KPL_LIB     = $(PADHOME)\kpl\bin-win\kpl.lib
KPL_DLL     = $(PADHOME)\kpl\bin-win\kpl.dll

# Make sure the VC++ tools are at the head of the path
#
PATH=$(TOOLS32)\bin;$(PATH)

cc32   = $(TOOLS32)\bin\cl -O2 -I$(TOOLS32)\include -I$(MFC)\include -I$(MFCSRC) $(PAD_FLAGS)
rc32   = $(TOOLS32)\bin\rc
link32 = $(TOOLS32)\bin\link

TK_INCLUDES = -I$(TOOLS32)\include -I$(TK_HOME)\win -I$(TK_HOME)\generic -I$(TK_HOME) \
              -I$(TK_HOME)\bitmaps -I$(TK_HOME)\Xlib -I$(TK_HOME)\Xlib\X11 -I$(TK_HOME)\compat \
              -I$(TCL_HOME)\generic -I$(TCL_HOME)\win -I$(PADHOME)\kpl\src -I.

# Library/Executable files
#
TCL_LIB	= $(TCL_HOME)\win\$(TCL_LIBNAME).lib
TK_LIB  = $(TK_HOME)\win\$(TK_LIBNAME).lib
PAD_DLL	= $(PADHOME)\bin-win\pad.dll
PAD_LIB	= $(PADHOME)\bin-win\pad.lib
KPL_LIB = $(PADHOME)\kpl\bin-win\kpl.lib
PADWISH	= padwish.exe
WINPAD  = pad-09b

!include	"$(MSDEV)\include\win32.mak"

#
# This is the list of source files, and the corresponding object
# files are created from them.
#
MAINOBJS = \
	$(PADHOME)\bin-win\tkConsole.obj \
	$(PADHOME)\bin-win\winMain.obj

GENERIC_OBJS = \
	$(PADHOME)\bin-win\alias.obj \
	$(PADHOME)\bin-win\alist.obj \
	$(PADHOME)\bin-win\anim-path.obj \
	$(PADHOME)\bin-win\anim-channel.obj \
	$(PADHOME)\bin-win\anim.obj \
	$(PADHOME)\bin-win\api.obj \
	$(PADHOME)\bin-win\bbox.obj \
	$(PADHOME)\bin-win\bind.obj \
	$(PADHOME)\bin-win\border.obj \
	$(PADHOME)\bin-win\Buffer.obj \
	$(PADHOME)\bin-win\button.obj \
	$(PADHOME)\bin-win\cache.obj \
	$(PADHOME)\bin-win\callback.obj \
	$(PADHOME)\bin-win\clist.obj \
	$(PADHOME)\bin-win\color.obj \
	$(PADHOME)\bin-win\colordb.obj \
	$(PADHOME)\bin-win\colorcube.obj \
	$(PADHOME)\bin-win\component.obj \
	$(PADHOME)\bin-win\container.obj \
	$(PADHOME)\bin-win\curves.obj \
	$(PADHOME)\bin-win\display.obj \
	$(PADHOME)\bin-win\drawimage.obj \
	$(PADHOME)\bin-win\events.obj \
	$(PADHOME)\bin-win\effect.obj \
	$(PADHOME)\bin-win\flist.obj \
	$(PADHOME)\bin-win\font.obj \
	$(PADHOME)\bin-win\fontcache.obj \
	$(PADHOME)\bin-win\fontdata.obj \
	$(PADHOME)\bin-win\gc.obj \
	$(PADHOME)\bin-win\gif.obj \
	$(PADHOME)\bin-win\gif-direct.obj \
	$(PADHOME)\bin-win\global.obj \
	$(PADHOME)\bin-win\grid.obj \
	$(PADHOME)\bin-win\group.obj \
	$(PADHOME)\bin-win\hashtab.obj \
	$(PADHOME)\bin-win\html.obj \
	$(PADHOME)\bin-win\image.obj \
	$(PADHOME)\bin-win\imagedata.obj \
	$(PADHOME)\bin-win\init.obj \
	$(PADHOME)\bin-win\ilist.obj \
	$(PADHOME)\bin-win\jpeg.obj \
	$(PADHOME)\bin-win\line.obj \
	$(PADHOME)\bin-win\line-font.obj \
	$(PADHOME)\bin-win\line-font-data.obj \
	$(PADHOME)\bin-win\list.obj \
	$(PADHOME)\bin-win\menu.obj \
	$(PADHOME)\bin-win\misc.obj \
	$(PADHOME)\bin-win\noisedata.obj \
	$(PADHOME)\bin-win\object.obj \
	$(PADHOME)\bin-win\o2.obj \
	$(PADHOME)\bin-win\o3.obj \
	$(PADHOME)\bin-win\pad.obj \
	$(PADHOME)\bin-win\pad-kpl.obj \
	$(PADHOME)\bin-win\pad-scheme.obj \
	$(PADHOME)\bin-win\pad-sound.obj \
	$(PADHOME)\bin-win\pad-tcl.obj \
	$(PADHOME)\bin-win\pad-tcl-cmd.obj \
	$(PADHOME)\bin-win\pad-tcl-config.obj \
	$(PADHOME)\bin-win\pad-tcl-create.obj \
	$(PADHOME)\bin-win\pad-tcl-sound.obj \
	$(PADHOME)\bin-win\plist.obj \
	$(PADHOME)\bin-win\point.obj \
	$(PADHOME)\bin-win\portal.obj \
	$(PADHOME)\bin-win\renderer.obj \
	$(PADHOME)\bin-win\resource.obj \
	$(PADHOME)\bin-win\restorer.obj \
	$(PADHOME)\bin-win\scrollbar.obj \
	$(PADHOME)\bin-win\slist.obj \
	$(PADHOME)\bin-win\streamdata.obj \
	$(PADHOME)\bin-win\streamitem.obj \
	$(PADHOME)\bin-win\pad-string.obj \
	$(PADHOME)\bin-win\text.obj \
	$(PADHOME)\bin-win\tiff.obj \
	$(PADHOME)\bin-win\tkwin.obj \
	$(PADHOME)\bin-win\trait.obj \
	$(PADHOME)\bin-win\tree.obj \
	$(PADHOME)\bin-win\tree-layout.obj \
	$(PADHOME)\bin-win\tkborder.obj \
	$(PADHOME)\bin-win\tkcolor.obj \
	$(PADHOME)\bin-win\tkkpl.obj \
	$(PADHOME)\bin-win\transform.obj \
	$(PADHOME)\bin-win\view.obj \
	$(PADHOME)\bin-win\win.obj \
	$(PADHOME)\bin-win\winregion.obj \
	$(PADHOME)\bin-win\winX.obj \
	$(PADHOME)\bin-win\winxdr.obj \
	$(PADHOME)\bin-win\xdrintf.obj \
	$(PADHOME)\bin-win\xdrutil.obj \
	$(PADHOME)\bin-win\ZFont.obj \
	$(PADHOME)\bin-win\ZFontType1.obj \
	$(PADHOME)\bin-win\ZFontType1Interp.obj

KPL_OBJS = \
	$(PADHOME)\kpl\bin-win\noise.obj \
	$(PADHOME)\kpl\bin-win\pnoise.obj \
	$(PADHOME)\kpl\bin-win\kpl_exec.obj \
	$(PADHOME)\kpl\bin-win\kpl_makePcode.obj \
	$(PADHOME)\kpl\bin-win\kpl_parse.obj \
	$(PADHOME)\kpl\bin-win\hash.obj \
	$(PADHOME)\kpl\bin-win\kpl_wrap_physics.obj \
	$(PADHOME)\kpl\bin-win\physics.obj \
	$(PADHOME)\kpl\bin-win\kpl_wrap_matrix.obj \
	$(PADHOME)\kpl\bin-win\matrix.obj \
	$(PADHOME)\kpl\bin-win\kpl_wrap_lib.obj \
	$(PADHOME)\kpl\bin-win\lib.obj \
	$(PADHOME)\kpl\bin-win\kpl_wrap_tracking.obj \
	$(PADHOME)\kpl\bin-win\tracking.obj \
	$(PADHOME)\kpl\bin-win\kpl_wrap_tcl.obj \
	$(PADHOME)\kpl\bin-win\kpl_wrap_vec.obj \
	$(PADHOME)\kpl\bin-win\vec.obj \
	$(PADHOME)\kpl\bin-win\tmesh.obj \
	$(PADHOME)\kpl\bin-win\dxf2raw.obj \
	$(PADHOME)\kpl\bin-win\mapping.obj \
	$(PADHOME)\kpl\bin-win\sort_box_list.obj \
	$(PADHOME)\kpl\bin-win\kpl_wrap_sumvec.obj \
	$(PADHOME)\kpl\bin-win\sumvec.obj \
	$(PADHOME)\kpl\bin-win\kpl_wrap_pump.obj \
	$(PADHOME)\kpl\bin-win\pump.obj \
	$(PADHOME)\kpl\bin-win\clip.obj \
	$(PADHOME)\kpl\bin-win\eval_spline.obj \
	$(PADHOME)\kpl\bin-win\virtual_space.obj \
	$(PADHOME)\kpl\bin-win\rpy.obj \
	$(PADHOME)\kpl\bin-win\letter_to_vectors.obj \
	$(PADHOME)\kpl\bin-win\proj.obj

all:  $(PADWISH)



$(PAD_LIB): $(KPL_LIB) $(GENERIC_OBJS)
	set LIB=$(TOOLS32)\lib
	lib $(KPL_LIB) $(MSDEV42_LIB) \
		-out:$(PAD_LIB)	@<<
		$(GENERIC_OBJS)
<<

$(PAD_DLL): $(KPL_LIB) $(GENERIC_OBJS) 
	set LIB=$(TOOLS32)\lib
	$(link32) $(linkdebug) $(dlllflags) $(MSDEV42_LIB) \
	$(TCL_LIB) $(TK_LIB) $(KPL_LIB) $(guilibsdll) wsock32.lib \
	-out:$(PAD_DLL) @<<
	$(GENERIC_OBJS)
<<

$(KPL_DLL): $(KPL_OBJS)
	$(link32) $(linkdebug) $(dlllflags)  \
	$(TCL_LIB) $(guilibsdll) $(MSDEV)\lib\wsock32.lib \
	-out:$(KPL_DLL) @<<
	$(KPL_OBJS)
<<

$(KPL_LIB): $(KPL_OBJS) 
	lib  $(KPL_OBJS) -out:$(KPL_LIB)

$(PADWISH): copylibs $(PAD_LIB) $(TCL_LIB) $(TK_LIB) $(MAINOBJS) $(PADHOME)\win\padwish.res
	set LIB=$(TOOLS32)\lib
	$(link32) $(linkdebug) $(lflags) \
	$(TCL_LIB) $(TK_LIB) $(PAD_LIB)  \
	msvcrt.lib oldnames.lib kernel32.lib  user32.lib gdi32.lib wsock32.lib \
	$(PADHOME)\win\padwish.res \
	-out:$(PADWISH)	 @<<
	$(MAINOBJS)				
<<


#
# This copies all of the run time libraries (tcl/tk and kpl)
# into the pad directory so that Pad++ will run without having
# to set any system environment variables (which is a pain
# to do on windows...)
#
copylibs:		
	if not exist $(PADHOME)\lib mkdir $(PADHOME)\lib
	cd $(PADHOME)\lib
	if not exist $(TCL_VERSION) mkdir $(TCL_VERSION)
	if not exist $(TCL_VERSION)\init.tcl copy $(TCL_HOME)\library $(TCL_VERSION)
	if not exist $(TK_VERSION) mkdir $(TK_VERSION)
	if not exist $(TK_VERSION)\bgerror.tcl copy $(TK_HOME)\library $(TK_VERSION)
	cd ..\win
	if not exist $(PADHOME)\bin-win mkdir $(PADHOME)\bin-win
	if not exist $(PADHOME)\win\$(TCL_LIBNAME).dll copy $(TCL_HOME)\win\$(TCL_LIBNAME).dll $(PADHOME)\win
	if not exist $(PADHOME)\win\$(TK_LIBNAME).dll copy $(TK_HOME)\win\$(TK_LIBNAME).dll $(PADHOME)\win
	if not exist $(PADHOME)\kpl\bin-win mkdir $(PADHOME)\kpl\bin-win

#
# General rules for compiling files
#
{$(PADHOME)\generic}.cpp{$(PADHOME)\bin-win}.obj:
	$(cc32) $(cdebug) $(cflags) $(cvarsdll) $(TK_INCLUDES) \
		-Fo$(PADHOME)\bin-win\ $<


{}.cpp{$(PADHOME)\bin-win}.obj:
	$(cc32) $(cdebug) $(cflags) $(cvarsdll) $(TK_INCLUDES) \
		-Fo$(PADHOME)\bin-win\ $<

{$(PADHOME)\kpl\src}.c{$(PADHOME)\kpl\bin-win}.obj:
	$(cc32) $(cdebug) $(cflags) $(cvarsdll)  $(TK_INCLUDES) \
		-Fo$(PADHOME)\kpl\bin-win\ $<

{$(PADHOME)\win\rc}.rc{$(PADHOME)\win}.res:
	$(rc32) -fo $@ -r $(TK_INCLUDES) -i $(PADHOME)\generic $<

#
# This makes a binary distribution of Pad++
# It copies all the files necessary to run Pad++
# into a single directory, excluding all unnecessary source code.
#
$(WINPAD) : 
	if exist \$(WINPAD) rmdir /s /q  \$(WINPAD)
	mkdir \$(WINPAD)
	mkdir \$(WINPAD)\bitmaps
	mkdir \$(WINPAD)\bitmaps\animation
	mkdir \$(WINPAD)\doc
	mkdir \$(WINPAD)\draw
	mkdir \$(WINPAD)\draw\scrapbook
	mkdir \$(WINPAD)\draw\fonts
	mkdir \$(WINPAD)\html
	mkdir \$(WINPAD)\images
	mkdir \$(WINPAD)\kpl
	mkdir \$(WINPAD)\kpl\lib
	mkdir \$(WINPAD)\lib
	mkdir \$(WINPAD)\win

	copy ..\License \$(WINPAD)
	copy ..\LicenseTerms \$(WINPAD)
	copy ..\Copyright \$(WINPAD)
	copy ..\README.windist \$(WINPAD)\README

	copy ..\bitmaps \$(WINPAD)\bitmaps
	copy ..\bitmaps\animation \$(WINPAD)\bitmaps\animation
	copy ..\doc \$(WINPAD)\doc
	copy ..\draw \$(WINPAD)\draw
	copy ..\draw\scrapbook \$(WINPAD)\draw\scrapbook
	copy ..\draw\fonts \$(WINPAD)\draw\fonts
	copy ..\html \$(WINPAD)\html
	copy ..\images   \$(WINPAD)\images
	copy ..\kpl\lib\std.kpl \$(WINPAD)\kpl\lib
	xcopy /s  ..\lib \$(WINPAD)\lib	
	copy padwish.exe \$(WINPAD)\win
	copy $(TK_HOME)\win\$(TK_LIBNAME).dll \$(WINPAD)\win
	copy $(TCL_HOME)\win\$(TCL_LIBNAME).dll \$(WINPAD)\win
	copy padinit \$(WINPAD)\win
	copy msvcirt.dll \$(WINPAD)\win
	copy msvcrt.dll \$(WINPAD)\win

#
# This removes all the files generated by this makefile.
#
clean:
	-del *.dll
	-del *.res
	-del padwish.exe
	-cd $(PADHOME)\kpl\bin-win
	-del *.obj
	-del *.lib
	-del *.dll
	-cd ..
	-rmdir bin-win
	-cd $(PADHOME)\bin-win
	-del *.obj
	-del *.lib
	-del *.dll
	-cd ..
	-rmdir bin-win
	-cd $(PADHOME)\lib\$(TCL_VERSION)
	-del *.*
	-cd ..
	-rmdir $(TCL_VERSION)
	-cd $(TK_VERSION)
	-del *.*
	-cd ..
	-rmdir $(TK_VERSION)
	-cd ..
	-rmdir lib
