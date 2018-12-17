#!/bin/bash
rm -f *.o
rm -f *.so
DIR="./incl_dir"
gcc -c -O2 -fno-strength-reduce -ansi -pedantic  -I$DIR/xc/programs/Xserver/hw/xfree86/common -I$DIR/xc/programs/Xserver/hw/xfree86 -I$DIR/xc/programs/Xserver/hw/xfree86/os-support -I$DIR/xc/programs/Xserver/mfb -I$DIR/xc/programs/Xserver/mi -I$DIR/xc/programs/Xserver/include -I$DIR/xc/programs/Xserver/os               -I$DIR/xc/exports/include/X11 -I$DIR/xc/include/extensions -I$DIR/xc/include  -I$DIR -I$DIR/xc/exports/include  -Dlinux -D__i386__ -D_POSIX_C_SOURCE=199309L -D_POSIX_SOURCE -D_XOPEN_SOURCE=500L -D_BSD_SOURCE -D_SVID_SOURCE -DSHAPE -DXINPUT -DXKB -DLBX -DXAPPGROUP -DXCSECURITY  -DDPMSExtension -DPIXPRIV  -DGCCUSESGAS -DSTATIC_COLOR -DAVOID_GLYPHBLT -DPIXPRIV  -DXFreeXDGA -DNDEBUG   -DFUNCPROTO=15 -DNARROWPROTO    -DPNP_MOUSE 	       -fPIC -DDYNAMIC_MODULE -DDEFAULT_MODULE_PATH=\"/usr/X11R6/lib/modules\"  xf86GtcoCC.c
if [ -r xf86GtcoCC.o ]
then
    gcc -o xf86GtcoCC.so -shared xf86GtcoCC.o
#    cp xf86GtcoCC.so /usr/X11R6/lib/modules
    echo
    echo "Success!"
    echo
else
    echo
    echo "Compile Failed"
    echo
fi
