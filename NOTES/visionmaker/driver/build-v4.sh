#!/bin/bash
rm -f *.o
DIR="/usr/src/redhat/BUILD/XFree86-4.3.0"
gcc  -c -O2 -fno-strength-reduce -ansi -pedantic -Wall -Wpointer-arith -Wstrict-prototypes -Wmissing-prototypes -Wmissing-declarations -Wredundant-decls -Wnested-externs  -I$DIR/xc/programs/Xserver/hw/xfree86/common -I$DIR/xc/programs/Xserver/hw/xfree86 -I$DIR/xc/programs/Xserver/hw/xfree86/os-support -I$DIR/xc/programs/Xserver/mfb -I$DIR/xc/programs/Xserver/mi -I$DIR/xc/programs/Xserver/include -I$DIR/xc/programs/Xserver/os -I$DIR/xc/include -I$DIR/xc/include/extensions  -I$DIR/xc/programs/Xserver/hw/xfree86/os-support/bus -Dlinux -D__i386__ -D_POSIX_C_SOURCE=199309L -D_POSIX_SOURCE -D_XOPEN_SOURCE -D_BSD_SOURCE -D_SVID_SOURCE  -D_GNU_SOURCE  -DSHAPE -DXINPUT -DXKB -DLBX -DXAPPGROUP -DXCSECURITY -DTOGCUP  -DXF86BIGFONT -DDPMSExtension  -DPIXPRIV -DPANORAMIX  -DGCCUSESGAS -DAVOID_GLYPHBLT -DPIXPRIV -DSINGLEDEPTH -DXFreeXDGA -DXvExtension -DXFree86LOADER  -DXFree86Server -DXF86VIDMODE  -DSMART_SCHEDULE -DX_BYTE_ORDER=X_LITTLE_ENDIAN -DNDEBUG   -DFUNCPROTO=15 -DNARROWPROTO  -DIN_MODULE -DXFree86Module xf86GtcoCC.c
if [ -r  xf86GtcoCC.o ]
then
    ld -r  xf86GtcoCC.o -o gtcc_drv.o
    chmod +x gtcc_drv.o
#    cp  gtcc_drv.o /usr/X11R6/lib/modules/input
    echo
    echo "Compile completed successfully"
    echo
else
    echo
    echo "Compile failed"
    echo
fi
