CC = cc -g -w
#CC = gcc -g -DSOLARIS
#CC = purify /usr/local/pkg/gcc/gcc-2.7.2/bin/gcc -g -Wformat
LIBJPG = libjpeg.a
LIBS = $(LIBJPG) -lm
SLIBS = $(LIBS) -lsocket -lnsl
OFILES = gd.o jpg2.o map.o rand.o server.o socket.o thumb.o thumb2.o zoom.o
CFILES = gd.c jpg2.c map.c mtables.c rand.c server.c socket.c thumb.c thumb2.c zoom.c
HFILES = gd.h jconfig.h jerror.h jmorecfg.h jpeglib.h server.h


server: $(OFILES) $(LIBJPG)
	$(CC) $(OFILES) -o server $(LIBS)
solaris: $(OFILES) $(LIBJPG)
	$(CC) $(OFILES) -o server $(SLIBS)
clean:
	/bin/rm -f server *.[o]
