CC = cc -g
LIBJPG = -ljpeg
LIBS = $(LIBJPG) -lm
OFILES = gd.o jpg2.o rand.o server.o thumb.o thumb2.o zoom.o
CFILES = gd.c jpg2.c mtables.c rand.c server.c thumb.c thumb2.c zoom.c
HFILES = gd.h jconfig.h jerror.h jmorecfg.h jpeglib.h server.h


thumbify: $(OFILES) $(LIBJPG)
	$(CC) $(OFILES) -o thumbify $(LIBS)
clean:
	/bin/rm -f thumbify *.[o] client

map.c: server.h
server.c: server.h
thumb.c: server.h
thumb2.c: server.h

client: client.o
	$(CC) client.o -o client
