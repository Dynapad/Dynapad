ROOT =
TCL_INCLUDE =
TK_INCLUDE =
LIBTCL =
LIBTK =
PADHOME = ..
ARCH = bin-i686-Linux
X11LIB = /usr/X11R6/lib
X11INCLUDE = /usr/X11R6/include
CC = gcc

testapi: testapi.o tkAppInit.o tkMain.o $(PADHOME)/generic/api.h
	$(CC) -g -o testapi testapi.o tkAppInit.o  tkMain.o \
		-L$(PADHOME)/$(ARCH) -L$(PADHOME)/kpl/$(ARCH) -L$(X11LIB) \
		-lpad -ljpeg -lkpl -ltk -ltcl -lXext -lX11 -lm -lstdc++ -ldl

testapi.o: testapi.cpp $(PADHOME)/generic/api.h
	$(CC) -g -c testapi.cpp -I$(X11INCLUDE) -I$(PADHOME)/generic -DPAD_UNIX -DPAD_TK

tkAppInit.o: tkAppInit.cpp
	$(CC) -g -c tkAppInit.cpp -I$(X11INCLUDE) -I$(PADHOME)/generic -DPAD_UNIX -DPAD_TK

tkMain.o: tkMain.cpp
	$(CC) -g -c tkMain.cpp -I$(X11INCLUDE) -I$(PADHOME)/generic -DPAD_UNIX -DPAD_TK


clean:
	rm -f *.o *~ testapi

