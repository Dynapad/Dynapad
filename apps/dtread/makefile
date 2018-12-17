PLT=/home/hci/plt-200

OBJ=dt.o dtcal.o dtframe.o dtstats.o dtusbio.o gauss.o dtmouse.o

CFLAGS=-I. -I$(PLT)/include -I/usr/X11R6/include

ALL: arf mzdt.so xtest

arf: arf.o dtlib.a
	cc -o arf arf.o dtlib.a -lm

dtlib.a: ${OBJ}
	ar cr dtlib.a ${OBJ}

mzdt.so: mzdt.o dtlib.a
	cc -shared -o mzdt.so mzdt.o dtlib.a $(PLT)/lib/mzdyn.o

xtest: xtest.o dtlib.a
	cc -o xtest xtest.o dtlib.a -lm -L/usr/X11R6/lib -lX11 -lXtst

XLIBS = -lXmu -lXt -lX11  -L/usr/X11R6/lib
xarf: xarf.o dtlib.a xgr.o
	cc -o xarf xgr.o xarf.o dtlib.a -lm $(XLIBS)

xgr.o: xgr.c
	gcc xgr.c -c
clean:
	-rm -f *.a *.o *.so arf xtest
