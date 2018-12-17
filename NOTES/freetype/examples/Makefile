#Cygwin
#FT2INC=-I/usr/X11R6/include/freetype2

#linux
FT2INC=-I/usr/include/freetype2

XINC=-I/usr/X11R6/include
XLIB=-L/usr/X11R6/lib

OBJS= HelloX ZoomX PanZoomX JitterX HelloX-full GoodbyeX RenderX List Match \
	Open OpenName OpenPattern \
	example main

ALL: $(OBJS)

HelloX: HelloX.o
	gcc -g -o HelloX HelloX.o -Wall $(XLIB) -lX11 -lXft

HelloX.o: HelloX.c
	gcc -g -o HelloX.o HelloX.c $(XINC) $(FT2INC) -c  -ansi -pedantic -Wall

ZoomX: ZoomX.o
	gcc -g -o ZoomX ZoomX.o -Wall $(XLIB) -lX11 -lXft

ZoomX.o: ZoomX.c
	gcc -g -o ZoomX.o ZoomX.c $(XINC) $(FT2INC) -c  -ansi -pedantic -Wall

PanZoomX: PanZoomX.o
	gcc -g -o PanZoomX PanZoomX.o -Wall $(XLIB) -lX11 -lXft -lm

PanZoomX.o: PanZoomX.c
	gcc -g -o PanZoomX.o PanZoomX.c $(XINC) $(FT2INC) -c  -ansi -pedantic -Wall

JitterX: JitterX.o
	gcc -g -o JitterX JitterX.o -Wall $(XLIB) -lX11 -lXft -lm

JitterX.o: JitterX.c
	gcc -g -o JitterX.o JitterX.c $(XINC) $(FT2INC) -c  -ansi -pedantic -Wall

HelloX-full: HelloX-full.o
	gcc -g -o HelloX-full HelloX-full.o -Wall $(XLIB) -lX11 -lXft

HelloX-full.o: HelloX-full.c
	gcc -g -o HelloX-full.o HelloX-full.c $(XINC) $(FT2INC) -c  -pedantic -Wall

GoodbyeX: GoodbyeX.o
	gcc -g -o GoodbyeX GoodbyeX.o -Wall $(XLIB) -lX11 -lXft

GoodbyeX.o: GoodbyeX.c
	gcc -g -o GoodbyeX.o GoodbyeX.c $(XINC) $(FT2INC) -c  -ansi -pedantic -Wall

RenderX: RenderX.o
	gcc -g -o RenderX RenderX.o -Wall $(XLIB) -lX11 -lXft

RenderX.o: RenderX.c
	gcc -g -o RenderX.o RenderX.c $(XINC) $(FT2INC) -c  -pedantic -Wall

List: List.o
	gcc -g -o List List.o -Wall $(XLIB) -lX11 -lXft -lfontconfig

List.o: List.c
	gcc -g -o List.o List.c $(XINC) $(FT2INC) -c  -ansi -pedantic -Wall

Match: Match.o
	gcc -g -o Match Match.o -Wall $(XLIB) -lX11 -lXft -lfontconfig

Match.o: Match.c
	gcc -g -o Match.o Match.c $(XINC) $(FT2INC) -c  -ansi -pedantic -Wall

Open: Open.o
	gcc -g -o Open Open.o -Wall $(XLIB) -lX11 -lXft -lfontconfig

Open.o: Open.c
	gcc -g -o Open.o Open.c $(XINC) $(FT2INC) -c  -ansi -pedantic -Wall

OpenPattern: OpenPattern.o
	gcc -g -o OpenPattern OpenPattern.o -Wall $(XLIB) -lX11 -lXft -lfontconfig

OpenPattern.o: OpenPattern.c
	gcc -g -o OpenPattern.o OpenPattern.c $(XINC) $(FT2INC) -c  -ansi -pedantic -Wall

OpenName: OpenName.o
	gcc -g -o OpenName OpenName.o -Wall $(XLIB) -lX11 -lXft -lfontconfig

OpenName.o: OpenName.c
	gcc -g -o OpenName.o OpenName.c $(XINC) $(FT2INC) -c  -ansi -pedantic -Wall

example: example.o
	gcc -g -o example example.o $(XLIB) -lfontconfig

example.o: example.c
	gcc -g -c  -o example.o example.c $(XINC)

main: main.o
	gcc -g -o main main.o $(XLIB) -lfontconfig

main.o: main.c
	gcc -g -c  -o main.o main.c $(XINC)

clean:
	rm -f *.o *.exe $(OBJS)
