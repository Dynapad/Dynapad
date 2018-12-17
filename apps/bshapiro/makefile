QTDIR=/usr/lib/qt-3.0.3
PLT=/home/hci/plt-200

moc_%.cpp: %.h
	moc $< -o $@

CPPFLAGS=-g -I/usr/include/kde -I$(QTDIR)/include -I$(PLT)/include -fno-rtti
LFLAGS=-g -L$(QTDIR)/lib -lkio -lkdeui -lkdecore -lqt -ldl

all: KPreviewer KPreviewer.so

KPreviewer: main.o KPreviewer.o moc_KPreviewer.o
	g++ -g -o KPreviewer main.o KPreviewer.o moc_KPreviewer.o $(LFLAGS)

KPreviewer.so: KPreviewer.o moc_KPreviewer.o ffi.o
	g++ -g -shared -o KPreviewer.so KPreviewer.o moc_KPreviewer.o ffi.o \
		$(PLT)/lib/mzdyn.o

clean:
	rm -f *.o KPreviewer *.so moc_*.cpp
