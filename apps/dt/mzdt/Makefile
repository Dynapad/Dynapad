PLT = /home/hci/plt
CPPFLAGS = -I../libdt -I$(PLT)/include

all: mzdt.so

mzdt.so: mzdt.o ../libdt/libdt.a
	cc -shared -o mzdt.so mzdt.o ../libdt/libdt.a $(PLT)/lib/mzdyn.o
