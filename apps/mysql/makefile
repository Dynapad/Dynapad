PLT=/home/hci/plt

OBJ=mysql.o

CFLAGS=-I$(PLT)/include

install: mysql.so
	mkdir -p $(DYNAHOME)/lib
	cp mysql.so mysql.ss $(DYNAHOME)/lib

mysql.so: mysql.o
	cc -shared -o mysql.so mysql.o $(PLT)/lib/mzdyn.o -L/usr/lib/mysql -lmysqlclient

clean:
	-rm -f *.a *.o *.so
