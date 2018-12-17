PLT=/home/stanonik/plt
MZC=$(PLT)/bin/mzc

CFLAGS=-I$(PLT)/include

install: mysql-prims.so

mysql-prims.so: mysql-prims.c
	$(MZC) --cc mysql-prims.c
	$(MZC) --ld mysql-prims.so ++ldf -L/usr/lib/mysql ++ldf -lmysqlclient mysql-prims.o
clean:
	-rm -f *.a *.o *.so
