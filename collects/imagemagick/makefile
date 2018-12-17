SHL=compiled/native/i386-linux/imagemagick.so

all: $(SHL)

imagemagick.o: imagemagick.c
	mzc --cc imagemagick.c

$(SHL): imagemagick.o
	mzc ++ldl -lMagick --ld $(SHL) imagemagick.o

clean:
	-rm -f *.o $(SHL)
