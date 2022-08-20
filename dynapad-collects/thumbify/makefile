OBJ= thumbify thumbifydir thumbifytree

all: $(OBJ)

thumbify: thumbifyone.ss thumbify.ss
	mzc --exe thumbify thumbifyone.ss

thumbifydir: thumbifydir.ss thumbify.ss
	mzc --exe thumbifydir thumbifydir.ss

thumbifytree: thumbifytree.ss thumbify.ss
	mzc --exe thumbifytree thumbifytree.ss

clean:
	-rm -f $(OBJ)
