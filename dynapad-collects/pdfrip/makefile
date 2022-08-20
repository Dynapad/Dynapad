OBJ= pdfrip pdfripdir pdfriptree importdir importtree composite compositedir \
	flatten-and-importtree movebaddir movebadtree

all: $(OBJ)

pdfrip: pdfripone.ss
	mzc --exe pdfrip pdfripone.ss

pdfripdir: pdfripdir.ss
	mzc --exe pdfripdir pdfripdir.ss

pdfriptree: pdfriptree.ss
	mzc --exe pdfriptree pdfriptree.ss

importdir: importdir.ss
	mzc --exe importdir importdir.ss

importtree: importtree.ss
	mzc --exe importtree importtree.ss

movebaddir: movebaddir.ss
	mzc --exe movebaddir movebaddir.ss

movebadtree: movebadtree.ss
	mzc --exe movebadtree movebadtree.ss

composite: compositeone.ss
	mzc --exe composite compositeone.ss

compositedir: compositedir.ss
	mzc --exe compositedir compositedir.ss

flatten-and-importtree: flatten-and-importtree.ss
	mzc --exe flatten-and-importtree flatten-and-importtree.ss

clean:
	-rm -f $(OBJ)
