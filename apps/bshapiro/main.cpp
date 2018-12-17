// just a test of committing

#include "KPreviewer.h"

int main(int argc, char ** argv)
{
  if (argc<4)
    {
      printf("command line error: try %s <maxwidth> '<url>' <outfile> [filetype (Default=PNG)\n",argv[0]);
      return 1;
    }
  char * fileType="PNG";
  if (argc==5)
    {
      fileType=argv[4];
    }
      

  KApplication a(argc, argv, "KWatcher");

  int width=atoi(argv[1]);

  KPreviewer *w = new KPreviewer(width, argv[2], argv[3], fileType);
  w->setGeometry(100,100,width,width);
  a.setMainWidget(w);
  w->show();
  return a.exec();
}
