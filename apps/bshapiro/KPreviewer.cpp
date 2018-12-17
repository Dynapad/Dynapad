#ifdef Bool   /*x-header error workaround*/
#undef Bool
#endif
#include <kio/previewjob.h>
#include <kfileitem.h>
#include <kio/netaccess.h>
#include <kmimetype.h>
#include "KPreviewer.h"

//using namespace KIO;

//namespace KIO{class NetAccess;}


KPreviewer::KPreviewer(const int width,  const char * urlStr, const char * outputPath, const char * fileType) : KMainWindow()
{
  setlinebuf(stdout); // in case we are piping this to something that does full buffering

  this->outputPath=outputPath;
  this->fileType=fileType;
  label=new QLabel("Hello!",this);
  pix=new QPixmap(); 
  pix->load("logo.png");
  label->setPixmap(*pix);
  label->setAutoResize(TRUE);
  KURL url(urlStr);
  //  url.setPath("/home/bshapiro/protocol.ps");
  KMimeType::Ptr mime = KMimeType::findByURL( url );
  cout << mime->name() <<"\n";

  KURL::List items;
  //items.append(&fileItem);
  items.append(url);
  //printf("Sanity: items.count()=%d\n",items.count());

  pPreviewJob = KIO::filePreview(items, width);
  
  connect( pPreviewJob, 
	   SIGNAL( gotPreview( const KFileItem *, 
			       const QPixmap & ) ),
	   this, 
	   SLOT( 
		slotPreview( const KFileItem *, const QPixmap & ) 
		) 
	   );
  label->setPixmap(KMimeType::pixmapForURL(url));
}


KPreviewer::~KPreviewer()
{
  delete pix;
  delete label;
}

void KPreviewer::closeEvent(QCloseEvent *e)
{
  kapp->beep();
  KMainWindow::closeEvent(e);
}

void KPreviewer::slotPreview(const KFileItem * fileItem, const QPixmap& newPixmap)
{
  printf("slotPreview activated!\n");
  this->pix=new QPixmap(newPixmap);
  label->setPixmap(*(this->pix));
  this->pix->save(outputPath, fileType);
  kapp->quit();
}

