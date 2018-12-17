
#include <kapp.h>
#include <kmainwindow.h>
#include <kwinmodule.h>
#include <stdio.h>
#include <netwm.h>
#include <unistd.h>
#include <fcntl.h>
#include <qlabel.h>
#include <qpixmap.h>

class KFileItem;
namespace KIO { class PreviewJob; }
namespace KIO { class NetAccess;  }
//class KIO::PreviewJob;

class KPreviewer : public KMainWindow
{

  Q_OBJECT

  public:
    void closeEvent(QCloseEvent*);
    KPreviewer(const int width, const char * urlStr, const char * outputPath, const char * fileType);
    ~KPreviewer();

  protected slots:
      void slotPreview(const KFileItem *, const QPixmap &);
  //void failed(const KFileItem *item);
  

  private:
      QLabel * label;
      QPixmap * pix;
      KIO::PreviewJob * pPreviewJob;
      const char * outputPath;
      const char * fileType;
};
