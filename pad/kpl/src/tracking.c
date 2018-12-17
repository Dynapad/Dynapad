
#ifndef SGI

void tracking(float threshold, int b[])
{
}

#else
#ifdef NOTRACK

tracking(float threshold, int b[])
{
}

#else

#include <stdio.h>
#include <fcntl.h>
#include <vl/vl.h>

#define THRESHOLD	90
#define THR_BAND	15
#define BORDER		5
#define HEAD_SIZE	20
#define SKIP_H		1
#define SKIP_V		1
#define ZOOM_NUM	1
#define ZOOM_DEN	2


tracking(float threshold, int b[])
{
  static VLServer svr;
  VLPath path;
  VLNode src, drn;
  static VLBuffer buf;
  VLTransferDescriptor xferDesc;
  VLEvent event;
  VLControlValue value;
  static int size, size_x, size_y;
  static int i, j;
  char *image;
  double average;
  int count;
  int x1, x2, max_width, last_width, y_max;
  int in_face;
  static first_time = 1;

  if (first_time) {  
  if (!(svr = vlOpenVideo(NULL))) {
    vlPerror("vlOpenVideo");
    exit(1);
    }

  /* Set up a video source node */
  src = vlGetNode(svr, VL_SRC, VL_VIDEO, VL_ANY);

  /* Set up a memory drain node */
  drn = vlGetNode(svr, VL_DRN, VL_MEM, VL_ANY);

  /* Create a path on the first available device */
  path = vlCreatePath(svr, VL_ANY, src, drn);
  if (!path) {
    vlPerror("vlCreatePath");
    exit(1);
  }

  /* Set up the hardware for and define the usage of the path */
  if (vlSetupPaths(svr,(VLPathList)&path, 1, VL_SHARE, VL_SHARE)<0) {
    vlPerror("vlSetupPaths");
    exit(1);
  }

  if(vlGetControl(svr, path, src, VL_SIZE, &value)) {
    vlPerror("vlGetControl(VL_SIZE)");
    exit(1);
  }
  size_x = value.xyVal.x * ZOOM_NUM / ZOOM_DEN;
  size_y = value.xyVal.y * ZOOM_NUM / ZOOM_DEN;


  value.intVal = VL_PACKING_Y_8_P /* VL_PACKING_RGB_8 */;
  if(vlSetControl(svr, path, drn, VL_PACKING, &value)) {
     vlPerror("vlGetControl(VL_PACKING)");
     exit(1);
  }

  value.fractVal.numerator = ZOOM_NUM;
  value.fractVal.denominator = ZOOM_DEN;
  if(vlSetControl(svr, path, drn, VL_ZOOM, &value)) {
     vlPerror("vlSetControl(VL_ZOOM)");
     exit(1);
  }

  /* Create a ring buffer for the data transfers */
  buf = vlCreateBuffer(svr, path, src, 1);

  /* Associate the ring buffer with the path */
  if (vlRegisterBuffer(svr, path, drn, buf)) {
     vlPerror("vlRegisterBuffer");
     exit(1);
  }

   /* Select desired events */
   if (vlSelectEvents(svr, path, VLTransferCompleteMask)) {
     vlPerror("vlSelectEvents");
     exit(1);
   }

   /* Get buffer size */
   size = vlGetTransferSize(svr, path);


   /* Set up the transfer */
   xferDesc.mode = VL_TRANSFER_MODE_CONTINUOUS;
   xferDesc.trigger = VLTriggerImmediate;
   xferDesc.count = 1;

   vlBeginTransfer(svr, path, 1, &xferDesc );
   first_time = 0;
  }

   if (vlNextEvent(svr, &event)) {
     vlPerror("vlNextEvent");
     exit(1);
   }
   else {
     image = vlGetActiveRegion(svr, buf, vlGetLatestValid(svr, buf));


  /* Image analysis */

  /* Look for 1st point threshold below the average intensity */
     for (i=count=average=0; i<size; i+=SKIP_H){
       if (i%size_x > (size_x - BORDER) || i%size_x < BORDER)
         continue;
       average += image[i];
       count++;
       if (image[i] < (average/count - threshold)) {
#ifdef BAND
         for (j=i; j<(i+size_x-(i%size_x)) ; j++) 
            if (image[j] >= (average/count - threshold+THR_BAND))
              break;
#endif
         break;
       }
     }
#ifdef BAND
     i = (i+j)/2;
#endif
     
     b[0] = i%size_x;
     b[1] = i/size_x;


  /* Discover the head width */
     average = average/count - threshold;
     x1=i%size_x; x2=i%size_x;
     i = i - size_x - x1; /* go to line start */
     in_face = 0;
     count=0; max_width = last_width = 0;
     for (; i< size && count < HEAD_SIZE ; i += SKIP_H) {
       if (i%size_x > (size_x - BORDER) || i%size_x < BORDER)
         continue;
       if (!in_face && image[i] < average) {
         in_face = 1;
         x1=i%size_x;
       }
       if (in_face && image[i] > average) {
         in_face = 0;
         x2 = i%size_x;
         i = i + size_x - x1;
         if (max_width < (x2 - x1)) {
           max_width = x2 - x1;
           y_max = i / size_x;
         }
         if (last_width < (x2 - x1)) {
           count = 0;
         }
         else  {
           count++;
         }
	 last_width = x2 - x1;
       }
     }
  }
  vlPutFree(svr, buf);

  /* End the data transfer */
/*
  vlEndTransfer(svr, path);
*/
  /* Disassociate the ring buffer from the path */
/*
  vlDeregisterBuffer(svr, path, drn, buf);
*/
  /* Destroy the path, free the memory it used */
/*
  vlDestroyPath(svr,path);
*/
  /* Destroy the ring buffer, free the memory it used */
/*
  vlDestroyBuffer(svr, buf);
*/
  /* Disconnect from the daemon */
/*
  vlCloseVideo(svr);
*/
  b[3] = max_width;
}

#endif
#endif


