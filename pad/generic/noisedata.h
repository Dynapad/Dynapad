
#ifndef __PAD_NOISEDATA_H
#define __PAD_NOISEDATA_H

class Pad_PList;

//
// Data used to generate noisy lines
// 

class Pad_NoiseData { 
  public:
    float pos;
    float freq;
    float amp;
    float steps;

    // Calculate bounding box of noisy line
    void Compute_bbox(Pad_Point *points, int length, Pad_Bool closed, Pad_Bool miter,
                            float lineWidth, float bbox_ret[4]);
    void Compute_bbox(Pad_PList &points, Pad_Bool closed, Pad_Bool miter,
                            float lineWidth, float bbox_ret[4]);

    // Return the points used by a noisy line
    Pad_Point *Get_points(Pad_Point *points, int length, Pad_Bool closed, int &numgot);
    Pad_Point *Get_points(Pad_PList &points, Pad_Bool closed, int &numgot);
};


//
// Iterator that generates points along a noisy line
//
class Pad_NoiseIterator {    
  public:
    void Setup(Pad_NoiseData *ndata, Pad_Point *pts, int len, Pad_Bool closed);
    void Setup(Pad_NoiseData *ndata, Pad_PList &pts, Pad_Bool closed);
    int Next(Pad_Point &point);

    Pad_NoiseIterator();	// Necessary to avoid compiler warnings about use before setting

  private:    
    Pad_NoiseData *nd;
    Pad_Point *points;
    Pad_Bool closed;
    int i, length;
    float a, pos, step, x1, y1, x2, y2, xamp, yamp;

    void Setup_segment(int i);
};


#endif
