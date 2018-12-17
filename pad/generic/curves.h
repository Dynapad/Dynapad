//
// curves.h - header file for curve utilities
//

// Bezier curves

// Generates points along a bezier curve
void Pad_Bezier(float x0, float y0, float x1, float y1, 
		float x2, float y2, float x3, float y3, 
		float beztol, void (*cb)(float*)); 

// Converts list of points into bezier curve segments
void Pad_FitBezier(float *pts, int nPts, float error, void (*cb)(float*));
