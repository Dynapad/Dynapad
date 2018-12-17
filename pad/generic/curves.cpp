#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "defs.h"		// using windefs.h to get ride of warning in win95
#include "curves.h"

/* Curve Tools - self contained module for spline curve utilities.

  Bezier curves
  -------------

     Pad_Bezier(x0, y0, x1, y1, x2, y2, x3, y3, error, cb)
          - Generates points for a bezier curve

     Pad_FitBezier(pts, npts, error, cb)
          - Generates a number of bezier segments to fit a freehand line
            (from Graphics Gems)
*/

// ----------------------------------------------------------------------


//
// Bezier curve->points function. N.B. This is guarenteed to output x0/y0
// (for the first curve in a sequence) and x3/y3 (for the last). 
//

void Pad_Bezier(float x0, float y0, float x1, float y1, 
		float x2, float y2, float x3, float y3, 
		float beztol,  /* error squared */
		void (*cb)(float*)) /* callback */
{
    float midx = (x0+3*x1+3*x2+x3)/8.0;
    float midy = (y0+3*y1+3*y2+y3)/8.0;
    float linx = (x0+x3)/2.0;
    float liny = (y0+y3)/2.0;
    float dx = midx-linx;
    float dy = midy-liny;
    float mag = dx*dx+dy*dy;
    
    if (mag < beztol) {
	if (x0 != x3 || y0 != y3) {
	    float pt[2];
	    pt[0] = x3;
	    pt[1] = y3;
	    cb(pt);
	}
    } else {
	float ax0 = x0;
	float ay0 = y0;
	float ax1 = (x0+x1)/2;
	float ay1 = (y0+y1)/2;
	float ax2 = (x0+2*x1+x2)/4;
	float ay2 = (y0+2*y1+y2)/4;
	float ax3 = midx;
	float ay3 = midy;
	Pad_Bezier(ax0, ay0, ax1, ay1, ax2, ay2, ax3, ay3, beztol, cb);
	
	float bx0 = midx;
	float by0 = midy;
	float bx1 = (x1+2*x2+x3)/4;
	float by1 = (y1+2*y2+y3)/4;
	float bx2 = (x2+x3)/2;
	float by2 = (y2+y3)/2;
	float bx3 = x3;
	float by3 = y3;
	Pad_Bezier(bx0, by0, bx1, by1, bx2, by2, bx3, by3, beztol, cb);
    }
}

// ----------------------------------------------------------------------

/*

Code to convert freehand curves into Bezier splines.

From: An Algorithm for Automatically Fitting Digitized Curves
      by Philip J. Schneider
      from "Graphics Gems", Academic Press, 1990

Modified by Jonathan Meyer for use in Pad++:
      Merged everything into single file
      Converted to Ansi C.
      Modified API to be suitable for Pad.
*/


#define MAXPOINTS	1000		/* The most points you can have */

typedef struct Point2Struct {	/* 2d point */
	float x, y;
    } Point2;

typedef Point2 Vector2;
typedef Point2 *BezierCurve;

/* returns squared length of input vector */	
static float V2SquaredLength(Vector2 *a)
{	
    return((a->x * a->x)+(a->y * a->y));
}

static float V2Length(Vector2 *a)
{
    return(sqrt(V2SquaredLength(a)));
}
	
/* negates the input vector and returns it */
static Vector2 *V2Negate(Vector2 *v)
{
    v->x = -v->x;  v->y = -v->y;
    return(v);
}

/* normalizes the input vector and returns it */
static Vector2 *V2Normalize(Vector2 *v)
{
    float len = sqrt(V2Length(v));
    if (len != 0.0) { v->x /= len;  v->y /= len; }
    return(v);
}

/* scales the input vector to the new length and returns it */
static Vector2 *V2Scale(Vector2 *v, float newlen)
{
    float len = V2Length(v);
    if (len != 0.0) { v->x *= newlen/len;   v->y *= newlen/len; }
    return(v);
}

/* return vector sum c = a+b */
static Vector2 *V2Add(Vector2 *a, Vector2 *b, Vector2 *c)
{
    c->x = a->x+b->x;  c->y = a->y+b->y;
    return(c);
}
	
/* return the dot product of vectors a and b */
static float V2Dot(Vector2 *a, Vector2 *b) 
{
    return((a->x*b->x)+(a->y*b->y));
}

/* return the distance between two points */
static float V2DistanceBetween2Points(Point2 *a, Point2 *b)
{
    float dx = a->x - b->x;
    float dy = a->y - b->y;
    return(sqrt((dx*dx)+(dy*dy)));
}

static Vector2 V2AddII(Vector2 a, Vector2 b)
{
    Vector2 c;
    c.x = a.x + b.x;  c.y = a.y + b.y;
    return (c);
}

static Vector2 V2ScaleIII(Vector2 v, float s)
{
    Vector2 result;
    result.x = v.x * s; result.y = v.y * s;
    return (result);
}

static Vector2 V2SubII(Vector2 a, Vector2 b)
{
    Vector2	c;
    c.x = a.x - b.x; c.y = a.y - b.y;
    return (c);
}

/*
 *  B0, B1, B2, B3 :
 *	Bezier multipliers
 */
static float B0(float u)
{
    float tmp = 1.0 - u;
    return (tmp * tmp * tmp);
}


static float B1(float u)
{
    float tmp = 1.0 - u;
    return (3 * u * (tmp * tmp));
}

static float B2(float u)
{
    float tmp = 1.0 - u;
    return (3 * u * u * tmp);
}

static float B3(float u)
{
    return (u * u * u);
}


/* Forward declarations */

static void FitCubic(Point2 *d, int first, int last, Vector2 tHat1, 
		     Vector2 tHat2, float error);

static BezierCurve  GenerateBezier(Point2 *d, int first, int last, 
				   float *uPrime, 
				   Vector2 tHat1, Vector2 tHat2);

static float *Reparameterize(Point2 *d, int first, int last, 
			      float *u, BezierCurve bezCurve);

static float NewtonRaphsonRootFind(BezierCurve Q, Point2 P, float u);

static Point2 BezierII(int degree, Point2 *V, float t);

static Vector2 ComputeLeftTangent(Point2 *d, int end);
static Vector2 ComputeRightTangent(Point2 *d, int end);
static Vector2 ComputeCenterTangent(Point2 *d, int center);

static float ComputeMaxError(Point2 *d, int first, int last, 
			      BezierCurve bezCurve, float *u, 
			      int *splitPoint);

static float *ChordLengthParameterize(Point2 *d, int first, int last);

static void DrawBezierCurve(BezierCurve b);

/*
 *  FitCubic :
 *  	Fit a Bezier curve to a (sub)set of digitized points
 */
static void FitCubic(Point2 *d, int first, int last, Vector2 tHat1, 
		     Vector2 tHat2, float error)
{
    BezierCurve	bezCurve; /*Control points of fitted Bezier curve*/
    float	*u;		/*  Parameter values for point  */
    float	*uPrime;	/*  Improved parameter values */
    float	maxError;	/*  Maximum fitting error	 */
    int		splitPoint;	/*  Point to split point set at	 */
    int		nPts;		/*  Number of points in subset  */
    float	iterationError; /*Error below which you try iterating  */
    int		maxIterations = 4; /*  Max times to try iterating  */
    Vector2	tHatCenter;   	/* Unit tangent vector at splitPoint */
    int		i;		

    iterationError = error * error;
    nPts = last - first + 1;

    /*  Use heuristic if region only has two points in it */
    if (nPts == 2) {
	    float dist = V2DistanceBetween2Points(&d[last], &d[first]) / 3.0;

		bezCurve = (Point2 *)malloc(4 * sizeof(Point2));
		bezCurve[0] = d[first];
		bezCurve[3] = d[last];
		V2Add(&bezCurve[0], V2Scale(&tHat1, dist), &bezCurve[1]);
		V2Add(&bezCurve[3], V2Scale(&tHat2, dist), &bezCurve[2]);
		DrawBezierCurve(bezCurve);
		return;
    }

    /*  Parameterize points, and attempt to fit curve */
    u = ChordLengthParameterize(d, first, last);
    bezCurve = GenerateBezier(d, first, last, u, tHat1, tHat2);

    /*  Find max deviation of points to fitted curve */
    maxError = ComputeMaxError(d, first, last, bezCurve, u, &splitPoint);
    if (maxError < error) {
		DrawBezierCurve(bezCurve);
		free((void *)u);
		free((void *)bezCurve);
		return;
    }


    /*  If error not too large, try some reparameterization  */
    /*  and iteration */
    if (maxError < iterationError) {
		for (i = 0; i < maxIterations; i++) {
	    	uPrime = Reparameterize(d, first, last, u, bezCurve);
	    	bezCurve = GenerateBezier(d, first, last, uPrime, tHat1, tHat2);
	    	maxError = ComputeMaxError(d, first, last,
				       bezCurve, uPrime, &splitPoint);
	    	if (maxError < error) {
			DrawBezierCurve(bezCurve);
			free((void *)u);
			free((void *)bezCurve);
			return;
	    }
	    free((void *)u);
	    u = uPrime;
	}
    }

    /* Fitting failed -- split at max error point and fit recursively */
    free((void *)u);
    free((void *)bezCurve);
    tHatCenter = ComputeCenterTangent(d, splitPoint);
    FitCubic(d, first, splitPoint, tHat1, tHatCenter, error);
    V2Negate(&tHatCenter);
    FitCubic(d, splitPoint, last, tHatCenter, tHat2, error);
}


/*
 *  GenerateBezier :
 *  Use least-squares method to find Bezier control points for region.
 *
 */
static BezierCurve  GenerateBezier(Point2 *d, int first, int last, 
				   float *uPrime, 
				   Vector2 tHat1, Vector2 tHat2)
{
    int 	i;
    Vector2 	A[MAXPOINTS][2];	/* Precomputed rhs for eqn	*/
    int 	nPts;			/* Number of pts in sub-curve */
    float 	C[2][2];			/* Matrix C		*/
    float 	X[2];			/* Matrix X			*/
    float 	det_C0_C1,		/* Determinants of matrices	*/
    	   	det_C0_X,
	   		det_X_C1;
    float 	alpha_l,		/* Alpha values, left and right	*/
    	   	alpha_r;
    Vector2 	tmp;			/* Utility variable		*/
    BezierCurve	bezCurve;	/* RETURN bezier curve ctl pts	*/

    bezCurve = (Point2 *)malloc(4 * sizeof(Point2));
    nPts = last - first + 1;

 
    /* Compute the A's	*/
    for (i = 0; i < nPts; i++) {
		Vector2		v1, v2;
		v1 = tHat1;
		v2 = tHat2;
		V2Scale(&v1, B1(uPrime[i]));
		V2Scale(&v2, B2(uPrime[i]));
		A[i][0] = v1;
		A[i][1] = v2;
    }

    /* Create the C and X matrices	*/
    C[0][0] = 0.0;
    C[0][1] = 0.0;
    C[1][0] = 0.0;
    C[1][1] = 0.0;
    X[0]    = 0.0;
    X[1]    = 0.0;

    for (i = 0; i < nPts; i++) {
        C[0][0] += V2Dot(&A[i][0], &A[i][0]);
		C[0][1] += V2Dot(&A[i][0], &A[i][1]);
/*					C[1][0] += V2Dot(&A[i][0], &A[i][1]);*/	
		C[1][0] = C[0][1];
		C[1][1] += V2Dot(&A[i][1], &A[i][1]);

		tmp = V2SubII(d[first + i],
	        V2AddII(
	          V2ScaleIII(d[first], B0(uPrime[i])),
		    	V2AddII(
		      		V2ScaleIII(d[first], B1(uPrime[i])),
		        			V2AddII(
	                  		V2ScaleIII(d[last], B2(uPrime[i])),
	                    		V2ScaleIII(d[last], B3(uPrime[i]))))));
	

	X[0] += V2Dot(&A[i][0], &tmp);
	X[1] += V2Dot(&A[i][1], &tmp);
    }

    /* Compute the determinants of C and X	*/
    det_C0_C1 = C[0][0] * C[1][1] - C[1][0] * C[0][1];
    det_C0_X  = C[0][0] * X[1]    - C[0][1] * X[0];
    det_X_C1  = X[0]    * C[1][1] - X[1]    * C[0][1];

    /* Finally, derive alpha values	*/
    if (det_C0_C1 == 0.0) {
		det_C0_C1 = (C[0][0] * C[1][1]) * 10e-12;
    }
    alpha_l = det_X_C1 / det_C0_C1;
    alpha_r = det_C0_X / det_C0_C1;


    /*  If alpha negative, use the Wu/Barsky heuristic (see text) */
    if (alpha_l < 0.0 || alpha_r < 0.0) {
		float	dist = V2DistanceBetween2Points(&d[last], &d[first]) /
					3.0;

		bezCurve[0] = d[first];
		bezCurve[3] = d[last];
		V2Add(&bezCurve[0], V2Scale(&tHat1, dist), &bezCurve[1]);
		V2Add(&bezCurve[3], V2Scale(&tHat2, dist), &bezCurve[2]);
		return (bezCurve);
    }

    /*  First and last control points of the Bezier curve are */
    /*  positioned exactly at the first and last data points */
    /*  Control points 1 and 2 are positioned an alpha distance out */
    /*  on the tangent vectors, left and right, respectively */
    bezCurve[0] = d[first];
    bezCurve[3] = d[last];
    V2Add(&bezCurve[0], V2Scale(&tHat1, alpha_l), &bezCurve[1]);
    V2Add(&bezCurve[3], V2Scale(&tHat2, alpha_r), &bezCurve[2]);
    return (bezCurve);
}


/*
 *  Reparameterize:
 *	Given set of points and their parameterization, try to find
 *   a better parameterization.
 *
 */
static float *Reparameterize(Point2 *d, int first, int last, 
			      float *u, BezierCurve bezCurve)
{
    int 	nPts = last-first+1;	
    int 	i;
    float	*uPrime;		/*  New parameter values	*/

    uPrime = (float *)malloc(nPts * sizeof(float));
    for (i = first; i <= last; i++) {
		uPrime[i-first] = NewtonRaphsonRootFind(bezCurve, d[i], u[i-
					first]);
    }
    return (uPrime);
}



/*
 *  NewtonRaphsonRootFind :
 *	Use Newton-Raphson iteration to find better root.
 */
static float NewtonRaphsonRootFind(BezierCurve Q, Point2 P, float u)
{
    float 		numerator, denominator;
    Point2 		Q1[3], Q2[2];	/*  Q' and Q''			*/
    Point2		Q_u, Q1_u, Q2_u; /*u evaluated at Q, Q', & Q''	*/
    float 		uPrime;		/*  Improved u			*/
    int 		i;
    
    /* Compute Q(u)	*/
    Q_u = BezierII(3, Q, u);
    
    /* Generate control vertices for Q'	*/
    for (i = 0; i <= 2; i++) {
		Q1[i].x = (Q[i+1].x - Q[i].x) * 3.0;
		Q1[i].y = (Q[i+1].y - Q[i].y) * 3.0;
    }
    
    /* Generate control vertices for Q'' */
    for (i = 0; i <= 1; i++) {
		Q2[i].x = (Q1[i+1].x - Q1[i].x) * 2.0;
		Q2[i].y = (Q1[i+1].y - Q1[i].y) * 2.0;
    }
    
    /* Compute Q'(u) and Q''(u)	*/
    Q1_u = BezierII(2, Q1, u);
    Q2_u = BezierII(1, Q2, u);
    
    /* Compute f(u)/f'(u) */
    numerator = (Q_u.x - P.x) * (Q1_u.x) + (Q_u.y - P.y) * (Q1_u.y);
    denominator = (Q1_u.x) * (Q1_u.x) + (Q1_u.y) * (Q1_u.y) +
		      	  (Q_u.x - P.x) * (Q2_u.x) + (Q_u.y - P.y) * (Q2_u.y);
    
    /* u = u - f(u)/f'(u) */
    uPrime = u - (numerator/denominator);
    return (uPrime);
}

	
		       
/*
 *  Bezier :
 *  	Evaluate a Bezier curve at a particular parameter value
 * 
 */
static Point2 BezierII(int degree, Point2 *V, float t)
{
    int 	i, j;		
    Point2 	Q;	        /* Point on curve at parameter t	*/
    Point2 	*Vtemp;		/* Local copy of control points		*/

    /* Copy array	*/
    Vtemp = (Point2 *)malloc((unsigned)((degree+1) 
				* sizeof (Point2)));
    for (i = 0; i <= degree; i++) {
		Vtemp[i] = V[i];
    }

    /* Triangle computation	*/
    for (i = 1; i <= degree; i++) {	
		for (j = 0; j <= degree-i; j++) {
	    	Vtemp[j].x = (1.0 - t) * Vtemp[j].x + t * Vtemp[j+1].x;
	    	Vtemp[j].y = (1.0 - t) * Vtemp[j].y + t * Vtemp[j+1].y;
		}
    }

    Q = Vtemp[0];
    free((void *)Vtemp);
    return Q;
}


/*
 * ComputeLeftTangent, ComputeRightTangent, ComputeCenterTangent :
 *Approximate unit tangents at endpoints and "center" of digitized curve
 */
static Vector2 ComputeLeftTangent(Point2 *d, int end)
{
    Vector2	tHat1;
    tHat1 = V2SubII(d[end+1], d[end]);
    tHat1 = *V2Normalize(&tHat1);
    return tHat1;
}

static Vector2 ComputeRightTangent(Point2 *d, int end)
{
    Vector2	tHat2;
    tHat2 = V2SubII(d[end-1], d[end]);
    tHat2 = *V2Normalize(&tHat2);
    return tHat2;
}


static Vector2 ComputeCenterTangent(Point2 *d, int center)
{
    Vector2	V1, V2, tHatCenter;

    V1 = V2SubII(d[center-1], d[center]);
    V2 = V2SubII(d[center], d[center+1]);
    tHatCenter.x = (V1.x + V2.x)/2.0;
    tHatCenter.y = (V1.y + V2.y)/2.0;
    tHatCenter = *V2Normalize(&tHatCenter);
    return tHatCenter;
}


/*
 *  ChordLengthParameterize :
 *	Assign parameter values to digitized points 
 *	using relative distances between points.
 */
static float *ChordLengthParameterize(Point2 *d, int first, int last)
{
    int		i;	
    float	*u;			/*  Parameterization		*/

    u = (float *)malloc((unsigned)(last-first+1) * sizeof(float));

    u[0] = 0.0;
    for (i = first+1; i <= last; i++) {
		u[i-first] = u[i-first-1] +
	  			V2DistanceBetween2Points(&d[i], &d[i-1]);
    }

    for (i = first + 1; i <= last; i++) {
		u[i-first] = u[i-first] / u[last-first];
    }

    return(u);
}


/*
 *  ComputeMaxError :
 *	Find the maximum squared distance of digitized points
 *	to fitted curve.
*/
static float ComputeMaxError(Point2 *d, int first, int last, 
			      BezierCurve bezCurve, float *u, 
			      int *splitPoint)
{
    int		i;
    float	maxDist;		/*  Maximum error		*/
    float	dist;		/*  Current error		*/
    Point2	P;			/*  Point on curve		*/
    Vector2	v;			/*  Vector from point to curve	*/

    *splitPoint = (last - first + 1)/2;
    maxDist = 0.0;
    for (i = first + 1; i < last; i++) {
		P = BezierII(3, bezCurve, u[i-first]);
		v = V2SubII(P, d[i]);
		dist = V2SquaredLength(&v);
		if (dist >= maxDist) {
	    	maxDist = dist;
	    	*splitPoint = i;
		}
    }
    return (maxDist);
}

// Pad++ API

static void (*callback)(float *);

static void DrawBezierCurve(BezierCurve curve)
{
    callback((float*)curve);
}

/*
 *  FitBezier :
 *  	Fit a Bezier curve to a set of digitized points.
 *      Takes an array of x/y pairs (floats), a count, and an error (squared),
 *      Calls cb(float *curve), where curve is an array of x/y pairs
 *      specifying the four points of the bezier curve.
 */
void Pad_FitBezier(float *pts, int nPts, float error, void (*cb)(float*))
{
    Point2 *d = (Point2*)pts;
    Vector2	tHat1, tHat2;	/*  Unit tangent vectors at endpoints */

    callback = cb;

    tHat1 = ComputeLeftTangent(d, 0);
    tHat2 = ComputeRightTangent(d, nPts - 1);
    FitCubic(d, 0, nPts - 1, tHat1, tHat2, error);
}



