
/* Convert between matrix and Roll/Pitch/Yaw.
   Adapted from Paul, Richard P., "Robot Manipulators" MIT Press, 1979, pg 67.

   Also pack and unpack into/from ascii strings.

		(KP, 2/95)
*/

#include <math.h>

#ifdef PAD_WIN
#  include "../../win/windefs.h"
#endif

#ifndef M_PI
#  define M_PI 3.1415926
#endif
#define K 52
#define KK (K*K/2)

#define Nx m[0]
#define Ox m[1]
#define Ax m[2]
#define Px m[3]

#define Ny m[4]
#define Oy m[5]
#define Ay m[6]
#define Py m[7]

#define Nz m[8]
#define Oz m[9]
#define Az m[10]
#define Pz m[11]

/* convert matrix to position and orientation */

void mat_to_po(float m[16], float po[6])
{
	float c, s;

	po[0] = Px;
	po[1] = Py;
	po[2] = Pz;

	po[3] = atan2(Ny, Nx);
	c     = cos(po[3]);
	s     = sin(po[3]);
	po[4] = atan2(-Nz, c * Nx + s * Ny);
	po[5] = atan2(s * Ax - c * Ay, -s * Ox + c * Oy);
}

/* convert to matrix from position and orientation */

void mat_from_po(float m[16], float po[6])
{
	float c1, s1, c2, s2, c3, s3;

	Px = po[0];
	Py = po[1];
	Pz = po[2];

	c1 = cos(po[3]); s1 = sin(po[3]);
	c2 = cos(po[4]); s2 = sin(po[4]);
	c3 = cos(po[5]); s3 = sin(po[5]);

	Nx = c1 * c2;
	Ny = s1 * c2;
	Nz = -s2;

	Ox = c1 * s2 * s3 - s1 * c3;
	Oy = s1 * s2 * s3 + c1 * c3;
	Oz = c2 * s3;

	Ax = c1 * s2 * c3 + s1 * s3;
	Ay = s1 * s2 * c3 - c1 * s3;
	Az = c2 * c3;

	m[12] = 0.;
	m[13] = 0.;
	m[14] = 0.;
	m[15] = 1.;
}

/* auxiliary routines for string packing/unpacking */

static int i_to_c(int i)
{
	return (i < 26) ? 'a' + i : 'A' + i - 26;
}

static int c_to_i(int c)
{
	return (c >= 'a') ? c - 'a' : c - 'A' + 26;
}

static void pack(char str[2], int x)
{
	if (x < 0)
		x = 0;
	else if (x >= K*K)
		x = K*K-1;

	str[0] = i_to_c( x / K );
	str[1] = i_to_c( x % K );
}

static int unpack(char str[2])
{
	return K * c_to_i(str[0]) + c_to_i(str[1]);
}

static float R = 2.0;
static float A = M_PI;
static float B = M_PI/2;
static float C = M_PI;

/* set the greatest position range that can be encoded */

void str_mat_range(float r)
{
	R = r;
}

/* convert to string from position and orientation */

void str_from_po(char str[], float po[])
{
	pack(str+ 0, (int)(KK * (po[0] + R) / R) );
	pack(str+ 2, (int)(KK * (po[1] + R) / R) );
	pack(str+ 4, (int)(KK * (po[2] + R) / R) );
	pack(str+ 6, (int)(KK * (po[3] + A) / A) );
	pack(str+ 8, (int)(KK * (po[4] + B) / B) );
	pack(str+10, (int)(KK * (po[5] + C) / C) );
	str[12] = '\0';
}

/* convert from string to position and orientation */

void str_to_po(char str[], float po[])
{
	while (*str && *str == ' ')
		str++;

	po[0] = R / KK * unpack(str+ 0) - R;
	po[1] = R / KK * unpack(str+ 2) - R;
	po[2] = R / KK * unpack(str+ 4) - R;
	po[3] = A / KK * unpack(str+ 6) - A;
	po[4] = B / KK * unpack(str+ 8) - B;
	po[5] = C / KK * unpack(str+10) - C;
}

