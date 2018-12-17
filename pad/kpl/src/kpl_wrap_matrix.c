
/* kpl wrapper for useful math routines */

#include <math.h>
#include "kpl.h"

#ifdef PAD_WIN
#  include "../../win/windefs.h"
#endif

static float a[16], b[16], c[16], d[16];

extern int      ephemeral(int id);
extern void	identity(float m[16]);
extern void	invert(float m[16]);
extern void     mat_from_po(float m[16], float po[6]);
extern void     mat_to_po(float m[16], float po[6]);
extern float	matscale(float m[16]);
extern void	movmat(float m[16]);
extern void	mult(float m[16], float m1[16]);
extern void	multsgi(float m[16], float m1[16]);
extern void	orient(float m[16], float m1[16]);
extern void	rotmat(float m[16], float theta);
extern void	scamat(float m[16]);
extern void     str_from_po(char str[], float po[]);
extern void     str_to_po(char str[], float po[]);
extern void	transpose(float m[16], float mt[16]);
extern void	veer(float m[16], float p1[3], float p2[3], float t);
extern int      vget(int i, int j, float *data);
extern void     vset(int i, int j, float *data, int len);
extern void	xform(float v[4], float m[16]);

static void wrap_mat_to_po(void)	/* Mat ~ => PO */
{
	kpl_pop(a);
	mat_to_po(a, b);
	kpl_push(b, 6);
}

static void wrap_po_to_mat(void)	/* PO ~ => Mat */
{
	kpl_pop(a);
	mat_from_po(b, a);
	kpl_push(b, 16);
}

static void wrap_mat_to_str(void)	/* Mat ~ => Str */
{
	char str[13];

	kpl_pop(a);
	mat_to_po(a, b);
	str_from_po(str, b);
	kpl_push(a, ephemeral(kpl_sscanvalue(str, a)));
}

static void wrap_str_to_mat(void)	/* Offset Str ~ => Mat */
{
	char str[13];

	kpl_pop(c);
	kpl_sprintvalue(str, a, kpl_pop(a));
	str_to_po(str, b);
        b[0] += c[0];
        b[1] += c[1];
        b[2] += c[2];
	mat_from_po(a, b);
	kpl_push(a, 16);
}

static float po[2][1000][6];

static void update_po(int vec, int j, float *a)
{
	float m[16];
	int k;

	for (k = 0 ; k < 6 ; k++) {
		po[0][j][k] = po[1][j][k];
		po[1][j][k] = a[k];
	}
	mat_from_po(m, a);
	vset(vec, j, m, 16);
}

/* unpack a vector of move/rotate matrices from a string, adding an x:y:z offset */

static void wrap_unpack_matrices(void)	/* str x:y:z lo[:hi] vec ~ */
{
	int vec, j, len, lo, hi;
	char str[1024], *s;

	kpl_pop(d);
	vec = d[0];

	len = kpl_pop(c);
	lo = c[0];
	hi = (len > 1) ? c[1] : lo + 512 / 12;

	kpl_pop(b);
	kpl_sprintvalue(str, a, kpl_pop(a));

	for (s = str ; isspace(*s) ; s++)
		;
	for (j = 0 ; j <= hi - lo && isalpha(s[12*j]) ; j++) {
		str_to_po(s + 12 * j, a);
		a[0] += b[0];
		a[1] += b[1];
		a[2] += b[2];
		update_po(vec, lo + j, a);
	}
}

/* in case we miss a frame, it's useful to approximate matrices from the previous two */

static void wrap_avg_matrices(void)	/* lo:hi vec ~ */
{
	int vec, j, len, lo, hi, k;

	kpl_pop(b);
	vec = b[0];

	len = kpl_pop(a);
	lo = a[0];
	hi = a[1];

	for (j = lo ; j <= hi ; j++) {
		for (k = 0 ; k < 6 ; k++)
			a[k] = 2 * po[1][j][k] - po[0][j][k];
		update_po(vec, j, a);
	}
}

/* pack a vector of move/rotate matrices into a string */

static void wrap_pack_matrices(void)	/* lo[:hi] vec ~ => str */
{
	int vec, j, len, lo, hi;
	float m[16];
	char str[1024];

	kpl_pop(b);
	vec = b[0];

	len = kpl_pop(a);
	lo = a[0];
	hi = (len > 1) ? a[1] : lo + 512 / 12;

	for (j = 0 ; j <= hi - lo && (len = vget(vec, lo + j, m)) ; j++) {
		mat_to_po(m, a);
		str_from_po(str + 12 * j, a);
	}

	kpl_push(a, ephemeral(kpl_sscanvalue(str, a)));
}

static void wrap_identity(void)
{
	identity(a);
	kpl_push(a, 16);
}

static void wrap_movmat(void)
{
	int la;

	la = kpl_pop(a);
	if (la < 2)
		a[1] = 0.;
	if (la < 3)
		a[2] = 0.;
	movmat(a);
	kpl_push(a, 16);
}

static void wrap_rotmat(void)
{
	kpl_pop(b);
	kpl_pop(a);
	rotmat(a, b[0]);
	kpl_push(a, 16);
}

static void wrap_scamat(void)
{
	int la;

	la = kpl_pop(a);
	if (la < 1)
		a[0] = 1.;
	if (la < 2)
		a[2] = a[1] = a[0];
	else if (la < 3)
		a[2] = 1.;
	scamat(a);
	kpl_push(a, 16);
}

static void wrap_transpose(void)
{
	kpl_pop(a);
	transpose(a, b);
	kpl_push(b, 16);
}

static void wrap_matscale(void)
{
	kpl_pop(a);
	a[0] = matscale(a);
	kpl_push(a, 1);
}

static void wrap_mult(void)
{
	kpl_pop(b);
	kpl_pop(a);
	mult(a, b);
	kpl_push(a, 16);
}        

static void wrap_multsgi(void)
{
	kpl_pop(b);
	kpl_pop(a);
	multsgi(a, b);
	kpl_push(a, 16);
}        

static void wrap_orient(void)
{
	kpl_pop(b);
	kpl_pop(a);
	orient(a, b);
	kpl_push(a, 16);
}
     
static void wrap_veer(void)		/* matrix point1 point2 fraction ~ => matrix */
{
	kpl_pop(d);
	if (kpl_pop(c) < 4)
		c[3] = 1.;
	if (kpl_pop(b) < 4)
		b[3] = 1.;
	kpl_pop(a);
	veer(a, b, c, d[0]);
	kpl_push(a, 16);
}

static void wrap_xform(void)
{
	int la, lb;

	lb = kpl_pop(b);
	la = kpl_pop(a);
	if (la < 4)
		a[3] = 1.;
	xform(a, b);
	kpl_push(a, la);
}

static void wrap_invert(void)
{
	kpl_pop(a);
	invert(a);
	kpl_push(a, 16);
}

void install_kpl_matrix(void)
{
	kpl_install("avg_matrices",	(Proc)wrap_avg_matrices);
	kpl_install("identity",		(Proc)wrap_identity);
	kpl_install("invert",		(Proc)wrap_invert);
	kpl_install("mat_to_po",	(Proc)wrap_mat_to_po);
	kpl_install("mat_to_str",	(Proc)wrap_mat_to_str);
	kpl_install("matscale",		(Proc)wrap_matscale);
	kpl_install("movmat",		(Proc)wrap_movmat);
	kpl_install("mult",		(Proc)wrap_mult);
	kpl_install("multsgi",		(Proc)wrap_multsgi);
	kpl_install("orient",		(Proc)wrap_orient);
	kpl_install("pack_matrices",	(Proc)wrap_pack_matrices);
	kpl_install("po_to_mat",	(Proc)wrap_po_to_mat);
	kpl_install("rotmat",		(Proc)wrap_rotmat);
	kpl_install("scamat",		(Proc)wrap_scamat);
	kpl_install("str_to_mat",	(Proc)wrap_str_to_mat);
	kpl_install("transpose",	(Proc)wrap_transpose);
	kpl_install("unpack_matrices",	(Proc)wrap_unpack_matrices);
	kpl_install("veer",		(Proc)wrap_veer);
	kpl_install("xform",		(Proc)wrap_xform);
}

