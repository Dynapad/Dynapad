
/*
(SGI ONLY)

    void gl_tmesh(float **v, float **n, float **t, int ntriangles);

Takes a list of vetices, normals and textures for triangular polygons.
(textures may be NULL to indicate no texture). Uses bgntmesh/endtmesh to draw
the triangles. Attempts to take advantage of the GL vertex registers - so
that multiple triangles are output between a bgntmesh/endtmesh pair, with
a considerable saving on the number of vertices passed to GL.

Note: figuring out if the vertex registers can be reused is a fair amount
of work. This work should be done within a makeobj/closeobj pair to
save the computation (or the vertex raw file data should be modified to
include the required info).
*/

int wireframe = 0;

#ifdef SGI

#include <gl/gl.h>

/* defines */
#define vertex(N) { n3f(n[N]); if (t) t2f(t[N]); v3f(v[N]); }
#define V_EQ(v1, v2) (v1[0] == v2[0] && v1[1] == v2[1] && v1[2] == v2[2])
#define UNDEFINED -1

void gl_tmesh(float **v, float **n, float **t, int ntriangles)
{
    /* these are used to mirror the two gl vertex registers */
    float *V0, *V1;
    int older;          /* either 0 or 1 */

    if (!ntriangles)
        return;

    /* handle wireframe mode */
    if (wireframe) {
        while (ntriangles) {
            bgnclosedline();
            vertex(0); vertex(1); vertex(2);
            endclosedline();
            n += 3;
            v += 3;
            if (t) t += 3;
            ntriangles--;
        }
        return; /* done */
    }


    bgntmesh();                 /* start first tmesh */
    vertex(0); V0 = v[0];       /* set vertex registers */
    vertex(1); V1 = v[1];
    older = 0;

    while (ntriangles) {
        int other;

        /* Determine which two vertices in the current triangle map onto
           tmesh registers, and set -other- to be the third vertex. If no
           two vertices are in the registers, -other- remains UNDEFINED.
        */
        other = UNDEFINED;
        if (V_EQ(V0, v[0])) {
            if (V_EQ(V1, v[1])) {
                other = 2;
            } else if (V_EQ(V1, v[2])) {
                other = 1;
            }
        } else if (V_EQ(V0, v[1])) {
            if (V_EQ(V1, v[0])) {
                other = 2;
            } else if (V_EQ(V1, v[2])) {
                other = 0;
            }
        } else if (V_EQ(V0, v[2])) {
            if (V_EQ(V1, v[0])) {
                other = 1;
            } else if (V_EQ(V1, v[1])) {
                other = 0;
            }
        }

        /* if -other- is still UNDEFINED, we can't reuse current contents of
           vertex registers so start a new tmesh.
        */
        if (other == UNDEFINED) {
            endtmesh();             /* end existing tmesh */
            bgntmesh();             /* start new one */
            vertex(0); V0 = v[0];   /* set registers */
            vertex(1); V1 = v[1];
            older = 0;
            other = 2;
        }

        /* Now look at what the next triangle would like to have left in
           the vertex registers and select which register the new vertex
           should replace.
        */
        if (ntriangles > 1) {
            /* look at next triangle */
            float *next_v0 = v[3];
            float *next_v1 = v[4];
            float *next_v2 = v[5];
            if (V_EQ(V0, next_v0) || V_EQ(V0, next_v1) || V_EQ(V0, next_v2)) {
                /* want to keep V0 */
                if (older == 0) {               /* swap to preserve V0 */
                    swaptmesh(); older = 1;
                }
            } else if (V_EQ(V1, next_v0) || V_EQ(V1, next_v1) || V_EQ(V1, next_v2)) {
                /* want to keep V0 */
                if (older == 1) {               /* swap to preserve V1 */
                    swaptmesh(); older = 0;
                }
            }
        }

        vertex(other);          /* output the new vertex */
        if (older == 1) {       /* update the vertex registers */
            V1 = v[other];
            older = 0;
        } else {
            V0 = v[other];
            older = 1;
        }

        ntriangles--;                /* housekeeping */
        n += 3;
        v += 3;
        if (t) t += 3;
    }

    endtmesh();

#undef V_EQ
#undef vertex
}

#endif
