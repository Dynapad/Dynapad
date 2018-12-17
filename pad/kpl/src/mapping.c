#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#ifdef PAD_WIN
#  include "../../win/windefs.h"
#endif


static struct ponto { float x;
                      float y;    
                      float z;} ponto;

typedef float  matrix3[4][4];

typedef struct vector
  {
  float   dx,
          dy,
          dz,
          modulo;
  } vector;


matrix3 transform;

void planarmatrix();
void planarmap();
void cylmatrix();
void cylmap();
void spherematrix();
void spheremap();


void unitmatrix3(matrix)
matrix3 matrix;
{
    int jj, ii;

    for (ii = 0; ii <= 3; ii++)
    {
        for (jj = 0; jj <= 3; jj++)
            matrix[ii][jj] = 0.0;
        matrix[ii][ii] = 1.0;
    }
}  /* unitmatrix3 */


vector vector_prod(vec1,vec2)
vector *vec1;
vector *vec2;
{
vector temp;

temp.dx = (vec1->dy * vec2->dz) - (vec2->dy * vec1->dz);
temp.dy = (vec1->dz * vec2->dx) - (vec2->dz * vec1->dx);
temp.dz = (vec2->dy * vec1->dx) - (vec2->dx * vec1->dy);

return temp;
}


vector vector_unit(vec1)
vector *vec1;
{
vector temp;
float modulus;

modulus = sqrt ( vec1->dx*vec1->dx + vec1->dy*vec1->dy + vec1->dz*vec1->dz);

temp.dx = vec1->dx/modulus;
temp.dy = vec1->dy/modulus;
temp.dz = vec1->dz/modulus;

return temp;
}


vector multmatvec(matrix,vec)
matrix3 matrix;
vector *vec;
{
vector temp;

temp.dx=matrix[0][0]*vec->dx+matrix[0][1]*vec->dy+matrix[0][2]*vec->dz+matrix[0][3];
temp.dy=matrix[1][0]*vec->dx+matrix[1][1]*vec->dy+matrix[1][2]*vec->dz+matrix[1][3];
temp.dz=matrix[2][0]*vec->dx+matrix[2][1]*vec->dy+matrix[2][2]*vec->dz+matrix[2][3];

return temp;
}

struct ponto multmatpont(matrix,pont)
matrix3 matrix;
struct ponto *pont;
{
struct ponto temp;

temp.x=matrix[0][0]*pont->x+matrix[0][1]*pont->y+matrix[0][2]*pont->z+matrix[0][3];
temp.y=matrix[1][0]*pont->x+matrix[1][1]*pont->y+matrix[1][2]*pont->z+matrix[1][3];
temp.z=matrix[2][0]*pont->x+matrix[2][1]*pont->y+matrix[2][2]*pont->z+matrix[2][3];

return temp;
}


void multmatrix3(matrix1, matrix2,
result)
matrix3 matrix1;
matrix3 matrix2;
matrix3 result;
{
    int ii, jj;
    matrix3 sum;
    float temp;
    float d[4], e[4];

    for (ii = 0; ii <= 3; ii++)
    {
        d[ii] = (matrix1[ii][1] * matrix1[ii][0]) +
            (matrix1[ii][3] * matrix1[ii][2]);
        e[ii] = (matrix2[0][ii] * matrix2[1][ii]) +
            (matrix2[2][ii] * matrix2[3][ii]);
    }

    for (ii = 0; ii <= 3; ii++)
    {
        for (jj = 0; jj <= 3; jj++)
        {
            temp = (matrix1[ii][1] + matrix2[0][jj]) *
                (matrix1[ii][0] + matrix2[1][jj]);
            temp += (matrix1[ii][3] + matrix2[2][jj]) *
                (matrix1[ii][2] + matrix2[3][jj]) - d[ii] - e[jj];
            sum[ii][jj] = temp;
        }
    }
    /* copy matrix back in case result is one of the other two */
    memcpy((void *)result, (void *)sum, sizeof(matrix3));
}  /* multiplymatrix3 */


void printmatrix(matrix)
matrix3 matrix;
{
int i,j;
for (i=0;i<=3;i++)
        {
        for (j=0;j<=3;j++)
                printf("%f ",matrix[i][j]);

        printf("\n");
        }

}







void mapping  ( int nverttex,int nfacetex,
                struct ponto *texvert,
                struct ponto *textfaces,
                struct ponto *texture,
                int nvertobj,
                struct ponto *vertobj)
{
unitmatrix3 (transform);

if (nverttex==4)
   {
   planarmatrix(texvert);
   planarmap(texture,nvertobj,vertobj);
   }

else
if (nverttex==nfacetex)
   {
   cylmatrix(nverttex,texvert,textfaces);
   cylmap(texture,nvertobj,vertobj);
   }

else
   {
   spherematrix(nverttex,texvert);
   spheremap(texture,nvertobj,vertobj);
   }

}





void planarmatrix(struct ponto *vert)
{
vector xmin, xmax, ymin, ymax;
matrix3 rotate,transp,scale;
vector vx,vy,vz;
vector winp2,winp3;
float minimum;
int i,j,min,aux,aux2;
vector swap;


if ( (vert[1].y==vert[2].y) && (vert[2].y==vert[3].y) && (vert[3].y==vert[4].y) )
        {
        printf("using Z as factor\n");
        minimum=vert[1].z;
        min=1;

        for (i=2;i<=4;i++)
                {
                if (vert[i].z<minimum)
                        {
                        minimum=vert[i].z;
                        min=i;
                        }
                }
        xmin.dx=vert[min].x;
        xmin.dy=vert[min].y;
        xmin.dz=vert[min].z;

        if (min!=1)
                {
                minimum=vert[1].z;
                j=1;
                }
        else
                {
                minimum=vert[2].z;
                j=2;
                }

        for (i=2;i<=4;i++)
                {
                if ( (vert[i].z<minimum) && (i!=min) )
                        {
                        minimum=vert[i].z;
                        j=i;
                        }
                }

        xmax.dx=vert[j].x;
        xmax.dy=vert[j].y;
        xmax.dz=vert[j].z;

        if (xmax.dx<xmin.dx)
                {
                swap=xmin;
                xmin=xmax;
                xmax=swap;
                }
        
        for (i=1;i<=4;i++) /* find the other 2 points */
                {
                if ( (i!=j) && (i!=min) )
                        aux=i;
                }
        
        for (i=1;i<=4;i++) /* find the last points */
                {
                if ( (i!=j) && (i!=min) && (i!=aux) )
                        aux2=i;
                }
        
        ymin.dx=vert[aux].x;
        ymin.dy=vert[aux].y;
        ymin.dz=vert[aux].z;

        ymax.dx=vert[aux2].x;
        ymax.dy=vert[aux2].y;
        ymax.dz=vert[aux2].z;
        
        if (ymax.dx<ymin.dx)
                {
                swap=ymin;
                ymin=ymax;
                ymax=swap;
                }
        }
else 
        {
        minimum=vert[1].y;
        min=1;

        for (i=2;i<=4;i++)
                {
                if (vert[i].y<minimum)
                        {
                        minimum=vert[i].y;
                        min=i;
                        }
                }
        xmin.dx=vert[min].x;
        xmin.dy=vert[min].y;
        xmin.dz=vert[min].z;

        if (min!=1)
                {
                minimum=vert[1].y;
                j=1;
                }
        else 
                {
                minimum=vert[2].y;
                j=2;
                }

        for (i=2;i<=4;i++)
                {
                if ( (vert[i].y<minimum) && (i!=min) )
                        {
                        minimum=vert[i].y;
                        j=i;
                        }
                }

        xmax.dx=vert[j].x;
        xmax.dy=vert[j].y;
        xmax.dz=vert[j].z;

        if (xmax.dx<xmin.dx)
                {
                swap=xmin;
                xmin=xmax;
                xmax=swap;
                }

        for (i=1;i<=4;i++) /* find the other 2 points */
                {
                if ( (i!=j) && (i!=min) )
                        aux=i;
                }
        
        for (i=1;i<=4;i++) /* find the last points */
                {
                if ( (i!=j) && (i!=min) && (i!=aux) )
                        aux2=i;
                }
        
        ymin.dx=vert[aux].x;
        ymin.dy=vert[aux].y;
        ymin.dz=vert[aux].z;

        ymax.dx=vert[aux2].x;
        ymax.dy=vert[aux2].y;
        ymax.dz=vert[aux2].z;
        
        if (ymax.dx<ymin.dx)
                {
                swap=ymin;
                ymin=ymax;
                ymax=swap;
                }
        }

vx.dx=xmax.dx-xmin.dx;
vx.dy=xmax.dy-xmin.dy;
vx.dz=xmax.dz-xmin.dz;

vx=vector_unit(&vx);
unitmatrix3(rotate);
rotate[0][0]=vx.dx;
rotate[0][1]=vx.dy;
rotate[0][2]=vx.dz;

vy.dx=ymin.dx-xmin.dx;
vy.dy=ymin.dy-xmin.dy;
vy.dz=ymin.dz-xmin.dz;

vy=vector_unit(&vy);
rotate[1][0]=vy.dx;
rotate[1][1]=vy.dy;
rotate[1][2]=vy.dz;

vz=vector_prod(&vx,&vy);
rotate[2][0]=vz.dx;
rotate[2][1]=vz.dy;
rotate[2][2]=vz.dz;

unitmatrix3(transp);
transp[0][3]=-xmin.dx;
transp[1][3]=-xmin.dy;
transp[2][3]=-xmin.dz;

multmatrix3(rotate,transp,transform);

winp2=multmatvec(transform,&xmax);
winp3=multmatvec(transform,&ymin);

unitmatrix3(scale);
scale[0][0]=1/winp2.dx;
scale[1][1]=1/winp3.dy;

multmatrix3(scale,transform,transform);
}



void planarmap(struct ponto *text,int nvert, struct ponto *vert)
{
int i;

for (i=1;i<=nvert;i++)
        text[i]=multmatpont(transform,&vert[i]);

}


void cylmatrix(int nvert,struct ponto *vert,struct ponto *faces)
{
int i,half;
vector midup,midlow,vx,vy,vz,aux;
matrix3 rotate,transp,scale;
float x=0,y=0,z=0;


half=nvert/2;
for (i=1;i<=half;i++)
        {
        x+=vert[i].x;
        y+=vert[i].y;
        z+=vert[i].z;
        }
x /= half;
y /= half;
z /= half;
midlow.dx=x;
midlow.dy=y;
midlow.dz=z;

x=0;
y=0;
z=0;
for (i=(half+1);i<=nvert;i++)
        {
        x+=vert[i].x;
        y+=vert[i].y;
        z+=vert[i].z;
        }
x /= half;
y /= half;
z /= half;
midup.dx=x;
midup.dy=y;
midup.dz=z;

vy.dx=midup.dx-midlow.dx;
vy.dy=midup.dy-midlow.dy;
vy.dz=midup.dz-midlow.dz;

vy=vector_unit(&vy);
if ( (vy.dx!=1) || (vy.dy!=-1) )
        {
        aux.dx=1;
        aux.dy=0;
        aux.dz=0;

        vz=vector_prod(&aux,&vy);
        vx=vector_prod(&vy,&vz);
        }
else
        {
        if (vy.dx==1)
                {
                vx.dx=0;
                vx.dy=1;
                vx.dz=0;
                vz.dx=0;
                vz.dy=0;
                vz.dz=1;
                }
        else
                {
                vx.dx=0;
                vx.dy=-1;
                vx.dz=0;
                vz.dx=0;
                vz.dy=1;
                vz.dz=0;
                }
        }


unitmatrix3(rotate);
rotate[0][0]=vx.dx;
rotate[0][1]=vx.dy;
rotate[0][2]=vx.dz;
rotate[1][0]=vy.dx;
rotate[1][1]=vy.dy;
rotate[1][2]=vy.dz;
rotate[2][0]=vz.dx;
rotate[2][1]=vz.dy;
rotate[2][2]=vz.dz;

unitmatrix3(transp);
transp[0][3]=-midlow.dx;
transp[1][3]=-midlow.dy;
transp[2][3]=-midlow.dz;

multmatrix3(rotate,transp,transform);

aux=multmatvec(transform,&midup);
unitmatrix3(scale);
scale[1][1]=1/aux.dy;

multmatrix3(scale,transform,transform);
}


void cylmap (struct ponto *text,int nvert,struct ponto *vert)
{
int i;
struct ponto *aux;
float modulus;

aux=(struct ponto *)malloc((nvert+1)*sizeof(struct ponto));

for (i=1;i<=nvert;i++)
        {
        aux[i]=multmatpont(transform,&vert[i]);
        
        text[i].y=aux[i].y;
        if (aux[i].z >= 0)
                {
                modulus = sqrt(aux[i].z*aux[i].z + aux[i].x*aux[i].x);
                text[i].x=((aux[i].x/modulus)+3)/4;
                }

        if (aux[i].z < 0)
                {
                modulus = sqrt(aux[i].z*aux[i].z + aux[i].x*aux[i].x);
                text[i].x=(1-(aux[i].x/modulus))/4;
                }
        }
}


void spherematrix(int nvert,struct ponto *vert)
{
float x=0,y=0,z=0;
int i;

for (i=1;i<=nvert;i++)
        {
        x+=vert[i].x;
        y+=vert[i].y;
        z+=vert[i].z;
        }

x /= nvert;
y /= nvert;
z /= nvert;

unitmatrix3(transform);
transform[0][3]=-x;
transform[1][3]=-y;
transform[2][3]=-z;

}


void spheremap(struct ponto *text,int nvert,struct ponto *vert)
{
int i;
struct ponto *aux;
float modulus;

aux=(struct ponto *)malloc((nvert+1)*sizeof(struct ponto));

for (i=1;i<=nvert;i++)
        {
        aux[i]=multmatpont(transform,&vert[i]);
        
        /* calculate y */
        modulus = sqrt(aux[i].x*aux[i].x + aux[i].y*aux[i].y + aux[i].z*aux[i].z);
        text[i].y=((aux[i].y/modulus)+1)/2;
        
        /* calculate x and z */
        if (aux[i].z >= 0)
                {
                modulus = sqrt(aux[i].z*aux[i].z + aux[i].x*aux[i].x);
                text[i].x=((aux[i].x/modulus)+3)/4;
                }

        if (aux[i].z < 0)
                {
                modulus = sqrt(aux[i].z*aux[i].z + aux[i].x*aux[i].x);
                text[i].x=(1-(aux[i].x/modulus))/4;
                }
        }        

}

