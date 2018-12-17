#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

#ifdef PAD_WIN
#  include "../../win/windefs.h"
#endif

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

#define PIVOT -1  /* object that is a pivot */
#define MAP -1
#define NOPIV -2  /* object that doesn't have a pivot */
#define NOMAP -2

static struct ponto { float x;
		      float y;    /* Um ponto */
		      float z;} ponto;

static int 
	nobj,    /* no. de objetos contidos na cena */
	npivots, /* no. de pivots na cena */
	nmaps,   /* no. de icones de mapas */
	i,j,     /* Contadores */
	VERBOSE;

static FILE     *fp_dxf,
		*filep_raw;

static char    
	**obj_name;     

static char     layer[6],
		vertex[7],
		search_name[20];

static struct ponto
	**vertices, /* Vetor que acumulara os */
		    /* vertices dos objetos   */
	
	**triang,   /* Vetor que acumulara o numero dos */
		    /* vertices que compoe as faces     */
	**normals, 
	
	**texture;  /* Texture vertices */


static int     *nvertices, /* Numero de vertices de cada objeto */
	       *ntriang,   /* Numero de ligacoes de cada objeto */
	       *pivtag,    /* Knows if object have a pivot or not */
	       *maptag;    /* Knows if object have a map or not */

static struct ponto 
		*pivotpt;


/* Prototipos */
void dxf2raw(char *);
int objects(void);
void tags(void);
int alloc_vertex(void);
void read_triang(void);
void calc_normal(void);
void normaliz(int,int);
void raw_file(void);
void pivot(int);
extern void mapping   ( int,int,struct ponto *,
			struct ponto *,struct ponto *,
			int,struct ponto *);


void dxf2raw(char *fname)
{
char dxfname[20],rawname[20];

strcpy (dxfname,fname);
strcat (dxfname,".dxf");
strcpy (rawname,fname);
strcat (rawname,".raw");

if ( (fp_dxf=fopen(dxfname,"r")) == NULL )
	{
	printf("\nFile %s not found\n",dxfname);
	exit (1);
	}

if ( (filep_raw=fopen(rawname,"w")) == NULL )
	{
	printf("\nI could not create file %s\n",rawname);
	exit(1);  /* Tchau */
	}

printf("Please wait... Convert %s to %s\n",dxfname,rawname);
objects();

alloc_vertex();

tags();

read_triang();
printf("\n");

calc_normal();
printf("\n");

raw_file();
printf("\n");

if ( fclose(fp_dxf) != 0 )
	{
	printf("\nError when closing file %s\n",dxfname);
	exit (1);
	}

if ( fclose(filep_raw) != 0 )
	{
	printf("\nError when closing file %s\n",rawname);
	exit (1);
	}


for (i=0;i<=nobj+1;i++)
	{
	free(vertices[i]);
	free(normals[i]);
	free(triang[i]);
	free(obj_name[i]);
	free(texture[i]);
	}

free(vertices);
free(normals);
free(triang);
free(nvertices);
free(triang);
free(pivtag);
free(maptag);
free(obj_name);
free(pivotpt);
free(texture);

printf("End of convertion!\n");
}




/*-----------------------------------------*/
/* int objects(): define the number and    */
/* the name of the objects in the scene,   */
/* and allocates memory for **object_name  */
/*-----------------------------------------*/

int objects(void)
{
char    lixo[20]; /* Serve para acumular strings inuteis */

nobj=0;
npivots=0;
nmaps=0;

/* Numero de Layers:              */
/* acha o primeira string=LAYER.  */

while ( strcmp(layer,"LAYER") != 0 )
	{

	if ( fscanf(fp_dxf,"%s",layer) == 0 )
		{
		fprintf(stderr,"\nError in the DXF file\n");
		fprintf(stderr,"probably this file isn't a DXF file.\n");
		exit(1);   /* ERROR */
		}

	}



/* o proximo argumento deve ser 70    */
/* o outro  deve ser o no. de objetos */

fscanf (fp_dxf,"%s %d",lixo,&nobj);


/* Aloca pivotpt */
pivotpt = (struct ponto *)malloc ((nobj+1)*sizeof(struct ponto));

/* Aloca memoria para obj_name de acordo com o no. de objetos */
obj_name=(char **)malloc( (nobj+2)*sizeof(char *) );
if ( obj_name == NULL )
	{
	fprintf(stderr,"\nError in memory allocation\n");
	fprintf(stderr,"function OBJECTS\n");
	exit(1);   /* ERRO */
	}


/* Aloca memoria para os nomes dos objetos */
for (i=0;i<=nobj;i++)
	obj_name[i]=(char *)malloc( 20 * sizeof(char) );


/* Coloca o nome dos objetos no obj_name */
for (i=0;i<nobj;i++)
	{

	/* acha o proxima string=LAYER.  */

	do
		fgets (layer,6,fp_dxf);
	while ( strcmp(layer,"LAYER") != 0 );



	/* Primeiro Argumento: 2 (lixo)      */
	/* Segundo argumento: nome do objeto */

	fscanf(fp_dxf,"%s %s",lixo,obj_name[i]);
	}
	
return 0;
}




void tags(void) /* Will "tag" every object that have a map or a pivot */
{
char oriname[20];  /* Original name */
int lenght;

nmaps=0;
npivots=0;
	
for (i=0;i<nobj;i++)
	{
	maptag[i]=NOMAP;  /* by default, objects don't have maps or pivots */
	pivtag[i]=NOPIV;
	}

for (i=0;i<nobj;i++)
	{
	lenght=strlen(obj_name[i]);
	
	if ( (obj_name[i][lenght-2]=='_')&&(obj_name[i][lenght-1]=='P') )
		{
		pivtag[i]=PIVOT; /* object[i] is a pivot */
		npivots++;

		strcpy(oriname,obj_name[i]);
		for (j=0;j<nobj;j++)
			{
			if ( (strncmp(oriname,obj_name[j],lenght-2)==0) && (i!=j) 
			     && (obj_name[j][lenght-2]!='_') 
			     && (obj_name[j][lenght-1]!='M') )
				
				pivtag[j]=i; /* Pivot of object[j] is object[i] */
			}
		}

	else
	if ( (obj_name[i][lenght-2]=='_')&&(obj_name[i][lenght-1]=='M') )
		{
		maptag[i]=MAP;
		nmaps++;

		strcpy(oriname,obj_name[i]);
		for (j=0;j<nobj;j++)
			{
			if ( (strncmp(oriname,obj_name[j],lenght-2)==0) && (i!=j) 
			     && (obj_name[j][lenght-2]!='_') 
			     && (obj_name[j][lenght-1]!='P') )
				
				maptag[j]=i;
			}
		}
	}
}



/*-------------------------------------------*/
/*  int alloc_vertex(): allocates memory for */
/*  several arrays...                        */
/*-------------------------------------------*/
int alloc_vertex()
{

long    pos_arq;     /* Posicao do arquivo, para ser */
		     /* Guardada e depois restaurada */

int     lixo,        /* informacoes inuteis do DXF */
	tipo_obj;    /* tipo do objeto             */




/* Aloca Vertices de acordo com o numero de objetos */
vertices = (struct ponto **)malloc( (nobj+2)*sizeof(struct ponto *) );
nvertices =(int *) malloc ( (nobj+2)*sizeof(int));
triang =(struct ponto **) malloc ( (nobj+2)*sizeof(struct ponto *) );
ntriang=(int *) malloc ( (nobj+2)*sizeof(int));
pivtag = (int *)malloc ( (nobj+2)*sizeof(int));
maptag = (int *)malloc ( (nobj+2)*sizeof(int));
normals=(struct ponto **) malloc ( (nobj+2)*sizeof(struct ponto *) );
texture=(struct ponto **) malloc ( (nobj+2)*sizeof(struct ponto *) );

/* Grava-se a posicao atual do arquivo */
pos_arq = ftell(fp_dxf);


/* Aloca memoria para os vertices de cada objeto */
for (i=0;i<nobj;i++)
	{

	/* procura a primeira ocorrencia */
	/* do nome do objeto em questao  */

	/* copia o nome do objeto no copia_nome */
	/* serve para fazer funcionar o strcmp  */
	char copia_nome[20];


	strcpy(copia_nome,obj_name[i]);
	while ( strcmp(search_name,copia_nome) != 0 )
		fscanf (fp_dxf,"%s",search_name);

	/* Proximos argumentos:  */
	/* 66: 1                 */
	/* 70: tipo de  objeto   */
	/* 71: no. de vertices   */
	/* 72: no. de triangulos */

	fscanf (fp_dxf,"%d %d %d",&lixo,&lixo,&lixo);
	fscanf (fp_dxf,"%d",&tipo_obj);
	if (tipo_obj != 64)
		{
		fprintf (stderr,"\nobject %s ",obj_name[i]);
		fprintf (stderr," not supported by the converter\n");
		exit (1);
		}

	fscanf (fp_dxf,"%d %d",&lixo,&nvertices[i]);
	fscanf (fp_dxf,"%d %d",&lixo,&ntriang[i]);

	vertices[i] =(struct ponto *) malloc( (nvertices[i]+2)*sizeof(struct ponto) );
	triang[i] = (struct ponto *)malloc ( (ntriang[i]+2)*sizeof(struct ponto) );
	normals[i] =(struct ponto *) malloc( (nvertices[i]+2)*sizeof(struct ponto) );
	texture[i]=(struct ponto *)malloc( (nvertices[i]+2)*sizeof(struct ponto) );
	}  /* repete para o proximo objeto */ 


/* restora a posicao do arquivo */
fseek (fp_dxf,pos_arq,0);



return 0; /* Tchau */

}



/*-----------------------------------------------*/
/* int read_triang(): le os triangulos, armazena */
/* os vetores de vertices, triang e normals      */
/*-----------------------------------------------*/

void read_triang()
{
int     lixo; /* lixo recebe 10,20 ou 30 */
char   slixo[10]; /* string inutil */

float   X,
	Y, /* Coordenadas do vertice */
	Z;

int     V1, /* Sao os tres vertices que serao escolhidos pelo  */
	V2, /* DXF para compor uma face, a partir de uma lista */
	V3; /* de vertices disponivel pelo arquivo             */



/* Grava os vertices no vetor *vertices */
/* para cada objeto.                    */

for (i=0;i<nobj;i++)
	{
	printf("reading object %s\n",obj_name[i]);

	/* Procura os VERTICES do objeto */
	if (VERBOSE==1)
		printf("lendo %d vertices\n",nvertices[i]);

	for (j=1;j<=nvertices[i];j++)
		{

		if (VERBOSE==2)
			printf("vertice %d de %d vertices\n",j,nvertices[i]);

		/* procura a string VERTEX */
		while ( strcmp(vertex,"VERTEX") != 0)
			fscanf (fp_dxf,"%s",vertex);

		/* proximos argumentos:   */
		/* 8, nome do objeto,     */
		/* 10: coord. X,          */
		/* 20: coord. Y,          */
		/* 30: coord. Z,          */
		/* 70, 192, 0             */

		fscanf (fp_dxf,"%d %s",&lixo,slixo);
		fscanf (fp_dxf,"%d %f",&lixo,&X);
		fscanf (fp_dxf,"%d %f",&lixo,&Y);
		fscanf (fp_dxf,"%d %f",&lixo,&Z);

		vertices[i][j].x=X;
		vertices[i][j].y=Y;
		vertices[i][j].z=Z;


		strcpy (vertex,""); /* limpa vertex */
		}


	/* faz as LIGACOES dos vertices para compor as faces */

	if (VERBOSE==1)
		printf("executando %d triangulos\n",ntriang[i]);

	for (j=1;j<=ntriang[i];j++)
		{
		int pula;

		if (VERBOSE==2)
			printf("triangulo %d de %d triangulos\n",j,ntriang[i]);

		/* procura a string VERTEX */
		while ( strcmp(vertex,"VERTEX") != 0)
			fscanf (fp_dxf,"%s",vertex);



		for (pula=1; pula<=10; pula++)
			fscanf (fp_dxf,"%s",slixo);

		fscanf (fp_dxf,"%d %d",&lixo,&V1);
		fscanf (fp_dxf,"%d %d",&lixo,&V2);
		fscanf (fp_dxf,"%d %d",&lixo,&V3);



		/* As faces escondidas sao determinadas  */
		/* por numeros negativos do arquivo DXF  */
	
		/* If it's not a map */
		if ( maptag[i] != MAP )
			{
			V1=abs(V1);
			V2=abs(V2);
			V3=abs(V3);
			}                                

		triang[i][j].x=V1;
		triang[i][j].y=V2;
		triang[i][j].z=V3;


		strcpy (vertex,""); /* limpa vertex */

		}
	}
}




void normaliz(int objt,int vert)
{
float modulo;

modulo=normals[objt][vert].x*normals[objt][vert].x;
modulo+=normals[objt][vert].y*normals[objt][vert].y;
modulo+=normals[objt][vert].z*normals[objt][vert].z;

modulo = sqrt(modulo);
if (modulo != 0)
	{
	normals[objt][vert].x /= modulo;
	normals[objt][vert].y /= modulo;
	normals[objt][vert].z /= modulo;
	}
else
	{
	printf("Modulus = 0\n");
	}

}



void pivot(int obj)
{
float x=0,y=0,z=0;

printf("calculating center of pivot %s\n",obj_name[i]);

for (j=1;j<=nvertices[obj];j++)
	{
	x+=vertices[obj][j].x;
	y+=vertices[obj][j].y;
	z+=vertices[obj][j].z;
	}
x /= nvertices[obj];
y /= nvertices[obj];
z /= nvertices[obj];

pivotpt[obj].x=x;
pivotpt[obj].y=y;
pivotpt[obj].z=z;

if (VERBOSE>=1)
	printf("pivot %s: %f %f %f\n",obj_name[obj],pivotpt[obj].x,pivotpt[obj].y,pivotpt[obj].z);

}





void calc_normal()
{
int cont,V1,V2,V3;
struct ponto u,v,prod;

for (i=0;i<nobj;i++)
	{
	if ( pivtag[i]==PIVOT )
		pivot(i);
	
	else
	if ( maptag[i]==MAP )
		{
		int boolean,obj;

		boolean=FALSE;
		for (j=0;j<nobj;j++)
			{
			if (maptag[j]==i)
				{
				boolean=TRUE;
				obj=j;
				}
			}
		if (boolean==FALSE)
			{
			printf("warning: Object %s is a map, but it's not assigned to any object\n",obj_name[i]);
			printf("please check if its name is correct\n");
			}
		else
			{
			printf("calculating mapping of object %s\n",obj_name[obj]);
			mapping(nvertices[i],ntriang[i],vertices[i],triang[i],texture[obj],nvertices[obj],vertices[obj]);
			}
		}
	else 
	   {
	   printf("Calculating normals of object %s\n",obj_name[i]);
	   for (j=1;j<=nvertices[i];j++)
		{
		if (VERBOSE>=1)                
			printf("normal do vertice %d\n",j);

		normals[i][j].x=0;
		normals[i][j].y=0;
		normals[i][j].z=0;
		
		for (cont=1;cont<=ntriang[i];cont++)
			{
			V1=triang[i][cont].x;
			V2=triang[i][cont].y;
			V3=triang[i][cont].z;

			if ( (V1==j) || (V2==j) || (V3==j) )
				{
				u.x=vertices[i][V2].x-vertices[i][V1].x;
				u.y=vertices[i][V2].y-vertices[i][V1].y;
				u.z=vertices[i][V2].z-vertices[i][V1].z;
				v.x=vertices[i][V3].x-vertices[i][V1].x;
				v.y=vertices[i][V3].y-vertices[i][V1].y;
				v.z=vertices[i][V3].z-vertices[i][V1].z;

				prod.x=u.y*v.z - u.z*v.y;
				prod.y=u.z*v.x - u.x*v.z;
				prod.z=v.y*u.x - u.y*v.x;

				normals[i][j].x += prod.x;
				normals[i][j].y += prod.y;
				normals[i][j].z += prod.z;
				}
			}
		normaliz(i,j);
		}
	   
	   }
	   
	}
}


void raw_file(void)
{
int V1,V2,V3;
float px, py, pz;

printf("Writing RAW file...\n");
fprintf(filep_raw,"%d\n",(nobj-npivots-nmaps));

for (i=0;i<nobj;i++)
   {
   if ( (pivtag[i]==PIVOT) || (maptag[i]==MAP) )
	{
	if (VERBOSE>=1)
		printf("skipped %s\n",obj_name[i]);
	}
	
   else
	{       
	fprintf(filep_raw,"object %s %d ",obj_name[i],ntriang[i]);

	if (pivtag[i]==NOPIV)
		{
		printf("Warning: does not exist a pivot for object %s\n",obj_name[i]);
		printf("Using 0 0 0 as pivot\n");
		fprintf(filep_raw,"0 0 0 ");
		px=0;
		py=0;
		pz=0;
		}
	
	else
		{
		px=pivotpt[pivtag[i]].x;
		py=pivotpt[pivtag[i]].y;
		pz=pivotpt[pivtag[i]].z;
		fprintf(filep_raw,"%f %f %f ", px,py,pz);
		}

	if (maptag[i]==NOMAP)
		fprintf(filep_raw,"0\n");
	else
		fprintf(filep_raw,"1\n");

	
	for (j=1;j<=nvertices[i];j++)
	    {
	    vertices[i][j].x -= px;
	    vertices[i][j].y -= py;
	    vertices[i][j].z -= pz;
	    }

	for (j=1;j<=ntriang[i];j++)
		{
		V1=triang[i][j].x;
		V2=triang[i][j].y;
		V3=triang[i][j].z;
				
		fprintf(filep_raw,"%f ",vertices[i][V1].x);
		fprintf(filep_raw,"%f ",vertices[i][V1].y);
		fprintf(filep_raw,"%f ",vertices[i][V1].z);
		
		fprintf(filep_raw,"%f ",normals[i][V1].x);
		fprintf(filep_raw,"%f ",normals[i][V1].y);
		fprintf(filep_raw,"%f ",normals[i][V1].z);
		
		if (maptag[i]!=NOMAP)
			fprintf(filep_raw,"%f %f\n",texture[i][V1].x,texture[i][V1].y);                        
		else
			fprintf(filep_raw,"\n");

		fprintf(filep_raw,"%f ",vertices[i][V2].x);
		fprintf(filep_raw,"%f ",vertices[i][V2].y);
		fprintf(filep_raw,"%f ",vertices[i][V2].z);
		
		fprintf(filep_raw,"%f ",normals[i][V2].x);
		fprintf(filep_raw,"%f ",normals[i][V2].y);
		fprintf(filep_raw,"%f ",normals[i][V2].z);
		
		if (maptag[i]!=NOMAP)
			fprintf(filep_raw,"%f %f\n",texture[i][V2].x,texture[i][V2].y);                        
		else
			fprintf(filep_raw,"\n");

		fprintf(filep_raw,"%f ",vertices[i][V3].x);
		fprintf(filep_raw,"%f ",vertices[i][V3].y);
		fprintf(filep_raw,"%f ",vertices[i][V3].z);
		
		fprintf(filep_raw,"%f ",normals[i][V3].x);
		fprintf(filep_raw,"%f ",normals[i][V3].y);
		fprintf(filep_raw,"%f ",normals[i][V3].z);

		if (maptag[i]!=NOMAP)
			fprintf(filep_raw,"%f %f\n",texture[i][V3].x,texture[i][V3].y);                        
		else
			fprintf(filep_raw,"\n");

		}       
	}
   }
}


/*
void main(int argc, char **argv)
{
if (!strcmp(argv[argc-1],"-v1"))
	VERBOSE=1;

if (!strcmp(argv[argc-1],"-v2"))
	VERBOSE=2;

dxf2raw(argv[1]);
}
*/
