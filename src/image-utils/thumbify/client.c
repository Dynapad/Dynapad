#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <sys/wait.h>
#include <sys/param.h>
#include <netinet/in.h>

#include <netdb.h>

#define BUFSIZE 1024 * 4
#define IMAGOPORTNUM 6666
#define IMAGOHOST "hci.ucsd.edu"
#define OUTPUTFILE "thumb.jpg"
#define INPUTFILE "http://artcrimes.gatech.edu/nyc/q3a.jpg"
#define INPUTFILE2 "http://portal.research.bell-labs.com/orgs/ssr/people/jon/dotplot/browser.half.gif"
#define INPUTFILE3 "http://www.cs.unm.edu/~jon/art/batllo1.jpg"
#define INPUTFILE4 "http://www-swiss.ai.mit.edu/photo/pcd1647/gorilla-head-and-shoulders-51.1.jpg"
#define INPUTFILE5 "http://bang.lanl.gov/solarsys/raw/ast/gaspra4.jpg"
#define INPUTFILE6 "http://bang.lanl.gov/solarsys/thumb/ast/gaspratc.gif"

int 
call_socket (hostname, portnum)
char *hostname;
{
  struct sockaddr_in sa;
  struct hostent *hp;
  int a, s;

  if ((hp = gethostbyname(hostname)) == NULL) { /* do we know the host's */
    errno = ECONNREFUSED;                       /* address? */
    return(-1);                                 /* no */
  }

  bzero(&sa,sizeof(sa));
  bcopy(hp->h_addr,(char *)&sa.sin_addr,hp->h_length); /* set address */
  sa.sin_family= hp->h_addrtype;
  sa.sin_port= htons((u_short)portnum);

  if ((s= socket(hp->h_addrtype,SOCK_STREAM,0)) < 0) /* get socket */
    return(-1);
  if (connect(s,&sa,sizeof sa) < 0) { /* connect */
    close(s);
    return(-1);
  }
  return(s);
}

int 
read_data (s,buf,n)
int s;                             /* connected socket */
char *buf;                         /* pointer to the buffer */
int n;                             /* number of characters (bytes) we want */
  { 
  int bcount,                      /* counts bytes read */
  br;                              /* bytes read this pass */

  bcount= 0;
  br= 0;
  while (bcount < n) {             /* loop until full buffer */
    if((br= read(s,buf,n-bcount)) > 0) {
      bcount += br;                /* increment byte counter */
      buf += br;                   /* move buffer ptr for next read */
    }
   if(br < 0)                      /* signal an error to the caller */
     return(-1);
  }
  return(bcount);
}

int
trim_tail (s)
char *s;
{
  char lastc = s[strlen(s) - 1];
  switch (lastc){
  case 10:
  case 12:
  case 13:
    s[strlen(s) - 1] = '\0';
    return 1;
  default:
    return 0;
  }
}

main (argc, argv)
int argc;
char **argv;
{
  int s, rc, count = 0;
  char buf[BUFSIZE], *input = INPUTFILE3;
  FILE *fp;
  int out;

  if(argc == 2){
    input = argv[1];
  }

  out = open(OUTPUTFILE, O_CREAT | O_WRONLY | O_TRUNC);
  if(out == -1){
    fprintf(stderr, "can't open output file\n");
    perror("open");
    exit(1);
  }
  s = call_socket(IMAGOHOST, IMAGOPORTNUM);
  if(s < 0){
    fprintf(stderr, "can't open socket to image server\n");
    exit(1);
  }
  if((fp = fdopen(s, "r")) == NULL){
    fprintf(stderr, "can't fdopen connection to image server\n");
    close(s);
    exit(1);
  }
  sprintf(buf, "GET_THUMB %s\nreturn: bits\n\n", input);
  if(write(s, buf, strlen(buf)) == -1){
    fprintf(stderr, "can't write to image server\n");
    close(s);
    exit(1);
  }

  for(;;){                        /* read off header */
    if(fgets(buf, BUFSIZE, fp) == NULL){
      fprintf(stderr, "closing connection\n");
      close(s);
      exit(1);
    }
    if(buf[0] == 13 || buf[0] == 12 || buf[0] == 10){  /* end of header */
      fprintf(stderr, "\n");
      break;
    }
    while(trim_tail(buf));
    fprintf(stderr, "%s\n", buf);
  }

  for(;;){  
    rc = fread(buf, sizeof(char), BUFSIZE, fp);
    if(rc <= 0){
      fprintf(stderr, "fread returned %d\n", rc);
      break;
    }
    count += rc;
    if(write(out, buf, rc) == -1){
      perror("write");
      break;
    }
    fprintf(stderr, "%d bytes copied\n", rc);
  }
  fprintf(stderr, "total of %d bytes copied\n", count);
  close(s);
  close(out);
}

