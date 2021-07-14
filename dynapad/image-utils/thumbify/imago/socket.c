#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/wait.h>
#include <sys/param.h>
#include <netinet/in.h>
#include <netdb.h>

 /*
   as children die we should catch their returns or else we get
   zombies, A Bad Thing. fireman() catches falling children.
 */
void 
fireman () {
  /*
  union wait wstatus;

  while(wait3(&wstatus,WNOHANG,NULL) > 0);
  */
  int statptr;
  while(wait3(&statptr, WNOHANG, NULL) > 0);
}


int 
call_socket (hostname, portnum)
char *hostname;
{
  struct sockaddr_in sa;
  struct hostent *hp;
  int a, s;

  if((hp = gethostbyname(hostname)) == NULL){ /* do we know the host's address     fprintf(stderr, hstrerror(h_errno));
*/
    fprintf(stderr, "gethostbyname error\n");
    errno = ECONNREFUSED;
    fprintf(stderr, "call_socket: connection refused. addressing error?\n");
    return(-1);                                 /* no */
  }

  bzero(&sa,sizeof(sa));
  bcopy(hp->h_addr,(char *)&sa.sin_addr,hp->h_length); /* set address */
  sa.sin_family = hp->h_addrtype;
  sa.sin_port = htons((u_short)portnum);

  if((s = socket(hp->h_addrtype,SOCK_STREAM,0)) < 0){ /* get socket */
    fprintf(stderr, "call_socket: can't get socket.\n");
    return(-1);
  }
  if(connect(s, (struct sockaddr *)&sa, sizeof sa) < 0) { /* connect */
    close(s);
    fprintf(stderr, "call_socket: can't connect.\n");
    return(-1);
  }
  return(s);
}

#ifdef old
 * Socket address, internet style.
struct sockaddr_in {
  short	sin_family;
  u_short sin_port;
  struct in_addr sin_addr;
  char sin_zero[8];
};

 * Internet address (a structure for historical reasons)
struct in_addr {
  u_long s_addr;
};

struct hostent {
  char *h_name;  /* official name of host */
  char **h_aliases;   /* alias list */
  int  h_addrtype;    /* host address type */
  int  h_length; /* length of address */
  char **h_addr_list; /* list of addresses from name server */
};
#define  h_addr h_addr_list[0]   /* address, for backward compatibility  */
#endif

int 
call_socket2 (hp, portnum)
struct hostent *hp;
{
  struct sockaddr_in sa;
  int s, i;

  if(hp == NULL){
    fprintf(stderr, "call_socket2: null hostent/n");
    return -1;
  }
  fprintf(stderr, "call_socket2: official name: %s\n", hp->h_name);
  fprintf(stderr, "call_socket2: h_addrtype: %d\n", hp->h_addrtype);
  fprintf(stderr, "call_socket2: h_length: %d\n", hp->h_length);
  fprintf(stderr, "call_socket2: address: %ld\n", (u_long)(hp->h_addr_list[0]));

  bzero(&sa,sizeof(sa));
  bcopy(hp->h_addr,(char *)&sa.sin_addr,hp->h_length); /* set address */
  sa.sin_family = hp->h_addrtype;
  sa.sin_port = htons((u_short)portnum);

  if((s = socket(hp->h_addrtype, SOCK_STREAM, 0)) < 0){ /* get socket */
    fprintf(stderr, "call_socket2: can't get socket.\n");
    perror("socket");
    return(-1);
  }
  if(connect(s, (struct sockaddr *)&sa, sizeof sa) < 0) { /* connect */
    close(s);
    fprintf(stderr, "call_socket2: can't connect.\n");
    return(-1);
  }
  return(s);
}

int 
call_socket3 (address, portnum)
u_long address;
int portnum;
{
  struct sockaddr_in sa;
  int s;

  if((s = socket(AF_INET, SOCK_STREAM, 0)) < 0){ /* get socket */
    fprintf(stderr, "call_socket3: can't get socket.\n");
    perror("socket");
    return(-1);
  }

  bzero(&sa, sizeof(struct sockaddr_in));
  sa.sin_family = AF_INET;
  sa.sin_port = htons((u_short)portnum);
  sa.sin_addr.s_addr = address;

  if(connect(s, (struct sockaddr *)&sa, sizeof sa) < 0) { /* connect */
    close(s);
    fprintf(stderr, "call_socket3: can't connect.\n");
    perror("connect");
    return(-1);
  }
  return(s);
}

/*
  establish a socket; originally from bzs@bu-cs.bu.edu
*/
int
establish (portnum)
u_short portnum;
{
  char myname[MAXHOSTNAMELEN+1];
  int s;
  struct sockaddr_in sa;
  struct hostent *hp;

  bzero(&sa,sizeof(struct sockaddr_in));       /* clear our address */
  gethostname(myname,MAXHOSTNAMELEN);          /* who are we? */
  hp = gethostbyname(myname);                  /* get our address info */
  if (hp == NULL)                              /* we don't exist !? */
    return(-1);
  sa.sin_family = hp->h_addrtype;              /* this is our host address */
  sa.sin_port = htons(portnum);                /* this is our port number */
  if((s = socket(AF_INET,SOCK_STREAM,0)) < 0)  /* create socket */
    return(-1);
  if(bind(s, (struct sockaddr *)&sa, sizeof sa) < 0) {            /* bind address to socket */
    close(s);
    return(-1);
  }
  listen(s, 3);                           /* max # of queued connects */
  return(s);
}

int 
get_connection (s)
int s;                                    /* socket created with establish() */
{
  struct sockaddr_in isa;                 /* address of socket */
  int i;                                  /* size of address */
  int t;                                  /* socket of connection */

  i = sizeof(isa);                        /* find socket's address */
  getsockname(s,(struct sockaddr *)&isa,&i);                 /* for accept() */

  if ((t = accept(s,(struct sockaddr *)&isa,&i)) < 0)     /* accept connection if there is one */
    return(-1);
  return(t);
}

