#include "server.h"
#include <errno.h>
#include <signal.h>

/*
  global command-line options
*/
int imago_port = DEFAULT_IMAGO_PORT;                   /* -i */
int proxy_port = DEFAULT_PROXY_PORT;                   /* -p */
char *proxy_host = DEFAULT_PROXY_HOST;                 /* -P */
char *server_url_dir = DEFAULT_SERVER_URL_DIR;         /* -u */
char *server_disk_dir = DEFAULT_SERVER_DISK_DIR;       /* -d */

/*
  these are global so signal can call cleanup()
*/

int s, t;

void
cleanup ()
{
  close(s);
  close(t);
  exit(0);
}

void
broken_pipe () {
  fprintf(stderr, "broken pipe\n");
  close(s);
  s = -1;
}

parse_options (argc, argv)
int argc;
char **argv;
{
  int c;
  extern char *optarg;
  extern int optind;
 
  while((c = getopt(argc, argv, "i:p:P:u:d:")) != -1){
    switch(c) {
    case 'i':
      imago_port = atoi(optarg);
      break;
    case 'p':
      proxy_port = atoi(optarg);
      break;
    case 'P':
      proxy_host = strdup(optarg);
      break;
    case 'u':
      server_url_dir = strdup(optarg);
      break;
    case 'd':
      server_disk_dir = strdup(optarg);
      break;
    case '?':
      usage();
      break;
    }
  }
  fprintf(stderr, "Imago listening on port %d\n", imago_port);
  fprintf(stderr, "  using proxy on host %s\n", proxy_host);
  fprintf(stderr, "  using proxy on port %d\n", proxy_port);
  fprintf(stderr, "  using server_url_dir %s\n", server_url_dir);
  fprintf(stderr, "  using server_disk_dir %s\n", server_disk_dir);
}

usage () {
  fprintf(stderr, "USAGE: server [-i imago_port][-p proxy_port][-P proxy_host][-u server_url_dir][-d server_disk_dir]\n");
  cleanup();
}

main (argc, argv)
int argc;
char **argv;
{
  signal(SIGINT, cleanup);
  signal(SIGPIPE, broken_pipe);
  parse_options(argc, argv);
  if((t = establish(imago_port)) < 0) { 
    perror("establish");
    exit(1);
  }

  for (;;) {                            
    if ((s = get_connection(t)) < 0) {  
      if (errno == EINTR)                
        continue;                       
      perror("accept");              
      exit(1);
    }
    parse_request(s);
    close(s);                 
  }
}

main0 (argc, argv)
int argc;
char **argv;
{
  signal(SIGINT, cleanup);                /* free socket on interrupt */
  signal(SIGCHLD, fireman);               /* this eliminates zombies */

  parse_options(argc, argv);
  if((s = establish(imago_port)) < 0) {      /* plug in the phone */
    perror("establish");
    exit(1);
  }


  for(;;) {                               /* loop for phone calls */
    if((t = get_connection(s)) < 0) {     /* get a connection */
      if(errno == EINTR)                  /* EINTR might happen on accept(), */
        continue;                         /* try again */
      perror("accept");                   /* bad */
      exit(1);
    }
    switch(fork()) {                      /* try to handle connection */
    case -1 :                             /* bad news. scream and die */
      perror("fork");
      close(s);
      close(t);
      exit(1);
    case 0 :                              /* we're the child, parse request */
      parse_request(t);
      exit(0);
    default :                             /* we're the parent so look for */
      close(t);                           /* another connection */
      continue;
    }
  }
}

