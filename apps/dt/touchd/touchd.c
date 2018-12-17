/* $Id: touchd.c,v 1.1.1.1 2005/10/09 04:28:49 stanonik Exp $ */
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/un.h>
#include "libdt.h"
#include "dtucsd.h"

#define RBUFLEN (1024)
#define SBUFLEN (64 * sizeof(dt_2dtouch))  /* frighteningly arbitrary */
#define ACK "OK\n"
#define SUN_DIR "/tmp/dt_sockets"
#define SUN_PATH "/tmp/dt_sockets/touchd_socket"
#define SELECT_TIMEOUT_MS (30)

typedef struct {
  unsigned int frame_number;
  unsigned char  user[4][224];
} dt_raw_frame;

static int sock;

static dt_device device;
static dt_table* table;
static dt_frame* frame[DT_MAX_USERS];
static dt_event* event;
static dt_2dtouch* touches[DT_MAX_USERS];

int make_response(char* buf, dt_2dtouch* touches[]);
static void dt_init(void);
static void dt_uninit(void);
static void create_socket(void);
static void delete_socket(void);
static void set_signal_handlers(void);
static void handler(int signum);
static void exit_gracefully(int status);

int
main(int argc, char** argv)
{
  char rbuf[RBUFLEN], sbuf[SBUFLEN];
  int rlen, slen;
  struct sockaddr_un from;
  socklen_t fromlen;
  fd_set readfds, none;
  struct timeval timeout;
  int select_rv;

  dt_init();
  create_socket();
  set_signal_handlers();

  while (1)
  {
    static dt_io_error ioerr;

    /*
    static dt_raw_frame raw_frame;

    if (read(device, &raw_frame, sizeof(raw_frame)) == sizeof(raw_frame))
    {
      printf("%u\n", raw_frame.frame_number);
    }
    else
    {
      perror("read");
      delete_socket();
      exit_gracefully(EXIT_FAILURE);
    }
    */

    if (dt_device_read(device, table, frame, &ioerr))
    {
      dt_user_id user_id;
      for (user_id = 0; user_id < DT_MAX_USERS; ++user_id)
        touches[user_id] = d4_get_2dtouches(user_id, frame[user_id]);
    }
    else
    {
      fprintf(stderr, "dt_device_read failed\n");
      delete_socket();
      exit_gracefully(EXIT_FAILURE);
    }

    FD_ZERO(&none); FD_ZERO(&readfds); FD_SET(sock, &readfds);
    timeout.tv_sec = 0; timeout.tv_usec = SELECT_TIMEOUT_MS * 1000;
    select_rv = select(sock+1, &readfds, &none, &none, &timeout);
    while (select_rv > 0)
    {
      if (FD_ISSET(sock, &readfds))  /* this had better be true */
      {
        fromlen = sizeof(from);
        rlen = recvfrom(sock, rbuf, RBUFLEN, 0,
            (struct sockaddr*)&from, &fromlen);
        if (rlen < 0)
          perror("recvfrom");

        slen = make_response((char*)&sbuf, touches);
        if (sendto(sock, sbuf, slen, 0, (struct sockaddr*)&from, fromlen) < 0)
          perror("sendto");
      }
      else
        fprintf(stderr, "select return value > 0 but socket unreadable!?\n");

      FD_ZERO(&none); FD_ZERO(&readfds); FD_SET(sock, &readfds);
      timeout.tv_sec = timeout.tv_usec = 0;
      select_rv = select(sock+1, &readfds, &none, &none, &timeout);
    }
    if (select_rv < 0)
      perror("select");
  }

  delete_socket();
  dt_uninit();
  exit_gracefully(EXIT_SUCCESS);
}

  int
make_response(char* buf, dt_2dtouch* touches[])
{
  dt_user_id user_id;
  int i;
  int nbytes = 0;

  for (user_id = 0; user_id < DT_MAX_USERS; ++user_id)
    for (i = 0; touches[user_id][i].status != EOL; ++i)
      if (touches[user_id][i].status == ALIVE)
      {
        memcpy(buf+nbytes, &(touches[user_id][i]), sizeof(**touches));
        nbytes += sizeof(**touches);
      }

  return nbytes;
}

  void
dt_init(void)
{
  dt_user_id u;

  table = dt_get_table((dt_user_id)0);

  if (table->status != DT_TABLE_STATUS_OK)
  {
    fprintf(stderr, "can't find DiamondTouch device\n");
    exit(EXIT_FAILURE);
  }

  device = dt_device_open(table, &(table->devname[0][0]));
  if (device == DT_BAD_DEVICE)
  {
    fprintf(stderr, "can't open device \"%s\"\n",
        &(table->devname[0][0]));
    exit(EXIT_FAILURE);
  }

  for (u = 0; u < DT_MAX_USERS; ++u)
    if (!(frame[u] = dt_frame_new(table)))
    {
      fprintf(stderr, "can't make dt_frame\n");
      exit(EXIT_FAILURE);
    }

  d4_init();
}

  void
dt_uninit(void)
{
  dt_user_id u;

  free(event);
  for (u = 0; u < DT_MAX_USERS; ++u)
    dt_frame_free(frame[u]);

  if (dt_device_close(table, device) < 0)
  {
    fprintf(stderr, "dt_device_close failed\n");
    exit(EXIT_FAILURE);
  }

  d4_uninit();
}

  void
create_socket(void)
{
  struct sockaddr_un name;

  sock = socket(AF_UNIX, SOCK_DGRAM, 0);
  if (sock < 0)
  {
    perror("socket");
    exit_gracefully(EXIT_FAILURE);
  }

  /* Create name. */
  name.sun_family = AF_UNIX;
  strcpy(name.sun_path, SUN_PATH);

  unlink(SUN_PATH);  /* hazard with multiple simultaneous invocations? */

  /* age-old trade-off: convenience vs. security */
  umask((mode_t)0000);
  mkdir(SUN_DIR, (mode_t)0777);

  /* Bind the UNIX domain address to the created socket */
  if (bind(sock, (struct sockaddr*)&name, sizeof(struct sockaddr_un)))
  {
    perror("bind");
    exit_gracefully(EXIT_FAILURE);
  }
}

  void
delete_socket(void)
{
  close(sock);
  unlink(SUN_PATH);
}

  void
set_signal_handlers(void)
{
  signal(SIGHUP, (sig_t)&handler);
  signal(SIGINT, (sig_t)&handler);
  signal(SIGPIPE, (sig_t)&handler);
  signal(SIGALRM, (sig_t)&handler);
  signal(SIGTERM, (sig_t)&handler);
  signal(SIGUSR1, (sig_t)&handler);
  signal(SIGUSR2, (sig_t)&handler);
}

  void
handler(int signum)
{
  exit_gracefully(EXIT_FAILURE);
}

  void
exit_gracefully(int status)
{
  delete_socket();
  dt_uninit();
  exit(status);
}
