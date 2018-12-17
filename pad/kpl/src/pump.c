
/*----- pump.c ----------------------------*/

#ifdef PAD_WIN
#  include <tcl.h>
#  include <tclWinPort.h>
#  include "../../win/windefs.h"
#endif

#include <stdio.h>
#include <assert.h>

#ifndef PAD_WIN
#  include <unistd.h>
#  include <sys/time.h>
#endif

#include <sys/types.h>
#ifdef SGI
#include <bstring.h>
#endif

#ifdef _AIX
#include <sys/select.h>
#endif

extern int read_any(int fd, char *buf, int len);
typedef struct {
  int fd_in;
  int pid;
  FILE *fp_in;
  FILE *fp_out;
  char *buf;
} pump_handle;

pump_handle *pump_open (const char *cmd)
{
#ifdef PAD_UNIX
  char *calloc();
  int rc, fd;
  int pipe_into[2];
  int pipe_from[2];
  pump_handle *p = (pump_handle *)calloc(1, sizeof(pump_handle));
  rc = pipe(pipe_into);  assert(rc==0);
  rc = pipe(pipe_from);  assert(rc==0);
  if ( (p->pid = fork()) == 0) {
    /* Child process */
    rc = close(0);             assert(rc==0);
    fd = dup(pipe_into[0]);    assert(fd==0);
    rc = close(pipe_into[0]);  assert(rc==0);
    rc = close(pipe_into[1]);  assert(rc==0);
    rc = close(1);             assert(rc==0);
    fd = dup(pipe_from[1]);    assert(fd==1);
    rc = close(pipe_from[0]);  assert(rc==0);
    rc = close(pipe_from[1]);  assert(rc==0);
    execl("/bin/sh", "sh", "-c", cmd, 0);
    exit(127);
  }
  /* Parent */
  rc = close(pipe_into[0]);  assert(rc==0);
  rc = close(pipe_from[1]);  assert(rc==0);
  p->fp_in  = fdopen(pipe_from[0], "r");  assert(p->fp_in);
  p->fp_out = fdopen(pipe_into[1], "w");  assert(p->fp_out);
  p->fd_in = pipe_from[0];
  p->buf = calloc(10000, sizeof(char));
  return p;
#else
  return NULL;
#endif
}

char *pump_buf(pump_handle *p)
{
  return p->buf;
}

void pump_close (pump_handle *p)
{
#ifdef PAD_UNIX
  fclose(p->fp_in);
  fclose(p->fp_out);
  free(p->buf);
  kill(p->pid, 9);
  wait(0);
#endif
}

int pump_read_any (pump_handle *p, char *str, int len)
{
  return read_any(p->fd_in, str, len);
}

int pump_getc (pump_handle *p)
{
  return getc(p->fp_in);
}

void pump_putc (int c, pump_handle *p)
{
  putc(c,p->fp_out);
}

void pump_flush (pump_handle *p)
{
  fflush(p->fp_out);
}

int read_any(int fd, char *buf, int len)
{
	fd_set fdset;
	struct timeval timeout;

	FD_ZERO(&fdset);
	FD_SET(fd, &fdset);
	timeout.tv_sec = 0;
	timeout.tv_usec = 0;
	select(FD_SETSIZE-1, &fdset, NULL, NULL, &timeout);
	if (FD_ISSET(fd, &fdset))
		return read(fd, buf, len);
	else
		return 0;
}


