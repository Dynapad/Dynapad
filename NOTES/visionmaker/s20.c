#include <termio.h>
#include <fcntl.h>
#include <errno.h>
#include <stdio.h>

main()
{
	int fd;
	struct termios t;
	int n;
	char buf[2510], obuf[515];
	int pkt = 0;
	int i, j;
	int obuflen = 0;
	int x, y, button, prox;
	char pressure;
	int z;
	char tiltx, tilty;

	/*if ((fd = open("/dev/ttyS0", O_RDWR|O_NDELAY, 0)) < 0) {*/
	if ((fd = open("/dev/ttyS0", O_RDWR, 0)) < 0) {
	    perror("open(/dev/ttyS0)");
	    exit(-1);
	}
	/*
	if (tcgetattr(fd, &t) < 0) {
	    fprintf(stderr, "tcgetattr failed\n");
	    exit(-1);
	}
	cfsetispeed(&t, B91000);
	cfsetospeed(&t, B91000);
	t.c_cflag &= ~(CSTOPB);
	t.c_cflag &= ~(CSIZE);
	t.c_cflag |= CS8;
	t.c_cflag &= ~(PARENB);
	t.c_cc[VMIN] = 1;
	t.c_cc[VTIME] = 10;
	t.c_iflag |= IXOFF;
	if (tcsetattr(fd, TCSANOW, &t) < 0) {
	    fprintf(stderr, "tcsetattr failed\n");
	    exit(-1);
	}
	*/
	
	/*
	 * %Z3\r is supposed to send proximity, but doesn't
	 * %VV2\r is supposed to send tilt and does, but also sends height
	 * %VV4\r is supposed to send height and does, but also sends tilt
	 */
	/*strcpy(buf, "\033%Z3\r%VV4\r%^20\r");*/
	/*strcpy(buf, "\033%VV4\r%^20\r");*/
	strcpy(buf, "\033%VV2\r%^20\r");
	if (write(fd, buf, strlen(buf)) < 0) {
	    perror("write(fd)");
	    exit(1);
	}
	while((n = read(fd, buf, 2510)) > 0) {
	    memcpy(&obuf[obuflen], buf, n);
	    obuflen += n;
	    for (i = 0; i < obuflen/10; i++) {
	        for (j = 0; j < 10; j++) {
	    	    switch (j) {
		        case 0: /*byte 1*/
			    if (!obuf[10*i]&0x80) {
			        fprintf(stderr, "out of sync byte 1\n");
			    }
			    button = (obuf[10*i]&0x7c)>>2;
			    x = (obuf[10*i]&0x03)<<14;
		            break;
		        case 1: /*byte 2*/
			    if (obuf[1 + 10*i]&0x80) {
			        fprintf(stderr, "out of sync byte 2\n");
			    }
			    x += (obuf[1 + 10*i]&0x7f)<<7;
			    break;
		        case 2: /*byte 3*/
			    if (obuf[2 + 10*i]&0x80) {
			        fprintf(stderr, "out of sync byte 3\n");
			    }
			    x += (obuf[2 + 10*i]&0x7f)<<0;
			    break;
		        case 3: /*byte 4*/
			    if (obuf[3 + 10*i]&0x80) {
			        fprintf(stderr, "out of sync byte 4\n");
			    }
			    prox = (obuf[3 + 10*i]&0x20)>>5;
			    x += (obuf[3 + 10*i]&0x18)<<16;
			    y = (obuf[3 + 10*i]&0x07)<<14;
			    break;
		        case 4: /*byte 5*/
			    if (obuf[4 + 10*i]&0x80) {
			        fprintf(stderr, "out of sync byte 5\n");
			    }
			    y += (obuf[4 + 10*i]&0x7f)<<7;
			    break;
		        case 5: /*byte 6*/
			    if (obuf[5 + 10*i]&0x80) {
			        fprintf(stderr, "out of sync byte 6\n");
			    }
			    y += (obuf[5 + 10*i]&0x7f)<<0;
			    break;
		        case 6: /*byte 7*/
			    if (obuf[6 + 10*i]&0x80) {
			        fprintf(stderr, "out of sync byte 7\n");
			    }
			    tiltx = (obuf[6 + 10*i]&0x7f)<<0;
			    if (tiltx & 0x40)
			        tiltx |= 0x80;
			    break;
		        case 7: /*byte 8*/
			    if (obuf[7 + 10*i]&0x80) {
			        fprintf(stderr, "out of sync byte 8\n");
			    }
			    tilty = (obuf[7 + 10*i]&0x7f)<<0;
			    if (tilty & 0x40)
			        tilty |= 0x80;
			    break;
		        case 8: /*byte 9*/
			    if (obuf[8 + 10*i]&0x80) {
			        fprintf(stderr, "out of sync byte 9\n");
			    }
			    pressure = (obuf[8 + 10*i]&0x7f)<<0;
			    break;
		        case 9: /*byte 10*/
			    if (obuf[9 + 10*i]&0x80) {
			        fprintf(stderr, "out of sync byte 10\n");
			    }
			    z = (obuf[9 + 10*i]&0x7f)<<0;
			    break;
		    }
		}
		fprintf(stderr, "read = %04d %x %d %d %d %d %d %d %d\n",
		    pkt, button, x, y, z, pressure, tiltx, tilty, prox);
	    }
	    memcpy(obuf, &obuf[10*(obuflen/10)], obuflen%10);
	    obuflen = obuflen%10;
	    pkt++;
	}
	fprintf(stderr, "exiting: n = %d\n", n);
	close(fd);
}
