/*
 * Copyright 1996 by Frederic Lepied, France. <Frederic.Lepied@sugix.frmug.org>
 *                                                                            
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is  hereby granted without fee, provided that
 * the  above copyright   notice appear  in   all  copies and  that both  that
 * copyright  notice   and   this  permission   notice  appear  in  supporting
 * documentation, and that   the  name of  Frederic   Lepied not  be  used  in
 * advertising or publicity pertaining to distribution of the software without
 * specific,  written      prior  permission.     Frederic  Lepied   makes  no
 * representations about the suitability of this software for any purpose.  It
 * is provided "as is" without express or implied warranty.                   
 *                                                                            
 * FREDERIC  LEPIED DISCLAIMS ALL   WARRANTIES WITH REGARD  TO  THIS SOFTWARE,
 * INCLUDING ALL IMPLIED   WARRANTIES OF MERCHANTABILITY  AND   FITNESS, IN NO
 * EVENT  SHALL FREDERIC  LEPIED BE   LIABLE   FOR ANY  SPECIAL, INDIRECT   OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
 * DATA  OR PROFITS, WHETHER  IN  AN ACTION OF  CONTRACT,  NEGLIGENCE OR OTHER
 * TORTIOUS  ACTION, ARISING    OUT OF OR   IN  CONNECTION  WITH THE USE    OR
 * PERFORMANCE OF THIS SOFTWARE.
 *
 */

#include "xinput.h"

static const char rcs_id[] = "$Id: setptr.c,v 1.1.1.1 2005/10/09 04:28:50 stanonik Exp $";

int
set_pointer(Display	*display,
	    int		argc,
	    char	*argv[],
	    char	*name,
	    char	*desc)
{
    XDeviceInfo		*info;
    XDevice		*device;
    int			xaxis = 0;
    int			yaxis = 1;
    
    if ((argc != 1) && (argc != 3)) {
	fprintf(stderr, "usage: xinput %s %s\n", name, desc);
	return EXIT_FAILURE;
    }
    
    if (argc == 3) {
	xaxis = atoi(argv[1]);
	yaxis = atoi(argv[2]);
    }
    
    info = find_device_info(display, argv[0], True);
    
    if (!info) {
	fprintf(stderr, "unable to find device %s\n", argv[0]);
	return EXIT_FAILURE;
    }    
	    
    device = XOpenDevice(display, info->id);

    if (device) {
	XChangePointerDevice(display, device, xaxis, yaxis);

	return EXIT_SUCCESS;
    } else {
	fprintf(stderr, "Unable to open device\n");
	return EXIT_FAILURE;
    }
}

/* end of setptr.c
 */
