#! /bin/sh
export DISPLAY=127.0.0.1:0.0
PATH=/usr/X11R6/bin:$PATH

# Cleanup from last run.
rm -rf /tmp/.X11-unix


# Notice that the window manager and the xterm will wait for
# the server to finish starting before trying to connect; the
# error "Cannot Open Display: 127.0.0.1:0.0" is not due to the
# clients attempting to connect before the server has started, rather
# that error is due to a bug in some versions of cygwin1.dll.  Upgrade
# to the latest cygwin1.dll if you get the "Cannot Open Display" error.
# See the Cygwin/XFree86 FAQ for more information:
# http://xfree86.cygwin.com/docs/faq/
#
# The error "Fatal server error: could not open default font 'fixed'" is
# caused by using a DOS mode mount for the mount that the Cygwin/XFree86
# fonts are accessed through.  See the Cygwin/XFree86 FAQ for more 
# information:
# http://xfree86.cygwin.com/docs/faq/cygwin-xfree-faq.html#q-error-font-eof


# Brief descriptions of XWin-specific options:
#
# -screen scr_num [width height]
#      Enable screen scr_num and optionally specify a width and
#      height for that screen.
#      Most importantly, any parameters specified before the first -screen
#      parameter apply to all screens.  Any options after the first -screen
#      parameter apply only to the screen that precedes the parameter.
#      Example:
#          XWin -fullscreen -screen 0 -screen 1 -depth 8 -screen 2
#      All screens will be fullscreen, but screen 2 will be depth 8, while
#      screens 0 and 1 will be the default depth (whatever depth Windows
#      is currently running at).
# -multiwindow
#      Start an integrated Windows-based window manager.  Not to be used
#      with -rootless nor -fullscreen.
# -rootless
#      Use a transparent root window with an external window manager
#      (such as twm).  Not to be used with -multiwindow nor
#      with -fullscreen.
# -fullscreen
#      Use a window as large as possible on the primary monitor.
# -multiplemonitors
#      Create a root window that covers all monitors on a
#      system with multiple monitors.
# -clipboard
#      Enable the integrated version of xwinclip.  Do not use in
#      conjunction with the xwinclip program.
# -depth bits_per_pixel
#      Specify the screen depth to run at (in bits per pixel) using a
#      DirectDraw-based engine in conjunction with the -fullscreen
#      option, ignored if the -fullscreen option is not specified.
#      By default, you will be using a DirectDraw based engine on any
#      system that supports it.
# -unixkill
#      Trap Ctrl+Alt+Backspace as a server shutdown key combination.
# -nounixkill
#      Disable Ctrl+Alt+Backspace as a server shutdown key combination (default).
#      Example:
#          XWin -unixkill -screen 0 -screen 1 -screen 2 -nounixkill
#      Screens 0 and 1 will allow Ctrl+Alt+Backspace, but screen 2 will not.
# -winkill
#      Trap Alt+F4 as a server shutdown key combination (default).
# -nowinkill
#      Disable Alt+F4 as a server shutdown key combination.
# -scrollbars
#      Enable resizing of the server display window.  Do not use in conjunction
#      with -multiwindow nor with -rootless.
# -nodecoration
#      Draw the server root window without a title bar or border.
#      Do not use with -mutliwindow nor with -rootless.
# -lesspointer
#      Hide the Windows mouse cursor anytime it is over any part of the
#      window, even if Cygwin/XFree86 is not the window with the focus.
# -refresh rate_in_Hz
#      Specify a refresh rate to use when used with the -fullscreen option.
# -trayicon
#      Enable the tray icon (default).
# -notrayicon
#      Disable the tray icon.
#      Example:
#          XWin -notrayicon -screen 0 -screen 1 -screen 2 -trayicon
#      Screens 0 and 1 will not have tray icons, but screen 2 will.
# -emulate3buttons [timeout]
#      Emulate 3 button mouse with an optional timeout in milliseconds.
# -xf86config
#      Specify an XF86Config-style configuration file.
# -keyboard
#      Specify a keyboard device from the configuration file.


#
# Startup the programs
# 


# Startup the X Server with the integrated Windows-based window manager.

XWin -rootless &


# Startup an xterm, using bash as the shell.

xterm -sl 1000 -sb -rightbar -ms red -fg yellow -bg black -geometry +500+500 -e /usr/bin/bash &


#
# Startup the twm window manager.
# WARNING: Do not use an external window manager in conjunction with
# the ``-multiwindow'' command-line parameter for XWin.  Doing so
# would start two window managers, which is never supposed to happen.
#

twm &  


# Set a background color.  Only needed when not using -multwindow for XWin.

xsetroot -solid aquamarine4


# Return from sh.
exit

