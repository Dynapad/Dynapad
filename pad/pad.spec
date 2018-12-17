Summary: pad++ zooming interface widget for Tcl/Tk
Name: pad
Version: 0.9p5
Release: 2
Copyright: Free Access Software, Pad++ Consortium
Group: Development/Languages
Source: http://hci.ucsd.edu/pad++/download/pad-%{version}.tar.gz
Requires: libjpeg >= 6b-9
Requires: tcl >= 8.0.4
Requires: tk >= 8.0.4

%description
Pad++ provides padwish, a zooming interface widget for Tcl/Tk,
and paddraw, a zoomable drawing application built using padwish.

Pad++ is Free Access Software.  It is not public domain, but may be
licensed for free for education, research and internal use.  See the
files "License", "LicenseTerms", and "Register" in /usr/local/lib/pad
for more information.

%prep

%build

%install

%files
/usr/local/bin/paddraw
/usr/local/bin/padwish
/usr/local/man/man1/pad.1
/usr/local/lib/pad/*
