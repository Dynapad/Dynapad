######################################################################
#
# Pad.pm
#    The Perl OO interface to Pad++
#
######################################################################

######################################################################
#
# Class Pad
#    The global class
#
######################################################################
package Pad;
use Carp;

use strict;
use vars qw($VERSION $DEBUG @ISA @EXPORT @EXPORT_OK);

require Exporter;
require DynaLoader;

@ISA = qw(Exporter DynaLoader);
# Items to export into callers namespace by default. Note: do not export
# names by default without a very good reason. Use EXPORT_OK instead.
# Do not simply export all your public functions/methods/constants.
@EXPORT = qw(
	
);
@EXPORT_OK = qw(DEBUG);
$VERSION = '0.01';
$DEBUG = 1;

BEGIN {
  require Pad::Colors;

  tie (%Pad::COLORS, "Pad::Colors", "/usr/X11R6/lib/X11/rgb.txt");
  %Pad::ANCHORS = ('center' => $Pad::ANCHORCENTER, 
		   'n'      => $Pad::ANCHORN, 
		   'ne'     => $Pad::ANCHORNE, 
		   'nw'     => $Pad::ANCHORNW,
		   'e'      => $Pad::ANCHORE,
		   'w'      => $Pad::ANCHORW,
		   's'      => $Pad::ANCHORS,
		   'se'     => $Pad::ANCHORSE,
		   'sw'     => $Pad::ANCHORSW
		  );
}

require Pad::Surface;
require Pad::Object;

bootstrap Pad $VERSION;

# Preloaded methods go here.

# Autoload methods go after =cut, and are processed by the autosplit program.

1;
__END__

=head1 NAME

Pad - Perl extension for the Pad++ zooming graphics library

=head1 SYNOPSIS

  use Pad;
  blah blah blah

=head1 DESCRIPTION

  This module provides an OO perl interface to Pad++.

=head1 AUTHOR

Jason E. Stewart, jasons@cs.unm.edu

=head1 SEE ALSO

perl(1).

=cut
