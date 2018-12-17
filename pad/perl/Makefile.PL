use ExtUtils::MakeMaker;
use Env (PADHOME);

chomp ($CPU = `uname -m | tr / -`);
chomp ($OS = `uname -s | tr / -`);

if ($OS eq "SunOS") {
    $OSRELEASE=`uname -r`;
    if ($OSRELEASE > 5.0) {
	$OS = "Solaris";
      }
  }
if ($OS eq "AIX") {
  $CPU = $HOSTTYPE;
}
if ($OS eq "IRIX") {
  $CPU = "MIPS";
}
if ($OS eq "IRIX64") {
  $CPU = "MIPS";
  $OS = "IRIX";
}
$ARCH = "bin-$CPU-$OS";

# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    'NAME'	=> 'Pad',
    'VERSION_FROM' => 'Pad.pm', # finds $VERSION
    'LIBS'	=> ['-L/usr/X11R6/lib -lm -lX11 -laudio -lXi -lXext -ljpeg'],   # e.g., '-lm' 
    'OBJECT'    => '$(O_FILES)',
    'MYEXTLIB'  => "$PADHOME/$ARCH/libpad.a $PADHOME/kpl/$ARCH/libkpl.a $PADHOME/$ARCH/libtk4.2.a $PADHOME/$ARCH/libtcl7.6.a",
    'DEFINE'	=> '',     # e.g., '-DHAVE_SOMETHING' 
    'INC'	=> '',     # e.g., '-I/usr/include/other' 
);
