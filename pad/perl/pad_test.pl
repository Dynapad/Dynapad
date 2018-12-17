# This is an example piece of code for using the perl interface to Pad++ 
# Currently the commands must be typed in by hand into the interpreter.
# i.e. do a .pad settoplevel perl, and then cut and paste in the entire file 

# import the environment variable PADHOME into our namespace
use Env (PADHOME);

# tell perl where to look for Pad.pm
use lib "$PADHOME/perl";

# autoload the Pad++ library
use Pad;
use Devel::Peek;
# use strict;
use diagnostics;

# create a new pad widget
$pad = Pad::Surface->new(".pad");

# pack the new pad
$pad->Pack();

# create, configure, and slide a rectangle on the pad
$rect = $pad->Create_object (type => "rectangle", coords => [0, 0, 50, 50]);
$rect->Set_options(-pen => "green", -penwidth => 5);
$rect->Slide (20, 20);

# create a line
$line = $pad->Create_object (type => "line", coords => [0, 0, 50, 50, 50, 100]);
$line->Set_options(-pen => "red", -penwidth => 10);

# The next section shows how to make callbacks. There are 3 forms 
#   - subroutine call
#   - anonymous subroutine
#   - class method
# each form has a with and without argument version. 
# ... => \&subname 
# ... => [\&subname?,args...?] 
# ... => sub { ... } 
# ... => [sub { ... }?,args...?] 
# ... => 'methodname' 
# ... => ['methodname'?,args...?] 

# set some bindings on $rect
sub ::hello {print "Hello World\n";};
$rect->Bind ('<ButtonPress-1>' => \&::hello);
$rect->Bind ('<Enter>'         => ['Set_options', -pen => "red"]);
$rect->Bind ('<Leave>'         => ['Set_options', -pen => "blue"]);

# set some bindings on $line
sub ::PrintIt { print "$_[1]\n";};
$line->Bind ('<ButtonPress-1>' => sub {print "Goodbye Cruel World\n";});
$line->Bind ('<Enter>' => [sub {print "$_[1]\n";}, "You say hello"]);
$line->Bind ('<Leave>' => [\&::PrintIt, "I say goodbye"]);

#create another line
$line2 =  $pad->Create_object (type => "line", coords => [0, 0, -50, 50, 50, -100], options => {-pen => "blue", -penwidth => 25});
$line2->Bind ('<ButtonPress-1>' => sub {print "Goodbye Cruel World\n";});

# create and configure a text object on the pad
$text = $pad->Create_object (type => "text");
$text->Set_options(-text => "Hello", -angle => 45);

# use the generic call mechanism for unimplemented methods on Class Pad
$pad->Call ("printtree");

# use the generic call mechanism for unimplemented methods on 
# Class Pad::Object
$text->Call("rotate 20");
