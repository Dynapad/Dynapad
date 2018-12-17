#!/usr/bin/perl

use Net::HTTP;
use strict;
use URI;

my $args = join(' ', @ARGV);
my $uri = new URI;
my $body;
my $src_root = "http://images.google.com/images?";
my $img_src;
my $file_root = "/tmp/googlerip/googlepic";
my $count = 1;
my $img_dest;

$uri->path("/images");
$uri->query_form("q" => $args);

my $s = Net::HTTP->new(Host => "images.google.com") || die $@;
$s->write_request(GET => $uri->as_string);
#my($code, $mess, %h) = $s->read_response_headers;

while (1) {
   my $buf;
   my $n = $s->read_entity_body($buf, 1024);
   last unless $n;
   $body .= $buf;
}

#print $body, "\n";

while($body =~ /src=\/images\?([^\s]*)(.*)/) {
    # get the img src
    $img_src = "$src_root"."$1";

    # set the img dest on file system
    # assumes file type is jpg <-- Bad hack!
    $img_dest = "$file_root"."$count".".jpg";

    ##############################
    # Retrieve the image...
    # eventually rewrite this in perl
    #

    # use wget to retrieve img from Google's cache
    # change the agent-string to Mozilla/5.0
    system("wget -U Mozilla/5.0 -O $img_dest $img_src");

    # use curl to retrive img from Google's cache
#    system("curl $img_src > $img_dest");    
    #
    ########################################

    $count++;

#    print $1, "\n";
    $body = $2;
}
