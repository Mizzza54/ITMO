#!/usr/bin/perl
use strict;
use warnings FATAL => 'all';
use List::MoreUtils qw(uniq);

my @words = qw();

while (my $line = <>) {
    # ^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\?([^#]*))?(#(.*))? - from RFC 3896
    $_ = $line;
    # /<\s*?.*?\s*?a.*?\s*?href\s*?=\s*?(([^:\/?#]+):)?(\/\/([^\/?#]*))([^?#]*)(\?([^#]*))?(#(.*))?"\s*?.*?\s*?>/g;
    /<\s*.*\s*a.*\s*?href\s*=\s*(([^:\/?#]+):)(\/\/([^\/#]*))([^#]*)(\?([^#]*))?(#(.*))?".*>/g;
    if (length($4)) {
        push(@words, $4);
    }

    if ($line eq "stop\n") {
        last;
    }
}

my @unique_words = uniq @words;

foreach my $value (sort @unique_words) {
    print($value);
    print("\n")
}