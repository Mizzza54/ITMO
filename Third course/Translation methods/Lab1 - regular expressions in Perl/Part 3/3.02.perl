#!/usr/bin/perl
use strict;
use warnings FATAL => 'all';

my $isTextStart = 0;
my $begin = 0;

while (<>) {
    s/<[^>]*>//g;
    if (/^\s*$/) {
        if ($isTextStart == 0) {

        } else {
            $begin = 1
        }
    } else {
        if ($isTextStart == 0) {
            $isTextStart = 1;
        }
        if ($begin == 1) {
            print "\n";
            $begin = 0;
        }
        s/^(\s)+|(\s)+$//g;
        s/(\s)(\s)+/ /g;
        print;
        print "\n";
    }
}