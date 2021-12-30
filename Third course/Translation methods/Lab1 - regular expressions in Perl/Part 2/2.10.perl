#!/usr/bin/perl
use strict;
use warnings FATAL => 'all';

my $regex = "a(.*?)a";
while (<>) {
    s/($regex){3}/bad/g;
    print;
}