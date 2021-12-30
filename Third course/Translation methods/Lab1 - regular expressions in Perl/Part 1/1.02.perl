#!/usr/bin/perl
use strict;
use warnings FATAL => 'all';

while (<>) {
    print if /(\b{wb}cat\b{wb})/ ;
}