#!/usr/bin/perl
use strict;
use warnings FATAL => 'all';

while (<>) {
    print if /^(0|11|10(1|00)*01)*$/;
}