#!/usr/bin/perl

use strict;
$^W = 1;

use Test::More;

eval "use Test::Pod::Coverage tests => 1";
plan skip_all => "Test::Pod::Covarage required for testing POD Coverage" if $@;
pod_coverage_ok ("Text::CSV_PP", { also_private => [ qr/^[A-Z_]+$/ ], }, "Text::CSV_PP is covered");


