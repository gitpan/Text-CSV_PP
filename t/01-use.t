
use Test::More tests => 3;
BEGIN { use_ok('Text::CSV_PP') };

package TestSubClass;

@TestSubClass::ISA = 'Text::CSV_PP';

package main;

isa_ok(Text::CSV_PP->new(),'Text::CSV_PP');

isa_ok(TestSubClass->new(),'Text::CSV_PP');
