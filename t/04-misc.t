# This test is from Text-CSV_XS-0.23

use Test::More tests => 15;
use Text::CSV_PP;
use strict;

my(@binFields) = ("abc\0def\n\rghi", "ab\"ce,\032\"'", "\377");

my($csv) = Text::CSV_PP->new({'binary' => 1});
ok($csv->combine(@binFields)) or print "Failed to encode binary fields\n";
my($string) = $csv->string();
ok($string eq qq("abc"0def\n\rghi","ab""ce,\032""'",\377))
    or printf("Encode: Expected \n%s\n, got \n%s\n",
             unpack("H*", qq("abc"0def\n\rghi","ab""ce,\032""'")),
             unpack("H*", $string));
ok($csv->parse($string)) or print "Failed to decode binary fields\n";
ok($csv->fields() == @binFields) or print "Wrong number of fields.\n";
ok(($csv->fields())[0] eq $binFields[0])
    or printf("Field 0: Expected %s, got %s.\n",
	      $binFields[0], ($csv->fields())[0]);
ok(($csv->fields())[1] eq $binFields[1])
    or printf("Field 1: Expected %s, got %s.\n",
	      $binFields[1], ($csv->fields())[1]);
ok(($csv->fields())[2] eq $binFields[2])
    or printf("Field 1: Expected %s, got %s.\n",
	      $binFields[1], ($csv->fields())[1]);
$csv->{'eol'} = "\r\n";
ok($csv->combine(@binFields)) or print "Failed to encode binary fields\n";
$string = $csv->string();
ok($string eq qq("abc"0def\n\rghi","ab""ce,\032""'",\377\r\n))
    or printf("Encode: Expected \n%s\n, got \n%s\n",
             unpack("H*", qq("abc"0def\n\rghi","ab""ce,\032""'")),
             unpack("H*", $string));
$csv->{'eol'} = "\n";
ok($csv->combine(@binFields)) or print "Failed to encode binary fields\n";
$string = $csv->string();
ok($string eq qq("abc"0def\n\rghi","ab""ce,\032""'",\377\n))
    or printf("Encode: Expected \n%s\n, got \n%s\n",
             unpack("H*", qq("abc"0def\n\rghi","ab""ce,\032""'")),
             unpack("H*", $string));
$csv->{'quote_char'} = undef;
ok($csv->combine("abc","def","ghi"));
ok($csv->string() eq qq(abc,def,ghi\n));


# Ken's test
{
    my $csv2 = Text::CSV_PP->new({'always_quote' => 1});
    ok($csv2->combine("abc","def","ghi"));
    ok($csv2->string() eq qq("abc","def","ghi"))
	or printf("Expected %s, got %s.\n", qq("abc","def","ghi"),
		  $csv2->string());
}
