#!/usr/bin/perl

use strict;
$^W = 1;	# use warnings core since 5.6

use Test::More tests => 39;

BEGIN {
    use_ok "Text::CSV_PP";
    plan skip_all => "Cannot load Text::CSV_PP" if $@;
    }

my $csv;
ok ($csv = Text::CSV_PP->new,				"new ()");

is ($csv->quote_char,			'"',		"quote_char");
is ($csv->escape_char,			'"',		"escape_char");
is ($csv->sep_char,			',',		"sep_char");
is ($csv->eol,				'',		"eol");
is ($csv->always_quote,			0,		"always_quote");
is ($csv->binary,			0,		"binary");
is ($csv->keep_meta_info,		0,		"keep_meta_info");
is ($csv->allow_loose_quotes,		0,		"allow_loose_quotes");
is ($csv->allow_loose_escapes,		0,		"allow_loose_escapes");
is ($csv->allow_whitespace,		0,		"allow_whitespace");
is ($csv->verbatim,			0,		"verbatim");

is ($csv->binary (1),			1,		"binary (1)");
my @fld = ( 'txt =, "Hi!"', "Yes", "", 2, undef, "1.09", "\r", undef );
ok ($csv->combine (@fld),				"combine");
is ($csv->string,
    qq{"txt =, ""Hi!""",Yes,,2,,1.09,"\r",},	"string");

is ($csv->sep_char (";"),		';',		"sep_char (;)");
is ($csv->quote_char ("="),		'=',		"quote_char (=)");
is ($csv->eol ("\r"),			"\r",		"eol (\\r)");
is ($csv->keep_meta_info (1),		1,		"keep_meta_info (1)");
is ($csv->always_quote (1),		1,		"always_quote (1)");
is ($csv->allow_loose_quotes (1),	1,		"allow_loose_quotes (1)");
is ($csv->allow_loose_escapes (1),	1,		"allow_loose_escapes (1)");
is ($csv->allow_whitespace (1),		1,		"allow_whitespace (1)");
is ($csv->verbatim (1),			1,		"verbatim (1)");
is ($csv->escape_char ("\\"),		"\\",		"escape_char (\\)");
ok ($csv->combine (@fld),				"combine");
is ($csv->string,
    qq{=txt \\=, "Hi!"=;=Yes=;==;=2=;;=1.09=;=\r=;\r},	"string");

# Funny settings, all three translate to \0 internally
$csv = Text::CSV_PP->new ({
    sep_char	=> undef,
    quote_char	=> undef,
    escape_char	=> undef,
    });
is ($csv->sep_char,		undef,		"sep_char undef");
is ($csv->quote_char,		undef,		"quote_char undef");
is ($csv->escape_char,		undef,		"escape_char undef");
ok ($csv->parse ("foo"),			"parse (foo)");
$csv->sep_char (",");
ok ($csv->parse ("foo"),			"parse (foo)");
ok (!$csv->parse ("foo,foo\0bar"),		"parse (foo)");
$csv->escape_char ("\\");
ok (!$csv->parse ("foo,foo\0bar"),		"parse (foo)");
$csv->binary (1);
ok (!$csv->parse ("foo,foo\0bar"),		"parse (foo)");

# And test erroneous calls

is (Text::CSV_PP::new (0),		   undef,	"new () as function");
is (Text::CSV_PP->new ({ oel     => "" }), undef,	"typo in attr");
is (Text::CSV_PP->new ({ _STATUS => "" }), undef,	"private attr");

1;
