# This test is from Text-CSV_XS-0.23

use Test::More tests => 7;
use Text::CSV_PP;
use strict;

#
#	"Pavel Kotala" <pkotala@logis.cz>
#
{
    my $csv = Text::CSV_PP->new({'quote_char' => '"',
				 'escape_char' => '\\',
				 'sep_char' => ';',
				 'binary' => 1});
    ok($csv);
    my @list = ("c:\\winnt", "text");
    ok($csv->combine(@list));
    my $line = $csv->string();
    ok($line);
    ok($csv->parse($line));
    my @olist = $csv->fields();
    ok(@list == @olist);
    ok($list[0] eq $olist[0]);
    ok($list[1] eq $olist[1]);
}
