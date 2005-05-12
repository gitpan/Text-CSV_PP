# This test is from Text-CSV_PP-0.23

use Test::More tests => 12;
use Text::CSV_PP;
use strict;

$| = 1;
$^W = 1;

my $csv = Text::CSV_PP->new({'types' => [Text::CSV_PP::IV(),
					 Text::CSV_PP::PV(),
					 Text::CSV_PP::NV()]});
ok($csv);
ok(@{$csv->{'types'}} == 3);
ok($csv->{'types'}->[0] == Text::CSV_PP::IV()  and
     $csv->{'types'}->[1] == Text::CSV_PP::PV()  and
     $csv->{'types'}->[2] == Text::CSV_PP::NV());
ok(length($csv->{'_types'}) == 3);
ok($csv->{'_types'} eq
     chr(Text::CSV_PP::IV()) . chr(Text::CSV_PP::PV()) .
     chr(Text::CSV_PP::NV()));

ok($csv->combine('', '', '1.00'));
ok($csv->string() eq ",,1.00");
my $warning;
$SIG{__WARN__} = sub { $warning = shift };
ok($csv->parse($csv->string()));
ok($warning =~ /numeric/);
my @fields = $csv->fields();
ok($fields[0] eq '0');
ok($fields[1] eq '');
ok($fields[2] eq '1');
