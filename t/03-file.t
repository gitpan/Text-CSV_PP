# This test is from Text-CSV_XS-0.23

use Test::More tests => 72;
use Text::CSV_PP;
use strict;

############################################################################


sub TestContents ($$@) {
    my ($csv, $fh, @input) = @_;
    ok($csv->combine(@input)) or print "Failed to parse input";
    my($got) = $fh->Contents();
    ok($csv->string() eq $got)
	or printf("Expected %s, got %s\n", $csv->string(), $got);
}
sub TestPrintRead ($$@) {
    my($csv, @input) = @_;
    my($fh) = IO_Scalar->new();

    ok($csv->print($fh, \@input));
    TestContents($csv, $fh, @input);
    ok($csv->getline($fh))
	or print("Failed to read.\n");
    ok($csv->fields() == @input)
	or print("Expected %d fields, got %d\n",
		 scalar($csv->fields()), scalar(@input));
    my($i);
    for ($i = 0;  $i < @input;  $i++) {
	ok(($csv->fields())[$i] eq $input[$i])
	    or printf("Expected field $i to be '%s', got '%s'\n",
		      $input[$i], ($csv->fields())[$i]);
    }
}
sub TestReadFailure ($$) {
    my($csv, $input) = @_;
    my($fh) = IO_Scalar->new();
    if (!$fh->print($input)  ||  !$fh->flush()) {
	die "Error while creating input file: $!";
    }
    ok(!$csv->getline($fh));
}
sub TestRead ($$@) {
    my($csv, $input, @expected) = @_;
    my($fh) = IO_Scalar->new();
    if (!$fh->print($input)  ||  !$fh->flush()) {
	die "Error while creating input file: $!";
    }
    my $fields = $csv->getline($fh);
    ok($fields) or print("Failed to read\n");
    ok(@expected == @$fields)
	or printf("Expected %d fields, got %d\n",
		  scalar(@expected), scalar($csv->fields()));
    my($i);
    for ($i = 0;  $i < @expected;  $i++) {
	if ($expected[$i] ne $$fields[$i]) {
	    printf("Field $i: Expected %s, got %s\n",
		   $expected[$i], $$fields[$i]);
	}
    }
}



my($csv) = Text::CSV_PP->new();

my($fh) = IO_Scalar->new();
ok(!$csv->print($fh, ["abc", "def\007", "ghi"]))
    or print "Bad character, but no failure\n";
TestPrintRead($csv, q(""));
TestPrintRead($csv, '', '');
TestPrintRead($csv, '', 'I said, "Hi!"', '');
TestPrintRead($csv, '"', 'abc');
TestPrintRead($csv, 'abc', '"');
TestPrintRead($csv, 'abc', 'def', 'ghi');
TestPrintRead($csv, "abc\tdef", 'ghi');
TestReadFailure($csv, '"abc')
    or print("Missing closing double-quote, but no failure\n");
TestReadFailure($csv, 'ab"c')
    or print("Double quote outside of double-quotes, but no failure.\n");
TestReadFailure($csv, '"ab"c"')
    or print("Bad character sequence, but no failure.\n");
TestReadFailure($csv, qq("abc\nc"))
    or print("Bad character, but no failure.\n");
TestRead($csv, q(","), ',');
TestRead($csv, qq("","I said,\t""Hi!""",""),
	 '', qq(I said,\t"Hi!"), '');


# This test because of a problem with DBD::CSV

$fh = IO_Scalar->new();
$csv->{binary} = 1;
$csv->{eol} = "\015\012";
ok($csv->print($fh, ["id","name"]))
    or print "Bad character, but no failure\n";
ok($csv->print($fh, [1, "Alligator Descartes"]));
ok($csv->print($fh, ["3", "Jochen Wiedmann"]));
ok($csv->print($fh, [2, "Tim Bunce"]));
ok($csv->print($fh, [" 4", "Andreas König"]));
ok($csv->print($fh, [5]));
my $contents;
ok(($contents = $fh->Contents()) eq <<"CONTENTS");
id,name\015
1,"Alligator Descartes"\015
3,"Jochen Wiedmann"\015
2,"Tim Bunce"\015
" 4","Andreas König"\015
5\015
CONTENTS

my $fields;
print "Retrieving data\n";
for (my $i = 0;  $i < 6;  $i++) {
    ok($fields = $csv->getline($fh))
	and print "Row $i: $fields (@$fields)\n";
}


############################################################################

package IO_Scalar;

sub new ($;\$) {
    my($proto, $strRef) = @_;
    my($self);
    if (!$strRef) {
	my($str) = "";
	$self = \$str;
    } elsif (ref($strRef) ne 'SCALAR') {
	die "Expected scalar ref";
    } else {
	$self = \$$strRef;
    }
    bless($self, (ref($proto) || $proto));
    $self;
}

sub print ($@) {
    my($self) = shift;
    while (@_ > 0) {
	my($str) = shift;
	if (defined($str)) {
	    $$self .= $str;
	}
    }
    1;
}

sub getline ($) {
    my($self) = shift;
    my($result);
    my($ifs) = $/;
    if (length($$self) == 0) {
	$result = undef;
    } elsif (defined($ifs)  &&  $$self =~ /^(.*?$ifs)(.*)$/s) {
	$result = $1;
	$$self = $2;
    } else {
	$result = $$self;
	$$self = '';
    }
    $result;
}

sub sref ($) {
    shift;
}

sub Contents ($) {
    ${shift()->sref};
}

sub flush ($) {
    1;
}
