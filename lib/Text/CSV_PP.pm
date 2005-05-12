package Text::CSV_PP;

################################################################################
#
# Text::CSV_PP - comma-separated values manipulation routines (PP version)
#
################################################################################
require 5.005;

use strict;
use vars qw($VERSION);
use Carp;

$VERSION = '1.00';

sub PV () { 0 }
sub IV () { 1 }
sub NV () { 2 }

################################################################################
# version
#  See Text::CSV_XS
################################################################################
sub version {
	return $VERSION;
}

################################################################################
# new
#  See Text::CSV_XS
################################################################################
sub new {
	my $proto = shift;
	my $attr  = shift || {};
	my $class = ref($proto) || $proto;
	my $self  = {
		quote_char  => '"',
		escape_char => '"',
		sep_char    => ',',
		eol         => '' ,
		%$attr,
	};

	$self->{_STATUS} = undef;
	$self->{_STRING} = undef;
	$self->{_FIELDS} = undef;
	$self->{_ERROR_INPUT} = undef;

	bless $self, $class;

	if(exists($self->{types})) {
		$self->types($self->{types});
	}

	return $self;
}

################################################################################
# status
#  See Text::CSV_XS
################################################################################
sub status {
	my $self = shift;
	return $self->{_STATUS};
}

################################################################################
# error_input
#  See Text::CSV_XS
################################################################################
sub error_input {
	my $self = shift;
	return $self->{_ERROR_INPUT};
}

################################################################################
# string
#  See Text::CSV_XS
################################################################################
sub string {
	my $self = shift;
	return $self->{_STRING};
}

################################################################################
# fields
#  See Text::CSV_XS
################################################################################
sub fields {
	my $self = shift;
	if (ref($self->{_FIELDS})) {
		return @{$self->{_FIELDS}};
	}
	return undef;
}

################################################################################
# combine
#  See Text::CSV_XS
################################################################################
sub combine {
	my $self = shift;
	my @part = @_;
	my $io;

	# at least one argument was given for "combining"...
	return $self->{_STATUS} = 0 unless(@part);

	if(UNIVERSAL::can($part[0],'print')){ # IO like object
		if(ref($part[1]) ne 'ARRAY'){
			croak("fields is not an array ref");
		}
		$io   = shift @part;
		@part = @{ shift @part };
	}

	$self->{_FIELDS}      = \@part;
	$self->{_ERROR_INPUT} = undef;
	$self->{_STRING}      = '';
	$self->{_STATUS}      = 0;

	my $always_quote = $self->{always_quote};
	my $binary       = $self->{binary};
	my $quot         = $self->{quote_char};
	my $sep          = $self->{sep_char};
	my $esc          = $self->{escape_char};

	if(!defined $quot){ $quot = ''; }

	my $re_esc = $self->{_re_escape}->{$quot}->{$esc} ||= qr/(\Q$quot\E|\Q$esc\E)/;
	my $re_sp  = $self->{_re_sp}->{$sep}              ||= qr/[\s\Q$sep\E]/;

	my $must_be_quoted;
	for my $column (@part) {

		if (!$binary and $column =~ /[^\x09\x20-\x7E]/) {
			# an argument contained an invalid character...
			$self->{_ERROR_INPUT} = $column;
			return $self->{_STATUS};
		}

		$must_be_quoted = 0;

		if($quot ne '' and $column =~ s/$re_esc/$esc$1/g){
			$must_be_quoted++;
		}
		if($column =~ /$re_sp/){
			$must_be_quoted++;
		}
		if($binary){
			$must_be_quoted++ if($column =~ s/\0/${esc}0/g);
		}

		if($always_quote or $must_be_quoted){
			$column = $quot . $column . $quot;
		}
	}

	if($io){
		return $io->print(join($sep,@part) . $self->{eol});
	}

	$self->{_STRING} = join($sep,@part) . $self->{eol};
	$self->{_STATUS} = 1;

	return $self->{_STATUS};
}

################################################################################
# parse
#  See Text::CSV_XS
################################################################################
sub parse {
	my $self = shift;
	my $line = shift;

	$self->{_STRING}      = $line;
	$self->{_FIELDS}      = undef;
	$self->{_STATUS}      = 0;
	$self->{_ERROR_INPUT} = $self->{_STRING};

	if(!defined $line){
		return $self->{_STATUS};
	}

	my $binary = $self->{binary};
	my $quot   = $self->{quote_char};
	my $sep    = $self->{sep_char};
	my $esc    = $self->{escape_char};
	my $types  = $self->types;

	$line =~ s/(?:\x0D\x0A|[\x0D\x0A])?$/$sep/;

	my $re_split = $self->{_re_split}->{$quot}->{$esc}->{$sep}
	   ||= qr/(\Q$quot\E[^\Q$quot$esc\E]*(?:\Q$esc\E[\Q$quot$esc\E0][^\Q$quot$esc\E]*)*\Q$quot\E|[^\Q$sep\E]*)\Q$sep\E/s;
	my $re_quoted       = $self->{_re_quoted}->{$quot}               ||= qr/^$quot(.*)$quot$/s;
	my $re_in_quot_esp1 = $self->{_re_in_quot_esp1}->{$esc}          ||= qr/\Q$esc\E(.)/;
	my $re_in_quot_esp2 = $self->{_re_in_quot_esp2}->{$quot}->{$esc} ||= qr/[\Q$quot$esc\E]/;
	my $re_conv_esc     = $self->{_re_conv_esc}->{$quot}->{$esc}     ||= qr/\Q$esc\E(\Q$quot\E|\Q$esc\E)/;
	my $re_conv_null    = $self->{_re_conv_null}->{$esc}             ||= qr/\Q$esc\E0/;
	my $re_quot_char    = $self->{_re_quot_char}->{$quot}            ||= qr/\Q$quot\E/;

	my $palatable = 1;
	my @part      = ();

	my $i = 0;
	for my $col ($line =~ /$re_split/g){

		if(!$binary and $col =~ /[^\x09\x20-\x7E]/){
			$palatable = 0;
			last;
		}

		if($col =~ $re_quoted){
			$col = $1;
			if(!$binary and $col =~ $re_in_quot_esp1){
				my $str = $1;
				if($str !~ $re_in_quot_esp2){
					$palatable = 0;
					last;
				}
			}
			$col =~ s/$re_conv_esc/$1/g;
			$col =~ s/$re_conv_null/\0/g;

			if($types and $types->[$i]){ # IV pr NV
				_check_type(\$col);
			}
		}
		elsif(!$binary and $col =~ $re_quot_char){
			$palatable = 0;
			last;
		}
		elsif($types and $types->[$i]){ # IV pr NV
			_check_type(\$col);
		}
		push @part,$col;
		$i++;
	}

	if($palatable and ! @part){
		$palatable = 0;
	}

	if($palatable){
		$self->{_ERROR_INPUT} = undef;
		$self->{_FIELDS}      = \@part;
	}

	return $self->{_STATUS} = $palatable;
}

################################################################################
# print
#  See Text::CSV_XS
################################################################################

sub print {
	my ($self,$io,$cols) = @_;
	$self->combine($io,$cols);
}

################################################################################
# getline
#  See Text::CSV_XS
################################################################################

sub getline {
	my ($self,$io) = @_;
	$self->parse($io->getline()) or return;
	[ $self->fields() ];
}

################################################################################
# type
#  See Text::CSV_XS
################################################################################

sub types {
	my $self = shift;

	if (@_) {
		if (my $types = shift) {
			$self->{'_types'} = join("", map{ chr($_) } @$types);
			$self->{'types'} = $types;
		}
		else {
			delete $self->{'types'};
			delete $self->{'_types'};
			undef;
		}
	}
	else {
		$self->{'types'};
	}
}

################################################################################
# _check_type
#  take an arg as scalar referrence.
#  if not numeric, make the value 0. otherwise INTEGERized.
################################################################################

sub _check_type {
	my $col_ref = shift;
	unless($$col_ref =~ /^[+-]?(?=\d|\.\d)\d*(\.\d*)?([Ee]([+-]?\d+))?$/){
		warn sprintf("Argument \"%s\" isn't numeric in subroutine entry",$$col_ref);
		$$col_ref = 0;
	}
	else{
		$$col_ref = sprintf("%d",$$col_ref);
	}
}

################################################################################
1;
__END__


=head1 NAME

Text::CSV_PP - comma-separated values manipulation routines (PP version)


=head1 SYNOPSIS

 use Text::CSV_PP;
 
 $csv = Text::CSV_PP->new();           # create a new object
 # If you want to handle non-ascii char.
 $csv = Text::CSV_PP->new({binary => 1});
 
 $status = $csv->combine(@columns);    # combine columns into a string
 $line   = $csv->string();             # get the combined string
 
 $status  = $csv->parse($line);        # parse a CSV string into fields
 @columns = $csv->fields();            # get the parsed fields
 
 $status = $csv->status();             # get the most recent status
 
 $status = $csv->print($io, $columns); # Write an array of fields immediately
                                       # to a file $io (ex. IO::File object)
 
 $columns = $csv->getline($io);        # Read a line from file $io, parse it
                                       # and return an array ref of fields
 
 $csv->types(\@array);                 # Set column types


=head1 DESCRIPTION

Text::CSV_PP is a Pure Perl module which has almost same functions
of L<Text::CSV_XS>. Text::CSV_XS is an useful module which provides
facilities for the composition and decomposition of comma-separated values.

=head1 METHODS

Almost descriptions are from Text::CSV_XS's pod documentation.

=over 4

=item version()

Returns the current module version.

=item new(\%attr)

Returns a new instance of Text::CSV_PP. The objects
attributes are described by the (optional) hash ref C<\%attr>.
Currently the following attributes are same as Text::CSV_XS:

=over 8

=item quote_char

The char used for quoting fields containing blanks, by default the
double quote character (C<">). A value of undef suppresses
quote chars.

=item eol

An end-of-line string to add to rows, usually C<undef> (nothing,
default), C<"\012"> (Line Feed) or C<"\015\012"> (Carriage Return,
Line Feed)

=item escape_char

The char used for escaping certain characters inside quoted fields,
by default the same character. (C<">)

=item sep_char

The char used for separating fields, by default a comme. (C<,>)

=item binary

If this attribute is TRUE, you may use binary characters in quoted fields,
including line feeds, carriage returns and NUL bytes. (The latter must
be escaped as C<"0>.) By default this feature is off.

=item types

A set of column types; this attribute is immediately passed to the
I<types> method below. You must not set this attribute otherwise,
except for using the I<types> method. For details see the description
of the I<types> method below.

=item always_quote

By default the generated fields are quoted only, if they need to, for
example, if they contain the separator. If you set this attribute to
a TRUE value, then all fields will be quoted. This is typically easier
to handle in external applications.

=back

To sum it up,

 $csv = Text::CSV_XS->new();

is equivalent to

 $csv = Text::CSV_XS->new({
     'quote_char'  => '"',
     'escape_char' => '"',
     'sep_char'    => ',',
     'binary'      => 0
 });

=item combine

 $status = $csv->combine(@columns);

This object function constructs a CSV string from the arguments, returning
success or failure. Upon success, C<string()> can be called to
retrieve the resultant CSV string.  Upon failure, the value returned by
C<string()> is undefined and C<error_input()> can be called to retrieve an
invalid argument.

=item print

 $status = $csv->print($io, $columns);

Similar to combine, but it expects an array ref as input (not an array!)
and the resulting string is immediately written
to the I<$io> object, typically an IO handle or any other object that
offers a I<print> method. Note, this implies that the following is wrong:

 open(FILE, ">whatever");
 $status = $csv->print(\*FILE, $columns);

The glob C<\*FILE> is not an object, thus it doesn't have a print
method. The solution is to use an IO::File object or to hide the
glob behind an IO::Wrap object. See L<IO::File(3)> and L<IO::Wrap(3)>
for details.

=item string

 $line = $csv->string();

This object function returns the input to C<parse()> or the resultant CSV
string of C<combine()>, whichever was called more recently.

=item parse

 $status = $csv->parse($line);

This object function decomposes a CSV string into fields, returning
success or failure.  Failure can result from a lack of argument or the
given CSV string is improperly formatted.  Upon success, C<fields()> can
be called to retrieve the decomposed fields .  Upon failure, the value
returned by C<fields()> is undefined and C<error_input()> can be called
to retrieve the invalid argument.

You may use the I<types()> method for setting column types. See the
description below.


=item getline

 $columns = $csv->getline($io);

This is the counterpart to print, like parse is the counterpart to
combine: It reads a row from the IO object $io using $io->getline()
and parses this row into an array ref. This array ref is returned
by the function or undef for failure.

The I<$csv-E<gt>string()>, I<$csv-E<gt>fields()> and I<$csv-E<gt>status()>
methods are meaningless, again.

=item types

 $csv->types(\@tref);

This method is used to force that columns are of a given type. For
example, if you have an integer column, two double columns and a
string column, then you might do a

 $csv->types([Text::CSV_XS::IV(),
              Text::CSV_XS::NV(),
              Text::CSV_XS::NV(),
              Text::CSV_XS::PV()]);

Column types are used only for decoding columns, in other words
by the I<parse()> and I<getline()> methods.

You can unset column types by doing a

 $csv->types(undef);

or fetch the current type settings with

 $types = $csv->types();

=item fields

 @columns = $csv->fields();

This object function returns the input to C<combine()> or the resultant
decomposed fields of C<parse()>, whichever was called more recently.

=item status

 $status = $csv->status();

This object function returns success (or failure) of C<combine()> or
C<parse()>, whichever was called more recently.

=item error_input

 $bad_argument = $csv->error_input();

This object function returns the erroneous argument (if it exists) of
C<combine()> or C<parse()>, whichever was called more recently.

=back

=head1 SPEED

Of course Text::CSV_PP is much more slow than CSV_XS.
Here is a benchmark test in Text-CSV_XS-0.23.
(with Pentium4 CPU 1.6GHz Mem 256M Windows2000 + ActivePerl)

 TEXT::CSV_PP
 Testing row creation speed ...
 10000 rows created in 0.9 cpu+sys seconds (10857 per sec)
 
 Testing row parsing speed ...
 10000 rows parsed in 1.2 cpu+sys seconds (8250 per sec)
 
 
 TEXT::CSV_XS
 Testing row creation speed ...
 10000 rows created in 0.3 cpu+sys seconds (37037 per sec)
 
 Testing row parsing speed ...
 10000 rows parsed in 0.6 cpu+sys seconds (17211 per sec)

=head1 CAVEATS

Below description is entirely from Text::CSV_XS's pod documentation.

This module is based upon a working definition of CSV format which may not be
the most general.

=over 4

=item 1 

Allowable characters within a CSV field include 0x09 (tab) and the inclusive
range of 0x20 (space) through 0x7E (tilde). In binary mode all characters
are accepted, at least in quoted fields:

=item 2

A field within CSV may be surrounded by double-quotes. (The quote char)

=item 3

A field within CSV must be surrounded by double-quotes to contain a comma.
(The separator char)

=item 4

A field within CSV must be surrounded by double-quotes to contain an embedded
double-quote, represented by a pair of consecutive double-quotes. In binary
mode you may additionally use the sequence C<"0> for representation of a
NUL byte.

=item 5

A CSV string may be terminated by 0x0A (line feed) or by 0x0D,0x0A
(carriage return, line feed).

=head1 AUTHOR

Makamaka Hannyaharamitu, E<lt>makamaka[at]cpan.orgE<gt>

Text::CSV_XS was written E<lt>joe[at]ispsoft.deE<gt>.

Text::CSV was written E<lt>alan[at]mfgrtl.comE<gt>.

=head1 COPYRIGHT AND LICENSE

Copyright 2005 by Makamaka Hannyaharamitu

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself. 

=head1 SEE ALSO

L<Text::CSV_XS>, L<Text::CSV>

I got many regexp bases from http://www.din.or.jp/~ohzaki/perl.htm

=cut
