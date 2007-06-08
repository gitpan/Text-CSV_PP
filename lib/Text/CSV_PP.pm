package Text::CSV_PP;

################################################################################
#
# Text::CSV_PP - comma-separated values manipulation routines (PP version)
#
################################################################################
require 5.005;

use strict;
use vars qw($VERSION);
use Carp ();

$VERSION = '1.05';

sub PV () { 0 }
sub IV () { 1 }
sub NV () { 2 }

sub IS_QUOTED () { 0x0001; }
sub IS_BINARY () { 0x0002; }

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

my %def_attr = (
    quote_char          => '"',
    escape_char         => '"',
    sep_char            => ',',
    eol                 => '',
    always_quote        => 0,
    binary              => 0,
    keep_meta_info      => 0,
    allow_loose_quotes  => 0,
    allow_loose_escapes => 0,
    allow_whitespace    => 0,
    types               => undef,

    _EOF                => 0,
    _STATUS             => undef,
    _FIELDS             => undef,
    _FFLAGS             => undef,
    _STRING             => undef,
    _ERROR_INPUT        => undef,
);


sub new {
    my $proto = shift;
    my $attr  = shift || {};
    my $class = ref($proto) || $proto or return;
    my $self  = { %def_attr };

    for my $prop (keys %$attr) { # if invalid attr, return undef
        return unless ($prop =~ /^[a-z]/ && exists $def_attr{$prop});
        $self->{$prop} = $attr->{$prop};
    }

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
    $_[0]->{_STATUS};
}
################################################################################
# error_input
#  See Text::CSV_XS
################################################################################
sub error_input {
    $_[0]->{_ERROR_INPUT};
}
################################################################################
# string
#  See Text::CSV_XS
################################################################################
sub string {
    $_[0]->{_STRING};
}
################################################################################
# fields
#  See Text::CSV_XS
################################################################################
sub fields {
    ref($_[0]->{_FIELDS}) ?  @{$_[0]->{_FIELDS}} : undef;
}
################################################################################
# combine
#  See Text::CSV_XS
################################################################################
sub combine {
    my ($self, @part) = @_;

    # at least one argument was given for "combining"...
    return $self->{_STATUS} = 0 unless(@part);

    $self->{_FIELDS}      = \@part;
    $self->{_ERROR_INPUT} = undef;
    $self->{_STRING}      = '';
    $self->{_STATUS}      = 0;

    my ($always_quote, $binary, $quot, $sep, $esc)
            = @{$self}{qw/always_quote binary quote_char sep_char escape_char/};

    if(!defined $quot){ $quot = ''; }

    return if ($sep eq $esc or $sep eq $quot);

    my $re_esc = $self->{_re_comb_escape}->{$quot}->{$esc} ||= qr/(\Q$quot\E|\Q$esc\E)/;
    my $re_sp  = $self->{_re_comb_sp}->{$sep}              ||= qr/[\s\Q$sep\E]/;

    my $must_be_quoted;
    for my $column (@part) {

        unless (defined $column) {
            $column = '';
            next;
        }

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

    $self->{_STRING} = join($sep, @part) . $self->{eol};
    $self->{_STATUS} = 1;

    return $self->{_STATUS};
}
################################################################################
# parse
#  See Text::CSV_XS
################################################################################
sub parse {
    my ($self, $line) = @_;
    #utf8::encode($line) if (utf8::is_utf8($line)); # TODO?
    @{$self}{qw/_STRING _FIELDS _STATUS _ERROR_INPUT/} = ($line, undef, 0, $line);

    return 0 if(!defined $line);

    my ($binary, $quot, $sep, $esc, $types, $keep_meta_info, $allow_whitespace)
         = @{$self}{qw/binary quote_char sep_char escape_char types keep_meta_info allow_whitespace/};

    return if ($sep eq $esc or $sep eq $quot);

    my $meta_flag      = $keep_meta_info ? [] : undef;
    my $re_split       = $self->{_re_split}->{$quot}->{$esc}->{$sep} ||= _make_regexp_split_column($esc, $quot, $sep);
    my $re_quoted       = $self->{_re_quoted}->{$quot}               ||= qr/^\Q$quot\E(.*)\Q$quot\E$/s;
    my $re_in_quot_esp1 = $self->{_re_in_quot_esp1}->{$esc}          ||= qr/\Q$esc\E(.)/;
    my $re_in_quot_esp2 = $self->{_re_in_quot_esp2}->{$quot}->{$esc} ||= qr/[\Q$quot$esc\E]/;
    my $re_quot_char    = $self->{_re_quot_char}->{$quot}            ||= qr/\Q$quot\E/;
    my $re_esc          = $self->{_re_esc}->{$quot}->{$esc}          ||= qr/\Q$esc\E(\Q$quot\E|\Q$esc\E|0)/;
    my $re_invalid_quot = $self->{_re_invalid_quot}->{$quot}->{$esc} ||= qr/^$re_quot_char|[^\Q$re_esc\E]$re_quot_char/;
    my $re_rs           = $self->{_re_rs}->{$/} ||= qr{$/?$}; # $/ .. input record separator

    if ($allow_whitespace) {
        $re_split = $self->{_re_split_allow_sp}->{$quot}->{$esc}->{$sep}
                     ||= _make_regexp_split_column_allow_sp($esc, $quot, $sep);
    }

    my $palatable = 1;
    my @part      = ();

    my $i = 0;
    my $flag;

    $line =~ s/$re_rs/$sep/; # $line =~ s/(?:\x0D\x0A|[\x0D\x0A])?$/$sep/;

    for my $col ($line =~ /$re_split/g) {

        if (!$binary and $col =~ /[^\x09\x20-\x7E]/) {
            $palatable = 0;
            last;
        }

        if ($keep_meta_info) {
            $flag = 0x0000;
            $flag |= IS_BINARY if ($col =~ /[^\x09\x20-\x7E]/);
        }

        if ($col =~ $re_quoted) {
            $flag |= IS_QUOTED if ($keep_meta_info);
            $col = $1;
            if (!$binary and $col =~ $re_in_quot_esp1) {
                my $str = $1;
                if ($str !~ $re_in_quot_esp2) {
                    unless ($self->{allow_loose_escapes}) {
                        $palatable = 0;
                        last;
                    }
                    else {
                        $col =~ s/\Q$esc\E(.)/$1/g;
                    }
                }
            }

            $col =~ s{$re_esc}{$1 eq '0' ? "\0" : $1}eg;

            if ($types and $types->[$i]) { # IV or NV
                _check_type(\$col);
            }
        }
        elsif (!$binary and $col =~ $re_invalid_quot) {
            unless ($self->{allow_loose_quotes} and $col =~ /$re_quot_char/) {
                $palatable = 0;
                last;
            }
        }
        elsif ($types and $types->[$i]) { # IV or NV
            _check_type(\$col);
        }

        push @part,$col;
        push @{$meta_flag}, $flag if ($keep_meta_info);

        $i++;
    }

    if ($palatable and ! @part) {
        $palatable = 0;
    }

    if ($palatable) {
        $self->{_ERROR_INPUT} = undef;
        $self->{_FIELDS}      = \@part;
    }

    $self->{_FFLAGS} = $keep_meta_info ? $meta_flag : [];

    return $self->{_STATUS} = $palatable;
}


sub _make_regexp_split_column {
    my ($esc, $quot, $sep) = @_;
    qr/(
        \Q$quot\E
            [^\Q$quot$esc\E]*(?:\Q$esc\E[\Q$quot$esc\E0][^\Q$quot$esc\E]*)*
        \Q$quot\E
        | # or
        [^\Q$sep\E]*
       )
       \Q$sep\E
    /xs;
}


sub _make_regexp_split_column_allow_sp {
    my ($esc, $quot, $sep) = @_;
    qr/[\x20\x09]*
       (
        \Q$quot\E
            [^\Q$quot$esc\E]*(?:\Q$esc\E[\Q$quot$esc\E0][^\Q$quot$esc\E]*)*
        \Q$quot\E
        | # or
        [^\Q$sep\E]*?
       )
       [\x20\x09]*\Q$sep\E[\x20\x09]*
    /xs;
}
################################################################################
# print
#  See Text::CSV_XS
################################################################################
sub print {
    my ($self, $io, $cols) = @_;

    if(ref($cols) ne 'ARRAY'){
        Carp::croak("Expected fields to be an array ref");
    }

    $self->combine(@$cols) or return '';

    print $io $self->string;
}
################################################################################
# getline
#  See Text::CSV_XS
################################################################################
sub getline {
    my ($self,$io) = @_;

    $self->{_EOF} = eof($io) ? 1 : '';

    my $line  = $io->getline();

    $line .= $io->getline() while ( defined $line and $line =~ tr/"// % 2 and !eof($io) );

    $self->parse($line) or return;

    [ $self->fields() ];
}
################################################################################
# eof
################################################################################
sub eof {
    $_[0]->{_EOF};
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
        Carp::carp sprintf("Argument \"%s\" isn't numeric in subroutine entry",$$col_ref);
        $$col_ref = 0;
    }
    else{
        $$col_ref = sprintf("%d",$$col_ref);
    }
}
################################################################################

sub meta_info {
    $_[0]->{_FFLAGS} ? @{ $_[0]->{_FFLAGS} } : undef;
}

sub is_quoted {
    return unless (defined $_[0]->{_FFLAGS});
    return if( $_[1] =~ /\D/ or $_[1] < 0 or  $_[1] > $#{ $_[0]->{_FFLAGS} } );

    $_[0]->{_FFLAGS}->[$_[1]] & IS_QUOTED ? 1 : 0;
}

sub is_binary {
    return unless (defined $_[0]->{_FFLAGS});
    return if( $_[1] =~ /\D/ or $_[1] < 0 or  $_[1] > $#{ $_[0]->{_FFLAGS} } );
    $_[0]->{_FFLAGS}->[$_[1]] & IS_BINARY ? 1 : 0;
}

BEGIN {
    for my $method (qw/quote_char escape_char sep_char eol always_quote binary allow_whitespace
                        keep_meta_info allow_loose_quotes allow_loose_escapes /) {
        eval qq|
            sub $method {
                \$_[0]->{$method} = \$_[1] if (defined \$_[1]);
                \$_[0]->{$method};
            }
        |;
    }
}

################################################################################
1;
__END__
=pod

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

Text::CSV_PP has almost same functions of L<Text::CSV_XS> which 
provides facilities for the composition and decomposition of
comma-separated values. As its name suggests, L<Text::CSV_XS>
is a XS module and Text::CSV_PP is a Puer Perl one.

=head1 METHODS

Almost descriptions are from Text::CSV_XS (0.23 - 0.29)'s pod documentation.

=over 4

=item version()

Returns the current module version.

=item new(\%attr)

Returns a new instance of Text::CSV_PP. The objects
attributes are described by the (optional) hash ref C<\%attr>.
Currently the following attributes are same as L<Text::CSV_XS>:

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

=item allow_whitespace

When this option is set to true, whitespace (TAB's and SPACE's)
surrounding the separation character is removed when parsing. So
lines like:

  1 , "foo" , bar , 3 , zapp

are now correctly parsed, even though it violates the CSV specs.
Note that B<all> whitespace is stripped from start and end of each
field. That would make is more a I<feature> than a way to be able
to parse bad CSV lines, as

 1,   2.0,  3,   ape  , monkey

will now be parsed as

 ("1", "2.0", "3", "ape", "monkey")

even if the original line was perfectly sane CSV.

=item allow_loose_quotes

By default, parsing fields that have C<quote_char> characters inside
an unquoted field, like

 1,foo "bar" baz,42

would result in a parse error. Though it is still bad practice to
allow this format, we cannot help there are some vendors that make
their applications spit out lines styled like this.

=item allow_loose_escapes

By default, parsing fields that have C<escapee_char> characters that
escape characters that do not need to be escaped, like:

 my $csv = Text::CSV_PP->new ({ esc_char => "\\" });
 $csv->parse (qq{1,"my bar\'s",baz,42});

would result in a parse error. Though it is still bad practice to
allow this format, this option enables you to treat all escape character
sequences equal.

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

=item keep_meta_info

By default, the parsing of input lines is as simple and fast as
possible. However, some parsing information - like quotation of
the original field - is lost in that process. Set this flag to
true to be able to retrieve that information after parsing with
the methods C<meta_info ()>, C<is_quoted ()>, and C<is_binary ()>
described below.  Default is false.

=back

To sum it up,

 $csv = Text::CSV_PP->new();

is equivalent to

 $csv = Text::CSV_PP->new({
     'quote_char'     => '"',
     'escape_char'    => '"',
     'sep_char'       => ',',
     'eol'            => '',
     'always_quote'   => 0,
     'binary'         => 0,
     'keep_meta_info' => 0,
 });

For all of the above mentioned flags, there is an accessor method
available where you can inquire for the current value, or change
the value


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

=item eof

 $eof = $csv->eof ();

If C<parse ()> or C<getline ()> was used with an IO stream, this
mothod will return true (1) if the last call hit end of file, otherwise
it will return false (''). This is useful to see the difference between
a failure and end of file.

=item types

 $csv->types(\@tref);

This method is used to force that columns are of a given type. For
example, if you have an integer column, two double columns and a
string column, then you might do a

 $csv->types([Text::CSV_PP::IV(),
              Text::CSV_PP::NV(),
              Text::CSV_PP::NV(),
              Text::CSV_PP::PV()]);

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

=item meta_info

 @flags = $csv->meta_info();

This object function returns the flags of the resultant decomposed
fields of C<parse ()>, whichever was called more recently.


For each field, a meta_info field will hold flags that tell something about
the field returned by the C<fields ()> method. The flags are bitwise-or'd like:

=over 4

=item 0x0001

The field was quoted.

=item 0x0002

The field was binary.

=back

See the C<is_*** ()> methods below.

=item is_quoted

  my $quoted = $csv->is_quoted ($column_idx);

Where C<$column_idx> is the (zero-based) index of the column in the
last result of C<parse ()>.

This returns a true value if the data in the indicated column was
enclosed in C<quote_char> quotes. This might be important for data
where C<,20070108,> is to be treated as a numeric value, and where
C<,"20070108",> is explicitly marked as character string data.

=item is_binary

  my $binary = $csv->is_binary ($column_idx);

Where C<$column_idx> is the (zero-based) index of the column in the
last result of C<parse ()>.

This returns a true value if the data in the indicated column
contained any byte out of the range [\x09\x20-\x7E]

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
Here is a benchmark test using an example code in Text-CSV_XS-0.29.

 Text::CSV_PP (1.05)
 Benchmark: running combine   1, combine  10, combine 100, parse     1, parse
 10, parse   100 for at least 3 CPU seconds...
 combine   1:  4 wallclock secs ( 3.23 usr +  0.00 sys =  3.23 CPU) @ 12279.22/s (n=39711)
 combine  10:  3 wallclock secs ( 3.16 usr +  0.00 sys =  3.16 CPU) @ 1876.74/s (n=5923)
 combine 100:  3 wallclock secs ( 3.19 usr +  0.00 sys =  3.19 CPU) @ 192.60/s (n=614)
 parse     1:  3 wallclock secs ( 3.25 usr +  0.00 sys =  3.25 CPU) @ 7623.69/s (n=24777)
 parse    10:  3 wallclock secs ( 3.25 usr +  0.00 sys =  3.25 CPU) @ 1334.46/s (n=4337)
 parse   100:  3 wallclock secs ( 3.22 usr +  0.02 sys =  3.24 CPU) @ 132.92/s (n=430)
 Benchmark: timing 50000 iterations of print    io...
 print    io: 32 wallclock secs (30.31 usr +  0.72 sys = 31.03 CPU) @ 1611.24/s (n=50000)
 Benchmark: timing 50000 iterations of getline  io...
 getline  io: 47 wallclock secs (47.33 usr +  0.28 sys = 47.61 CPU) @ 1050.22/s (n=50000)
 File was 46050000 bytes long, line length 920


 Text::CSV_XS (0.29)
 Benchmark: running combine   1, combine  10, combine 100, parse     1, parse
 10, parse   100 for at least 3 CPU seconds...
 combine   1:  3 wallclock secs ( 3.09 usr +  0.00 sys =  3.09 CPU) @ 59718.49/s (n=184769)
 combine  10:  3 wallclock secs ( 3.09 usr +  0.00 sys =  3.09 CPU) @ 26825.09/s (n=82970)
 combine 100:  4 wallclock secs ( 3.16 usr +  0.00 sys =  3.16 CPU) @ 3741.53/s (n=11812)
 parse     1:  3 wallclock secs ( 3.20 usr +  0.00 sys =  3.20 CPU) @ 27434.59/s (n=87873)
 parse    10:  3 wallclock secs ( 3.23 usr +  0.00 sys =  3.23 CPU) @ 4576.38/s (n=14800)
 parse   100:  3 wallclock secs ( 3.19 usr +  0.00 sys =  3.19 CPU) @ 483.53/s (n=1541)
 Benchmark: timing 50000 iterations of print    io...
 print    io:  3 wallclock secs ( 2.11 usr +  0.39 sys =  2.50 CPU) @ 20008.00/s (n=50000)
 Benchmark: timing 50000 iterations of getline  io...
 getline  io: 12 wallclock secs (11.44 usr +  0.19 sys = 11.63 CPU) @ 4300.71/s ( n=50000)
 File was 46050000 bytes long, line length 920


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

Text::CSV_XS was written by E<lt>joe[at]ispsoft.deE<gt>
and maintained by E<lt>h.m.brand[at]xs4all.nlE<gt>.

Text::CSV was written by E<lt>alan[at]mfgrtl.comE<gt>.


=head1 COPYRIGHT AND LICENSE

Copyright 2005-2007 by Makamaka Hannyaharamitu, E<lt>makamaka[at]cpan.orgE<gt>

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself. 

=head1 SEE ALSO

L<Text::CSV_XS>, L<Text::CSV>

I got many regexp bases from L<http://www.din.or.jp/~ohzaki/perl.htm>

=cut
