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

$VERSION = '1.06';

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

my $ERRORS = {
        # PP and XS
        1001 => "sep_char is equal to quote_char or escape_char",
        2027 => "EIQ - Quoted field not terminated",
        2110 => "ECB - Binary character in Combine, binary off",

        # PP Only Error
        4000 => "ECR - Binary character in field, binary off",
        4001 => "EIQ - Needless ESC in quoted field",
        4002 => "EIQ - Unescaped ESC in quoted field",
        4003 => "EIF - ESC CR",
        4004 => "EUF - ",

        0    => "",
};


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
    chomp_verbatim      => 0,
    types               => undef,
    verbatim            => 0,

    _EOF                => 0,
    _STATUS             => undef,
    _FIELDS             => undef,
    _FFLAGS             => undef,
    _STRING             => undef,
    _ERROR_INPUT        => undef,
    _ERROR_DIAG         => undef,
);

################################################################################
# new
################################################################################
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
################################################################################
sub status {
    $_[0]->{_STATUS};
}
################################################################################
# error_input
################################################################################
sub error_input {
    $_[0]->{_ERROR_INPUT};
}
################################################################################
# error_diag
################################################################################
sub error_diag {
    defined $_[0]->{_ERROR_DIAG} or return;
    my $diag = $_[0]->{_ERROR_DIAG};
    my $context = wantarray;

    my $diagobj = bless \$diag, 'Text::CSV::ErrorDiag';

    unless (defined $context) { # Void context
        print STDERR "# CSV_PP ERROR: ", 0 + $diag, " - $diag\n";
        return;
    }

    return $context ? (0 + $diagobj, "$diagobj") : $diagobj;
}
################################################################################
# string
################################################################################
sub string {
    $_[0]->{_STRING};
}
################################################################################
# fields
################################################################################
sub fields {
    ref($_[0]->{_FIELDS}) ?  @{$_[0]->{_FIELDS}} : undef;
}
################################################################################
# combine
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

    return $self->_set_error_diag(1001) if ($sep eq $esc or $sep eq $quot);

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
            $self->_set_error_diag(2110);
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
################################################################################
my %allow_eol = ("\r" => 1, "\r\n" => 1, "\n" => 1, "" => 1);

sub parse {
    my ($self, $line) = @_;
    #utf8::encode($line) if (utf8::is_utf8($line)); # TODO?
    @{$self}{qw/_STRING _FIELDS _STATUS _ERROR_INPUT/} = ($line, undef, 0, $line);

    return 0 if(!defined $line);

    my ($binary, $quot, $sep, $esc, $types, $keep_meta_info, $allow_whitespace, $eol)
         = @{$self}{qw/binary quote_char sep_char escape_char types keep_meta_info allow_whitespace eol/};

    $sep  = "\0" unless (defined $sep);
    $esc  = "\0" unless (defined $esc);
    $quot = "\0" unless (defined $quot);

    return $self->_set_error_diag(1001) if (($sep eq $esc or $sep eq $quot) and $sep ne "\0");

    my $meta_flag      = $keep_meta_info ? [] : undef;
    my $re_split       = $self->{_re_split}->{$quot}->{$esc}->{$sep} ||= _make_regexp_split_column($esc, $quot, $sep);
    my $re_quoted       = $self->{_re_quoted}->{$quot}               ||= qr/^\Q$quot\E(.*)\Q$quot\E$/s;
    my $re_in_quot_esp1 = $self->{_re_in_quot_esp1}->{$esc}          ||= qr/\Q$esc\E(.)/;
    my $re_in_quot_esp2 = $self->{_re_in_quot_esp2}->{$quot}->{$esc} ||= qr/[\Q$quot$esc\E0]/;
    my $re_quot_char    = $self->{_re_quot_char}->{$quot}            ||= qr/\Q$quot\E/;
    my $re_esc          = $self->{_re_esc}->{$quot}->{$esc}          ||= qr/\Q$esc\E(\Q$quot\E|\Q$esc\E|0)/;
    my $re_invalid_quot = $self->{_re_invalid_quot}->{$quot}->{$esc} ||= qr/^$re_quot_char|[^\Q$re_esc\E]$re_quot_char/;
    my $re_rs           = $self->{_re_rs}->{$/} ||= qr{\Q$/\E?$}; # $/ .. input record separator

    if ($allow_whitespace) {
        $re_split = $self->{_re_split_allow_sp}->{$quot}->{$esc}->{$sep}
                     ||= _make_regexp_split_column_allow_sp($esc, $quot, $sep);
    }

    my $palatable = 1;
    my @part      = ();

    my $i = 0;
    my $flag;

    if (defined $eol and $eol eq "\r") {
        $line =~ s/[\r ]*\r[ ]*$//;
    }

    if ($self->{verbatim}) {
        $line .= $sep;
    }
    else {
        if (defined $eol and !$allow_eol{$eol}) {
            $line .= $sep;
        }
        else {
            
            $line =~ s/(?:\x0D\x0A|\x0A)?$|(?:\x0D\x0A|\x0A)[ ]*$/$sep/;
        }
    }

    for my $col ($line =~ /$re_split/g) {

        if ($keep_meta_info) {
            $flag = 0x0000;
            $flag |= IS_BINARY if ($col =~ /[^\x09\x20-\x7E]/);
        }

        if (!$binary and $col =~ /[^\x09\x20-\x7E]/) {
            $self->_set_error_diag(4000); # ECR - Binary character in field, binary off
            $palatable = 0;
            last;
        }

        if ($col =~ $re_quoted) {
            $flag |= IS_QUOTED if ($keep_meta_info);
            $col = $1;

            if ($col =~ $re_in_quot_esp1) {
                my $str = $1;
                if ($str !~ $re_in_quot_esp2) {
                    unless ($self->{allow_loose_escapes}) {
                        $self->_set_error_diag(4001); # Needless ESC in quoted field
                        $palatable = 0;
                        last;
                    }
                    else {
                        $col =~ s/\Q$esc\E(.)/$1/g;
                    }
                }
            }
            else {
                if ($col =~ /(?<!\Q$esc\E)\Q$esc\E/) {
                    $self->_set_error_diag(4002); # No escaped ESC in quoted field
                    $palatable = 0;
                    last;
                }
            }

            $col =~ s{$re_esc}{$1 eq '0' ? "\0" : $1}eg;

            if ($types and $types->[$i]) { # IV or NV
                _check_type(\$col, $types->[$i]);
            }
        }

        # quoted but invalid

        elsif ($col =~ $re_invalid_quot) {

            unless ($self->{allow_loose_quotes} and $col =~ /$re_quot_char/) {
                $self->_set_error_diag(2027);
                $palatable = 0;
                last;
            }

        }

        elsif ($types and $types->[$i]) { # IV or NV
            _check_type(\$col, $types->[$i]);
        }

        # unquoted

        else {

            if (!$self->{verbatim} and $col =~ /\r\n|\n/) {
                unless (defined $eol and !$allow_eol{$eol}) {
                    $col =~ s/(?:\r\n|\n).*//sm;
                }
            }

            if ($col =~ /\Q$esc\E\r$/) { # for t/15_flags : test 165 'ESC CR' at line 203
                $self->_set_error_diag(4003);
                $palatable = 0;
                last;
            }

            if ($col =~ /.\Q$esc\E$/) { # for t/65_allow : test 53-54 parse('foo\') at line 62, 65
                $self->_set_error_diag(4004);
                $palatable = 0;
                last;
            }

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
################################################################################
sub getline {
    my ($self,$io) = @_;

    $self->{_EOF} = eof($io) ? 1 : '';

    my $line = $io->getline();
    my $quot = $self->{quote_char};
    my $re   = qr/(?:\Q$quot\E)/;

    $line .= $io->getline() while ( defined $line and scalar(my @list = $line =~ /$re/g) % 2 and !eof($io) );

    my $eol = $self->{eol};

    if (defined $eol and defined $line) {
        $line =~ s/\Q$eol\E$//;
    }

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
################################################################################
# _check_type
#  take an arg as scalar referrence.
#  if not numeric, make the value 0. otherwise INTEGERized.
################################################################################
sub _check_type {
    my ($col_ref, $type) = @_;
    unless ($$col_ref =~ /^[+-]?(?=\d|\.\d)\d*(\.\d*)?([Ee]([+-]?\d+))?$/) {
        Carp::carp sprintf("Argument \"%s\" isn't numeric in subroutine entry",$$col_ref);
        $$col_ref = 0;
    }
    elsif ($type == NV) {
        $$col_ref = sprintf("%G",$$col_ref);
    }
    else {
        $$col_ref = sprintf("%d",$$col_ref);
    }
}
################################################################################
# _set_error_diag
################################################################################
sub _set_error_diag {
    $_[0]->{_ERROR_DIAG} = $_[1];
    return;
}
################################################################################

BEGIN {
    for my $method (qw/quote_char escape_char sep_char eol always_quote binary allow_whitespace
                        keep_meta_info allow_loose_quotes allow_loose_escapes verbatim/) {
        eval qq|
            sub $method {
                \$_[0]->{$method} = \$_[1] if (defined \$_[1]);
                \$_[0]->{$method};
            }
        |;
    }
}


################################################################################

package Text::CSV::ErrorDiag;

use strict;
use overload (
    '""' => \&stringify,
    '+'  => \&numeric,
    '-'  => \&numeric,
    '*'  => \&numeric,
    '/'  => \&numeric,
);


sub numeric {
    my ($left, $right) = @_;
    return ref $left ? $$left : $$right;
}


sub stringify {
    $ERRORS->{ $_[0] + 0 };
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
 
 $status       = $csv->status ();      # get the most recent status
 $bad_argument = $csv->error_input (); # get the most recent bad argument
 $diag         = $csv->error_diag ();  # if an error occured, explains WHY
 
 $status = $csv->print ($io, $colref); # Write an array of fields
                                       # immediately to a file $io
 $colref = $csv->getline ($io);        # Read a line from file $io,
                                       # parse it and return an array
                                       # ref of fields
 $eof = $csv->eof ();                  # Indicate if last parse or
                                       # getline () hit End Of File
 
 $csv->types(\@t_array);               # Set column types


=head1 DESCRIPTION

Text::CSV_PP has almost same functions of L<Text::CSV_XS> which 
provides facilities for the composition and decomposition of
comma-separated values. As its name suggests, L<Text::CSV_XS>
is a XS module and Text::CSV_PP is a Puer Perl one.


=head1 FUNCTIONS

Most of the documentation was shamelessly copied and replaced
from H.Merijn Brand's Text::CSV_XS (0.32).

=over 4

=item version ()

(Class method) Returns the current module version.

=item new (\%attr)

(Class method) Returns a new instance of Text::CSV_PP. The objects
attributes are described by the (optional) hash ref C<\%attr>.
Currently the following attributes are available:

=over 4

=item eol

An end-of-line string to add to rows, usually C<undef> (nothing,
default), C<"\012"> (Line Feed) or C<"\015\012"> (Carriage Return,
Line Feed). Cannot be longer than 7 (ASCII) characters.

If both C<$/> and C<eol> equal C<"\015">, parsing lines that end on
only a Carriage Return without Line Feed, will be C<parse>d correct.
Line endings, whether in C<$/> or C<eol>, other than C<undef>,
C<"\n">, C<"\r\n">, or C<"\r"> are not (yet) supported for parsing.

=item sep_char

The char used for separating fields, by default a comma. (C<,>).
Limited to a single-byte character, usually in the range from 0x20
(space) to 0x7e (tilde).

The separation character can not be equal to the quote character.
The separation character can not be equal to the escape character.

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

=item quote_char

The char used for quoting fields containing blanks, by default the
double quote character (C<">). A value of undef suppresses
quote chars. (For simple cases only).
Limited to a single-byte character, usually in the range from 0x20
(space) to 0x7e (tilde).

The quote character can not be equal to the separation character.

=item allow_loose_quotes

By default, parsing fields that have C<quote_char> characters inside
an unquoted field, like

 1,foo "bar" baz,42

would result in a parse error. Though it is still bad practice to
allow this format, we cannot help there are some vendors that make
their applications spit out lines styled like this.

=item escape_char

The character used for escaping certain characters inside quoted fields.
Limited to a single-byte character, usually in the range from 0x20
(space) to 0x7e (tilde).

The C<escape_char> defaults to being the literal double-quote mark (C<">)
in other words, the same as the default C<quote_char>. This means that
doubling the quote mark in a field escapes it:

  "foo","bar","Escape ""quote mark"" with two ""quote marks""","baz"

If you change the default quote_char without changing the default
escape_char, the escape_char will still be the quote mark.  If instead 
you want to escape the quote_char by doubling it, you will need to change
the escape_char to be the same as what you changed the quote_char to.

The escape character can not be equal to the separation character.

=item allow_loose_escapes

By default, parsing fields that have C<escape_char> characters that
escape characters that do not need to be escaped, like:

 my $csv = Text::CSV_PP->new ({ escape_char => "\\" });
 $csv->parse (qq{1,"my bar\'s",baz,42});

would result in a parse error. Though it is still bad practice to
allow this format, this option enables you to treat all escape character
sequences equal.

=item binary

If this attribute is TRUE, you may use binary characters in quoted fields,
including line feeds, carriage returns and NULL bytes. (The latter must
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

=item verbatim

This is a quite controversial attribute to set, but it makes hard
things possible.

The basic thought behind this is to tell the parser that the normally
special characters newline (NL) and Carriage Return (CR) will not be
special when this flag is set, and be dealt with as being ordinary
binary characters. This will ease working with data with embedded
newlines.

When C<verbatim> is used with C<getline ()>, getline
auto-chomp's every line.

Imagine a file format like

  M^^Hans^Janssen^Klas 2\n2A^Ja^11-06-2007#\r\n

where, the line ending is a very specific "#\r\n", and the sep_char
is a ^ (caret). None of the fields is quoted, but embedded binary
data is likely to be present. With the specific line ending, that
shouldn't be too hard to detect.

By default, Text::CSV_PP' parse function however is instructed to only
know about "\n" and "\r" to be legal line endings, and so has to deal
with the embedded newline as a real end-of-line, so it can scan the next
line if binary is true, and the newline is inside a quoted field.
With this attribute however, we can tell parse () to parse the line
as if \n is just nothing more than a binary character.

For parse () this means that the parser has no idea about line ending
anymore, and getline () chomps line endings on reading.

=back

To sum it up,

 $csv = Text::CSV_PP->new ();

is equivalent to

 $csv = Text::CSV_PP->new ({
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
     verbatim            => 0,
     });

For all of the above mentioned flags, there is an accessor method
available where you can inquire for the current value, or change
the value

 my $quote = $csv->quote_char;
 $csv->binary (1);

It is unwise to change these settings halfway through writing CSV
data to a stream. If however, you want to create a new stream using
the available CSV object, there is no harm in changing them.

=item combine

 $status = $csv->combine (@columns);

This object function constructs a CSV string from the arguments, returning
success or failure.  Failure can result from lack of arguments or an argument
containing an invalid character.  Upon success, C<string ()> can be called to
retrieve the resultant CSV string.  Upon failure, the value returned by
C<string ()> is undefined and C<error_input ()> can be called to retrieve an
invalid argument.

=item print

 $status = $csv->print ($io, $colref);

Similar to combine, but it expects an array ref as input (not an array!)
and the resulting string is not really created, but immediately written
to the I<$io> object, typically an IO handle or any other object that
offers a I<print> method. Note, this implies that the following is wrong:

 open FILE, ">", "whatever";
 $status = $csv->print (\*FILE, $colref);

The glob C<\*FILE> is not an object, thus it doesn't have a print
method. The solution is to use an IO::File object or to hide the
glob behind an IO::Wrap object. See L<IO::File(3)> and L<IO::Wrap(3)>
for details.

For performance reasons the print method doesn't create a result string.
In particular the I<$csv-E<gt>string ()>, I<$csv-E<gt>status ()>,
I<$csv->fields ()> and I<$csv-E<gt>error_input ()> methods are meaningless
after executing this method.

=item string

 $line = $csv->string ();

This object function returns the input to C<parse ()> or the resultant CSV
string of C<combine ()>, whichever was called more recently.

=item parse

 $status = $csv->parse ($line);

This object function decomposes a CSV string into fields, returning
success or failure.  Failure can result from a lack of argument or the
given CSV string is improperly formatted.  Upon success, C<fields ()> can
be called to retrieve the decomposed fields .  Upon failure, the value
returned by C<fields ()> is undefined and C<error_input ()> can be called
to retrieve the invalid argument.

You may use the I<types ()> method for setting column types. See the
description below.

=item getline

 $colref = $csv->getline ($io);

This is the counterpart to print, like parse is the counterpart to
combine: It reads a row from the IO object $io using $io->getline ()
and parses this row into an array ref. This array ref is returned
by the function or undef for failure.

The I<$csv-E<gt>string ()>, I<$csv-E<gt>fields ()> and I<$csv-E<gt>status ()>
methods are meaningless, again.

=item eof

 $eof = $csv->eof ();

If C<parse ()> or C<getline ()> was used with an IO stream, this
method will return true (1) if the last call hit end of file, otherwise
it will return false (''). This is useful to see the difference between
a failure and end of file.

=item types

 $csv->types (\@tref);

This method is used to force that columns are of a given type. For
example, if you have an integer column, two double columns and a
string column, then you might do a

 $csv->types ([Text::CSV_PP::IV (),
               Text::CSV_PP::NV (),
               Text::CSV_PP::NV (),
               Text::CSV_PP::PV ()]);

Column types are used only for decoding columns, in other words
by the I<parse ()> and I<getline ()> methods.

You can unset column types by doing a

 $csv->types (undef);

or fetch the current type settings with

 $types = $csv->types ();

=over 4

=item IV

Set field type to integer.

=item NV

Set field type to numeric/float.

=item PV

Set field type to string.

=back

=item fields

 @columns = $csv->fields ();

This object function returns the input to C<combine ()> or the resultant
decomposed fields of C<parse ()>, whichever was called more recently.

=item meta_info

 @flags = $csv->meta_info ();

This object function returns the flags of the input to C<combine ()> or
the flags of the resultant decomposed fields of C<parse ()>, whichever
was called more recently.

For each field, a meta_info field will hold flags that tell something about
the field returned by the C<fields ()> method or passed to the C<combine ()>
method. The flags are bitwise-or'd like:

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
contained any byte in the range [\x00-\x08,\x10-\x1F,\x7F-\xFF]

=item status

 $status = $csv->status ();

This object function returns success (or failure) of C<combine ()> or
C<parse ()>, whichever was called more recently.

=item error_input

 $bad_argument = $csv->error_input ();

This object function returns the erroneous argument (if it exists) of
C<combine ()> or C<parse ()>, whichever was called more recently.

=item error_diag

 $csv->error_diag ();
 $error_code  = 0  + $csv->error_diag ();
 $error_str   = "" . $csv->error_diag ();
 ($cde, $str) =      $csv->error_diag ();

If (and only if) an error occured, this function returns the diagnostics
of that error.

If called in void context, it will print the internal error code and the
associated error message to STDERR.

If called in list context, it will return the error code and the error
message in that order.

If called in scalar context, it will return the diagnostics in a single
scalar, a-la $!. It will contain the error code in numeric context, and
the diagnostics message in string context.

To achieve this behavior with CSV_PP, the returned diagnostics is blessed object.


=back


=head1 DIAGNOSTICS

If an error occured, $csv->error_diag () can be used to get more information
on the cause of the failure. Note that for speed reasons, the internal value
is never cleared on success, so using the value returned by error_diag () in
normal cases - when no error occured - may cause unexpected results.

Note: CSV_PP's diagnostics is different from CSV_XS's:

Text::CSV_XS parses csv strings by dividing one character
while Text::CSV_PP by using the regular expressions.
That difference makes the different cause of the failure.
Though diagnostics number 1001, 2027 and 2110 are common with CSV_XS.

Currently these errors are available:

=over 2

=item 1001 "sep_char is equal to quote_char or escape_char"

=item 2027 "EIQ - Quoted field not terminated"

=item 2110 "ECB - Binary character in Combine, binary off"

=item 4000 "ECR - Binary character in field, binary off"

=item 4001 "EIQ - Needless ESC in quoted field"

=item 4002 "EIQ - Unescaped ESC in quoted field"


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
