# This test is from Text-CSV_XS-0.23

use Test::More tests => 23;
use Text::CSV_PP;

my $csv = Text::CSV_PP->new();


ok(!$csv->combine())  # fail - missing argument
    or print "Missing argument, but no failure\n";
ok(!$csv->combine('abc', "def\n", 'ghi'))  # fail - bad character
    or print "Bad character, but no failure\n";
ok($csv->combine('') && ($csv->string eq q()))  # succeed
    or printf("Expected %s, got %s\n", q(), $csv->string());
ok($csv->combine('', ' ') && ($csv->string eq q(," ")))  # succeed
    or printf("Expected %s, got %s\n", q("",""), $csv->string());
ok($csv->combine('', 'I said, "Hi!"', '') &&
     ($csv->string eq q(,"I said, ""Hi!""",)))  # succeed
    or printf("Expected %s, got %s\n", q("","I said, ""Hi!""",""),
	      $csv->string());
ok($csv->combine('"', 'abc') && ($csv->string eq q("""",abc)))  # succeed
    or printf("Expected %s, got %s\n", q("""","abc"), $csv->string());
ok($csv->combine(',') && ($csv->string eq q(",")))  # succeed
    or printf("Expected %s, got %s\n", q("""","abc"), $csv->string());
ok($csv->combine('abc', '"') && ($csv->string eq q(abc,"""")))  # succeed
    or printf("Expected %s, got %s\n", q("abc",""""), $csv->string());
ok($csv->combine('abc', 'def', 'ghi', 'j,k') &&
     ($csv->string eq q(abc,def,ghi,"j,k")))  # succeed
    or printf("Expected %s, got %s\n", q(abc,def,ghi,"j,k"),
	      $csv->string());
ok($csv->combine("abc\tdef", 'ghi') &&
     ($csv->string eq qq("abc\tdef",ghi)))  # succeed
    or printf("Expected %s, got %s\n", qq("abc\tdef","ghi"),
	      $csv->string());
ok(!$csv->parse())
    or print "Missing argument, but no failure\n";
ok(!$csv->parse('"abc'))
    or print("Missing closing double-quote, but no failure\n");
ok(!$csv->parse('ab"c'))
    or print("Double quote outside of double-quotes, but no failure.\n");
ok(!$csv->parse('"ab"c"'))
    or print("Bad character sequence, but no failure.\n");
ok(!$csv->parse(qq("abc\nc")))
    or print("Bad character, but no failure.\n");
ok(!$csv->status())
    or print("Wrong status\n");
ok($csv->parse(q(",")) and ($csv->fields())[0] eq ',')  # success
    or printf("Expected field 0 to be ',', got %s\n", ($csv->fields())[0]);
ok($csv->parse(qq("","I said,\t""Hi!""","")));
ok(($csv->fields())[0] eq '')
    or printf("Expected field 0 to be '', got %s\n",
	      ($csv->fields())[0]);
ok(($csv->fields())[1] eq qq(I said,\t"Hi!"))
    or printf("Expected field 1 to be '%s', got %s\n",
              qq(I said,\t"Hi!"), ($csv->fields())[1]);
ok(($csv->fields())[2] eq '')
    or printf("Expected field 2 to be '', got %s\n",
	      ($csv->fields())[2]);
ok($csv->status())
    or print("Wrong status\n");


ok($csv->combine('', 2, 3.4, 'a', 'a b')
     && ($csv->string eq q(,2,3.4,a,"a b")))  # succeed
    or printf("Expected %s, got %s\n", q(""), $csv->string());


