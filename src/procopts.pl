#!/usr/bin/env perl

# procopt.pl - generate options info for plines, from options.txt file
#
# Written by Micah Cowan <micah@cowan.name>.
#
# Please feel free to treat the contents of this file as if it were in
# the public domain.

use strict;
use warnings;

use constant STATE_PREAMBLE => 0;
use constant STATE_INIT => 1;
use constant STATE_ACT  => 2;
use constant STATE_DESC => 3;

{
    my $d = {
        infname => "options.txt",
        outfname => "OptBuiltins.hs",
    };

    open $d->{'inf'}, '<', $d->{'infname'} or die $d->{'infname'};
    open $d->{'outf'}, '>', $d->{'outfname'} or die $d->{'outfname'};
    my $inf = $d->{'inf'};
    my $outf = $d->{'outf'};

    my $state = STATE_PREAMBLE;

    &preamblize($d);

    my $line;
    while (defined($line = <$inf>)) {
        if ($state == STATE_PREAMBLE) {
            if ($line =~ /^---$/) {
                $state = STATE_INIT;
                print $outf "builtinOptions = [\n";
            }
            else {
                print $outf $line;
            }
            next;
        }

        if ($state == STATE_DESC) {
            if (&parse_opt_desc($d, $line)) {
                next;
            }
            else {
                &finish_opt($d);
                $state = STATE_INIT;
            }
        }

        if ($state == STATE_INIT) {
            next if $line =~ /^\s*(?:#.*)?$/;

            &parse_opt_line($d, $line);
            $state = STATE_ACT;
        }
        elsif ($state == STATE_ACT) {
            &parse_opt_act($d, $line);
            $state = STATE_DESC;
        }
    }

    &eulogize($d);
}

sub preamblize {
    my $d = shift;
    my $infname = $d->{'infname'};
    my $outf = $d->{'outf'};

    print $outf <<"END";
{-
    DO NOT EDIT THIS FILE.
    This file was automatically generated. Edit the file "$infname" instead.
-}

END
}

sub parse_opt_line {
    my ($d, $line) = @_;

    my ($spc, $opts, $arg) = $line =~ /
        ^
        (\s*)                       # leading whitespace
        (-{1,2}[^\s,]+              # a long or short option
         (?:,\s+    -{1,2}[^\s,]+)*)# ...optionally followed by more,
                                    #    comma-separated
        (?: \s+  ([A-Za-z_-]+))?    # optional argument.
        $
    /x;

    unless (defined $opts) {
        die "Error parsing options on " . $d->{'infname'} . " line $..";
    }

    if (! exists $d->{'indent'}) {
        $d->{'indent'} = $spc;
    }
    elsif ($spc ne $d->{'indent'}) {
        die "Indentation mismatch on " . $d->{'infname'} . " line $..";
    }

    my @opts = split /, */, $opts;
    my (@shopts, @lopts);
    foreach my $opt (@opts) {
        if ($opt =~ s/^--//) {
            push @lopts, $opt;
        }
        else {
            $opt =~ s/^-//;
            unless ($opt =~ /^.$/) {
                die "Error: multi-char short opt \"$opt\" on "
                    . $d->{'infname'} . " line $..";
            }
            push @shopts, $opt;
        }
    }

    chomp $line;
    $line =~ s/^$spc//;
    $d->{'curopt'}{'tag'} = $line;
    $d->{'curopt'}{'arg'} = (defined $arg);
    $d->{'curopt'}{'shopts'} = \@shopts;
    $d->{'curopt'}{'lopts'} = \@lopts;
    &register_opts($d, \@shopts, \@lopts);
}

sub register_opts {
    my ($d, $shopts, $lopts) = @_;
    my $infname = $d->{'infname'};

    foreach my $type (['shopts', $shopts], ['lopts', $lopts]) {
        my ($name, $ref) = @$type;
        foreach my $opt (@$ref) {
            if (exists $d->{$name}{$opt}) {
                die "Option $opt already exists! at $infname line $.";
            }
            $d->{$name}{$opt} = $d->{'curopt'};
        }
    }
}

sub parse_opt_act {
    my ($d, $line) = @_;
    my $indent = $d->{'indent'};
    my $subindent = $d->{'subindent'};
    my $infname = $d->{'infname'};

    chomp $line;
    my ($spc, $act) = $line =~ /
        ^
        (\s*)   # Leading space (should start with previous indent)
        :       # Colon
        (.*)    # Everything after that is the function result of a lookup
                #   on this option.
    /x;


    if (not (defined $spc) && (defined $act)) {
        die "Didn't find action line at $infname line $.";
    }
    elsif (! exists $d->{'subindent'}) {
        # subindent should be space atop existing indent.
        my $indent = $d->{'indent'};
        if ($spc !~ /^$indent\s/) {
            die "Sub-indent doesn't build on previous indentation at"
                . " $infname line $.";
        }
        $d->{'subindent'} = $spc;
    }

    $d->{'curopt'}{'act'} = $act;
}

sub parse_opt_desc {
    my ($d, $line) = @_;
    my $indent = $d->{'indent'};
    my $subindent = $d->{'subindent'};
    my $infname = $d->{'infname'};

    my $legal = 1;

    chomp $line;
    
    if ($line !~ /\S/) {
        # Blank line
        $d->{'curopt'}{'blanks'}++;
    }
    else {

        if ($line =~ s/^$subindent//) {
            # A real text line, possibly following a number of blanks.
            my @blanks = exists $d->{'curopt'}{'blanks'} ?
                ('') x $d->{'curopt'}{'blanks'} :
                ();
            push @{$d->{'curopt'}{'description'}}, (@blanks, $line);
            delete $d->{'curopt'}{'blanks'} if @blanks;
        }
        elsif ($line =~ /^$indent/) {
            $legal = 0;
            delete $d->{'curopt'}{'blanks'} if exists $d->{'curopt'}{'blanks'};
        }
        else {
            die "Couldn't match indentation at $infname line $.";
        }
    }

    return $legal;
}

sub hstring {
    my $str = shift;

    $str =~ s/\\/\\\\/;
    $str =~ s/"/\\"/;
    $str = "\"${str}\"";

    return $str;
}

sub hstrl {
    my $list = shift;

    my $idt = ' ' x 8;
    my $retval =  "${idt}\[\n${idt}";
    local $" = "\n${idt}, ";
    $retval .= "@{[ map { &hstring($_) } @$list ]}";
    $retval .= "\n${idt}]";

    return $retval;
}

sub finish_opt {
    my $d = shift;
    my $c = $d->{'curopt'};
    my $px = "  , ";
    unless ($d->{'first_printed'}) {
        $px = "    ";
        $d->{'first_printed'} = 1;
    }
    my $shopts = $c->{'shopts'};
    my $lopts = &hstrl($c->{'lopts'});
    my $arg = $c->{'arg'} ? 'TakesArg' : 'TakesNoArg';
    my $act = '(' . $c->{'act'} . ')';
    my $tag = &hstring($c->{'tag'});
    my $description = &hstrl($c->{'description'});
    my $outf = $d->{'outf'};
    print $outf <<END;
${px}OptBuiltin
        "@{$shopts}"
$lopts
        $arg
        $act
        $tag
$description
END

    delete $d->{'curopt'};
}

sub eulogize {
    my $d = shift;
    my $outf = $d->{'outf'};
    &finish_opt($d);

    print $outf "  ]\n";
}
