#!/usr/bin/perl
use strict;
use warnings;

my @lines;
my %labels;
my $pc = 0;
while(<>) {
    s/^\s*//;
    s/\s*$//;
    chomp;
    my ($insn, $comment) = split /\s*;\s*/, $_, 2;
    while ($insn =~ s/^([^:]*):\s*//) {
	my $label = $1;
	die "Unusable numeric label \"$label\"" if $label =~ /^\d+$/;
	die "Unusable non-word label \"\Q$label\E\"" if $label !~ /^\w+$/;
	die "Register name \"$label\" used as label" if $label =~ /^([a-h]|pc)$/i;
	# In principle, labels could alias register names as long as
	# they're only used as the first argument of a jump, and this
	# would be trivial to implement, but it'd probably be a footgun.
	$labels{$label} = $pc;
	$comment = defined $comment ? " $comment" : "";
	$comment = "[<-$label]$comment";
    }
    my ($opcode, $args) = split /\s+/, $insn, 2;
    my @args;
    @args = split /\s*,\s*/, $args if defined $args;
    push @lines, [$opcode, @args, $comment];
    ++$pc if defined $opcode;
}
for my $line (@lines) {
    my ($opcode, @args) = @$line;
    my $comment = pop @args;
    for my $i (0..$#args) {
	if ($args[$i] =~ /^\w+$/ && $args[$i] !~ /^\d+$/ && ($args[$i] !~ /^(?:[a-h]|pc)$/i)) {
	    # Labels allowed as immediates, in case of operations on PC or saved PC values.
	    # Labels not yet allowed as data addresses.
	    my $label = $args[$i];
	    my $addr = $labels{$label};
	    $comment = defined $comment ? " $comment" : "";
	    $comment = "[$addr->$label]$comment";
	    $args[$i] = $addr;
	}
    }
    if (defined $opcode) {
	my $oa = "$opcode ".join(",",@args);
	$oa .= " " x (20 - length $oa) if defined $comment;
	print $oa;
    }
    print "; $comment" if defined $comment;
    print "\n";
}
