#!/usr/bin/perl

map{s/^\s*//}(@lines=grep{/\S/}<>);

for($s=0; $s<=$#lines; ++$s) {
    $line=$lines[$s];
    $targ=$#plines+1;
    while($line=~s/^(\S+):\s*//) {
	$labl{$1}=$targ;
	push @{$rlabl[$targ]},$1;
    }
    die "Empty line after label removal!\n" unless $line=~/\S/;
    if ($line=~s/^=(\S*)//) {
	for $labl (@{$rlabl[$targ]}) {
#	    print STDERR "antpp: repointing $labl at $1\n";
	    $labl{$labl}=\$labl{$1};
	}
	$rlabl[$targ]=[];
	next;
    }
    
    $x=($line=~s/^(\d+)x\s*//)?$1:1;
    push @plines,(([$line,$s+1])x$x)
}

sub resolv { (ref $_[0])?resolv(${$_[0]}):$_[0] }

print map { ++$n; ($_,$s)=@$_; s/(?<!\S)\.(?!\S)/$n/g;
	    s/(?<!\S)\.(\S+)/(exists $labl{$1})?resolv($labl{$1})
	    :((warn "Undefined label \"$1\" on input line $s!\n"),$n)/ge;
	    $_ } @plines;

