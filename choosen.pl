#!/usr/bin/env perl

use strict;
use warnings;

my $nengs = $ARGV[0];	# parameter: number of engines
my $verbose = 0;
$verbose = 1 if defined $ARGV[1] && $ARGV[1] eq "-v";

my $base  = $ENV{HOME};
my $rfile = "$base/Tour/.rating.txt";
my $ifile = "$base/Tour/.inactive.txt";
my $edir  = "$base/Engines";

my %ratings;
my %inactive;
my $lastelo;

# Engines listed in inactive will be ignored
if (open INACT, $ifile) {
	while (my $line = <INACT>) {
		chomp $line;
		$inactive{$line}++;	# one engine per line
	}
	close INACT;
}

# Collect the data about the engine ratings from the rating file
open RATING, $rfile || die "Can't open rating file $rfile: $!\n";
while (my $line = <RATING>) {
	chomp $line;
	my $plc = substr $line, 0, 4;
	next if $plc !~ /^ +\d+$/;
	$line = substr $line, 5;
	my ($eng, $elo, $delta, $rest) = split /\s+/, $line, 5;
	$ratings{$eng} = [ $elo, $delta ];
	$lastelo = $elo;
}
close RATING;

die "No engines found in the rating file!\n" if !defined $lastelo;

my $elocorr = 1 - $lastelo;

# "Normalize" the elo (last engine on 1) and compute the product
# elo * delta, which will give the "probability" to participate
# in the next tournament
for my $eng (keys %ratings) {
	my ($elo, $delta) = @{$ratings{$eng}};
	my $celo = $elo + $elocorr;
	my $fact = $delta * $celo;
	$ratings{$eng} = $fact;
}

# Collect the known engines (binaries):
chdir $edir || die "Can't chdir to $edir: $!\n";

my (@rated, @unrated);
my @engs = glob "Barbarossa-*";

for my $eng (@engs) {
	next if !-f $eng || !-x $eng;	# must be executable file
	$eng =~ /^Barbarossa-(\d+\.\d+\.\d+)-(.+)$/;
	my ($ver, $short) = ($1, $2);
	next if $inactive{$short};	# ignore inactive ones
	if (exists $ratings{$short}) {
		push @rated, $short;
	} else {
		push @unrated, $short;
	}
}

print("We have " . scalar(@unrated) . " unrated, "
	. scalar(@rated) . " rated and " . scalar(keys %inactive)
	. " inactive engines\n")
	if $verbose;

# Now we choose the engines to run next tournament
# We always take at least one rated engine in the list, to have
# a good connected rated list next time,
# then fill as many unrated engines as possible (at random),
# then and further rated engines based on probabilities
my $cunrated = scalar @unrated;
$cunrated = $nengs - 1 if $cunrated >= $nengs;
my $crated = $nengs - $cunrated;

my %choosen;
print "Choose $cunrated unrated\n" if $verbose;
my $i = 0;
while ($i < $cunrated) {
	++$i;
	my @tochoose = grep { !exists $choosen{$_} } @unrated;
	my @opts = map { [$_, 1] } @tochoose;
	my $eng = choose_random(\@opts);
	$choosen{$eng} = 1;
}

print "Choose $crated rated\n" if $verbose;
$i = 0;
while ($i < $crated) {
	++$i;
	my @tochoose = grep { !exists $choosen{$_} } @rated;
	my @opts = map { [$_, $ratings{$_}] } @tochoose;
	my $eng = choose_random(\@opts);
	$choosen{$eng} = 1;
}

print join(" ", keys(%choosen)), "\n";

# Choose random one position from a list of options
# given the unnormalized probabilities, in form of:
# ((opt1, prob1), (opt2, prob2), ..., (optn, probn))
# Prob parts are positive integer values (negative ones or 0
# will be treated as 1)
sub choose_random {
	#print "In random\n";
	my $options = shift;
	my $sum = 0;
	for my $opt (@{$options}) {
		$opt->[1] = 1 if $opt->[1] <= 0;
		$opt->[1] = $opt->[1] + $sum;
		$sum = $opt->[1];
	}
	#print "Sum = $sum\n";
	my $r = int(rand($sum));
	for my $opt (@{$options}) {
		return $opt->[0] if ($r < $opt->[1]);
	}
}
