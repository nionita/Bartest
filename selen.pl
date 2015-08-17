#!/usr/bin/env perl

# Select engines based on a rating file (.rating.txt)
# Rules:
# - black listed engines (.inactive.txt) are ignored
# - at least one rated engine will be selected
# - non-rated engines will be selected as long as there are free places
# - rated engines with upper rate limit less then highest lower rate limit of the
# best rated engine will be selected only at the end, just to complete the free places
# - all other rated engines (means: not black listed, rated enough) are selected randomly
# with a probability corresponding to a score (see below)

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
my $suplow;

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
	my $elower = $elo - $delta;
	$suplow = $elower if (!defined $suplow || $suplow < $elower);
	$lastelo = $elo;
}
close RATING;

die "No engines found in the rating file!\n" if !defined $lastelo;

# Collect the known engines (binaries):
chdir $edir || die "Can't chdir to $edir: $!\n";

my (@good, @bad, @unrated);
my @files = glob "Barbarossa-*";
my $lowest;
my $ina = 0;

for my $eng (@files) {
	next if !-f $eng || !-x $eng;	# must be executable file
	$eng =~ /^Barbarossa-(\d+\.\d+\.\d+)-(.+)$/;
	my ($ver, $short) = ($1, $2);
	next if (!defined $ver || !defined $short);
	print "$eng: " if $verbose;
	if ($inactive{$short}) {	# ignore inactive ones
		++$ina;
		print "- inactive\n" if $verbose;
		next;
	}
	if (exists $ratings{$short}) {
		my ($elo, $delta) = @{$ratings{$short}};
		$lowest = $elo if (!defined $lowest || $elo < $lowest);
		my $eupper = $elo + $delta;
		if ($eupper >= $suplow) {
			push @good, $short;
			print "- good\n" if $verbose;
		} else {
			push @bad, $short;
			print "- bad\n" if $verbose;
		}
	} else {
		push @unrated, $short;
		print "- unrated\n" if $verbose;
	}
}

print("We have " . scalar(@unrated) . " unrated, "
	. scalar(@good) . " good, " . scalar(@bad) . " bad and "
	. $ina . " inactive engines\n")
	if $verbose;

my $elocorr = 1 - $lowest;	# 1 is for the lowest existing, not ignored, rated engine

# "Normalize" the elo (last engine on 1) and compute the product
# elo * delta, which will give the base for the probability
# to participate in the next tournament
print "Normalize elo, probability factors:\n" if $verbose;
#for my $eng (keys %ratings) {
for my $eng (@good, @bad) {
	my ($elo, $delta) = @{$ratings{$eng}};
	my $celo = $elo + $elocorr;
	my $fact = $delta * $celo;
	$ratings{$eng} = $fact;
	print "$eng: $elo -> $celo * $delta = $fact\n" if $verbose;
}

# Now we choose the engines to run next tournament
# We always take at least one rated engine in the list, to have
# a good connected rated list next time,
# then fill as many unrated engines as possible (at random),
# then and further rated "good" engines based on probabilities,
# and then (if needed) rated "bad" engines based on probabilities
my $toselect = $nengs;

# Unrated:
my $cunrated = scalar @unrated;
$cunrated = $toselect - 1 if $cunrated >= $toselect;
$toselect = $toselect - $cunrated;

# Good:
my $cgood = scalar @good;
$cgood = $toselect if $cgood > $toselect;
$toselect = $toselect - $cgood;

# Bad:
my $cbad = scalar @bad;
$cbad = $toselect if $cbad > $toselect;
$toselect = $toselect - $cbad;

print "Not enough engines to select, wanted: $nengs, missing: $toselect\n" if $toselect > 0;

my $i = 0;
my %choosen;
if ($cunrated > 0) {
	print "Choose $cunrated unrated\n" if $verbose;
	if ($cunrated < scalar @unrated) {
		while ($i < $cunrated) {
			++$i;
			my @tochoose = grep { !exists $choosen{$_} } @unrated;
			my @opts = map { [$_, 1] } @tochoose;
			my $eng = choose_random(\@opts);
			$choosen{$eng} = 1;
		}
	} else {
		for my $eng (@unrated) {
			$choosen{$eng} = 1;
		}
	}
}

print "Choose $cgood good rated\n" if $verbose;
if ($cgood < scalar @good) {
	$i = 0;
	while ($i < $cgood) {
		++$i;
		my @tochoose = grep { !exists $choosen{$_} } @good;
		my @opts = map { [$_, $ratings{$_}] } @tochoose;
		my $eng = choose_random(\@opts);
		$choosen{$eng} = 1;
	}
} else {
	for my $eng (@good) {
		$choosen{$eng} = 1;
	}
}

if ($cbad > 0) {
	print "Choose $cbad bad rated\n" if $verbose;
	if ($cbad < scalar @bad) {
		$i = 0;
		while ($i < $cbad) {
			++$i;
			my @tochoose = grep { !exists $choosen{$_} } @bad;
			my @opts = map { [$_, $ratings{$_}] } @tochoose;
			my $eng = choose_random(\@opts);
			$choosen{$eng} = 1;
		}
	} else {
		for my $eng (@bad) {
			$choosen{$eng} = 1;
		}
	}
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
