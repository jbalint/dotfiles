#!/usr/bin/env perl

use File::Basename;

$templatefile = "$ENV{'HOME'}/.vim/tools/unknown.wiki";

sub process_file {
	my $filename = $_[0];
	my $dir = dirname($filename);
	open (FILE, $filename) or die "cannot open $filename!";
	while (<>) {
		while (m/\b(\w*[A-Z]\w*[a-z0-9]+\w*[A-Z]\w*)\b/g) {
			my $t = $1;
			my $targetfile = $t . ".wiki";
			if (-f "$dir/$targetfile") {
				# print "$t\t$targetfile\t1\n";
				$tags{$t} = "$t\t$targetfile\t1\n";
			} else {
				# TODO: bla.wiki -> unknown.wiki
				# print "$1\tbla.wiki\t1\n";
				$tags{$t} = "$t\t$templatefile\t1\n";
			}
		}
	}
}

%tags = ( );

print STDERR "@ARGV\n";

foreach $f (@ARGV) {
	print STDERR "$f\n";
}
foreach $f (@ARGV) {
	process_file($f);
}

foreach $k (sort (keys %tags)) {
	print $tags{$k};
}
