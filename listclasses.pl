#!/usr/bin/perl

use strict;
use warnings;
use File::Find;
use Archive::Zip qw(:ERROR_CODES :CONSTANTS);

sub path_to_class_name {
    (my $path) = @_;
    $path =~ s/\//./g;
    $path =~ s/\.java$//g;
    $path =~ s/\.class$//g;
    return $path;
}

sub process_dir {
    (my $dir) = @_;
    $dir .= "/" if (not $dir =~ /\/$/);
    find(sub { 
        if (/^[A-Z][a-zA-Z0-9]+\.java/) {
            my $path = $File::Find::name;
            $path =~ s/^$dir//;
            print path_to_class_name($path) . "\n";
        }
         }, $dir);
}

sub process_jar {
    (my $jar) = @_;
    my $zip = Archive::Zip->new();

    $zip->read($jar) == AZ_OK or die "Unable to read jar file: $jar";
    my @class_paths = $zip->membersMatching("^([a-z]+/)*[A-Z][a-zA-Z0-9]+\.class");
    foreach my $path (@class_paths) {
        print path_to_class_name($path->fileName()) . "\n";
    }
}

my @cp = split(":", $ARGV[0]);
foreach my $path (@cp) {
    if ($path =~ /.*\.jar/) {
        process_jar($path);
    } else {
        process_dir($path);
    }
}
