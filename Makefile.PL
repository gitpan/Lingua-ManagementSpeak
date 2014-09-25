#!/usr/bin/env perl
use strict;
use warnings;
use ExtUtils::MakeMaker;

WriteMakefile(
    'NAME'             => 'Lingua::ManagementSpeak',
    'AUTHOR'           => 'Gryphon Shafer <gryphon@cpan.org>',
    'VERSION_FROM'     => 'lib/Lingua/ManagementSpeak.pm',
    'ABSTRACT_FROM'    => 'lib/Lingua/ManagementSpeak.pm',
    'PL_FILES'         => {},
    'MIN_PERL_VERSION' => '5.010',
    'PREREQ_PM'        => {
        'Test::Most' => 0,
    },
    'dist'  => { 'COMPRESS' => 'gzip -9f', 'SUFFIX' => 'gz' },
    'clean' => { 'FILES'    => 'Lingua-ManagementSpeak-*' },
    ( $ExtUtils::MakeMaker::VERSION >= 6.3002 ? ( 'LICENSE' => 'perl' ) : () ),
    ( eval { ExtUtils::MakeMaker->VERSION(6.46) } ? ( 'META_MERGE' => {
        'meta-spec' => { version => 2 },
        'resources' => {
            'repository' => {
                'type' => 'git',
                'url'  => 'https://github.com/gryphonshafer/Lingua-ManagementSpeak.git',
                'web'  => 'https://github.com/gryphonshafer/Lingua-ManagementSpeak',
            },
        } } ) : ()
    ),
);
