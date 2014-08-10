use strict;
use warnings;
use ExtUtils::MakeMaker;

WriteMakefile(
    NAME             => 'Lingua::ManagementSpeak',
    AUTHOR           => 'Gryphon Shafer <gryphon@cpan.org>',
    VERSION_FROM     => 'lib/Lingua/ManagementSpeak.pm',
    ABSTRACT_FROM    => 'lib/Lingua/ManagementSpeak.pm',
    PL_FILES         => {},
    MIN_PERL_VERSION => '5.10.1',
    PREREQ_PM        => {
        'Test::Most' => 0,
    },
    dist  => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
    clean => { FILES => 'Lingua-ManagementSpeak-*' },
    ( $ExtUtils::MakeMaker::VERSION >= 6.3002 ? ('LICENSE'=> 'perl') : () ),
);
