use 5.006;
use ExtUtils::MakeMaker;

WriteMakefile(
    NAME              => 'Lingua::ManagementSpeak',
    VERSION_FROM      => 'lib/Lingua/ManagementSpeak.pm',
    PREREQ_PM         => {}, # e.g., Module::Name => 1.1
    ($] >= 5.005 ?     ## Add these new keywords supported since 5.005
      (ABSTRACT_FROM  => 'lib/Lingua/ManagementSpeak.pm',
       AUTHOR         => 'Gryphon Shafer <gryphon@gryphonshafer.com>') : ()),
);
