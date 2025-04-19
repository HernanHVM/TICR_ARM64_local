# TICR_ARM64_local
Two modified Perl scripts using Claude Sonnet to run the TICR pipeline by Stenz (https://github.com/nstenz/TICR) for SNaQ and PhyloNetworks on Apple M chips with the ARM64 architecture, locally.

## Use Rosetta2 and Homebrew to install an appropriate Perl version to run scripts:

## Appropriate Perl installation
time arch -x86_64 /usr/local/Homebrew/bin/brew install perl

## Run MrBayes on each locus
### modifiqué el mb.pl para brincar la parte de buscar un servidor externo y en vez usar compu local (yes, I speak Spanish natively)
time arch -x86_64 /opt/local/bin/perl ../TICR/scripts/mb_hvm.pl nexus.tar.gz -m mb-block1.txt -o mb-output

## check MrBayes convergence of the runs with:
time arch -x86_64 /opt/local/bin/perl ../TICR/scripts/mb_hvm.pl mb-output -c 0.05

## run Bucky to obtain concordance factors (CFs)
time arch -x86_64 /opt/local/bin/perl ../TICR/scripts/bucky_hvm_claude.pl mb-output/nexus.mb.tar -o bucky-output

