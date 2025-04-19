#!/usr/bin/perl
use strict;
use warnings;
use POSIX;
use Digest::MD5;
use Getopt::Long;
use Cwd qw(abs_path);
use Fcntl qw(:flock SEEK_END);
use POSIX qw(ceil :sys_wait_h);
use File::Path qw(remove_tree);
use Time::HiRes qw(time usleep);

# Get OS name
my $os_name = $^O;

# Turn on autoflush
$|++;

# Max number of forks to use locally
my $max_forks = get_free_cpus();

# MrBayes block which will be used for each run
my $mb_block;

# Where this script is located
my $script_path = abs_path($0);

# Directory script was called from
my $init_dir = abs_path(".");

# Where the script was called from
my $initial_directory = $ENV{PWD};

# General script settings
my $no_forks;

# Allow for reusing info from an old run
my $input_is_dir = 0;

# Allow user to specify mbsum output as input for the script
my $input_is_mbsum = 0;

# How the script was called
my $invocation = "perl bucky.pl @ARGV";

# Name of output directory
my $project_name = "bucky-".int(time());

# BUCKy settings
my $alpha = 1;
my $ngen = 1000000;

my @unlink;

# Read commandline settings 
GetOptions(
    "no-forks"          => \$no_forks,
    "alpha|a=s"         => \$alpha,
    "ngen|n=i"          => \$ngen,
    "no-mbsum|s"        => \$input_is_mbsum,
    "n-threads|T=i"     => \$max_forks,
    "out-dir|o=s"       => \$project_name,
    "help|h"            => sub { print &help; exit(0); },
    "usage"             => sub { print &usage; exit(0); },
);

# Force single process if no_forks is set
$max_forks = 1 if $no_forks;

# Get paths to required executables
my $bucky = check_path_for_exec("bucky");
my $mbsum = check_path_for_exec("mbsum") if (!$input_is_mbsum);

# Check that BUCKy version >= 1.4.4
check_bucky_version($bucky);

my $archive = shift(@ARGV);

# Some error checking
die "You must specify an archive file.\n\n", &usage if (!defined($archive));
die "Could not locate '$archive', perhaps you made a typo.\n" if (!-e $archive);
die "Invalid alpha for BUCKy specified, input must be a float or 'infinity'.\n" if ($alpha !~ /(^inf(inity)?)|(^\d+(\.\d+)?$)/i);

# Input is a previous run directory, reuse information
$input_is_dir++ if (-d $archive);

print "\nScript was called as follows:\n$invocation\n\n";

my $archive_root;
my $archive_root_no_ext;
if (!$input_is_dir) {
    # Clean run with no prior output
    # Extract name information from input file
    if ($input_is_mbsum) {
        ($archive_root = $archive) =~ s/.*\/(.*)/$1/;
        ($archive_root_no_ext = $archive) =~ s/(.*\/)?(.*)(\.tar(\.gz)?$)|(\.tgz$)/$2/;
    }
    else {
        ($archive_root = $archive) =~ s/.*\/(.*)/$1/;
        ($archive_root_no_ext = $archive) =~ s/(.*\/)?(.*)(\.mb\.tar(\.gz)?$)|(\.mb\.tgz$)/$2/;
    }
    die "Could not determine archive root name, did you specify the proper input?\n" if (!defined($archive_root_no_ext));

    # Initialize working directory
    mkdir($project_name) || die "Could not create '$project_name'$!.\n" if (!-e $project_name);

    my $archive_abs_path = abs_path($archive);
    run_cmd("ln -s $archive_abs_path $project_name/$archive_root") if (! -e "$project_name/$archive_root");
}
else {
    # Prior output available, set relevant variables
    $project_name = $archive;
    my @contents = glob("$project_name/*");

    # Determine the archive name by looking for a symlink
    my $found_name = 0;
    foreach my $file (@contents) {
        if (-l $file) {
            $file =~ s/\Q$project_name\E\///;
            $archive = $file;
            $found_name = 1;
        }
    }
    die "Could not locate archive in '$project_name'.\n" if (!$found_name);

    # Extract name information from input file
    if ($input_is_mbsum) {
        ($archive_root = $archive) =~ s/.*\/(.*)/$1/;
        ($archive_root_no_ext = $archive) =~ s/(.*\/)?(.*)(\.tar(\.gz)?$)|(\.tgz$)/$2/;
    }
    else {
        ($archive_root = $archive) =~ s/.*\/(.*)/$1/;
        ($archive_root_no_ext = $archive) =~ s/(.*\/)?(.*)(\.mb\.tar(\.gz)?$)|(\.mb\.tgz$)/$2/;
    }
    die "Could not determine archive root name, is your input file properly named?\n" if (!defined($archive_root_no_ext));
}

# The name of the output archive
my $mbsum_archive = "$archive_root_no_ext.mbsum.tar.gz";
my $bucky_archive = "$archive_root_no_ext.BUCKy.tar";
my $quartet_output = "$archive_root_no_ext.CFs.csv";

$mbsum_archive = $archive if ($input_is_mbsum);

chdir($project_name);

# Change how Ctrl+C is interpreted to allow for clean up
$SIG{'INT'} = 'INT_handler';

# Define and initialize directories
my $mb_out_dir = "mb-out/";
my $mb_sum_dir = "mb-sum/";

mkdir($mb_out_dir) or die "Could not create '$mb_out_dir': $!.\n" if (!-e $mb_out_dir);
mkdir($mb_sum_dir) or die "Could not create '$mb_sum_dir': $!.\n" if (!-e $mb_sum_dir);

# Check if completed quartets from a previous run exist
my %complete_quartets;
if (-e $bucky_archive && -e $quartet_output) {
    print "Archive containing completed quartets found for this dataset found in '$bucky_archive'.\n";
    print "Completed quartets within in this archive will be removed from the job queue.\n\n";

    # See which quartets in the tarball are complete
    chomp(my @complete_quartets_tarball = `tar tf '$init_dir/$project_name/$bucky_archive'`);

    # Add quartets to a hash for easier lookup
    my %complete_quartets_tarball;
    foreach my $complete_quartet (@complete_quartets_tarball) {
        $complete_quartets_tarball{$complete_quartet}++;
    }

    # Load csv into memory
    open(my $quartet_output_file, "<", "$init_dir/$project_name/$quartet_output");
    (my @quartet_info = <$quartet_output_file>);
    close($quartet_output_file);

    # Remove header line
    my $header = shift(@quartet_info);

    # Rewrite csv to include only quartets also contained in tarball
    open($quartet_output_file, ">", "$init_dir/$project_name/$quartet_output");
    print {$quartet_output_file} $header;
    foreach my $quartet (@quartet_info) {
        my ($taxon1, $taxon2, $taxon3, $taxon4) = split(",", $quartet);
        my $quartet_name = "$taxon1--$taxon2--$taxon3--$taxon4";
        my $quartet_name_tarball = $quartet_name.".tar.gz";

        if (exists($complete_quartets_tarball{$quartet_name_tarball})) {
            $complete_quartets{$quartet_name}++;
            print {$quartet_output_file} $quartet;
        }
    }
    close($quartet_output_file);
}

my @taxa;
my @genes;
if ($input_is_mbsum) {
    # Unarchive input genes
    chomp(@genes = `tar xvf '$init_dir/$project_name/$archive' -C $mb_sum_dir 2>&1`);
    @genes = map { s/x //; $_ } @genes if ($os_name eq "darwin");

    die "No genes found in '$archive'.\n" if (!@genes);

    # Move into MrBayes output directory
    chdir($mb_sum_dir);

    # Remove subdirectories that may have been a part of the tarball
    my @dirs;
    my @gene_roots;
    foreach my $gene (@genes) {
        (my $gene_root = $gene) =~ s/.*\///;
        next if ($gene eq $gene_root);

        # Move mbsum files so they are no longer in subdirectories
        if (!-d $gene) {
            system("mv '$gene' '$gene_root'");
            push(@gene_roots, $gene_root);
        }
        else {
            push(@dirs, $gene);
        }
    }
    @genes = @gene_roots if (@gene_roots);

    # Remove any subdirectories that may have been in the input
    foreach my $dir (@dirs) {
        remove_tree($dir);
    }

    # Parse taxa present in each gene, determine which are shared across all genes
    my %taxa;
    foreach my $gene (@genes) {
        my @taxa = @{parse_mbsum_taxa($gene)};

        # Count taxa present
        foreach my $taxon (@taxa) {
            $taxa{$taxon}++;
        }
    }

    # Add taxa present in all genes to analysis
    foreach my $taxon (keys %taxa) {
        if ($taxa{$taxon} == scalar(@genes)) {
            push(@taxa, $taxon);
        }
    }

    # Archive genes
    system("tar", "czf", $mbsum_archive, @genes);
}
else {
    # Unarchive input genes
    chomp(@genes = `tar xvf '$init_dir/$archive' -C '$mb_out_dir' 2>&1`);
    @genes = map { s/x //; $_ } @genes if ($os_name eq "darwin");

    # Move into MrBayes output directory
    chdir($mb_out_dir);

    # Check that each gene has a log file
    my %taxa;
    foreach my $gene (@genes) {
        # Unzip a single gene
        chomp(my @mb_files = `tar xvf '$gene' 2>&1`);
        @mb_files = map { s/x //; $_ } @mb_files if ($os_name eq "darwin");

        # Locate the log file output by MrBayes
        my $log_file_name;
        foreach my $file (@mb_files) {
            if ($file =~ /\.log$/) {
                $log_file_name = $file;
                last;
            }
        }
        die "Could not locate log file for '$gene'.\n" if (!defined($log_file_name));

        # Parse log file for run information
        my $mb_log = parse_mb_log($log_file_name);

        # Check for taxa present
        my @taxa = @{$mb_log->{TAXA}};
        foreach my $taxon (@taxa) {
            $taxa{$taxon}++;
        }

        # Clean up
        unlink(@mb_files);
    }

    # Add taxa present in all genes to analysis
    foreach my $taxon (keys %taxa) {
        if ($taxa{$taxon} == scalar(@genes)) {
            push(@taxa, $taxon);
        }
    }
}

# Create list of possible quartets
my @quartets = combine(\@taxa, 4);

my $original_size = scalar(@quartets);

# Remove completed quartets
if (%complete_quartets) {
    foreach my $index (reverse(0 .. $#quartets)) {
        my $quartet = $quartets[$index];
        $quartet = join("--", @{$quartet});
        if (exists($complete_quartets{$quartet})) {
            splice(@quartets, $index, 1);
        }
    }
}

print "Found ".scalar(@taxa)." taxa shared across all genes in this archive, ".scalar(@quartets).
      " of $original_size possible quartets will be run using output from ".scalar(@genes)." total genes.\n";

# Go back to working directory
chdir("..");

# Determine whether or not we need to run mbsum on the specified input
my $should_summarize = 1;
if (-e $mbsum_archive && $input_is_dir && !$input_is_mbsum) {
    chomp(my @sums = `tar tf '$mbsum_archive'`) || die "Something appears to be wrong with '$mbsum_archive'.\n";

    # Check that each gene has actually been summarized, if not redo the summaries
    if (scalar(@sums) != scalar(@genes)) {
        unlink($mbsum_archive);
    }
    else {
        $should_summarize = 0;
    }
}

$should_summarize = 0 if ($input_is_mbsum);

# Summarize MrBayes output if needed
if ($should_summarize) {
    # Run mbsum on each gene
    print "Summarizing MrBayes output for ".scalar(@genes)." genes.\n";

    my @pids;
    foreach my $gene (@genes) {
        # Wait until a CPU is available
        until(okay_to_run(\@pids)) {};

        my $pid;
        until (defined($pid)) { $pid = fork(); usleep(30000); }

        # The child fork
        if ($pid == 0) {
            run_mbsum($gene);
            exit(0);
        }
        else {
            push(@pids, $pid);
        }
    }

    # Wait for all summaries to finish
    foreach my $pid (@pids) {
        waitpid($pid, 0);
    }
    undef(@pids);

    # Remove directory storing mb output
    remove_tree($mb_out_dir);

    # Archive and zip mb summaries
    chdir($mb_sum_dir);
    system("tar", "czf", $mbsum_archive, glob("*.sum"));
    system("cp", $mbsum_archive, "..");
    chdir("..");
}

die "\nAll quartets have already been completed.\n\n" if (!@quartets);

# Initialize the output files
if (!-e $quartet_output) {
    open(my $quartet_output_file, ">", $quartet_output);
    print {$quartet_output_file} "taxon1,taxon2,taxon3,taxon4,CF12_34,CF12_34_lo,CF12_34_hi,CF13_24,CF13_24_lo,CF13_24_hi,CF14_23,CF14_23_lo,CF14_23_hi,ngenes\n";
    close($quartet_output_file);
}

if (!-e $bucky_archive) {
    system("tar", "cf", $bucky_archive, []);
}

# Move into mbsum directory for processing
chdir($mb_sum_dir);

# Process quartets
my $time = time();
my $num_digits = get_num_digits({'NUMBER' => scalar(@quartets)});
my $complete_count = 0;

# Process quartets in parallel batches
my @pids;
my %complete_queue;
my $complete_queue_max_size = 100;

foreach my $quartet_idx (0 .. $#quartets) {
    # Wait until a CPU is available
    until(okay_to_run(\@pids)) {};
    
    my $pid;
    until (defined($pid)) { $pid = fork(); usleep(30000); }
    
    # Child process - run BUCKy analysis
    if ($pid == 0) {
        my @quartet = @{$quartets[$quartet_idx]};
        my $quartet_name = join("--", @quartet);
        
        # Create prune tree file contents required for BUCKy
        my $count = 0;
        my $prune_tree_output = "translate\n";
        foreach my $member (@quartet) {
            $count++;
            $prune_tree_output .= " $count $member";
            if ($count == 4) {
                $prune_tree_output .= ";\n";
            }
            else {
                $prune_tree_output .= ",\n";
            }
        }
        
        # Write prune tree file
        my $prune_file_path = "$quartet_name-prune.txt";
        open(my $prune_file, ">", $prune_file_path);
        print {$prune_file} $prune_tree_output;
        close($prune_file);
        
        my @bucky_args;
        # Invocation changes if we want to use a prior of infinity
        if ($alpha =~ /(^inf(inity)?)/i) {
            @bucky_args = ('--use-independence-prior', '-n', $ngen);
        }
        else {
            @bucky_args = ('-a', $alpha, '-n', $ngen);
        }
        
        # Run BUCKy
        system($bucky, @bucky_args, '-cf', 0, '-o', $quartet_name, '-p', $prune_file_path, glob("*.sum"));
        
        # Archive results
        my $quartet_archive_name = "$quartet_name.tar.gz";
        system("tar", "czf", $quartet_archive_name, glob("$quartet_name*"));
        
        # Get results info
        my $num_genes = get_used_genes("$quartet_name.out");
        my $split_info = parse_concordance_output("$quartet_name.concordance", $num_genes);
        
        # Write to output file with locking
        open(my $quartet_output_file, ">>", "../$quartet_output");
        flock($quartet_output_file, LOCK_EX);
        seek($quartet_output_file, 0, SEEK_END);
        print {$quartet_output_file} "$split_info\n";
        flock($quartet_output_file, LOCK_UN);
        close($quartet_output_file);
        
        # Add to archive with locking
        open(my $bucky_archive_file, "<", "../$bucky_archive");
        flock($bucky_archive_file, LOCK_EX);
        system("tar", "rf", "../$bucky_archive", $quartet_archive_name);
        flock($bucky_archive_file, LOCK_UN);
        close($bucky_archive_file);
        
        # Clean up
        unlink(glob("$quartet_name*"));
        
        exit(0);
    }
    else {
        push(@pids, $pid);
    }
    
    # Periodically report progress
    my $completed = 0;
    foreach my $pid (@pids) {
        my $wait = waitpid($pid, WNOHANG);
        $completed++ if $wait > 0;
    }
    $complete_count += $completed;
    
    # Print progress every few quartets
    if ($quartet_idx % 5 == 0 || $quartet_idx == $#quartets) {
        printf("  Analyses complete: %".$num_digits."d/%d.\r", $complete_count, scalar(@quartets));
    }
}

# Wait for all analyses to complete
foreach my $pid (@pids) {
    waitpid($pid, 0);
    $complete_count++;
    printf("  Analyses complete: %".$num_digits."d/%d.\r", $complete_count, scalar(@quartets));
}

print "\n  All analyses completed.\n";
print "Total execution time: ", sec2human(time() - $time), ".\n\n";

# Clean up
rmdir("$initial_directory/$project_name/$mb_sum_dir");

# Function definitions from the original script
sub parse_mb_log {
    my $log_file_name = shift;

    # Open the specified mb log file and parse useful information from it
    my @taxa;
    my $ngen;
    my $nruns;
    my $burnin;
    my $burninfrac;
    my $samplefreq;
    open(my $log_file, "<", $log_file_name);
    while (my $line = <$log_file>) {
        if ($line =~ /Taxon\s+\d+ -> (\S+)/) {
            push(@taxa, $1);
        }
        elsif ($line =~ /Setting number of runs to (\d+)/) {
            $nruns = $1;
        }
        elsif ($line =~ /Setting burnin fraction to (\S+)/) {
            $burninfrac = $1;
        }
        elsif ($line =~ /Setting chain burn-in to (\d+)/) {
            $burnin = $1;
        }
        elsif ($line =~ /Setting sample frequency to (\d+)/) {
            $samplefreq = $1;
        }
        elsif ($line =~ /Setting number of generations to (\d+)/) {
            $ngen = $1;
        }
    }
    close($log_file);

    if (defined($burnin)) {
        return {'SAMPLEFREQ' => $samplefreq, 'NRUNS' => $nruns, 'BURNIN' => $burnin, 'TAXA' => \@taxa};
    }
    else {
        return {'NGEN' => $ngen, 'NRUNS' => $nruns, 'BURNINFRAC' => $burninfrac,
                'SAMPLEFREQ' => $samplefreq, 'TAXA' => \@taxa};
    }
}

sub parse_mbsum_taxa {
    my $mbsum_file_name = shift;

    # Open the specified mbsum file and parse its taxa list
    my @taxa;
    my $in_translate_block = 0;
    open(my $mbsum_file, "<", $mbsum_file_name);
    while (my $line = <$mbsum_file>) {
        $in_translate_block++ if ($line =~ /translate/);

        if ($in_translate_block == 1 && $line =~ /\d+\s+([^,;]+)/) {
            push(@taxa, $1);
        }
    }
    close($mbsum_file);

    # Check if there were multiple translate blocks in the file which is indicative of an error with the creation of the file
    die "Something is amiss with '$mbsum_file_name', multiple translate blocks ($in_translate_block) were detected when there should only be one.\n" if ($in_translate_block > 1);

    # Check that we actually parsed something, otherwise input is improperly formatted/not mbsum output
    die "No taxa parsed for file '$mbsum_file_name', does '$archive' actually contain mbsum output?.\n" if (!@taxa);

    return \@taxa;
}

sub run_mbsum {
    my $tarball = shift;

    # Unzip specified tarball
    chomp(my @mb_files = `tar xvf '$mb_out_dir$tarball' -C '$mb_out_dir' 2>&1`);
    @mb_files = map { s/x //; $_ } @mb_files if ($os_name eq "darwin");

    # Determine name for this partition's log file
    my $log_file_name;
    foreach my $file (@mb_files) {
        if ($file =~ /\.log$/) {
            $log_file_name = $file;
            last;
        }
    }
    die "Could not locate log file for '$tarball'.\n" if (!defined($log_file_name));

    # Parse log file
    my $mb = parse_mb_log("$mb_out_dir$log_file_name");

    (my $gene_name = $tarball) =~ s/\.nex\.tar\.gz//;

    # Determine number of trees mbsum should remove from each file
    my $trim;
    if ($mb->{BURNIN}) {
        $trim = $mb->{BURNIN} + 1;
    }
    else {
        $trim = ((($mb->{NGEN} / $mb->{SAMPLEFREQ}) * $mb->{NRUNS} * $mb->{BURNINFRAC}) / $mb->{NRUNS}) + 1;
    }

    # Summarize gene's tree files
    system("$mbsum '$mb_out_dir$gene_name.'*.t -n $trim -o '$mb_sum_dir$gene_name.sum' >/dev/null 2>&1");

    # Clean up extracted files
    chdir($mb_out_dir);
    unlink(@mb_files);
}

sub okay_to_run {
    my $pids = shift;

    # Free up a CPU by sleeping for 10 ms
    usleep(10000);

    my $current_forks = scalar(@{$pids});
    foreach my $index (reverse(0 .. $current_forks - 1)) {
        next if ($index < 0);

        my $pid = @{$pids}[$index];
        my $wait = waitpid($pid, WNOHANG);

        # Successfully reaped child
        if ($wait > 0) {
            $current_forks--;
            splice(@{$pids}, $index, 1);
        }
    }

    return ($current_forks < $max_forks);
}

sub get_used_genes {
    my $file_name = shift;

    # Number of genes actually used by BUCKy
    my $num_genes;

    # Open up the BUCKy output file
    open(my $bucky_out, "<", $file_name);
    while (my $line = <$bucky_out>) {
        if ($line =~ /Read (\d+) genes with a total of/) {
            $num_genes = $1;
            last;
        }
    }
    close($bucky_out);

    die "Error determining number of genes used by BUCKy ($file_name).\n" if (!defined($num_genes));

    return $num_genes;
}

sub parse_concordance_output {
    my ($file_name, $ngenes) = @_;

    my @taxa;
    my %splits;

    # Open up the specified output file
    open(my $concordance_file, "<", $file_name);

    my $split;
    my $in_translate;
    my $in_all_splits;
    while (my $line = <$concordance_file>) {

        # Parse the translate table
        if ($in_translate) {
            if ($line =~ /\d+ (.*)([,;])/) {
                my $taxon = $1;
                my $line_end = $2;
                push(@taxa, $taxon);

                $in_translate = 0 if ($line_end eq ';');
            }
        }

        # Parse the split information
        if ($in_all_splits) {

            # Set the split we are parsing information from
            if ($line =~ /^(\{\S+\})/) {
                my $current_split = $1;
                if ($current_split eq "{1,4|2,3}") {
                    $split = "14|23";
                }
                elsif ($current_split eq "{1,3|2,4}") {
                    $split = "13|24";
                }
                elsif ($current_split eq "{1,2|3,4}") {
                    $split = "12|34";
                }
            }

            # Parse mean number of loci for split
            if ($line =~ /=\s+(\S+) \(number of loci\)/) {
                $splits{$split}->{"CF"} = $1 / $ngenes;
            }

            # Parse 95% confidence interval
            if ($line =~ /95% CI for CF = \((\d+),(\d+)\)/) {
                $splits{$split}->{"95%_CI_LO"} = ($1 / $ngenes);
                $splits{$split}->{"95%_CI_HI"} = ($2 / $ngenes);
            }
        }

        $in_translate++ if ($line =~ /^translate/);
        $in_all_splits++ if ($line =~ /^All Splits:/);
    }

    # Concat taxa names together
    my $return = join(",", @taxa);
    $return .= ",";

    # Concat split proportions with their 95% CI to return
    if (exists($splits{"12|34"})) {
        $return .= $splits{"12|34"}->{"CF"}.",".$splits{"12|34"}->{"95%_CI_LO"}.",".$splits{"12|34"}->{"95%_CI_HI"}.",";
    }
    else {
        $return .= "0,0,0,";
    }

    if (exists($splits{"13|24"})) {
        $return .= $splits{"13|24"}->{"CF"}.",".$splits{"13|24"}->{"95%_CI_LO"}.",".$splits{"13|24"}->{"95%_CI_HI"}.",";
    }
    else {
        $return .= "0,0,0,";
    }

    if (exists($splits{"14|23"})) {
        $return .= $splits{"14|23"}->{"CF"}.",".$splits{"14|23"}->{"95%_CI_LO"}.",".$splits{"14|23"}->{"95%_CI_HI"};
    }
    else {
        $return .= "0,0,0";
    }

    # Append number of genes used
    $return .= ",$ngenes";

    return $return;
}

sub INT_handler {
	#dump_quartets(\%complete_queue);

	unlink(@unlink);

	# Kill ssh process(es) spawned by this script
	foreach my $pid (@pids) {
		#kill(-9, $pid);
		#kill(15, $pid);
		kill(1, $pid);
	}

	# Move into gene directory
	chdir("$initial_directory/$project_name");

	rmdir($mb_out_dir);

	# Try to delete directory once per second for five seconds, if it can't be deleted print an error message
	# I've found this method is necessary for analyses performed on AFS drives
	my $count = 0;
	until (!-e $mb_sum_dir || $count == 5) {
		$count++;

		remove_tree($mb_sum_dir, {error => \my $err});
		sleep(1);
	}
	#logger("Could not clean all files in './$gene_dir/'.") if ($count == 5);
	print "Could not clean all files in './$mb_sum_dir/'.\n" if ($count == 5);

	exit(0);
}

sub clean_up {
	my $settings = shift;

	my $remove_dirs = $settings->{'DIRS'};
	my $current_dir = getcwd();

#	chdir($alignment_root);
#	unlink(glob($gene_dir."$alignment_name*"));
#	#unlink($server_check_file) if (defined($server_check_file));
#
#	if ($remove_dirs) {
#		rmdir($gene_dir);
#	}
	chdir($current_dir);
}

sub get_num_digits {
	my $settings = shift;

	my $number = $settings->{'NUMBER'};

	my $digits = 1;
	while (floor($number / 10) != 0) {
		$number = floor($number / 10);
		$digits++;
	}

	return $digits;
}

sub sec2human {
	my $secs = shift;

	# Constants
	my $secs_in_min = 60;
	my $secs_in_hour = 60 * 60;
	my $secs_in_day = 24 * 60 * 60;

	$secs = int($secs);

	return "0 seconds" if (!$secs);

	# Calculate units of time
	my $days = int($secs / $secs_in_day);
	my $hours = ($secs / $secs_in_hour) % 24;
	my $mins = ($secs / $secs_in_min) % 60;
	$secs = $secs % 60;

	# Format return nicely
	my $time;
	if ($days) {
		$time .= ($days != 1) ? "$days days, " : "$days day, ";
	}
	if ($hours) {
		$time .= ($hours != 1) ? "$hours hours, " : "$hours hour, ";
	}
	if ($mins) {
		$time .= ($mins != 1) ? "$mins minutes, " : "$mins minute, ";
	}
	if ($secs) {
		$time .= ($secs != 1) ? "$secs seconds " : "$secs second ";
	}
	else {
		# Remove comma
		chop($time);
	}
	chop($time);

	return $time;
}

sub get_free_cpus {

	my $os_name = $^O;

	# Returns a two-member array containing CPU usage observed by top,
	# top is run twice as its first output is usually inaccurate
	my @percent_free_cpu;
	if ($os_name eq "darwin") {
		# Mac OS
		chomp(@percent_free_cpu = `top -i 1 -l 2 | grep "CPU usage"`);
	}
	else {
		# Linux
		chomp(@percent_free_cpu = `top -b -n2 -d0.05 | grep "Cpu(s)"`);
	}

	my $percent_free_cpu = pop(@percent_free_cpu);

	if ($os_name eq "darwin") {
		# Mac OS
		$percent_free_cpu =~ s/.*?(\d+\.\d+)%\s+id.*/$1/;
	}
	else {
		# linux
		$percent_free_cpu =~ s/.*?(\d+\.\d)\s*%?ni,\s*(\d+\.\d)\s*%?id.*/$1 + $2/; # also includes %nice as free
		$percent_free_cpu = eval($percent_free_cpu);
	}

	my $total_cpus;
	if ($os_name eq "darwin") {
		# Mac OS
		$total_cpus = `sysctl -n hw.ncpu`;
	}
	else {
		# linux
		$total_cpus = `grep --count 'cpu' /proc/stat` - 1;
	}

	my $free_cpus = ceil($total_cpus * $percent_free_cpu / 100);

	if ($free_cpus == 0 || $free_cpus !~ /^\d+$/) {
		$free_cpus = 1; # assume that at least one cpu can be used
	}

	return $free_cpus;
}

sub run_cmd {
	my $command = shift;

	my $return = system($command);

	if ($return) {
		logger("'$command' died with error: '$return'.\n");
		#kill(2, $parent_pid);
		exit(0);
	}
}

sub check_path_for_exec {
	my $exec = shift;

	my $path = $ENV{PATH}.":."; # include current directory as well
	my @path_dirs = split(":", $path);

	my $exec_path;
	foreach my $dir (@path_dirs) {
		$dir .= "/" if ($dir !~ /\/$/);
		$exec_path = abs_path($dir.$exec) if (-e $dir.$exec && -x $dir.$exec && !-d $dir.$exec);
	}

	die "Could not find the following executable: '$exec'. This script requires this program in your path.\n" if (!defined($exec_path));
	return $exec_path;
}

# I grabbed this from StackOverflow so that's why its style is different #DontFixWhatIsntBroken:
# https://stackoverflow.com/questions/10299961/in-perl-how-can-i-generate-all-possible-combinations-of-a-list
sub combine {
	my ($list, $n) = @_;
	die "Insufficient list members" if ($n > @$list);

	return map [$_], @$list if ($n <= 1);

	my @comb;

	for (my $i = 0; $i+$n <= @$list; ++$i) {
		my $val  = $list->[$i];
		my @rest = @$list[$i + 1 .. $#$list];
		push(@comb, [$val, @$_]) for combine(\@rest, $n - 1);
	}

	return @comb;
}

sub check_bucky_version {
	my $bucky = shift;

	print "\nChecking for BUCKy version >= 1.4.4...\n";

	# Run BUCKy with --version and extract version info
	chomp(my @version_info = grep { /BUCKy version/ } `$bucky --version`);
	my $version_info = shift(@version_info);

	die "  Could not determine BUCKy version.\n" if (!defined($version_info));

	# Get the actual version number
	my $version;
	if ($version_info =~ /BUCKy\s+version\s+([^\s|,]+)/) {
		$version = $1;
	}
	die "  Could not determine BUCKy version.\n" if (!defined($version_info));

	# Version testing
	#my @versions = qw/2.1b 1.2.1000 1 0.9.8 2.3 1.4.5 1.4.3 1.4 1.500.2 1.4.4 1.4.4.1/;
	#foreach my $version (@versions) {

	print "  BUCKy version: $version.\n";

	# Split version number based on period delimiters
	my @version_parts = split(/\./, $version);

	# Future proofing if letters are ever used (we won't ever care about them)
	@version_parts = map { s/[a-zA-Z]+//g; $_ } @version_parts;
	die "  Error determining BUCKy version.\n" if (!@version_parts);

	# Check that version is >= 1.4.4
	if (defined($version_parts[0]) && $version_parts[0] > 1) {
		print "  BUCKy version check passed.\n";
		return;
	}
	elsif ((defined($version_parts[0]) && $version_parts[0] == 1) && (defined($version_parts[1]) && $version_parts[1] > 4)) {
		print "  BUCKy version check passed.\n";
		return;
	}
	elsif (((defined($version_parts[0]) && $version_parts[0] == 1) && (defined($version_parts[1]) && $version_parts[1] == 4))
		  && defined($version_parts[2]) && $version_parts[2] >= 4) {
		print "  BUCKy version check passed.\n";
		return;
	}
	else {
		die "  BUCKy version check failed, update to version >= 1.4.4.\n";
	}

	#print "\n";
	#}
}

sub usage {
	return "Usage: bucky.pl [MRBAYES TARBALL]\n";
}

sub help {
print <<EOF;
@{[usage()]}
Parallel execution of BUCKy on all possible quartets in a given alignment

  -a, --alpha            value of alpha to use when running BUCKy, use "infinity" for infinity (default: 1)
  -n, --ngen             number of generations to run BUCKy MCMC chain (default: 1000000 generations)
  -o, --out-dir          name of the directory to store output files in (default: "bucky-" + Unix time of script invocation)
  -T, --n-threads        the number of forks ALL hosts running analyses can use concurrently (default: current number of free CPUs)
  -s, --no-mbsum         informs the script that the input is a tarball containing output already parsed by mbsum
  --machine-file         file name containing hosts to ssh onto and perform analyses on, passwordless login MUST be enabled
                         for each host specified in this file
  --port                 specifies the port to utilize on the server (Default: 10003)
  -h, --help             display this help and exit
  --usage                display proper script invocation format

Examples:
  perl bucky.pl align.mb.tar --machine-file hosts.txt     runs BUCKy using computers specified in hosts.txt using MrBayes output
                                                          stored in align.mb.tar

Mail bug reports and suggestions to <noah.stenz.github\@gmail.com>
EOF
exit(0);
}
