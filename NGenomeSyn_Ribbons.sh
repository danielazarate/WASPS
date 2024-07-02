# Workflow for whole-genome synteny analysis for Vespula wasps
# NGenomeSyn
# Author: Daniela Zarate, PhD.
__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__. 

# Installation

  git clone https://github.com/hewm2008/NGenomeSyn.git
        cd NGenomeSyn;	chmod -R 755 bin/*
        ./bin/NGenomeSyn  -h 
__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__. 

# Working directory for German:
/bigdata/brelsfordlab/glagu001/biogeo_podz/syn/alan/

# Working directory for Daniela:
/rhome/danielaz/bigdata/wasps/synteny_ribbons

__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__. 

# Files needed:

# .len files for each species wich has three columns:
# 1. The chromosome name
# 2. The start of the chromosome. 
# 3. The length of the chromosome. 

# .link files for links between each spcies, has 6 columns:
# 1. Chromosome number (chr01)
# 2. Marker position start 
# 3. Marker position end (+1 for SNP) 
# 4. Chromosome number (just number)
# 5. Linkage map centimorgan start 
# 6. Linkage map centimorgan end (+1 for SNP)

__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__. 


# Input = in.conf

##################################### global parameters ###########################################

SetParaFor = global

GenomeInfoFile1=v.pen.len ## V.pen.linkage.map.len
GenomeInfoFile2=v.con.len  ## V.con.linkage.map.len
GenomeInfoFile3=v.vid.len ## V.vid.linkage.map.len

LinkFileRef2VsRef1 =               # link file between V.pen vs V. con
LinkFileRef2VsRef3 =               # link file between V. pen vs V.vid        
LinkFileRef3VsRe1 =                # link file between V. con vs V. vid

####### Format (chrA StartA EndA chrB StartB End ...other parameters)
## Note: link files could be occurred multiple times


################################ Figure #########################################################

######## Dovetail-Genome1 ########
SetParaFor = Genome1  ## GenomeALL/GenomeX

ChrNameShow=0         ## show the chr name of this genomes
GenomeName=Dovetail assembly

######## fglacv1.1-Genome2 ########
SetParaFor = Genome2  ## GenomeALL/GenomeX 

ChrNameShow=0         ## show the chr name of this genomes    
GenomeName=Rescaffolded
SpeRegionFile=grey20_centromeres.bed      ## centromeres highlighted
SpeRegionWidthRatio=1.25 ##

######## lg-Genome3 ########
SetParaFor = Genome3  ## GenomeALL/GenomeX 

ChrNameShow=1         ## show the chr name of this genomes
ChrNameColor=pink    ## chr name color to pink
ChrNameShiftY=30
GenomeName=Linkage Map

############# adjust link setting##########
SetParaFor=LinkALL           ### LinkALL/LinkX  setting for link X
StyleUpDown=UpDown        ## UpDown DownUp UpUp DownDown line


__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__. 


# Run NgenomeSyn:

NGenomeSyn  -InConf  test.conf -OutPut test

# Just run one chr at a time 
./NGenomeSyn-1.41/bin/NGenomeSyn -InConf  three.conf -OutPut test

Warning : at Link File ./vcon_vpen.link can't find the chr Scaffold25 at the  the 1 Genome File info
Warning : at Link File ./vcon_vpen.link can't find the chr Scaffold25 at the  the 1 Genome File info
Warning : at Link File ./vcon_vpen.link can't find the chr Scaffold25 at the  the 1 Genome File info
Warning : at Link File ./vcon_vpen.link can't find the chr Scaffold25 at the  the 1 Genome File info
Warning : at Link File ./vcon_vpen.link can't find the chr Scaffold25 at the  the 1 Genome File info
Warning : at Link File ./vcon_vpen.link can't find the chr Scaffold25 at the  the 1 Genome File info


__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__. 


# Visualization:

long_format <- function(x) {
  # Create all combinations of colony, caste, haplotype
  distinct_colony <- unique(x$colony)
  distinct_caste <- unique(x$caste)
  distinct_haplotype <- unique(x$haplotype)
 
  all_combinations <- expand.grid(colony = distinct_colony, caste = distinct_caste, haplotype = distinct_haplotype)
  # Merge with the original data to obtain counts
  merged_data <- all_combinations %>%
  left_join(file, by = c("colony", "caste", "haplotype")) %>%
  mutate(haplotype_count = ifelse(is.na(haplotype_count), 0, haplotype_count))
 
 
  num_rows_merged <- nrow(merged_data)
 
  total_combinations <- length(distinct_colony) * length(distinct_caste) * length(distinct_haplotype)
 
  # Check if all combinations were made
  if (num_rows_merged == total_combinations) {
    print("All combinations were made successfully.")
  } else {
    print("Some combinations are missing.")
  }
 
  print(merged_data)
}

__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__. 

