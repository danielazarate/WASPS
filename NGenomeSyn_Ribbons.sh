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


# Input = vespula.conf

##################################### global parameters ###########################################

###  # Detailed parameters please see  at the  NGenomeSyn_manual_English.pdf 

SetParaFor = global
########## global and required parameters ######
GenomeInfoFile1=./v.con.len ### set path for Genome1, GenomeInfoFileX, X is the number of Genome
GenomeInfoFile2=./vpen.ref.len ### set path for Genome2, Format(chr start End ...)
GenomeInfoFile3=./v.vid.len
LinkFileRef1VsRef2=./vcon_vpen.link ### link information between Genome1 and Genome2
                                    ### LinkFileRefXXVsRefYY : link info between GenomeXX and GenomeYY
                                    ### Format (chrA StartA EndA chrB StartB EndB ...
LinkFileRef2VsRef3=./vvid_vpen.link

############ canvas and figure[optional] ######
#body=1200                 ### size of canvas with width and height. plot region:  (up/down/left/right)=(55,25,100,120)
#up=55
#down=25
#left=100
#right=120
#CanvasHeightRitao=1.0     ## adjust height of the plot
#CanvasWidthRitao=1.0      ## adjust width of the plot
#NoPng=1                   ## No OutPut the png File


############# adjust genome setting [optional]##########
SetParaFor = Genome1       ## GenomeALL/GenomeX
#ZoomChr=1.0               ## adjust chr length, 1 for equal; >1 for enlarge; <1  for
#RotateChr=30              ## rotate the chr with 30 degrees
#ShiftX=0
#ShiftY=0                  ## move the start of chr to (X,Y)
#MoveToX                   ## MoveToY   ## similar to ShiftX and ShiftY


#ChrWidth=20               ## chr width
#LinkWidth=180             ## link height between this genome and next genome
#ChrSpacing=10             ## spacing width of chr/scaffolds
#NormalizedScale=0         ## custom scale for the geome relative the  default.
#SpeRegionFile=Spe.bed     ## input file for highlighted regions[chr start end key1=value1] in the genome.
#ZoomRegion                ## Zoom the specific Region,format (ZoomRegion=chr2:1000:5000)


#GenomeNameRatio
#GenomeName
## GenomeName  GenomeNameSizeRatio  GenomeNameColor  GenomeNameShiftX GenomeNameShiftY
## ChrNameShow ChrNameShiftX ChrNameShiftY ChrNameSizeRatio ChrNameColor ChrNameRotate
## ShowCoordinates=1     ## Show Coordinates with other para [ScaleNum=10 ScaleUpDown ScaleUnit LabelUnit  LablefontsizeRatio  RotateAxisText NoShowLabel ]


SetParaFor = Genome2       ##

############# adjust link setting [optional]##########
SetParaFor=Link1           ### LinkALL/LinkX  setting for link X
#StyleUpDown=UpDown        ## UpDown DownUp UpUp DownDown line
#Reverse=1                 ## reverse links
#HeightRatio=1.0           ## ratio of link height relative to the default
## other attributes:  fill|stroke|stroke-opacity|fill-opacity|stroke-width

__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__. 


# Run NgenomeSyn:
# NGenomeSyn  -InConf  test.conf -OutPut test

# Just run one chr at a time 
./NGenomeSyn-1.41/bin/NGenomeSyn -InConf  vespula.conf -OutPut vespula.out
