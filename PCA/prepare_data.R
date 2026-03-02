# load file
vcf <- read.vcfR("D:/Innsbruck/Innsbruck2/stairwayplot/data/all.islands.DP10.maxmiss0.8.thin100.recode.vcf")
head(vcf)               #check
vcf@fix[1:10,1:5]       #check

# transform for R (adegenet)
OGgenlight <- vcfR2genlight(vcf)

# add island information
islands <- read.table(
  text = readLines("D:/Innsbruck/Innsbruck2/stairwayplot/data/popmap_all_islands_t_h_g.txt"),
  sep = "\t",
  header = FALSE,
  stringsAsFactors = FALSE
) 
colnames(islands) <- c("ID", "Group")  
pop(OGgenlight) <- islands$Group[match(indNames(OGgenlight), islands$ID)]
pop(OGgenlight)
# quality control
#source("quality_control.R")

# 3 islands -> 3 genlights 
genlight_t <- OGgenlight[pop(OGgenlight) == "t", ]
genlight_h <- OGgenlight[pop(OGgenlight) == "h", ]
genlight_g <- OGgenlight[pop(OGgenlight) == "g", ]

# add pop info
pop <- read.table(
  text = readLines("D:/Innsbruck/Innsbruck2/stairwayplot/data/ID_Pop_noOutgroup.txt"),
  sep = "\t",
  header = FALSE,
  stringsAsFactors = FALSE
) 
colnames(pop) <- c("ID", "Group") 

pop(genlight_t) <- pop$Group[match(indNames(genlight_t), pop$ID)]
pop(genlight_h) <- pop$Group[match(indNames(genlight_h), pop$ID)]
pop(genlight_g) <- pop$Group[match(indNames(genlight_g), pop$ID)]
