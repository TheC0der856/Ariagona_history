library(vcfR)
library(adegenet)
library(RLDNe)

# load filtered vcf file
vcf <- read.vcfR("C:/Users/Gronefeld/Desktop/history_of_genflow/Stairwayplots/populations.snps.vcf")
#source("C:/Users/Gronefeld/Desktop/history_of_genflow/filter_for_Ne.R")


gen <- vcfR2genind(vcf_oneSNP)
#indNames(gen)

# eine Population & eine Generation

# El Hierro: 
# east, 2010: T130, T131, T132, T139, T140, T141, T142, T143, T163, T164, T165, T166
# west, 2023: SG026, SG027, SG028, SG029, SG075, SG076, SG077, SG078, SG079, SG080, SG081, SG082, SG083, SG084, SG085, SG086, SG087, SG088, SG117, SG118, SG119, SG120, SG121, SG122

# La Gomera: 
# north, 2024: SG180, SG181, SG182, SG183, SG184, SG185, SG186, SG187, SG188, SG189, SG190
# center, 2010: T123, T124, T125, T127, T128, T144, T146, T147, T148, T150, T151, T152, T153, T154, T155, T156, T157, T158, T159, T160, T162, T190, T194, T195, T206, T218, T219, T220
# east, 2015: T227, T228, T229, T230, T231, T241, T242, T243, T244, T247, T248, T249, T252, T253

# Tenerife: 
# west, 2023: SG001, SG002, SG003, SG004, SG005, SG032, SG033, SG034, SG035, SG036, SG037, SG038, SG039, SG040, SG041, SG042, SG043, SG051, SG052, SG053, SG054, SG055, SG056, SG057, SG058, SG059, SG060, SG062, SG065, SG066, SG067, SG068, SG069, SG070, SG071, SG072, SG073, SG074, SG123, SG124, SG126, SG127, SG128, SG129, SG130, SG131, SG132, SG133, SG134, SG135, SG136, SG138, SG140, SG141, SG142, SG144, SG145, SG146, SG147, SG148
# east Lagunetas, 2023: SG044, SG045, SG102, SG103, SG104, SG106, SG112, SG113, SG114, SG115, SG116, SG149, SG150, SG151, SG152, SG153, SG154, SG155, SG156, SG157, SG158, SG159, SG162, SG163, SG165, SG166, SG167, SG169, SG170, SG171, SG174, SG175, SG176, SG177, SG178, SG179 
# east Cementerio, 2024: SG191, SG192, SG193, SG208, SG209, SG210, SG211, SG212
# east Cementerio, 2023: SG030, SG031, SG046, SG047, SG048, SG049, SG050
# east westAnaga, 2024: SG194, SG195, SG196, SG197, SG198, SG199, SG200, SG201, SG202, SG203, SG204, SG205, SG206, SG207

# IDs
Hierro_east2010_IDs <- c("T130", "T131", "T132", "T139", "T140", "T141", "T142", "T143", "T163", "T164", "T165", "T166")
Hierro_west2023_IDs <- c("SG026", "SG027", "SG028", "SG029", "SG075", "SG076", "SG077", "SG078", "SG079", "SG080", "SG081", "SG082", "SG083", "SG084", "SG085", "SG086", "SG087", "SG088", "SG117", "SG118", "SG119", "SG120", "SG121", "SG122")
Gomera_north2023_IDs <- c("SG180", "SG181", "SG182", "SG183", "SG184", "SG185", "SG186", "SG187", "SG188", "SG189", "SG190")
Gomera_center2010_IDs <- c("T123", "T124", "T125", "T127", "T128", "T144", "T146", "T147", "T148", "T150", "T151", "T152", "T153", "T154", "T155", "T156", "T157", "T158", "T159", "T160", "T162", "T190", "T194", "T195", "T206", "T218", "T219", "T220")
Gomera_east2015 <- c("T227", "T228", "T229", "T230", "T231", "T241", "T242", "T243", "T244", "T247", "T248", "T249", "T252", "T253")
Tenerife_west2023_IDs <- c("SG001", "SG002", "SG003", "SG004", "SG005", "SG032", "SG033", "SG034", "SG035", "SG036", "SG037", "SG038", "SG039", "SG040", "SG041", "SG042", "SG043", "SG051", "SG052", "SG053", "SG054", "SG055", "SG056", "SG057", "SG058", "SG059", "SG060", "SG062", "SG065", "SG066", "SG067", "SG068", "SG069", "SG070", "SG071", "SG072", "SG073", "SG074", "SG123", "SG124", "SG126", "SG127", "SG128", "SG129", "SG130", "SG131", "SG132", "SG133", "SG134", "SG135", "SG136", "SG138", "SG140", "SG141", "SG142", "SG144", "SG145", "SG146", "SG147", "SG148")
#Tenerife_Lagunetas2023_IDs <- c("SG044", "SG045", "SG102", "SG103", "SG104", "SG106", "SG112", "SG113", "SG114", "SG115", "SG116", "SG149", "SG150", "SG151", "SG152", "SG153", "SG154", "SG155", "SG156", "SG157", "SG158", "SG159", "SG162", "SG163", "SG165", "SG166", "SG167", "SG169", "SG170", "SG171", "SG174", "SG175", "SG176", "SG177", "SG178", "SG179")
#Tenerfie_Cementerio2024_IDs <- c("SG191", "SG192", "SG193", "SG208", "SG209", "SG210", "SG211", "SG212")
Tenerife_eAnaga_Lagunetas2023_IDs <- c("SG044", "SG045", "SG102", "SG103", "SG104", "SG106", "SG112", "SG113", "SG114", "SG115", "SG116", "SG149", "SG150", "SG151", "SG152", "SG153", "SG154", "SG155", "SG156", "SG157", "SG158", "SG159", "SG162", "SG163", "SG165", "SG166", "SG167", "SG169", "SG170", "SG171", "SG174", "SG175", "SG176", "SG177", "SG178", "SG179" , "SG030", "SG031", "SG046", "SG047", "SG048", "SG049", "SG050")
Tenerife_wAnaga2024_IDs <- c("SG194", "SG195", "SG196", "SG197", "SG198", "SG199", "SG200", "SG201", "SG202", "SG203", "SG204", "SG205", "SG206", "SG207")

# Subset genind
groups <- list(
  Hierro_east2010 = Hierro_east2010_IDs,
  Hierro_west2023 = Hierro_west2023_IDs,
  Gomera_north2023 = Gomera_north2023_IDs,
  Gomera_center2010 = Gomera_center2010_IDs,
  Gomera_east2015 = Gomera_east2015,
  Tenerife_west2023 = Tenerife_west2023_IDs,
  #Tenerife_Lagunetas2023 = Tenerife_Lagunetas2023_IDs,
  #Tenerfie_Cementerio2024 = Tenerfie_Cementerio2024_IDs,
  Tenerife_eAnaga_Lagunetas2023 = Tenerife_eAnaga_Lagunetas2023_IDs,
  Tenerife_wAnaga2024 = Tenerife_wAnaga2024_IDs
)
all_ids <- unlist(groups)
gen_subset <- gen[indNames(gen) %in% all_ids, ]
pop_vector <- rep(NA, nInd(gen_subset))
names(pop_vector) <- indNames(gen_subset)
for(pop_name in names(groups)) {
  ids <- groups[[pop_name]]
  pop_vector[indNames(gen_subset) %in% ids] <- pop_name
}
pop(gen_subset) <- as.factor(pop_vector)
# check
#table(pop(gen_subset))


# Format genind to use NeEstimator
gt_mat <- extract.gt(vcf_dp, element = "GT")
ids <- indNames(gen_subset)
gt_sub <- gt_mat[, ids, drop = FALSE]
gt_sub[is.na(gt_sub) | gt_sub == ""] <- "./."
allele_values <- unique(unlist(
  strsplit(
    gsub("\\|", "/", as.vector(gt_sub)),
    "/",
    fixed = TRUE
  )
))
unexpected <- setdiff(allele_values, c("0", "1", "."))
if (length(unexpected) > 0) {
  stop("Unerwartete Allele: ", paste(unexpected, collapse = ", "))
}
allele_matrix <- matrix(
  "0",
  nrow = length(ids),
  ncol = 2 * nrow(gt_sub)
)
for (i in seq_len(nrow(gt_sub))) {
  x <- gsub("\\|", "/", gt_sub[i, ])
  a1 <- sub("^([^/]+).*", "\\1", x)
  a2 <- ifelse(
    grepl("/", x),
    sub("^[^/]+/([^/]+).*$", "\\1", x),
    "."
  )
  recode <- function(a) {
    ifelse(a == "0", "1",
           ifelse(a == "1", "2", "0"))
  }
  allele_matrix[, 2 * i - 1] <- recode(a1)
  allele_matrix[, 2 * i] <- recode(a2)
}
locus_names <- unlist(lapply(
  seq_len(nrow(gt_sub)),
  function(i) {
    c(paste0("Locus_", i, ".A1"),
      paste0("Locus_", i, ".A2"))
  }
))
colnames(allele_matrix) <- locus_names
rownames(allele_matrix) <- ids
pop_vector <- as.character(pop(gen_subset))
names(pop_vector) <- indNames(gen_subset)
geno_table <- data.frame(
  pop = pop_vector[ids],
  ind_id = ids,
  allele_matrix,
  check.names = FALSE,
  stringsAsFactors = FALSE
)
rownames(geno_table) <- NULL


efgl <- readInData(geno_table ,genotypeStart = 3,pedigreeColumn = 1,nameColumn = 2, missingAlleles = "0")
rldne <- exportGenePop_RLDNe(EFGLdata = efgl)
rldne <- create_LDNe_params(rldne)
std_out <- run_LDNe(rldne)
df <- read_LDNeOutFile(rldne)

write.csv(df,
          "C:/Users/Gronefeld/Desktop/history_of_genflow/LDNe_results.csv",
          row.names = FALSE)
