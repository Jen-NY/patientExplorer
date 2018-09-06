# Test data was generated from ICGC studies available at the COSMIC page.


## WES
# The list of mutations found in the ICGC(CLLE-ES) study was downloaded from https://cancer.sanger.ac.uk/cosmic/study/overview?study_id=340 (2018-09-05)

cll <- read.csv("/Users/huellein/Documents/project/cll/script/patientExplorer/Study_genesWed Sep  5 09_31_07 2018.csv")
head(cll)

# How many mutations do we find per sample?
nrow(cll)/length(unique(cll$Sample.Name))

cll$Sample.Name <- NULL


# We find on average 27 mutations per sample. I will randomy select between 12 and 55 rows from the variant list and add count and allele frequency data.

# Sample 1
no_of_mutations_range <- 12:55
no_of_mutations <- sample(no_of_mutations_range, 1)
randomNo <- sample(1:nrow(cll), no_of_mutations)

s1 <- cll[randomNo,]
s1$counts_control <- sample(1:200, nrow(s1))
s1$counts_tumor <- sample(1:200, nrow(s1))
s1$VAF <- round(s1$counts_tumor/(s1$counts_control + s1$counts_tumor), 2)
s1$Sample <- "16S0001"

# Sample 2
no_of_mutations_range <- 12:55
no_of_mutations <- sample(no_of_mutations_range, 1)
randomNo <- sample(1:nrow(cll), no_of_mutations)

s2 <- cll[randomNo,]
s2$counts_control <- sample(1:200, nrow(s2))
s2$counts_tumor <- sample(1:200, nrow(s2))
s2$VAF <- round(s2$counts_tumor/(s2$counts_control + s2$counts_tumor), 2)
s2$Sample <- "16S0002"

s <- rbind(s1, s2)

write.csv(s, "wes_random_data.csv", row.names = FALSE)


## Breast carcinoma
brca <- read.csv("/Users/huellein/Documents/project/cll/script/patientExplorer/Study_genes_BRCA-ICGC_Wed Sep  5 11_39_17 2018.csv")
head(brca)

# Count how many variants were found per patient
nrow(brca) / length(unique(brca$Sample.Name))

# 16S0008
no_of_mutations_range <- 80:200
no_of_mutations <- sample(no_of_mutations_range, 1)
randomNo <- sample(1:nrow(brca), no_of_mutations)

b1 <- brca[randomNo,]
b1$counts_control <- sample(100:600, nrow(b1))
b1$counts_tumor <- sample(100:600, nrow(b1))
b1$VAF <- round(b1$counts_tumor/(b1$counts_control + b1$counts_tumor), 2)
b1$Sample <- "16S0008"

# 17S0003
no_of_mutations_range <- 80:200
no_of_mutations <- sample(no_of_mutations_range, 1)
randomNo <- sample(1:nrow(brca), no_of_mutations)

b2 <- brca[randomNo,]
b2$counts_control <- sample(100:600, nrow(b2))
b2$counts_tumor <- sample(100:600, nrow(b2))
b2$VAF <- round(b2$counts_tumor/(b2$counts_control + b2$counts_tumor), 2)
b2$Sample <- "17S0003"

# 17S0011
no_of_mutations_range <- 80:200
no_of_mutations <- sample(no_of_mutations_range, 1)
randomNo <- sample(1:nrow(brca), no_of_mutations)

b3 <- brca[randomNo,]
b3$counts_control <- sample(100:600, nrow(b3))
b3$counts_tumor <- sample(100:600, nrow(b3))
b3$VAF <- round(b3$counts_tumor/(b3$counts_control + b3$counts_tumor), 2)
b3$Sample <- "17S0011"

b <- rbind(b1, b2, b3)

write.csv(b, "ampliconSeq_random_data.csv", row.names = FALSE)
