## 4. Download the data set programatically --------------------------------------------------------------------------------------------------------
library(openxlsx);
census2015 <- read.xlsx('https://www.ahrq.gov/sites/default/files/wysiwyg/sdohchallenge/data/SDOH_ZCTA_2015.xlsx');
ahrqCodebook2015 <- read.xlsx('https://www.ahrq.gov/sites/default/files/wysiwyg/sdohchallenge/data/sdoh_codebook_final.xlsx', sheet ='ZCTA_2015');
sdi <- read.xlsx('https://www.graham-center.org/content/dam/rgc/documents/maps-data-tools/sdi/ACS2015_zctaallvars.xlsx');
library(readr);
outcomes <- read_csv('data/OutcomeData.csv');


## 5. Merge and get into the same format--------------------------------------------------------------------------------------------------------
# check colnames
colnames(census2015)
colnames(ahrqCodebook2015)
colnames(sdi)
colnames(outcomes)

# change colnames
colnames(ahrqCodebook2015);
toupper(colnames(ahrqCodebook2015))
colnames(ahrqCodebook2015) <- toupper(colnames(ahrqCodebook2015))
colnames(sdi);
toupper(colnames(sdi))
colnames(sdi) <- toupper(colnames(sdi))

# finding the overlapped colnames
intersect(colnames(census2015), colnames(ahrqCodebook2015))
intersect(colnames(census2015), colnames(sdi)) # 1
intersect(colnames(census2015), colnames(outcomes)) # 1
intersect(colnames(ahrqCodebook2015), colnames(sdi))
intersect(colnames(ahrqCodebook2015), colnames(outcomes))
intersect( colnames(sdi), colnames(outcomes)) # 1

# merge data
# df <- merge(outcomes, census2015, by = intersect(colnames(census2015), colnames(outcomes)), all.x = TRUE) # intersect not good for this step
# dat0 <- merge(df, sdi, by = intersect(colnames(df), colnames(sdi)), all.x = TRUE)# intersect not good for this step
df <- merge(outcomes, census2015, by = "ZCTA", all.x = TRUE)
dat0 <- merge(df, sdi, by = "ZCTA", all.x = TRUE)


## 6. Prepare analytical data set--------------------------------------------------------------------------------------------------------
set.seed(10)
dat0$sample <- sample(c("train", "test"), nrow(dat0), rep=T, prob = c(1/2, 1/2)) 
dtrain <- subset(dat0, sample == "train")
dtest <- subset(dat0, sample=="test")
nrow(dtrain)
nrow(dtest)

# save data
saveRDS(dat0, file = "dat0.Rds")
saveRDS(dtrain, file = "dtrain.Rds")
saveRDS(dtest, file = "dtest.Rds")

## Run the whole scripts-------------------------------------------------------------------------------
# source("C:/Users/niej/Desktop/2021-08-23 Certificate in Biomedical Data Science/TSCI 5230 Analytical Programming for Biomedical Data Science/FA21TSCI5230_project/data.R", echo=TRUE)
