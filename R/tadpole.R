options(digits = 3)
library(knitr)
library(ADNIMERGE)
library(ggplot2)
library(dplyr)
library(caret)
library(Hmisc)
library(gridExtra)
library(RColorBrewer)
source("https://adni.bitbucket.io/myfunctions.R")
theme_set(theme_bw())

### TADPOLE ----
###
### Each row represents data for one particular visit of a subject, and each
### column represents a feature or measurement (commonly called biomarker) from
### the subject at that particular visit.
###
### The first columns in the spreadsheet contain unique identifiers: RID (roster
### ID) uniquely identifies every subject, VISCODE (visit code) is the timepoint
### when the visit takes place (bl is baseline or month 0, m06 is month 6, etc
### ..), SITE represents the site ID where the visit took place. Other important
### columns are: EXAMDATE represents the date of the clinical examination, AGE
### is their age at baseline visit, PTEDUCAT represents their total years of
### education.
###
### Here is a list of biomarkers we suggest participants unfamiliar with ADNI
### data to start with:
### * The main measures to be predicted: DX, ADAS13, Ventricles
### * Cognitive tests: CDRSB, ADAS11, MMSE, RAVLT_immediate
### * MRI measures: Hippocampus, WholeBrain, Entorhinal, MidTemp
### * PET measures: FDG, AV45
### * CSF measures: ABETA_UPENNBIOMK9_04_19_17  (amyloid-beta level in CSF),
###   TAU_UPENNBIOMK9_04_19_17 (tau level), PTAU_UPENNBIOMK9_04_19_17
###   (phosphorylated tau level)
### * Risk factors: APOE4, AGE
###
### Other important biomarkers that participants can consider are the various
### MRI, PET and DTI measures for the hippocampus, entorhinal cortex, temporal
### and parietal lobe structures. Use the dictionary file
### (TADPOLE_D1_D2_Dict.csv) and search for keywords such as "hippocampus" or
### "hippocampal" to find the necessary columns. For example, column
### ST44CV_UCSFFSL_02_01_16_UCSFFSL51ALL_08_01_16 represents the volume of the
### left hippocampus. If desired, the measures for the left and right structures
### can be averaged together.
###
### >65 risk increase every 5 yo
### female more than male
###
###
### TADPOLE datasets include three main types of structural MRI markers of
### atrophy:
### 1. ROI volumes
### 2. ROI cortical thicknesses
### 3. ROI surface areas
### where an ROI (region of interest) is a 3D sub-region of the brain such as
### the inferior temporal lobe. Obtaining these structural MRI markers from the
### images is a long and complicated process. This involves registering (i.e.
### aligning) the MRI images with each other and performing a segmentation of
### the main brain structures using an atlas-based technique. More information
### can be found on the Freesurfer website:
### https://surfer.nmr.mgh.harvard.edu/fswiki/LongitudinalProcessing
###
### These measures are computed with an image analysis software called
### Freesurfer using two pipelines: cross-sectional (each subject visit is
### independent) or longitudinal (uses information from all the visits of a
### subject). The longitudinal measures are ?more robust?, but the downside is
### that there are more missing values in our TADPOLE spreadsheet. The MRI
### biomarkers in TADPOLE can be found in the columns containing UCSFFSX
### (cross-sectional) and UCSFFSL (longitudinal).
###
### The D1 a comprehensive longitudinal data set for training.
### The D2 a comprehensive longitudinal data set for prediction.
### The D3 dataset is for cross-sectional prediction. For each participant in
###  D2, the final visit only and a limited number of data columns to mimic
###  screening data for a clinical trial: demographics, cognitive test scores,
###  and structural MRI (derived brain volumes).
###
### * The MRI biomarkers consist of FreeSurfer longitudinally processed ROIs
###   from UCSFFSL tables
### * The DTI biomarkers added represent ROI summary measures (e.g. mean
###   diffusivity MD, axial diffusivity AD) taken from the spreadsheet
###   DTIROI_04_30_14.csv.
### * CSF biomarkers: Amyloid-beta, Tau and P-Tau were taken from the
###   Elecsys analysis, which can be found in the UPENNBIOMK9_04_19_17.csv
###   spreadsheet.
tadpole_d1_d2 <- readr::read_csv("data/TADPOLE_D1_D2.csv", guess_max = 20000)
tadpole_d1_d2_dict <- readr::read_csv("data/TADPOLE_D1_D2_DICT.csv", guess_max = 20000)
tadpole_d3 <- readr::read_csv("data/TADPOLE_D3.csv", guess_max = 20000)
tadpole_d1 <- filter(tadpole_d1_d2, D1 == 1)
tadpole_d2 <- filter(tadpole_d1_d2, D2 == 1)


### Initial biomarkers:
###
### MRI measures: Hippocampus (Hippocampus_bl) (numeric, normal) (UCSF Hippocampus)
### MRI measures: WholeBrain (WholeBrain_bl) (numeric, normal) (UCSF WholeBrain)
### MRI measures: Entorhinal (Entorhinal_bl) (numeric, normal) (UCSF Entorhinal)
### MRI measures: MidTemp (MidTemp_bl) (numeric, normal) (UCSF Med Temp)
###
### PET measures: FDG (FDG_bl) (numeric, normal) ("Average FDG-PET of angular, temporal, and posterior cingulate")
### PET measures: AV45 (AV45_bl) (numeric) ("Average AV45 SUVR of frontal, anterior cingulate, precuneus, and parietal cortex relative to the cer")
###
### CSF measures: ABETA_UPENNBIOMK9_04_19_17  (amyloid-beta level in CSF) (numeric, lognormal)
### CSF measures: TAU_UPENNBIOMK9_04_19_17 (tau level) (numeric, lognormal)
### CSF measures: PTAU_UPENNBIOMK9_04_19_17 (phosphorylated tau level) (numeric, lognormal, bimodal?dx)
###
### Risk factors: APOE4 (numeric)
### Risk factors: AGE (numeric, normal)
###
### Cognitive tests: CDRSB (CDRSB_bl) (numeric)
### Cognitive tests: ADAS11 (ADAS11_bl) (numeric, lognormal, dx changes dist)
### Cognitive tests: MMSE (MMSE_bl) (numeric)
### Cognitive tests: RAVLT_immediate (RAVLT_immediate_bl) (numeric, normal)
###
###
### Predict: ADAS13 (ADAS13_bl) (numeric, lognormal, dx changes dist)
### Predict: Ventricles (Ventricles_bl) (UCSF Ventricles)
### Predict: DX (DX_bl, DX_CHANGE) "1=Stable:NL to NL, 2=Stable:MCI to MCI,
### 3=Stable:AD to AD, 4=Conv:NL to MCI, 5=Conv:MCI to AD, 6=Conv:NL to AD,
###  7=Rev:MCI to NL, 8=Rev:AD to MCI, 9=Rev:AD to NL, -1=Not available"
###
###  PTID = 005_S_0223
###
###  NL = Normal?
###  CN = cognitively normal
###  MCI = mild cognitive impairment (Late, Early)
###  SMC = subjective memory complain
###  AD = Alzheimer's disease
###
###
###
### You are asked to forecast three features of each rollover individual at the
###  time of their future data provision. Each feature is a common or likely
###   outcome measure for clinical trial
###
### Clinical status -> DX (give probability of the class)
### ADAS-Cog13 score -> ADAS13 (best-guess a value as well as a 50% confidence interval)
### Ventricles volume, divided by intracranial volume -> Ventricles (best-guess a value as well as a 50% confidence interval)
###
###
### Methods:
### - Regression
### - ML
### - Disease Progression Model
###

tad1 <- tadpole_d1 %>%
  select(
    RID, PTID, VISCODE, SITE, D1, D2, COLPROT, ORIGPROT, EXAMDATE,
    PTGENDER, PTEDUCAT, PTETHCAT, PTRACCAT, APOE4, AGE,
    Hippocampus, Hippocampus_bl,
    WholeBrain, WholeBrain_bl,
    Entorhinal, Entorhinal_bl,
    MidTemp, MidTemp_bl,
    FDG, FDG_bl,
    AV45, AV45_bl,
    ABETA_UPENNBIOMK9_04_19_17,
    TAU_UPENNBIOMK9_04_19_17,
    PTAU_UPENNBIOMK9_04_19_17,
    CDRSB, CDRSB_bl,
    ADAS11, ADAS11_bl,
    MMSE, MMSE_bl,
    RAVLT_immediate, RAVLT_immediate_bl,
    DX, DX_bl, DXCHANGE,
    ADAS13, ADAS13_bl,
    Ventricles, Ventricles_bl
  ) %>%
  mutate(
    ABETA_UPENNBIOMK9_04_19_17 = if_else(ABETA_UPENNBIOMK9_04_19_17 == "<200", "190", ABETA_UPENNBIOMK9_04_19_17),
    TAU_UPENNBIOMK9_04_19_17 = if_else(TAU_UPENNBIOMK9_04_19_17 == "<80", "70", TAU_UPENNBIOMK9_04_19_17),
    PTAU_UPENNBIOMK9_04_19_17 = if_else(PTAU_UPENNBIOMK9_04_19_17 == "<8", "7", PTAU_UPENNBIOMK9_04_19_17)
  ) %>%
  mutate(
    ABETA_UPENNBIOMK9_04_19_17 = as.numeric(ABETA_UPENNBIOMK9_04_19_17),
    TAU_UPENNBIOMK9_04_19_17 = as.numeric(TAU_UPENNBIOMK9_04_19_17),
    PTAU_UPENNBIOMK9_04_19_17 = as.numeric(PTAU_UPENNBIOMK9_04_19_17)
  )

dxfactors <- c(
  "Stable:NL to NL", "Stable:MCI to MCI", "Stable:AD to AD", "Conv:NL to MCI", "Conv:MCI to AD",
  "Conv:NL to AD", "Rev:MCI to NL", "Rev:AD to MCI", "Rev:AD to NL"
)

tad1$DXCHANGE <- factor(tad1$DXCHANGE, 1:9, dxfactors)
tad1$PTGENDER <- factor(tad1$PTGENDER)
tad1$PTETHCAT <- factor(tad1$PTETHCAT)
tad1$PTRACCAT <- factor(tad1$PTRACCAT)

tad2 <- tadpole_d2 %>%
  select(
    RID, PTID, VISCODE, SITE, D1, D2, COLPROT, ORIGPROT, EXAMDATE,
    PTGENDER, PTEDUCAT, PTETHCAT, PTRACCAT, APOE4, AGE,
    Hippocampus, Hippocampus_bl,
    WholeBrain, WholeBrain_bl,
    Entorhinal, Entorhinal_bl,
    MidTemp, MidTemp_bl,
    FDG, FDG_bl,
    AV45, AV45_bl,
    ABETA_UPENNBIOMK9_04_19_17,
    TAU_UPENNBIOMK9_04_19_17,
    PTAU_UPENNBIOMK9_04_19_17,
    CDRSB, CDRSB_bl,
    ADAS11, ADAS11_bl,
    MMSE, MMSE_bl,
    RAVLT_immediate, RAVLT_immediate_bl,
    DX, DX_bl, DXCHANGE,
    ADAS13, ADAS13_bl,
    Ventricles, Ventricles_bl
  ) %>%
  mutate(
    ABETA_UPENNBIOMK9_04_19_17 = if_else(ABETA_UPENNBIOMK9_04_19_17 == "<200", "190", ABETA_UPENNBIOMK9_04_19_17),
    TAU_UPENNBIOMK9_04_19_17 = if_else(TAU_UPENNBIOMK9_04_19_17 == "<80", "70", TAU_UPENNBIOMK9_04_19_17),
    PTAU_UPENNBIOMK9_04_19_17 = if_else(PTAU_UPENNBIOMK9_04_19_17 == "<8", "7", PTAU_UPENNBIOMK9_04_19_17)
  ) %>%
  mutate(
    ABETA_UPENNBIOMK9_04_19_17 = as.numeric(ABETA_UPENNBIOMK9_04_19_17),
    TAU_UPENNBIOMK9_04_19_17 = as.numeric(TAU_UPENNBIOMK9_04_19_17),
    PTAU_UPENNBIOMK9_04_19_17 = as.numeric(PTAU_UPENNBIOMK9_04_19_17)
  )

tad2$DXCHANGE <- factor(tad2$DXCHANGE, 1:9, dxfactors)
tad2$PTGENDER <- factor(tad2$PTGENDER)
tad2$PTETHCAT <- factor(tad2$PTETHCAT)
tad2$PTRACCAT <- factor(tad2$PTRACCAT)

## Need to get other variables to forecast this
tad3 <- tadpole_d3 %>% select(
  RID, VISCODE, COLPROT, EXAMDATE,
  PTGENDER, PTEDUCAT, PTETHCAT, PTRACCAT, AGE,
  Hippocampus,
  WholeBrain,
  Entorhinal,
  MidTemp,
  DX,
  ADAS13,
  Ventricles
)

tad3$PTGENDER <- factor(tad3$PTGENDER)
tad3$PTETHCAT <- factor(tad3$PTETHCAT)
tad3$PTRACCAT <- factor(tad3$PTRACCAT)

# scatterplot matrix
dataset <- select(tad1, -(RID:EXAMDATE), -ADAS13, -ADAS13_bl, -Ventricles, -Ventricles_bl) %>% filter(complete.cases(.))
dataset$DX[dataset$DX == "NL to MCI"] <- "MCI"
dataset$DX[dataset$DX == "MCI to Dementia"] <- "Dementia"
dataset$DX[dataset$DX == "MCI to NL"] <- "NL"
dataset$DX <- factor(dataset$DX, ordered = TRUE)
dataset$DX_bl <- factor(dataset$DX_bl)

validation <- select(tad2, -(RID:EXAMDATE), -ADAS13, -ADAS13_bl, -Ventricles, -Ventricles_bl) %>% filter(complete.cases(.))
validation$DX[validation$DX == "NL to MCI"] <- "MCI"
validation$DX[validation$DX == "MCI to Dementia"] <- "Dementia"
validation$DX[validation$DX == "MCI to NL"] <- "NL"
validation$DX <- factor(validation$DX, ordered = TRUE)
validation$DX_bl <- factor(validation$DX_bl)

metric <- "Accuracy"
control <- trainControl(
  method = "cv", number = 5, classProbs = T,
  summaryFunction = multiClassSummary,
  savePredictions = T
)
set.seed(7)
fit <- train(DX ~ ., data = dataset, method = "rf", metric = metric, trControl = control)
set.seed(7)
# summarize accuracy of models
predictions <- predict(fit, validation)
confusionMatrix(predictions, validation$DX)

varImp(fit, scale = FALSE)
