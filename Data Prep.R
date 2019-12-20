#Load Libraries Required

library(readxl)
library(tidyverse)
library(lubridate)
#library(FactoMineR)
#library(factoextra)
#library(NbClust)
#library(janitor)
library(caret)
library(DMwR)


# Load Non electives - fix dates and then group into stays

Non_Electives_2013_2015 <- read_excel("data/Non Electives 2013-2015.xlsx", 
                                      col_types = c("numeric", "text", "numeric", "numeric",
                                                    "text", "numeric", "numeric", "numeric", "skip"))

Non_Electives_2016_2019 <- read_excel("data/Non Electives 2016-2019.xlsx", 
                                      col_types = c("numeric", "text", "numeric", 
                                                    "numeric", "text", "numeric", "numeric", 
                                                    "numeric", "skip"))

Non_Electives <- bind_rows(Non_Electives_2013_2015,
                           Non_Electives_2016_2019)

Non_Electives$PeriodDate <- ymd(Non_Electives$PeriodDate)

Non_Electives <- Non_Electives %>%
  group_by(EpisodeNo, HistoryNo) %>%
  summarise(AdmissionDate = min(PeriodDate),
            DischargeDate = max(PeriodDate),
            Admit_Specialty = first(Specialty, order_by = PeriodDate),
            Discharge_Specialty = last(Specialty, order_by = PeriodDate),
            Length_Of_Stay = sum(BedDays)
  )




# Load HIstory Number and MRN Lookup

His_and_MRN <- read_excel("data/His and MRN.xlsx", 
                          col_types = c("numeric", "numeric"))


# # Load Home Care Packages and Lookup History Numbers
# HCP <- read_excel("data/HCP.xlsx", col_types = c("numeric", 
#                                                  "text", "skip", "skip"))
# 
# HCP$HCPOnListDate <- ymd(HCP$HCPOnListDate)
# 
# HCP <- HCP %>%
#   left_join(His_and_MRN, by = 'MRN')
# 
# 


# Build OPD - Import All Files

Outpatients_2012 <- read_excel("data/Outpatients 2012-62.xlsx")
Outpatients_2012$PeriodDate <- ymd(Outpatients_2012$PeriodDate)

Outpatients_2013 <- read_excel("data/Outpatients 2013-71.xlsx")
Outpatients_2013$PeriodDate <- ymd(Outpatients_2013$PeriodDate)

Outpatients_2014 <- read_excel("data/Outpatients 2014-77.xlsx")
Outpatients_2014$PeriodDate <- ymd(Outpatients_2014$PeriodDate)

Outpatients_2015 <- read_excel("data/Outpatients 2015-85.xlsx")
Outpatients_2015$PeriodDate <- ymd(Outpatients_2015$PeriodDate)

Outpatients_2016_to_2019 <- read_excel("data/Outpatients 2016-2019-91.xlsx", 
                                       col_types = c("numeric", "text", "text", 
                                                     "numeric", "numeric", "skip"))
Outpatients_2016_to_2019$PeriodDate <- ymd(Outpatients_2016_to_2019$PeriodDate)

Outpatients_2016_to_2019_part2 <- read_excel("data/Outpatients 2016-2019-91 part 2.xlsx", 
                                             col_types = c("numeric", "text", "text", 
                                                           "numeric", "numeric", "skip"))
Outpatients_2016_to_2019_part2$PeriodDate <- ymd(Outpatients_2016_to_2019_part2$PeriodDate)


# Build OPD - Make One Large Data frame and sort by Date

OPD <- bind_rows(Outpatients_2012,
                 Outpatients_2013,
                 Outpatients_2014,
                 Outpatients_2015,
                 Outpatients_2016_to_2019,
                 Outpatients_2016_to_2019_part2)

OPD <- arrange(OPD, PeriodDate) %>%
  select(HistoryNo, Specialty, PeriodDate, OPD_Attend = 'O/P Count')



# # Theatre Activity
# 
# Theatre <- read_excel("data/Theatre.xlsx", 
#                       col_types = c("numeric", "numeric", "numeric", 
#                                     "text", "text", "text", "numeric", 
#                                     "skip", "skip"))
# 
# Theatre$SurgeryDate <-  ymd(substr(Theatre$SurgeryDate,1,10))


# Electives

ElectivesIP <- read_excel("data/Elective IP Beaumont - 2013-2019.xlsx", 
                          col_types = c("text", "skip", "text", 
                                        "text", "numeric", "skip", "numeric", 
                                        "numeric", "text", "text"))
ElectivesIP$IPDC <- "IP"

ElectivesDC <- bind_rows(read_excel("data/Elective DC - 2019.xlsx",
                                    col_types = c("text", "skip", "text",
                                                  "text", "skip", "numeric", "numeric",
                                                  "numeric", "text", "text")),
                         read_excel("data/Elective DC - 2017-2018.xlsx",
                                    col_types = c("text", "skip", "text",
                                                  "text", "skip", "numeric", "numeric",
                                                  "numeric", "text", "text")),
                         read_excel("data/Elective DC - 2015-2016.xlsx",
                                    col_types = c("text", "skip", "text",
                                                  "text", "skip", "numeric", "numeric",
                                                  "numeric", "text", "text")),
                         read_excel("data/Elective DC - 2013-2014.xlsx",
                                    col_types = c("text", "skip", "text",
                                                  "text", "skip", "numeric", "numeric",
                                                  "numeric", "text", "text"))
)
ElectivesDC$IPDC <- "DC"
Electives <- bind_rows(ElectivesIP, ElectivesDC)
Electives$IPDC <- as.factor(Electives$IPDC)

Electives$MRN <- as.numeric(Electives$MRN)
Electives$ADMDATE <- dmy(Electives$ADMDATE)
Electives$DISDATE <- dmy(Electives$DISDATE)
Electives$SEX <- factor(Electives$SEX, levels = c(1,2), labels = c("Male", "Female"))

Electives$LOS <- as.numeric(Electives$DISDATE - Electives$ADMDATE)
Electives <- Electives %>%
  filter(is.na(PROC1) == FALSE)

Electives <- Electives %>%
  left_join(His_and_MRN, by = "MRN")

mean(is.na(Electives$HistoryNo))



# 
# Electives <- Electives %>%
#   group_by(EpisodeNo, HistoryNo, Age, Sex) %>%
#   summarise(AdmissionDate = min(PeriodDate),
#             DischargeDate = max(PeriodDate),
#             Admit_Specialty = first(Specialty, order_by = PeriodDate),
#             Discharge_Specialty = last(Specialty, order_by = PeriodDate),
#             Length_Of_Stay = sum(BedDays)
#   )


# Create Mode function - https://www.tutorialspoint.com/r/r_mean_median_mode

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Summarise HIPE and Elective by Length of Stay and underlying variance

ElectivesSummary <- Electives %>%
  group_by(PROC1) %>%
  filter(n() >= 10, !is.na(PROC1)) %>%
  summarise(meanLOS = mean(LOS),
            sdLOS = sd(LOS),
            medianLOS = median(LOS),
            lowerqrt = quantile(LOS)[2],
            upperqrt = quantile(LOS)[4],
            NQAISTrim = (quantile(LOS)[4]+ 3*(quantile(LOS)[4]-quantile(LOS)[2])),
            modeLOS = getmode(LOS),
            mode_median_diff = modeLOS - medianLOS,
            mode_mean_diff = modeLOS - meanLOS)


# Ward Hierarchical Clustering

d <- dist(ElectivesSummary[2:10], method = "euclidean") # distance matrix
hclustfit <- hclust(d, method="ward.D2") 
plot(hclustfit) # display dendogram
groups <- cutree(hclustfit, k=10) # cut tree into 10 clusters

# draw dendogram with red borders around the 10 clusters 

rect.hclust(hclustfit, k=10, border="red")

# write the clusters back to the table
ElectivesSummary$ClusterClass <- as.factor(groups)

# Plot the clusters by colour with Mean vs Median and size as the standard deviation
ggplot(ElectivesSummary, aes(x = meanLOS, y = medianLOS, size = sdLOS))+
  geom_point(aes(col = ClusterClass, fill = ClusterClass))+
  scale_color_brewer(palette = "Paired")+
  scale_fill_brewer(palette = "Paired")+
  theme_minimal()+
  theme(legend.position = "top")+
  scale_size(range = c(0, 10))+
  labs(title = "Procedure Clustering Results")

# Scale the HIPE Elective data for kmeans testing
df <- scale(ElectivesSummary[,2:10])

# Elbow method
fviz_nbclust(df, kmeans, method = "wss") +
  geom_vline(xintercept = 6, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(df, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Gap statistic
# nboot = 50 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
# set.seed(123)
# fviz_nbclust(df, kmeans, nstart = 25,  method = "gap_stat", nboot = 500)+
#   labs(subtitle = "Gap statistic method")

kmeansfit <- kmeans(df, 10, nstart = 10000)

ElectivesSummary$Kmeanscluster <- as.factor(kmeansfit$cluster)

ggplot(ElectivesSummary, aes(x = meanLOS, y = medianLOS, size = sdLOS))+
  geom_point(aes(col = Kmeanscluster, fill = Kmeanscluster))+
  scale_color_brewer(palette = "Paired")+
  scale_fill_brewer(palette = "Paired")+
  theme_minimal()+
  theme(legend.position = "top")+
  scale_size(range = c(0, 10))+
  labs(title = "K Means Clustering Results")


# Prepare Classification
ElectivesClassed <- Electives %>%
  left_join(ElectivesSummary, by = "PROC1") %>%
  filter(is.na(PROC1) == FALSE, is.na(meanLOS) == FALSE) %>%
  mutate(LOSclass = case_when(LOS <= modeLOS ~ "Good",
                              LOS > modeLOS ~ "Long Stay")) %>%
  mutate(LOSclass = factor(LOSclass, levels = c("Good","Long Stay")))


ElectivesClassed %>%
  group_by(LOSclass) %>%
  summarise(n())


# Join the OPD data and the elective data

CombinedOPD <- ElectivesClassed %>%
  filter(is.na(HistoryNo) == FALSE, IPDC == "IP") %>%
  left_join(OPD, by = "HistoryNo") %>%
  filter(PeriodDate < ADMDATE, is.na(Specialty) == FALSE) %>%
  group_by(HistoryNo, ADMDATE, DISDATE, SEX, AGE, LOS, DIAG1, PROC1, LOSclass, Kmeanscluster, Specialty) %>%
  summarise(OPDCount = sum(OPD_Attend)) %>%
  spread(Specialty, OPDCount, fill = 0)

CombinedOPD %>%
  group_by(LOSclass) %>%
  summarise(n())

CombinedOPDUpsamp <- ElectivesClassed %>%
  filter(is.na(HistoryNo) == FALSE, IPDC == "DC") %>%
  left_join(OPD, by = "HistoryNo") %>%
  filter(PeriodDate < ADMDATE, is.na(Specialty) == FALSE) %>%
  sample_n(5000) %>%
  group_by(HistoryNo, ADMDATE, DISDATE, SEX, AGE, LOS, DIAG1, PROC1, LOSclass, Kmeanscluster, Specialty) %>%
  summarise(OPDCount = sum(OPD_Attend)) %>%
  spread(Specialty, OPDCount, fill = 0)

CombinedOPD <- bind_rows(CombinedOPD, CombinedOPDUpsamp)

CombinedOPD %>%
  group_by(LOSclass) %>%
  summarise(n())

#Combined <- clean_names(CombinedNew)

# Join the Non Elective and the Elective Data
CombinedIP <- ElectivesClassed %>%
  filter(is.na(HistoryNo) == FALSE) %>%
  left_join(Non_Electives, by = "HistoryNo") %>%
  filter(AdmissionDate < ADMDATE) %>%
  group_by(HistoryNo, ADMDATE, DISDATE, SEX, AGE, LOS, DIAG1, PROC1, LOSclass, Kmeanscluster, Discharge_Specialty) %>%
  summarise(Length_Of_Stay = sum(Length_Of_Stay)) %>%
  spread(Discharge_Specialty, Length_Of_Stay, fill = 0)

#CombinedIP <- clean_names(CombinedIP)
CombinedOPD[is.na(CombinedOPD)] <- 0

# Principal Components Analysis

Combined_PCA <- prcomp(CombinedOPD[,11:47])
fviz_screeplot(Combined_PCA)

CombinedOPD <- bind_cols(CombinedOPD[,1:9],
                         OPDPCA1 = Combined_PCA$x[,1],
                         OPDPCA2 = Combined_PCA$x[,2] )

CombinedIP[is.na(CombinedIP)] <- 0

CombinedIP_PCA <- prcomp(CombinedIP[,11:40])
fviz_screeplot(CombinedIP_PCA)


CombinedIP <- bind_cols(CombinedIP[,1:9],
                        IPUNSCH1 = CombinedIP_PCA$x[,1],
                        IPUNSCH2 = CombinedIP_PCA$x[,2],
                        IPUNSCH3 = CombinedIP_PCA$x[,3])

CombinedIPandOPD <- CombinedOPD %>%
  left_join(CombinedIP, by = c("HistoryNo", "ADMDATE", "DISDATE", "SEX", "AGE", "LOS", "DIAG1", "PROC1", "LOSclass")) %>%
  left_join(select(ElectivesSummary, PROC1, medianLOS, meanLOS, sdLOS, modeLOS, mode_median_diff, mode_mean_diff), by = "PROC1")

CombinedIPandOPD[is.na(CombinedIPandOPD)] <- 0

formodelling <- CombinedIPandOPD[,-c(1,2,3,6,7,8)]

# Save Datasets as RData files, clear environment and reload
# 
# save(His_and_MRN, HCP, file = "data.RData")
# save(OPD, file = "opd_data.RData")
# save(Theatre, file = "theatre.RData")
# save(Non_Electives, file = "Non_Electives.RData")
# save(Electives,ElectivesClassed, ElectivesSummary, file = "Electives.RData")
# save(hipe_Electives_classed, HIPE_Elect, HIPE_Elect_Summary, file = "HIPE_Elective.RData")
# save(Combined, file = "Combined.RData")
save(formodelling, file = "formodellingmode.RData")
save(CombinedIP_PCA, Combined_PCA, file = "PCAmode.RData")
save(ElectivesSummary, file = "ElectivesSummary.RData")

rm(list=ls())

# load("data.RData")
# load("opd_data.RData")
# load("theatre.RData")
# load("Non_Electives.RData")
# load("Electives.RData")
# load("HIPE_Elective.RData")
# load("Combined.RData")

load("formodellingmode.RData")

# Create Training and Testing Partition

set.seed(123)
index <- createDataPartition(formodelling$LOSclass, p = 0.80, times = 1)
training <- formodelling[index$Resample1,]
testing <- formodelling[-index$Resample1,]


# # Create a bigger simulation of the training set for ensemble build later
# 
# trainingsim <- SMOTE(LOSclass ~. , data = as.data.frame(training), perc.over = 100, perc.under = 200)
# 
# save(formodelling, index, training, testing, trainingsim, file = "formodellingmode.RData")


#set up an ensemble using the training set

ensembleindex <- createDataPartition(training$LOSclass, p = 0.5 , times = 1)
ensembletrain <- training[ensembleindex$Resample1,]
ensembletinnertest <- training[-ensembleindex$Resample1,]

save(formodelling, index, training, testing, trainingsim, ensembletinnertest, ensembletrain, ensembleindex,  file = "formodellingmode.RData")
