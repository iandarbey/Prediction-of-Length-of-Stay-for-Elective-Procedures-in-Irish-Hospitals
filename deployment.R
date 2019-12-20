# Load necessary Libraries and PCA model, XGBoost model and electives summary ----

library(caret)
library(tidyverse)
library(readxl)

initial.options <- commandArgs(trailingOnly = FALSE)
file.arg.name <- "--file="
script.name <- sub(file.arg.name, "", initial.options[grep(file.arg.name, initial.options)])
script.basename <- dirname(script.name)
setwd(script.basename)

load("PCAmode.RData")
load("xgboostmodel.RData")
load("ElectivesSummary.RData")

# PCA names ----

ip_pca_names <- names(CombinedIP_PCA$rotation[,1])
opd_pca_names <- names(Combined_PCA$rotation[,1])

# Non-Electives Read in and PCA Prediction ----

Non_Electives <- read_excel("Non Electives.xlsx", 
                            col_types = c("numeric", "text", "numeric", 
                                          "numeric", "text", "numeric", "numeric", 
                                          "numeric", "skip"))

Non_Electives <- Non_Electives %>%
  group_by(EpisodeNo, HistoryNo) %>%
  summarise(AdmissionDate = min(PeriodDate),
            DischargeDate = max(PeriodDate),
            Admit_Specialty = first(Specialty, order_by = PeriodDate),
            Discharge_Specialty = last(Specialty, order_by = PeriodDate),
            Length_Of_Stay = sum(BedDays)
  )


Non_Electives <- Non_Electives %>%
  group_by(HistoryNo, Discharge_Specialty) %>%
  summarise(Length_Of_Stay = sum(Length_Of_Stay)) %>%
  spread(Discharge_Specialty, Length_Of_Stay, fill = 0)

Missing <- setdiff(ip_pca_names, names(Non_Electives))
Non_Electives[Missing] <- 0                   

Non_Electives_PCA <- Non_Electives %>%
  select(ip_pca_names) %>%
  predict(CombinedIP_PCA, .)

Non_Electives <- bind_cols(Non_Electives[,1],
                           IPUNSCH1 = Non_Electives_PCA[,1],
                           IPUNSCH2 = Non_Electives_PCA[,2],
                           IPUNSCH3 = Non_Electives_PCA[,3])


# Outpatients Read in and PCA Prediction ----


Outpatients <- read_excel("Outpatients.xlsx") %>%
  select(HistoryNo, Specialty, PeriodDate, OPD_Attend = 'O/P Count')

Outpatients <- Outpatients %>%
  group_by(HistoryNo, Specialty) %>%
  summarise(OPDCount = sum(OPD_Attend)) %>%
  spread(Specialty, OPDCount, fill = 0)

Missing <- setdiff(opd_pca_names, names(Outpatients))
Outpatients[Missing] <- 0


Outpatients_PCA <- Outpatients %>%
  select( opd_pca_names) %>%
  predict(Combined_PCA, .)


Outpatients <- bind_cols(Outpatients[,1],
                           OPDPCA1 = Outpatients_PCA[,1],
                           OPDPCA2 = Outpatients_PCA[,2])


# Read in Procedures ----

Procedures <- read_excel("Procedures.xlsx")

Procedures$SEX <- factor(Procedures$SEX, levels = c(1,2), labels = c("Male", "Female"))


# Combined Data for Predictions ----

Combined <- Procedures %>%
  left_join(Outpatients, by = "HistoryNo") %>%
  left_join(Non_Electives, by = "HistoryNo") %>%
  left_join(ElectivesSummary, by = "PROC1")

Combined[is.na(Combined)] <- 0


# Output Predictions ----

Combined$LOSClass <- predict(xgboostmodel, Combined)
write_excel_csv(Combined, "PredictionOutput.csv")


