#########################
## Purpose: Dose Interval Analysis
## Coder: Avnika B. Amin
## Creation Date: January 24, 2022
##################################

library(tidyverse)
library(lubridate)
library(readxl)
library(RColorBrewer)

option1 <- "C:/Users/sez6/OneDrive - CDC/Surveillance and Analytics Team/Vaccine Breakthrough Surveillance Data/Line-level Data from COETF/LL Outputs/2022.01"
option2 <- "C:/Users/sez6/OneDrive - CDC/Surveillance and Analytics Team/Vaccine Breakthrough Surveillance Data/Option 2 LL+Agg Datasets/Output Linelist Datasets/2022.01"

#### Data import
option1data.1 <- read_csv(paste0(option1, "/LL_ID_IN_MN_JAN.csv"), col_types = cols(.default = "c"))
option1data.2 <- read_csv(paste0(option1, "/LL_NYC_TN_UT_JAN.csv"), col_types = cols(.default = "c"))

option1data <- rbind(option1data.1, option1data.2)

rm(option1data.1, option1data.2)

option1subset <- 
  option1data %>% 
  filter(vax_status == "Fully", vax_prod %in% c("Pfizer", "Moderna")) %>% 
  select(state, sex, age_calc, age_grp, mmwr_wk, year, year_mmwr_wk, pos_test_date, vaxdate1_fix, vaxdate2_fix, vaxmfr1_fix, vaxmfr2_fix, vax_prod, vax_status, dateSeq_fix, mfrSeq_fix, vax_first_dt, vax_start_dt, vax_compl_dt, fully_vax_dt, ethnicity, starts_with("race_"), death_yn)

option2data <- read_csv(paste0(option2, "/Full Linelist_2022Jan18.csv"), col_types = cols(.default = "c"))

option2subset <- 
  option2data %>% 
  filter(vax_prod %in% c("Pfizer", "Moderna")) %>% 
  select(state = juris, sex, age_calc, age_grp = age_group, mmwr_wk, year, year_mmwr_wk, pos_test_date, vaxdate1_fix, vaxdate2_fix, vaxmfr1_fix, vaxmfr2_fix, vax_prod, vax_status, dateSeq_fix, mfrSeq_fix, vax_first_dt, vax_start_dt, vax_compl_dt, fully_vax_dt, ethnicity, starts_with("race_"), outcome, death_related)

### Recoding data subsets so that variables all have the same names

linelist <- bind_rows(option1subset, option2subset)
rm(option1subset, option2subset)

lapply(linelist, unique)

linelist <- 
  linelist %>% 
  filter(state != "NYC", age_grp != "5-11", age_grp != "12-17") %>% 
  mutate(
    sexF = case_when(
      sex %in% c("Female", "F") ~ "F", 
      sex %in% c("Male", "M") ~ "M", 
      sex %in% c("Unknown", "Missing", "U") | is.na(sex) ~ "U", 
      sex %in% c("Other", "T", "A", "O", "A different Identity") ~ "O", 
      TRUE ~ "C"), 
    age_grpF = case_when(
      age_grp %in% c("18-29", "30-49") ~ "18-49", 
      age_grp == "50-64" ~ age_grp, 
      age_grp %in% c("65-79", "80+") ~ "65+", 
      TRUE ~ "C"), 
    vaxmfr1F = case_when(
      vaxmfr1_fix %in% c("Pfizer", "1") ~ "Pfizer", 
      vaxmfr1_fix %in% c("Moderna", "2") ~ "Moderna", 
      vaxmfr1_fix %in% c("Unknown", "9") ~ "exclude", 
      TRUE ~ "C"), 
    vaxmfr2F = case_when(
      vaxmfr2_fix %in% c("Pfizer", "1") ~ "Pfizer", 
      vaxmfr2_fix %in% c("Moderna", "2") ~ "Moderna", 
      TRUE ~ "C"), 
    
    vax_statusF = "Fully", 
    
    pos_test_dateF = ymd(pos_test_date), 
    vaxdate1F = ymd(vaxdate1_fix), 
    vaxdate2F = ymd(vaxdate2_fix), 
    vax_first_dtF = ymd(vax_first_dt), 
    vax_start_dtF = ymd(vax_start_dt), 
    vax_compl_dtF = ymd(vax_compl_dt), 
    fully_vax_dtF = ymd(fully_vax_dt), 
      
    dateSeqF = paste(substr(is.na(vaxdate1F), 1, 1), substr(is.na(vaxdate2F), 1, 1), sep = "/"), 
    
    mfrSeqF = paste(substr(is.na(vaxmfr1F), 1, 1), substr(is.na(vaxmfr2F), 1, 1), sep = "/"))


linelistF <- 
  linelist %>% 
  select(state, sexF, age_grpF, vaxmfr1F, vaxmfr2F, vax_statusF, pos_test_dateF, vaxdate1F, vaxdate2F, vax_first_dtF, vax_start_dtF, vax_compl_dtF, fully_vax_dtF, starts_with("race_"), death_yn, outcome, death_related, vax_prod, mmwr_wk) %>% 
  mutate(keepMe = case_when(
    vaxmfr1F == "Pfizer" & vaxmfr2F == "Pfizer" ~ "Y", 
    vaxmfr1F == "Moderna" & vaxmfr2F == "Moderna" ~ "Y", 
    TRUE ~ "N")) %>% 
  filter(keepMe == "Y")

summary(linelistF$pos_test_dateF)

linelistF <- 
  linelistF %>% 
  filter(pos_test_dateF >= mdy(04142021), pos_test_dateF <= mdy(09042021))

table(linelistF$vaxmfr1F, linelistF$vaxmfr2F, useNA = "ifany") # checks out

table(linelistF$sexF, linelistF$vax_prod, useNA = "ifany")
prop.table(table(linelistF$sexF, linelistF$vax_prod, useNA = "ifany"), margin=2)

table(linelistF$age_grpF, linelistF$vax_prod, useNA = "ifany")
prop.table(table(linelistF$age_grpF, linelistF$vax_prod, useNA = "ifany"), margin=2)

analyzeme <- 
  linelistF %>% 
  mutate(doseInt = as.duration(interval(vaxdate1F, vaxdate2F))/ddays()) 

table(analyzeme$vax_prod)

table(analyzeme$sexF, analyzeme$vax_prod, useNA = "ifany")
prop.table(table(analyzeme$sexF, analyzeme$vax_prod, useNA = "ifany"), margin=2)

table(analyzeme$age_grpF, analyzeme$vax_prod, useNA = "ifany")
prop.table(table(analyzeme$age_grpF, analyzeme$vax_prod, useNA = "ifany"), margin=2)

tapply(analyzeme$doseInt, analyzeme$vax_prod, summary)

analyzeme %>% 
  ggplot(aes(x = doseInt)) + 
  geom_histogram(binwidth = 1) + 
  xlab("Days Between 1st and 2nd Doses") + ylab("Number of Cases") + 
  facet_wrap(~vax_prod, scales = "free_y", nrow = 2) + theme_bw() + 
  geom_vline(xintercept = 23)

analyzeme %>% 
  ggplot(aes(x = doseInt)) + 
  geom_histogram(binwidth = 1) + 
  xlab("Days Between 1st and 2nd Doses") + ylab("Number of Cases") + 
  facet_wrap(~vax_prod, scales = "free_y", nrow = 1) + theme_bw() + 
  geom_vline(xintercept = 23.5, color = "red") + geom_vline(xintercept = 30.5, color = "red")


analyzeme %>% 
  ggplot(aes(x = doseInt)) + 
  geom_histogram(binwidth = 1) + 
  xlab("Days Between 1st and 2nd Doses") + ylab("Number of Cases") + 
  facet_wrap(~vax_prod, scales = "free_y", nrow = 1) + theme_bw() + 
  geom_vline(xintercept = 21.5, color = "red") + geom_vline(xintercept = 26.5, color = "red") + 
  geom_vline(xintercept = 30.5, color = "red") + geom_vline(xintercept = 34.5, color = "red")

analyzeme <- 
  analyzeme %>% 
  mutate(doseInt3 = case_when(
    doseInt < 17 ~ "0", 
    doseInt >= 17 & doseInt <= 23 ~ "1", 
    doseInt >= 24 & doseInt <= 30 ~ "2", 
    doseInt >= 31 & doseInt <= 37 ~ "3", 
    doseInt > 37 ~ "4"), 
    
    doseInt5 = case_when(
      doseInt < 17 ~ "0", 
      doseInt >= 17 & doseInt <= 21 ~ "1", 
      doseInt >= 22 & doseInt <= 26 ~ "2", 
      doseInt >= 27 & doseInt <= 30 ~ "3", 
      doseInt >= 31 & doseInt <= 34 ~ "4", 
      doseInt >= 35 & doseInt <= 37 ~ "5", 
      doseInt > 37 ~ "6"))

table(analyzeme$doseInt1, analyzeme$vax_prod, useNA = "ifany")
table(analyzeme$doseInt1, analyzeme$age_grpF, analyzeme$vax_prod, useNA = "ifany")
prop.table(table(analyzeme$age_grpF, analyzeme$vax_prod, useNA = "ifany"), margin=2)

table(analyzeme$doseInt2, analyzeme$vax_prod, useNA = "ifany")
table(analyzeme$doseInt2, analyzeme$age_grpF, analyzeme$vax_prod, useNA = "ifany")

analyzeme <- 
  analyzeme %>% 
  mutate(altInt = case_when(
    doseInt >= 19 & doseInt <= 24 ~ "1", 
    doseInt >= 25 & doseInt <= 31 ~ "2", 
    doseInt >= 32 & doseInt <= 37 ~ "3"))

table(analyzeme$altInt, analyzeme$age_grpF, analyzeme$vax_prod, useNA = "ifany")

write.csv(analyzeme, file = "C:/Users/sez6/OneDrive - CDC/Surveillance and Analytics Team/Analytics/Dose interval/Dose Interval Dataset 2022JAN24.csv", row.names = F)

aggregate3 <- 
  analyzeme %>% 
  group_by(vax_prod, mmwr_wk, age_grpF, doseInt3) %>% 
  tally() %>% 
  mutate(mmwr_wk = as.numeric(mmwr_wk))
    
vds3 <- read_xlsx(path = "C:/Users/sez6/OneDrive - CDC/Surveillance and Analytics Team/Analytics/Dose interval/VDS66_Results.xlsx", sheet = "3 category")
names(vds3) <- c("state", "mmwr_wk", "week_date", "vax_prod", "age_grpF", "doseInt3_0", "doseInt3_1", "doseInt3_2", "doseInt3_3", "doseInt3_4")
vds3_grouped <- 
  vds3 %>% 
  pivot_longer(cols = starts_with("doseInt3"), names_to = c("grouping", "doseInt3"), names_pattern = "(.*)_(.*)", values_to = "n") %>% 
  mutate(n = if_else(n == "SUPPRESSED", 5, as.numeric(n))) %>% 
  select(-grouping) %>% 
  group_by(mmwr_wk, week_date, vax_prod, age_grpF, doseInt3) %>% 
  summarize(denom = sum(n, na.rm = T))

analyzeme3 <- full_join(aggregate3, vds3_grouped, by = c("vax_prod", "mmwr_wk", "age_grpF", "doseInt3")) %>% 
  mutate(n = if_else(is.na(n), as.integer("0"), n)) %>%
  mutate(rate = n / denom * 100000) %>% 
  filter(mmwr_wk %in% aggregate3$mmwr_wk) %>%
  filter(age_grpF != "All Ages (>=18)") %>% 
  mutate(doseInt3F = factor(doseInt3, levels = c("0", "1", "2", "3", "4"), labels = c("<17 days", "17-23 days", "24-30 days", "31-37 days", ">37 days"), ordered = T))

analyzeme3 %>% 
  ggplot(aes(x = mmwr_wk, y = rate, color = doseInt3F)) + 
  geom_line() + facet_wrap(~vax_prod+age_grpF, nrow = 2, labeller = labeller(.multi_line = F)) + scale_color_manual(name = "Time Between Doses", values = brewer.pal(n=5, "Blues")[c(2:6)]) + 
  theme_bw() + ylab("Cases per 100,000") + xlab("MMWR Week") + 
  theme(legend.position = "bottom")

ggsave(filename = "./Surveillance and Analytics Team/Analytics/Dose interval/Preliminary Analysis 2022.01.24/Rates_3Categories.Tiff", device="tiff", units = "in", width = 8, height = 5)

aggregate5 <- 
  analyzeme %>% 
  group_by(vax_prod, mmwr_wk, age_grpF, doseInt5) %>% 
  tally() %>% 
  mutate(mmwr_wk = as.numeric(mmwr_wk))

vds5 <- read_xlsx(path = "C:/Users/sez6/OneDrive - CDC/Surveillance and Analytics Team/Analytics/Dose interval/VDS66_Results.xlsx", sheet = "5 category")
names(vds5) <- c("state", "mmwr_wk", "week_date", "vax_prod", "age_grpF", "doseInt5_0", "doseInt5_1", "doseInt5_2", "doseInt5_3", "doseInt5_4", "doseInt5_5", "doseInt5_6")
vds5_grouped <- 
  vds5 %>% 
  pivot_longer(cols = starts_with("doseInt5"), names_to = c("grouping", "doseInt5"), names_pattern = "(.*)_(.*)", values_to = "n") %>% 
  mutate(n = if_else(n == "SUPPRESSED", 5, as.numeric(n))) %>% 
  select(-grouping) %>% 
  group_by(mmwr_wk, week_date, vax_prod, age_grpF, doseInt5) %>% 
  summarize(denom = sum(n, na.rm = T))

analyzeme5 <- full_join(aggregate5, vds5_grouped, by = c("vax_prod", "mmwr_wk", "age_grpF", "doseInt5")) %>% 
  mutate(n = if_else(is.na(n), as.integer("0"), n)) %>%
  mutate(rate = n / denom * 100000) %>% 
  filter(mmwr_wk %in% aggregate5$mmwr_wk) %>%
  filter(age_grpF != "All Ages (>=18)") %>% 
  mutate(doseInt5F = factor(doseInt5, levels = c("0", "1", "2", "3", "4", "5", "6"), labels = c("<17 days", "17-21 days", "22-26 days", "27-30 days", "31-34 days", "35-37 days", ">37 days"), ordered = T))

analyzeme5 %>% 
  ggplot(aes(x = mmwr_wk, y = rate, color = doseInt5F)) + 
  geom_line() + facet_wrap(~vax_prod+age_grpF, nrow = 2, labeller = labeller(.multi_line = F)) + scale_color_manual(name = "Time Between Doses", values = brewer.pal(n=7, "Blues")[c(2:8)]) + 
  theme_bw() + ylab("Cases per 100,000") + xlab("MMWR Week") + 
  theme(legend.position = "bottom")

ggsave(filename = "./Surveillance and Analytics Team/Analytics/Dose interval/Preliminary Analysis 2022.01.24/Rates_5Categories.Tiff", device="tiff", units = "in", width = 8, height = 5)

hmmm <- analyzeme %>% mutate(outcome = if_else(vax_prod == "Moderna", 0, 1))
logistic <- glm(outcome ~ doseInt3, data = hmm, family = binomial)
exp(coefficients(logistic))
