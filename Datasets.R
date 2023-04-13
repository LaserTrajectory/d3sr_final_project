library(here)

setwd(here())

library(tidyverse)
library(gdata)
library(writexl)

# tcpd.ge.data <- read.csv("D:/Raghav/Ashoka/Major Courses/Sem 6/Data Science/Final Project/tcpd.ge.data.csv")
tcpd.ae.data <- read.csv("./TCPD_AE_All_States_2023-3-12.csv")
pmay_g.data <- read.csv("./pmay-g_clean.csv")
pmgsy.data <- read.csv("./pmgsy_clean.csv")
sbm_g.data <- read.csv("./sbm-gcsv_clean.csv")
sbm_u.data <- read.csv("./sbm-ucsv_clean.csv")

names(sbm_u.data) <- paste0("SBMU_", names(sbm_u.data))
sbm_u.data <- sbm_u.data %>% mutate(Year = as.numeric(substr(SBMU_Financial.Year, 1, 4))) %>% 
  rename(State_Name = SBMU_State_UT)

names(sbm_g.data) <- paste0("SBMG_", names(sbm_g.data))
sbm_g.data <- sbm_g.data %>% mutate(Year = as.numeric(substr(SBMG_Financial.Year, 1, 4))) %>% 
  rename(State_Name = SBMG_State_UT)

names(pmay_g.data) <- paste0("PMAYG_", names(pmay_g.data))
pmay_g.data <- pmay_g.data %>% mutate(Year = as.numeric(substr(PMAYG_Fiscal.Year, 1, 4))) %>% 
  rename(State_Name = PMAYG_State_UT)

names(pmgsy.data) <- paste0("PMGSY_", names(pmgsy.data))
pmgsy.data <- pmgsy.data %>% mutate(Year = as.numeric(substr(PMGSY_Financial.Year, 1, 4))) %>% 
  rename(State_Name = PMGSY_State)


tcpd.ae.data <- tcpd.ae.data %>% filter(Year >= 2015 & Year <= 2022) %>%
  mutate(State_Name = str_replace(State_Name, "_", " "))


grouped_ae_smbu <- full_join(tcpd.ae.data, sbm_u.data, by = c("State_Name", "Year"))

grouped_ae_sbmu_sbmg <- full_join(grouped_ae_smbu, sbm_g.data, by = c("State_Name", "Year"))

grouped_ae_sbmu_sbmg_pmayg <- full_join(grouped_ae_sbmu_sbmg, pmay_g.data, by = c("State_Name", "Year"))

tcpd.ae.schemes.data <- full_join(grouped_ae_sbmu_sbmg_pmayg, pmgsy.data, by = c("State_Name", "Year"))

tcpd.ae.schemes.data <- tcpd.ae.schemes.data %>% group_by(State_Name) %>% arrange(Year, .by_group = TRUE)

# write_xlsx(tcpd.ae.schemes.data, "D:/Raghav/Ashoka/Major Courses/Sem 6/Data Science/Final Project/tcpd.ae.schemes.xlsx")


## Taking PMGSY: 
# In the TCPD Dataset, create a variable Election Year = 1 for all observations.
# Merge TCPD Dataset with All other datasets and carry out the steps done above to get in into desrired order.
# Checking ratio of net unspent balance over total balance available: 
  ## Create a variable = Net Unspent Balance Available / Total Funds Available 
# Group by State and Year, average the ratio for each election year to get one observation for the election year
# Summarise the ratios for each State-Year
#Plot a line of best fit with Ratios on the Y-axis, Year on the X-Axis, and see for Andhra Pradesh for the trends look

ratio_table <- tcpd.ae.schemes.data %>% 
  mutate(Unspent_Ratio = PMGSY_Net.Unspent.Balance.Available / PMGSY_Total.Funds.Available) %>% 
  mutate(Is_Election_Year = ifelse(!is.na(Assembly_No), 1, 0)) %>% 
  filter(Poll_No == 0 | is.na(Poll_No)) %>% 
  group_by(State_Name, Year, Is_Election_Year) %>% summarise(UnspentAvg = mean(Unspent_Ratio))

ratio_table


### making a test change just to make sure the git pushing is working lol

### making another test change








