
#install.packages("tidyverse")
library(tidyverse)
#install.packages("readxl")
library(readxl)
#install.packages("countrycode")
library(countrycode)
library(survival)
library(haven)
#install.packages("zoo")
library(zoo)


## Comment Section:
# Points to ponder

# Terminated agreement replaced with another agreement
# Multilateral column, whether country-dyad is part of any multileral episode
# Multiple dependent variables incorporating different choices
# Independent variable included the trade flows
# Independent variable included ratios of GDPs
# Independent variable whether the country-dyad has Bit in the T-1 period. Lag term for dyad.
# Cumulative agreements column


## Upload the Bilateral investment treaty dataset(.xlsx file) scraped from https://investmentpolicy.unctad.org/international-investment-agreements 
bit_dataset <- read_xlsx(file.choose())

bit_dataset <- filter(bit_dataset, !is.na(NO.))

bit_dataset <- filter(bit_dataset, str_detect(bit_dataset$`SHORT TITLE`, "BIT"))

##*************Terminated agreements************************************##
##**********************************************************************##
##****************Seek guidance from Professor Richman******************##

bit_dataset<- filter(bit_dataset, STATUS !="Terminated")

## Whether the terminated agreements were negotiated or not? The code below answer this question
## Comment out the code below to answer this question
# terminated_parties <- filter(bit_dataset, STATUS == "Terminated")$PARTIES

#for (ter in sort(terminated_parties)){
#  print(filter(bit_dataset, str_detect(bit_dataset$PARTIES, pattern = ter)))
#}

##***********************************************************************##
##***********************************************************************##

bit_dataset <- mutate(bit_dataset, Year = str_extract(bit_dataset$`SHORT TITLE`, pattern = "\\d{4}"))

bit_dataset <- mutate(bit_dataset, Country_1 = str_extract(bit_dataset$`SHORT TITLE`, pattern = "[ A-Za-z]*"),
                      Country_2 = str_extract(bit_dataset$`SHORT TITLE`, pattern = "-[ A-Za-z, '-.]*BIT"))

bit_dataset <- mutate(bit_dataset, Country_2 = str_remove(bit_dataset$Country_2, pattern = "BIT"))

bit_dataset <- mutate(bit_dataset, Country_2 = str_remove(bit_dataset$Country_2, pattern = "- "))

bit_dataset <- mutate(bit_dataset, Country_1 = str_trim(bit_dataset$Country_1, side = "both"),
                      Country_2 = str_trim(bit_dataset$Country_2, side = "both"))

smaller_dyadic_dataset <- select(bit_dataset, `SHORT TITLE`, PARTIES, Country_1, Country_2, Year)

##******************************************Guinea-Bissau****************************************##
##***********************************************************************************************##

filtered_rows <- filter(smaller_dyadic_dataset, str_detect(smaller_dyadic_dataset$`SHORT TITLE`, pattern = "Guinea-Bissau" ))

Problemetic_row_indexes <- which(smaller_dyadic_dataset$Country_2 %in% filtered_rows$Country_2)

smaller_dyadic_dataset$Country_2[c(38, 186,2501)] <-c("United Arab Emirates","Morocco", "Portugal")

#*************************************************************************************************##
#***************************************BLEU******************************************************##

BLUE_firstParty <- filter(smaller_dyadic_dataset, str_detect(smaller_dyadic_dataset$`SHORT TITLE`, pattern = "^BLEU" ))

Blue_indexes_first_party <- which(smaller_dyadic_dataset$`SHORT TITLE` %in% BLUE_firstParty$`SHORT TITLE`)

BLUE_2ndParty <-  str_remove(BLUE_firstParty$`SHORT TITLE`, pattern = "BLEU \\(Belgium-Luxembourg Economic Union\\) -")

BLUE_2ndParty <- str_remove(BLUE_2ndParty, pattern = " BIT.*")

smaller_dyadic_dataset$Country_2[Blue_indexes_first_party] <- BLUE_2ndParty

smaller_dyadic_dataset$Country_2 <- str_replace(smaller_dyadic_dataset$Country_2, pattern = "BLEU \\(Belgium-Luxembourg Economic Union\\)", replacement = "BLEU")

##***********************************************************************************************************##
##*************************************Miscellaneous cleaning************************************************##

smaller_dyadic_dataset$Country_2 <- str_replace(smaller_dyadic_dataset$Country_2, pattern = "-Russian Federation", replacement = "Russian Federation")

smaller_dyadic_dataset$Country_2 <- str_trim(smaller_dyadic_dataset$Country_2, side = "both")
unique(smaller_dyadic_dataset$Year) ## Mali - South Africa BIT does not have a year

#******Replacing "T" with "Turkey"******************
#***************************************************
unique(smaller_dyadic_dataset$Country_1) ##"T" stands for Turkey

Turkey_row <- filter(smaller_dyadic_dataset, str_detect(smaller_dyadic_dataset$Country_1, pattern = "\\bT\\b"))
Turkey_index <- which(smaller_dyadic_dataset$Country_1 %in% Turkey_row$Country_1)
smaller_dyadic_dataset$Country_1[10] <-"Turkey"

#*******Replacing "C" with "C?te d'Ivoire"*******************
Cote_rows <- filter(smaller_dyadic_dataset, str_detect(smaller_dyadic_dataset$Country_1, pattern = "\\bC\\b"))

Cote_indexes <- which(smaller_dyadic_dataset$Country_1 %in% Cote_rows$Country_1)

smaller_dyadic_dataset$Country_1[Cote_indexes] <- "C?te d'Ivoire"
##**********Missing country 2 names*****************************
which(is.na(smaller_dyadic_dataset$Country_2))
smaller_dyadic_dataset[which(is.na(smaller_dyadic_dataset$Country_2)),]

smaller_dyadic_dataset$Country_2[c(204,987)] <- "C?te d'Ivoire"  ## It can become tricky later on. Keep an eye.
smaller_dyadic_dataset$Country_2[c(2597,2598)] <- "BLEU"        ## It can become tricky later on. Keep an eye.
#************************************************************************************************************##
#************************************************************************************************************##

Cum_bit_agreements <- smaller_dyadic_dataset %>%
  arrange(Year) %>%
  mutate(Bit_agreement = 1,
         Total_bit_agreement = cumsum(Bit_agreement))

Cum_bit_agreements$Year <- as.double(Cum_bit_agreements$Year)

Cum_bit_agreements <- select(Cum_bit_agreements, - Total_bit_agreement)

#**************************************************************************************************************##
#**************************************************************************************************************##

all_countries <- unique(c(unique(smaller_dyadic_dataset$Country_1), unique(smaller_dyadic_dataset$Country_2)))

ccode <- countrycode(sourcevar = all_countries, origin = "country.name", destination = "cown")
all_countries[is.na(ccode)]  

all_countries <- all_countries[!duplicated(ccode)]  # some same countries exist with two or more different names plus six countries with no cow


# Create dyadic combinations
dyadic_combinations <- expand.grid(Country_1 = all_countries, Country_2 = all_countries)

# Filter out combinations where Country1 is the same as Country2 to avoid duplicates
dyadic_combinations <- dyadic_combinations[dyadic_combinations$Country_1 != dyadic_combinations$Country_2, ]

# Sort the columns in each row to ensure order doesn't affect uniqueness
dyadic_combinations <- t(apply(dyadic_combinations, 1, function(x) sort(x)))

# Remove duplicates
dyadic_combinations <- unique(dyadic_combinations)

# Reset row names
row.names(dyadic_combinations) <- NULL

dyadic_combinations <- as.data.frame(dyadic_combinations)

colnames(dyadic_combinations) <- c("Country_1", "Country_2")


dyad_year_dataset <- group_by(dyadic_combinations, Country_1, Country_2) %>%
                     expand(Year = full_seq(c(1958, 2023),1))%>%
                     ungroup()

##*Be careful about the rows in the merged dataset. Why the rows in merged dataset are greater than the rows in left dataset?
##*Are there any duplicate rows in the two dataset? I will carry out exploration later.
dyad_year_dataset_merged <- left_join( dyad_year_dataset, Cum_bit_agreements, by = c("Country_1", "Country_2", "Year"))

dyad_year_dataset_merged  <- group_by(dyad_year_dataset_merged, Country_1, Country_2)%>%
                             fill(Bit_agreement, .direction = "down")%>%
                             ungroup() 

dyad_year_dataset_merged  <- replace_na(dyad_year_dataset_merged, replace = list(Bit_agreement = 0))

dyad_year_dataset_merged <- group_by(dyad_year_dataset_merged, Year) %>%
                            mutate(Cum_bits_byYear = sum(Bit_agreement,na.rm = T))%>%
                             ungroup()

ccode1 <- countrycode(sourcevar = all_countries, origin = "country.name", destination = "cown")

df_countrynmae_cown <- data.frame(ccode1, all_countries)
                      colnames(df_countrynmae_cown) <- c("ccode1", "Country_names")

dyad_year_dataset_merged <- left_join(dyad_year_dataset_merged, df_countrynmae_cown, by = c("Country_1" = "Country_names"))%>%
                            rename("ccode_1" = "ccode1")%>%
                            left_join(df_countrynmae_cown, by = c("Country_2" = "Country_names"))%>%
                            rename("ccode_2" = "ccode1")%>%
                            select(Country_1, Country_2, Year, ccode_1, ccode_2, Bit_agreement,Cum_bits_byYear)

dyad_year_dataset_merged <- distinct(dyad_year_dataset_merged, Country_1, Country_2, Year, .keep_all = T)

##Remove Hong Kong. There is no cow coding for Hong Kong

dyad_year_dataset_merged <- filter(dyad_year_dataset_merged, !is.na(ccode_1), !is.na(ccode_2))

##******************************************************************************************##
##******************When dyad was at risk of signing agreement******************************##
##******************************************************************************************
world_data <- read_dta(file.choose())

country_names <- unique(world_data$location)

year_independence<- world_data$indy

cow   <- countrycode(sourcevar = country_names, origin = "country.name", destination = "cown")

df_names_year_cow <- as_tibble(data.frame(country_names, cow, year_independence))

filter(df_names_year_cow, is.na(year_independence))

## Independence year for China, Japan, and Ethiopia was missing

df_names_year_cow <- df_names_year_cow %>%
  mutate(year_independence = case_when( (country_names == "China" & is.na(year_independence)) ~ 1949,
                                        (country_names == "Ethiopia" & is.na(year_independence))~1941,
                                        (country_names == "Japan" & is.na(year_independence))~1952,
                                        TRUE~year_independence))

df_names_year_cow <- filter(df_names_year_cow, !is.na(cow))

unique(dyad_year_dataset_merged$ccode_2)[!unique(dyad_year_dataset_merged$ccode_2) %in% (df_names_year_cow$cow)] ## missing observations for Montenegro, Yougoslavia, Serbia and South Sudan


add_rows <- data.frame(country_names = c("Timor-Leste"), cow = c(860), year_independence = c(2002) )

df_names_year_cow <- rbind(df_names_year_cow, add_rows)

dyad_year_dataset_merged <- left_join(dyad_year_dataset_merged, df_names_year_cow, by = c("ccode_1"  = "cow"))%>%
                            rename("year_independence_1" = "year_independence")%>%
                            left_join(df_names_year_cow, by = c("ccode_2"  = "cow"))%>%
                            rename("year_independence_2" = "year_independence")%>%
                            select(-c("country_names.x", "country_names.y"  ))

dyad_year_dataset_merged <- dyad_year_dataset_merged %>%
                            mutate(year_greater = ifelse(year_independence_1 > year_independence_2, year_independence_1, year_independence_2),
                                   at_risk      = ifelse(year_greater > 1958, year_greater, 1958) )

at_risk_year <- dyad_year_dataset_merged %>%
                group_by(Country_1, Country_2)%>%
                summarise(first_value = first(at_risk),
                          Year = first_value)%>%
                ungroup()

dyad_year_dataset_merged  <- left_join(dyad_year_dataset_merged, at_risk_year, by = c("Country_1" = "Country_1","Country_2" = "Country_2", "Year" = "Year"))%>%
                              group_by(Country_1, Country_2)%>%
                              fill(first_value, .direction = "down")%>%
                              ungroup()%>%
                              filter(!is.na(first_value))%>%
                              select(-c("year_independence_1","year_independence_2","year_greater","at_risk","first_value"))%>%
                              left_join(at_risk_year, by = c("Country_1" = "Country_1", "Country_2" = "Country_2", "Year" = "Year"))
                   

##*************************Load Power_trade_imbalance dataset********************************##
##*******************************************************************************************##

power_trade_imblance <- read_csv(file.choose())

power_trade_imblance <- power_trade_imblance %>% 
  select(c("country.x", "year", "gdppc"))

dyad_year_dataset_merged <- left_join(dyad_year_dataset_merged, power_trade_imblance, by = c("Country_1" = "country.x", "Year" = "year"))%>%
  rename(gdppc_1 = gdppc)%>%
  left_join(power_trade_imblance, by = c("Country_2" = "country.x", "Year" = "year"))%>%
  rename(gdppc_2 = gdppc)



## This code is used to include all the rows until the event occurs.

dyad_year_dataset_merged  <- dyad_year_dataset_merged %>%
  group_by(Country_1, Country_2)%>%
  mutate(cum_bits = cumsum(Bit_agreement))%>%
  filter(cum_bits <= 1)%>%
  ungroup()%>%
  select(-cum_bits)





##**************************************************************************
##**************Trade dyadic data*******************************************

dyadic_trade <- read_csv(file.choose(), na = "-9")

dyadic_trade <- dyadic_trade %>%
  select(c("ccode1", "ccode2","year", "importer1", "importer2", "flow1","flow2" ))

dyad_year_dataset_merged <- left_join(dyad_year_dataset_merged, dyadic_trade, by = c("ccode_1" = "ccode1","ccode_2" = "ccode2","Year" = "year"))%>%
                            left_join(dyadic_trade, by = c("ccode_2" = "ccode1","ccode_1" = "ccode2","Year" = "year"))%>%
                            mutate(flow1.x = ifelse(is.na(flow1.x) & !is.na(flow1.y), flow2.y, flow1.x),
                                   flow2.x  = ifelse(is.na(flow2.x) & !is.na(flow2.y), flow1.y, flow2.x ))%>%
                            select(c("Country_1","Country_2", "Year", "ccode_1", "ccode_2","Bit_agreement", "Cum_bits_byYear", "flow1.x", "flow2.x","gdppc_1","gdppc_2","first_value"))%>%
                            rename("flow1" = "flow1.x", "flow2" = "flow2.x")%>%
                            mutate(trade_volume = flow1 + flow2)

dyad_year_dataset_merged <-   dyad_year_dataset_merged %>%
  group_by(Country_1, Country_2) %>%
  mutate(Lag_bit_agreement = lag(Bit_agreement,1)) %>%
  ungroup()


dyad_year_dataset_merged <- dyad_year_dataset_merged %>%
                            group_by(Country_1, Country_2) %>%
                            mutate(Lag_Year = Year -1) %>%
                            ungroup()

##******************************************************************************
##*******************************GDP_ratio**************************************
dyad_year_dataset_merged <- dyad_year_dataset_merged %>%
  mutate(diffgdppc = gdppc_1 - gdppc_2,
         host_code     = ifelse(gdppc_1 < gdppc_2, ccode_1, ccode_2),
         home_code     = ifelse(gdppc_1 > gdppc_2, ccode_1, ccode_2) )

dyad_year_dataset_merged <- dyad_year_dataset_merged %>%
  mutate(GDP_ratio = ifelse(gdppc_1 > gdppc_2, gdppc_1/gdppc_2, gdppc_2/gdppc_1))

dyad_year_dataset_merged <- dyad_year_dataset_merged %>%
  mutate(GDP_ratio = ifelse(is.infinite(GDP_ratio), NA, GDP_ratio))


dyad_year_dataset_merged <- dyad_year_dataset_merged %>%
  mutate(gdppc_host = ifelse(gdppc_1 < gdppc_2, gdppc_1, gdppc_2),
         gdppc_home = ifelse(gdppc_1 > gdppc_2, gdppc_1, gdppc_2))


##******************************************************************************
##******************************************************************************

##******************************Further merging********************************************##
##*****************************************************************************************##
library(tidyverse)
library(countrycode)
library(readxl)


##*************************************Current account balance*********************************##
##*********************************************************************************************##
##*********************************************************************************************##

current_account_balance <- read_csv(file.choose(), skip = 4)



Current_account_balance_longer <- pivot_longer(data = current_account_balance, 
                                               cols = -c("Country Name", "Country Code", "Indicator Name", "Indicator Code"),
                                               names_to = "Year",
                                               values_to = "Current_accountBalance")


Country_names <- unique(Current_account_balance_longer$`Country Name`)

Country_code <- unique(current_account_balance$`Country Code`)

correlates_of_war_coding <- countrycode(sourcevar = Country_code,
                                        origin = "iso3c",
                                        destination = "cown")

df_contry_code_names_cown <- data.frame(Country_names, Country_code, correlates_of_war_coding)


Current_account_balance_longer <- left_join(Current_account_balance_longer, df_contry_code_names_cown, by = c("Country Name" = "Country_names", "Country Code" = "Country_code" )) %>% 
  select(`Country Name`,`Country Code`,correlates_of_war_coding, Year,Current_accountBalance ) %>%
  filter(!is.na(correlates_of_war_coding))

Current_account_balance_longer$Year <- as.double(Current_account_balance_longer$Year)

Current_account_balance_longer <- filter(Current_account_balance_longer, Year <=2022)


dyad_year_dataset_merged <- left_join(dyad_year_dataset_merged, Current_account_balance_longer, by = c("ccode_1" = "correlates_of_war_coding",  "Year" = "Year"))%>%
                            select(-c("Country Code" , "Country Name"))%>%
                            rename("Current_accountBalance_1" = "Current_accountBalance")%>%
                            left_join(Current_account_balance_longer, by = c("ccode_1" = "correlates_of_war_coding",  "Year" = "Year"))%>%
                            select(-c("Country Code" , "Country Name"))%>%
                            rename("Current_accountBalance_2" = "Current_accountBalance")


##********************************************************************************##
##****************************External stock**************************************##
##********************************************************************************##

external_debt <- read_xls(file.choose(), skip = 3)



external_debt_longer <- pivot_longer(data = external_debt, 
                                     cols = -c("Country Name", "Country Code", "Indicator Name", "Indicator Code"),
                                     names_to = "Year",
                                     values_to = "External_debt_stock")



country_names_debt <- unique(external_debt_longer$`Country Name`)

country_code_debt  <- unique(external_debt_longer$`Country Code`)

correlates_of_war_coding_debt <-countrycode(sourcevar = country_code_debt,
                                            origin = "iso3c",
                                            destination = "cown")

df_contry_code_names_cown_debt <- data.frame(country_names_debt, country_code_debt, correlates_of_war_coding_debt)


external_debt_longer <- left_join(external_debt_longer, df_contry_code_names_cown_debt, by = c("Country Name" = "country_names_debt", "Country Code" = "country_code_debt")) %>%
  select(`Country Name`, `Country Code`, correlates_of_war_coding_debt, Year,External_debt_stock ) %>%
  filter(!is.na(correlates_of_war_coding_debt))

external_debt_longer$Year <- as.double(external_debt_longer$Year)

dyad_year_dataset_merged <- left_join(dyad_year_dataset_merged, external_debt_longer, by = c("Country_1" = "Country Name", "Year" = "Year")) %>%
  select(-c("Country Code", "correlates_of_war_coding_debt"))%>%
  rename("External_debt_stock_1" = "External_debt_stock")%>%
  left_join(external_debt_longer, by = c("Country_2" = "Country Name", "Year" = "Year"))%>%
  select(-c("Country Code", "correlates_of_war_coding_debt"))%>%
  rename("External_debt_stock_2" = "External_debt_stock")


##*********************************************Short term external debt****************************************##
##*************************************************************************************************************##
##*************************************************************************************************************##

shortTermDebtToExternalDebt <- read_csv(file.choose(), skip = 4)

shortTermDebtToExternalDebt_longer <- pivot_longer(data = shortTermDebtToExternalDebt, 
                                                   cols = -c("Country Name", "Country Code", "Indicator Name", "Indicator Code"),
                                                   names_to = "Year",
                                                   values_to = "ShortTermDebt(%ofExternalDebt)")


country_names_edebt <- unique(shortTermDebtToExternalDebt_longer$`Country Name`)

country_code_edebt  <- unique(shortTermDebtToExternalDebt_longer$`Country Code`)

correlates_of_war_coding_edebt <-countrycode(sourcevar = country_code_edebt,
                                             origin = "iso3c",
                                             destination = "cown")

df_contry_code_names_cown_edebt <- data.frame(country_names_edebt, country_code_edebt, correlates_of_war_coding_edebt)



shortTermDebtToExternalDebt_longer <- left_join(shortTermDebtToExternalDebt_longer, df_contry_code_names_cown_edebt, by = c("Country Name" = "country_names_edebt", "Country Code" = "country_code_edebt" )) %>%
  select(`Country Name`, `Country Code`, correlates_of_war_coding_edebt, Year, `ShortTermDebt(%ofExternalDebt)` ) %>%
  filter(!is.na(correlates_of_war_coding_edebt))

shortTermDebtToExternalDebt_longer$Year <- as.double(shortTermDebtToExternalDebt_longer$Year)

dyad_year_dataset_merged <- left_join(dyad_year_dataset_merged, shortTermDebtToExternalDebt_longer, by = c("Country_1" = "Country Name", "Year" = "Year")) %>%
  select(-c("Country Code", "correlates_of_war_coding_edebt"))%>%
  rename("ShortTermDebt(%ofExternalDebt)_1" = "ShortTermDebt(%ofExternalDebt)")%>%
  left_join(shortTermDebtToExternalDebt_longer, by = c("Country_2" = "Country Name", "Year" = "Year"))%>%
  select(-c("Country Code", "correlates_of_war_coding_edebt"))%>%
  rename("ShortTermDebt(%ofExternalDebt)_2" = "ShortTermDebt(%ofExternalDebt)")

##**********************************Imf credit*****************************##
##*************************************************************************##

Imf_credit <- read_csv(file.choose(), skip =4 )

Imf_credit_longer <- pivot_longer(data = Imf_credit, 
                                  cols = -c("Country Name", "Country Code", "Indicator Name", "Indicator Code"),
                                  names_to = "Year",
                                  values_to = "Imf_credit_US$")


country_names_imfcredit <- unique(Imf_credit_longer$`Country Name`)
country_codes_imfcredit <- unique(Imf_credit_longer$`Country Code`)

correlates_of_war_coding_Imfcredit <- countrycode(sourcevar = country_codes_imfcredit,
                                                  origin = "iso3c",
                                                  destination = "cown")

df_contry_code_names_cown_imfcredit <- data.frame(country_names_imfcredit,country_codes_imfcredit,correlates_of_war_coding_Imfcredit)

Imf_credit_longer <- left_join(Imf_credit_longer, df_contry_code_names_cown_imfcredit, by = c("Country Name" = "country_names_imfcredit", "Country Code" = "country_codes_imfcredit")) %>%
  select(`Country Name`,`Country Code`, correlates_of_war_coding_Imfcredit, Year, `Imf_credit_US$`) %>%
  filter(!is.na(correlates_of_war_coding_Imfcredit))

Imf_credit_longer$Year <- as.double(Imf_credit_longer$Year)

dyad_year_dataset_merged <- left_join(dyad_year_dataset_merged, Imf_credit_longer, by = c("Country_1" = "Country Name", "Year" = "Year" ))%>%
  select(-c("Country Code", "correlates_of_war_coding_Imfcredit"))%>%
  rename("Imf_credit_US$_1" = "Imf_credit_US$")%>%
  left_join(Imf_credit_longer, by = c("Country_2" = "Country Name", "Year" = "Year" ))%>%
  select(-c("Country Code", "correlates_of_war_coding_Imfcredit"))%>%
  rename("Imf_credit_US$_2" = "Imf_credit_US$")

##*********************Convert into host-home variables*******************************##
##************************************************************************************##
##************************************************************************************##

dyad_year_dataset_merged <- dyad_year_dataset_merged %>%
  mutate(cab_host = ifelse(diffgdppc < 0, Current_accountBalance_1, Current_accountBalance_2),
         cab_home = ifelse(diffgdppc > 0, Current_accountBalance_1, Current_accountBalance_2),
         ext_debt_host = ifelse(diffgdppc < 0, External_debt_stock_1, External_debt_stock_2),
         ext_debt_home = ifelse(diffgdppc > 0,External_debt_stock_1 , External_debt_stock_2),
         shortTermDebt_host = ifelse(diffgdppc < 0,`ShortTermDebt(%ofExternalDebt)_1`,`ShortTermDebt(%ofExternalDebt)_2` ),
         shortTermDebt_home = ifelse(diffgdppc > 0, `ShortTermDebt(%ofExternalDebt)_1`, `ShortTermDebt(%ofExternalDebt)_2`),
         Imfcredit_host = ifelse(diffgdppc < 0, `Imf_credit_US$_1`, `Imf_credit_US$_2`),
         Imfcredit_home = ifelse(diffgdppc > 0, `Imf_credit_US$_1`, `Imf_credit_US$_2`))

##*******************************************FDI_inflows(%GDP)***************************************##
##***************************************************************************************************##

FDI_inflows <- read_csv(file.choose(), skip = 4)

FDI_inflows_longer <- pivot_longer(data = FDI_inflows, 
                                   cols = -c("Country Name", "Country Code", "Indicator Name", "Indicator Code"),
                                   names_to = "Year",
                                   values_to = "FDI_inflows")


country_names_fdi_inflow <- unique(FDI_inflows_longer$`Country Name`)

country_code_fdi_inflow  <- unique(FDI_inflows_longer$`Country Code`)

correlates_of_war_coding_fdi_inflow <-countrycode(sourcevar = country_code_fdi_inflow,
                                                  origin = "iso3c",
                                                  destination = "cown")

df_contry_code_names_cown_fdi_inflow <- data.frame(country_names_fdi_inflow,country_code_fdi_inflow , correlates_of_war_coding_fdi_inflow)



FDI_inflows_longer  <- left_join(FDI_inflows_longer, df_contry_code_names_cown_fdi_inflow, by = c("Country Name" = "country_names_fdi_inflow", "Country Code" = "country_code_fdi_inflow" )) %>%
                       select(`Country Name`, `Country Code`,correlates_of_war_coding_fdi_inflow, Year, FDI_inflows  ) %>%
                       filter(!is.na(correlates_of_war_coding_fdi_inflow))

FDI_inflows_longer$Year <- as.double(FDI_inflows_longer$Year)

dyad_year_dataset_merged <- left_join(dyad_year_dataset_merged,FDI_inflows_longer , by = c("ccode_1" = "correlates_of_war_coding_fdi_inflow", "Year" = "Year")) %>%
                             select(-c("Country Code", "Country Name"))%>%
                            rename("FDI_inflows_1" = "FDI_inflows")%>%
                            left_join(FDI_inflows_longer, by = c("ccode_2" = "correlates_of_war_coding_fdi_inflow", "Year" = "Year"))%>%
                            select(-c("Country Code", "Country Name"))%>%
                            rename("FDI_inflows_2" = "FDI_inflows")


##***************************************FDI_outflow(%GDP)******************************************##
##**************************************************************************************************##
##*
FDI_outflow <- read_csv(file.choose(), skip = 4)

FDI_outflows_longer <- pivot_longer(data = FDI_outflow, 
                                    cols = -c("Country Name", "Country Code", "Indicator Name", "Indicator Code"),
                                    names_to = "Year",
                                    values_to = "FDI_outflows")


country_names_fdi_outflow <- unique(FDI_outflows_longer$`Country Name`)

country_code_fdi_outflow  <- unique(FDI_outflows_longer$`Country Code`)

correlates_of_war_coding_fdi_outflow <-countrycode(sourcevar = country_code_fdi_outflow,
                                                   origin = "iso3c",
                                                   destination = "cown")

df_contry_code_names_cown_fdi_outflow <- data.frame(country_names_fdi_outflow,country_code_fdi_outflow , correlates_of_war_coding_fdi_outflow)



FDI_outflows_longer <- left_join(FDI_outflows_longer, df_contry_code_names_cown_fdi_outflow, by = c("Country Name" = "country_names_fdi_outflow", "Country Code" = "country_code_fdi_outflow" )) %>%
                       select(`Country Name`, `Country Code`,correlates_of_war_coding_fdi_outflow, Year, FDI_outflows  ) %>%
                       filter(!is.na(correlates_of_war_coding_fdi_outflow))

FDI_outflows_longer$Year <- as.double(FDI_outflows_longer$Year)

dyad_year_dataset_merged <- left_join(dyad_year_dataset_merged,FDI_outflows_longer , by = c("ccode_1" = "correlates_of_war_coding_fdi_outflow", "Year" = "Year")) %>%
                            select(-c("Country Code", "Country Name"))%>%
                            rename("FDI_outflows_1" = "FDI_outflows")%>%
                            left_join(FDI_outflows_longer, by = c("ccode_2" = "correlates_of_war_coding_fdi_outflow", "Year" = "Year"))%>%
                            select(-c("Country Code", "Country Name"))%>%
                            rename("FDI_outflows_2" = "FDI_outflows")

##*******************************saving_Data*************************************##
##*******************************************************************************##
#write_csv(dyad_year_dataset_merged, "bits_homehost.csv")
##*******************************************************************************##
##*******************************************************************************##

dyad_year_dataset_merged <- dyad_year_dataset_merged %>%
                            mutate(FDI_inflow_host = ifelse(diffgdppc < 0, FDI_inflows_1, FDI_inflows_2),
                            FDI_outflow_home = ifelse(diffgdppc > 0, FDI_outflows_1, FDI_outflows_2))

##*******************************************Common Colonizer*********************************************************##
##********************************************************************************************************************##


common_colonizer <- read_xls(file.choose())


common_colonizer <- common_colonizer %>%
  filter(iso_o != iso_d)%>%
  select(c( "iso_o", "iso_d", "colony","comcol"))

common_colonizer <- common_colonizer %>%
  mutate(cow_1 = countrycode(sourcevar = common_colonizer$iso_o,
                             origin = "iso3c",
                             destination = "cown"),
         cow_2 = countrycode(sourcevar = common_colonizer$iso_d,
                             origin = "iso3c",
                             destination = "cown"))%>%
  filter(!is.na(cow_1),!is.na(cow_2))


dyad_year_dataset_merged <- dyad_year_dataset_merged %>%
  left_join(common_colonizer, by = c("ccode_1" = "cow_1", "ccode_2" = "cow_2"))%>%
  left_join(common_colonizer, by = c("ccode_2" = "cow_1", "ccode_1" = "cow_2"))%>%
  mutate(colony.x = ifelse(!is.na(colony.y)& is.na(colony.x), colony.y, colony.x),
         comcol.x = ifelse(!is.na(comcol.y)& is.na(comcol.x), comcol.y, comcol.x))%>%
  select(-c("iso_o.x", "iso_d.x","iso_o.y","iso_d.y", "colony.y","comcol.y"))%>%
  rename("comcol" = "comcol.x", "colony" = "colony.x")

##*************************Defense Ties*******************************##
##********************************************************************##
##********************************************************************##
alliance_dyad_yearly <- read_csv(file.choose())

alliance_dyad_yearly <- filter(alliance_dyad_yearly,year > 1957) %>%
  select(ccode1, ccode2, year, defense)

alliance_dyad_yearly <- distinct(alliance_dyad_yearly, ccode1, ccode2, year, .keep_all = TRUE)

dyad_year_dataset_merged <- left_join(dyad_year_dataset_merged, alliance_dyad_yearly, by = c("ccode_1" = "ccode1","ccode_2" = "ccode2", "Year" = "year" ))%>%
  left_join(alliance_dyad_yearly, by = c("ccode_2" = "ccode1","ccode_1" = "ccode2", "Year" = "year" ))%>%
  mutate(defense.x = ifelse(!is.na(defense.y) & is.na(defense.x), defense.y, defense.x))%>%
  select(-c("defense.y"))%>%
  rename("defense" = "defense.x")


##*******************************Common Language*************************************************
##***********************************************************************************************

language_data <- read_dta(file.choose())

language_data <- language_data %>%
  filter(iso_o != iso_d)%>%
  select(c( "iso_o", "iso_d", "col"))


language_data <- language_data %>%
  mutate(cow_1 = countrycode(sourcevar = language_data$iso_o,
                             origin = "iso3c",
                             destination = "cown"),
         cow_2 = countrycode(sourcevar = language_data$iso_d,
                             origin = "iso3c",
                             destination = "cown"))%>%
  filter(!is.na(cow_1),!is.na(cow_2))


dyad_year_dataset_merged <- dyad_year_dataset_merged %>%
  left_join(language_data, by = c("ccode_1" = "cow_1", "ccode_2" = "cow_2"))%>%
  left_join(language_data, by = c("ccode_2" = "cow_1", "ccode_1" = "cow_2"))%>%
  mutate(col.x = ifelse(!is.na(col.y)& is.na(col.x), col.y, col.x ))%>%
  select(-c("iso_o.x", "iso_d.x","iso_o.y","iso_d.y", "col.y"))%>%
  rename("common_official_lang" = "col.x")

##***********************************************************************************************
##****************************Annual GDP growth**************************************************

Annual_gdp_growth <- read_csv(file.choose(), skip =4)

Annual_gdp_growth_longer <- pivot_longer(data = Annual_gdp_growth, 
                                         cols = -c("Country Name", "Country Code", "Indicator Name", "Indicator Code"),
                                         names_to = "Year",
                                         values_to = "Gdp_growth")


country_names_gdp_growth <- unique(Annual_gdp_growth_longer$`Country Name`)

country_code_gdp_growth  <- unique(Annual_gdp_growth_longer$`Country Code`)

correlates_of_war_coding_gdp_growth <-countrycode(sourcevar = country_code_gdp_growth,
                                                  origin = "iso3c",
                                                  destination = "cown")

df_contry_code_names_cown_gdp_growth <- data.frame(country_names_gdp_growth,country_code_gdp_growth  , correlates_of_war_coding_gdp_growth)



Annual_gdp_growth_longer <-  left_join(Annual_gdp_growth_longer,df_contry_code_names_cown_gdp_growth , by = c("Country Name" = "country_names_gdp_growth", "Country Code" = "country_code_gdp_growth" )) %>%
  select(`Country Name`, `Country Code`,correlates_of_war_coding_gdp_growth, Year, Gdp_growth  ) %>%
  filter(!is.na(correlates_of_war_coding_gdp_growth))

Annual_gdp_growth_longer$Year <- as.double(Annual_gdp_growth_longer$Year)

dyad_year_dataset_merged <- left_join(dyad_year_dataset_merged,Annual_gdp_growth_longer , by = c("Country_1" = "Country Name", "Year" = "Year")) %>%
  select(-c("Country Code", "correlates_of_war_coding_gdp_growth"))%>%
  rename("Gdp_growth_1"  = "Gdp_growth" )%>%
  left_join(Annual_gdp_growth_longer, by = c("Country_2" = "Country Name", "Year" = "Year"))%>%
  select(-c("Country Code", "correlates_of_war_coding_gdp_growth"))%>%
  rename("Gdp_growth_2"  = "Gdp_growth" )


dyad_year_dataset_merged <- dyad_year_dataset_merged %>%
  mutate(GDP_growth_host = ifelse(diffgdppc < 0, Gdp_growth_1, Gdp_growth_2),
         GDP_growth_home = ifelse(diffgdppc > 0,Gdp_growth_1, Gdp_growth_2 ))



##*******************************************Polity dataset**************************************
##***********************************************************************************************

polity_dataset <- read_csv(file.choose())

polity_dataset <- polity_dataset %>%
  select(country, year, ccode, polity)

dyad_year_dataset_merged <-  left_join(dyad_year_dataset_merged, polity_dataset, by = c("ccode_1"="ccode", "Year" = "year"))%>%
  left_join(polity_dataset, by = c("ccode_2"="ccode", "Year" = "year"))%>%
  rename("polity_1" = "polity.x" , "polity_2" = "polity.y")%>%
  mutate(Polity_host = ifelse(diffgdppc < 0, polity_1, polity_2),
         Polity_home = ifelse(diffgdppc > 0, polity_1, polity_2))
##***********************************************************************************************************
##***********************************************************************************************************
##***********************Variable to retain******************************************************************

dyad_year_dataset_merged <- filter(dyad_year_dataset_merged, !is.na(ccode_1) & !is.na(ccode_2))

dyad_year_dataset_merged <- dyad_year_dataset_merged %>%
  select(-c("flow1","flow2","gdppc_1","gdppc_2","Current_accountBalance_1","Current_accountBalance_2","External_debt_stock_1",
            "External_debt_stock_2",`ShortTermDebt(%ofExternalDebt)_1`, `ShortTermDebt(%ofExternalDebt)_2`, `Imf_credit_US$_1` ,               
            `Imf_credit_US$_2`,"FDI_inflows_1", "FDI_inflows_2", "FDI_outflows_1", "FDI_outflows_2", "Gdp_growth_1", "Gdp_growth_2",
            "country.x", "country.y","polity_1","polity_2"))

dyad_year_dataset_merged <- dyad_year_dataset_merged %>%
  unite("dyad_combine",ccode_1, ccode_2, sep ="", remove = FALSE)


dyad_year_dataset_merged <- dyad_year_dataset_merged %>%
                            mutate(Cum_bits_byYear = Cum_bits_byYear/(length(unique(dyad_combine))))

##***********************************************************************************************************
##***********************************************************************************************************
##***********************Failed multilateral negotiations****************************************************
uruguay_1994 <- read_xlsx(file.choose())

uruguay_1994 <- uruguay_1994 %>%
                mutate(cow_1994 = countrycode(sourcevar = Countries_Marrakesh_agreement, origin = "country.name", destination = "cown"))%>%
                filter(!is.na(cow_1994)) ## remove Hong Kong and Macau No cow coding available for them

Oecd_1998  <- read_xlsx(file.choose())

Oecd_1998 <- Oecd_1998 %>%
            mutate(cow_1998 = countrycode(sourcevar = oecd_mai_agreement, origin = "country.name", destination = "cown"))


complex_df <- tibble(year_cols = c(1994, 1998), multilateral_investment_participants = list(uruguay_1994$cow_1994, Oecd_1998$cow_1998))


#data_1994_1998_inv <- filter(dyad_year_dataset_merged, Year == 1994| Year ==1998)


dyad_year_dataset_merged <- left_join(dyad_year_dataset_merged, complex_df, by = c("Year" ="year_cols"))

##********Checking whether a dyad is engaged in multilateral negotiations********************
##*******************************************************************************************

Country_1_in <- mapply(function(y, x) (y %in% x), dyad_year_dataset_merged$ccode_1, dyad_year_dataset_merged$multilateral_investment_participants )

Country_2_in <- mapply(function(z, x) (z %in% x), dyad_year_dataset_merged$ccode_2, dyad_year_dataset_merged$multilateral_investment_participants )

Both_country_in <- Country_1_in & Country_2_in

Both_country_in <- ifelse(Both_country_in == TRUE, 1 , 0)

dyad_year_dataset_merged <- dyad_year_dataset_merged %>%
                            mutate(is_dyad_in_multilateral = Both_country_in)%>%
                            select(-multilateral_investment_participants)
  

write_dta(dyad_year_dataset_merged, "bits_vars.dta")
##**********************************************************************************************
##**********************************************************************************************
