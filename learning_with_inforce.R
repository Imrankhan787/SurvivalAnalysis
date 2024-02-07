

library(tidyverse)
library(countrycode)
library(haven)
library(lubridate)
library(zoo)
library(dplyr)


gdp_FDI_data <- read_csv(file.choose())

gdp_FDI_data <- gdp_FDI_data %>%
                dplyr::select(Time, `Country Name`, `Country Code`, 
                "Foreign direct investment, net inflows (% of GDP) [BX.KLT.DINV.WD.GD.ZS]",
                "GDP per capita (constant 2015 US$) [NY.GDP.PCAP.KD]" )%>%
                 rename("FDI_inflows" = `Foreign direct investment, net inflows (% of GDP) [BX.KLT.DINV.WD.GD.ZS]`,
                        "GDP_pcapita" = `GDP per capita (constant 2015 US$) [NY.GDP.PCAP.KD]`)%>%
                mutate(FDI_inflows = as.double(FDI_inflows),
                       GDP_pcapita  = as.double(GDP_pcapita))

gdp_FDI_cow <-  gdp_FDI_data %>%
                mutate(cow_code = countrycode(sourcevar = `Country Code` ,
                                origin = "iso3c",
                                destination = "cown"))%>%
                filter(!is.na(cow_code))                                     ## remove all the countries for which there is no country code available

gdp_FDI_cow <- gdp_FDI_cow %>%
              rename("Country_isocode" = `Country Code`,
                      "Country_Name"  = `Country Name`)

#write_dta(gdp_FDI_cow, "gdp_FDI_cow_Professor.dta")


##*****************************************************************************##
##************************Learning variable************************************##

learning_variable <- read_dta(file.choose())

learning_year <-learning_variable %>%
                dplyr::select(year, cowcode,partcow, stb)%>%
                filter(cowcode ==2, partcow == 20)%>%
                dplyr::select(year,stb)

#write_dta(learning_year, "learning_year_Professor.dta")




library(tidyverse)
library(readxl)
library(lubridate)


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

bits_data <- read_csv(file.choose())


bit_dataset <- filter(bits_data, !is.na(NO.))

bit_dataset <- filter(bit_dataset, str_detect(bit_dataset$`SHORT TITLE`, "BIT"))

##*************Terminated agreements************************************##
##**********************************************************************##
##****************Seek guidance from Professor Richman******************##

bit_dataset<- filter(bit_dataset, STATUS == "In force")

## Whether the terminated agreements were negotiated or not? The code below answer this question
## Comment out the code below to answer this question
# terminated_parties <- filter(bit_dataset, STATUS == "Terminated")$PARTIES

#for (ter in sort(terminated_parties)){
#  print(filter(bit_dataset, str_detect(bit_dataset$PARTIES, pattern = ter)))
#}

##***********************************************************************##
##***********************************************************************##

#bit_dataset <- mutate(bit_dataset, Year = str_extract(bit_dataset$`SHORT TITLE`, pattern = "\\d{4}"))

bit_dataset <- mutate(bit_dataset, Country_1 = str_extract(bit_dataset$`SHORT TITLE`, pattern = "[ A-Za-z]*"),
                      Country_2 = str_extract(bit_dataset$`SHORT TITLE`, pattern = "-[ A-Za-z, '-.]*BIT"))

bit_dataset <- mutate(bit_dataset, Country_2 = str_remove(bit_dataset$Country_2, pattern = "BIT"))

bit_dataset <- mutate(bit_dataset, Country_2 = str_remove(bit_dataset$Country_2, pattern = "- "))

bit_dataset <- mutate(bit_dataset, Country_1 = str_trim(bit_dataset$Country_1, side = "both"),
                      Country_2 = str_trim(bit_dataset$Country_2, side = "both"))

bit_dataset <- mutate(bit_dataset, Year = year((bit_dataset$`DATE OF ENTRY INTO FORCE`)))

smaller_dyadic_dataset <- select(bit_dataset, `SHORT TITLE`, PARTIES, Country_1, Country_2, Year)

##******************************************Guinea-Bissau****************************************##
##***********************************************************************************************##

filtered_rows <- filter(smaller_dyadic_dataset, str_detect(smaller_dyadic_dataset$`SHORT TITLE`, pattern = "Guinea-Bissau" ))

Problemetic_row_indexes <- which(smaller_dyadic_dataset$Country_2 %in% filtered_rows$Country_2)

smaller_dyadic_dataset$Country_2[Problemetic_row_indexes] <- "Portugal"

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


#*******Replacing "C" with "Côte d'Ivoire"*******************
Cote_rows <- filter(smaller_dyadic_dataset, str_detect(smaller_dyadic_dataset$Country_1, pattern = "\\bC\\b"))

Cote_indexes <- which(smaller_dyadic_dataset$Country_1 %in% Cote_rows$Country_1)

smaller_dyadic_dataset$Country_1[Cote_indexes] <- "Côte d'Ivoire"
##**********Missing country 2 names*****************************
which(is.na(smaller_dyadic_dataset$Country_2))
smaller_dyadic_dataset[which(is.na(smaller_dyadic_dataset$Country_2)),]

smaller_dyadic_dataset$Country_2[91] <- "Côte d'Ivoire"  ## It can become tricky later on. Keep an eye.
smaller_dyadic_dataset$Country_2[c(2001,2002)] <- "BLEU"        ## It can become tricky later on. Keep an eye.

##**************************************************************
##**************************************************************
enforced_bits_agreement <- smaller_dyadic_dataset %>%
                          select(Country_1, Country_2, Year)%>%
                          mutate(country_1cow = countrycode(Country_1, origin = 'country.name', destination = 'cown'),
                                country_2cow = countrycode(Country_2, origin = 'country.name', destination = 'cown'))%>%
                          filter(!is.na(country_1cow), !is.na(country_2cow) ) %>%
                          mutate(Bit_agreement = 1)

##********Convert dyadic dataset into panel dataset**************************
##***************************************************************************
enforced_bits_agreement <- enforced_bits_agreement %>%
                           mutate(country_1bits = Bit_agreement,
                           country_2bits = Bit_agreement)


first_country_bits <- enforced_bits_agreement %>%
                      group_by(country_1cow, Year)%>%
                      summarise(total_first= sum(Bit_agreement))%>%
                       ungroup()

second_country_bits <- enforced_bits_agreement %>%
                       group_by(country_2cow, Year)%>%
                       summarise(total_second= sum(Bit_agreement))%>%
                       ungroup()

Panel_dataset_bits <- full_join(first_country_bits, second_country_bits, by = c("country_1cow" = "country_2cow", "Year"= "Year"))

Panel_dataset_bits <- Panel_dataset_bits %>%
                      mutate(total_first = ifelse(is.na(total_first), 0, total_first),
                             total_second = ifelse(is.na(total_second), 0, total_second))

Panel_dataset_bits <- Panel_dataset_bits %>%
                      mutate(Grand_total = total_first + total_second)


Panel_dataset_bits <- Panel_dataset_bits %>%
                      select(country_1cow, Year, Grand_total)%>%
                      rename("country_cown" = "country_1cow",
                      "Total_bits" = "Grand_total")

full_panel <- Panel_dataset_bits %>%
               group_by(country_cown)%>%
               expand(Year = full_seq(c(1959, 2023),1))%>%
               ungroup()

Bits_in_panel <- left_join(full_panel, Panel_dataset_bits, by = c("country_cown", "Year") )%>%
                 mutate(Total_bits = ifelse(is.na(Total_bits),0, Total_bits))

gdp_FDI_cow$Time = as.double(gdp_FDI_cow$Time)


## Total bits gives you total bits per year

Bits_gdp_FDI_panel <- left_join(Bits_in_panel, gdp_FDI_cow, by = c("country_cown" = "cow_code", "Year" = "Time") )

Bits_gdp_FDI_panel <- group_by(Bits_gdp_FDI_panel, country_cown)%>%
                      mutate(Total_bits = cumsum(Total_bits))%>%
                      ungroup()

##*********************************************************************************************
##*********************************************************************************************
panel_learning_dataset <- Bits_gdp_FDI_panel %>%
                          group_by(country_cown)%>%
                          mutate(rollinnMeanpast5yrs_hFDI   = lag(rollmean(FDI_inflows, 5, na.pad=TRUE, align = "right"),1),
                          rollingMeanpast5yrs_hgdppc = lag(rollmean(GDP_pcapita, 5, na.pad=TRUE, align = "right"),1),
                          rollingMean5yrs_bits = lag(rollmean(Total_bits, 5, na.pad=TRUE, align = "right"),1))%>%
                          ungroup()


panel_learning_dataset1 <- panel_learning_dataset %>%
  filter(!is.na(rollinnMeanpast5yrs_hFDI), !is.na(rollingMean5yrs_bits)) 

all_years <-sort(unique(panel_learning_dataset1$Year))


grouped_df <-  group_split(panel_learning_dataset1, Year, .keep = TRUE)

model_list <- map(grouped_df, function(x) lm(scale(x$rollinnMeanpast5yrs_hFDI) ~ scale(x$rollingMean5yrs_bits) + scale(x$rollingMeanpast5yrs_hgdppc), data = x))

learning_coefficient <- map_dbl(model_list, function(x) summary(x)$coefficients[-1][1])

learning_df = data.frame(Year = all_years, learning = learning_coefficient)

bits_allvar <- read_dta(file.choose())

bits_dataset <- left_join(bits_allvar,learning_df, by = c("Year" = "Year" ))


write_dta(bits_dataset, "bits_30dec.dta")

###********************DONOT DO IT AGAIN AND AGAIN***********************
###**********************************************************************
###**********************************************************************

bits_dataset_7thJan <- read_dta(file.choose())

colnames(bits_dataset_7thJan)
bits_dataset_7thJan$Cum_bits_byYear

bits_dataset_7thJan <-bits_dataset_7thJan %>%
                     mutate(Cum_bits_byYear = Cum_bits_byYear/100)

write_dta(bits_dataset_7thJan, "bits_dataset_7thJan.dta")








