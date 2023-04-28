library(gapminder)
library(WDI)
library(dplyr)
library(plyr)
library(tidyverse)
library(countrycode)

df_BNP_0<-WDI(
  country = "all",
  indicator = c('gdppc'="NY.GDP.PCAP.PP.KD"),  
  start = 2000,
  end = 2019,
  extra = TRUE,
  cache = NULL,
  latest = NULL,
  language = "en"
)

df_BNP_0 <- df_BNP_0 %>%
  select(country, region, income, iso2c, iso3c, year, gdppc) %>% 
  arrange(iso3c, year)

df_BNP_0 <-  df_BNP_0 %>%
  mutate_all(na_if,"")

df_BNP_0 <- df_BNP_0[!grepl("Aggregates", df_BNP_0$region),]

df_BNP_1 <- df_BNP_0[complete.cases( df_BNP_0$gdppc, df_BNP_0$iso3c),] %>%  
  mutate(year = as.numeric(year)) %>% 
  arrange(year)

df_BNP_1 <- df_BNP_1[order(df_BNP_1$country),]

# Datasett for BNP 2000 

BNP_2000 <- df_BNP_1 %>% 
  arrange(iso3c, year) %>% 
  group_by(iso3c) %>% 
  slice(1) %>%
  ungroup()

BNP_2000 <- BNP_2000 %>%
  select(- year)

BNP_both <- left_join(df_BNP_1,BNP_2000, by=c("country", "iso2c", "iso3c", "region", "income")) 

names(BNP_both)[names(BNP_both) == "gdppc.x"] <- "BNP_per_inn" 
names(BNP_both)[names(BNP_both) == "gdppc.y"] <- "BNP_per_inn_Y0" 

## Utdaning 

utdan_1<-WDI(
  country = "all",
  indicator = c('educ'="BAR.SCHL.15UP"),  
  start = 2000,
  end = 2019,
  extra = TRUE,
  cache = NULL,
  latest = NULL,
  language = "en"
)

utdan_1 <- utdan_1 %>%
  select(country, region, income, iso2c, iso3c, year, educ) %>% 
  arrange(iso3c, year)

utdan_1 <- utdan_1[complete.cases(utdan_1$educ),] %>%  
  arrange(iso3c, year)


utdan_1 <- utdan_1 %>%
  arrange(iso3c, year) %>%
  mutate(educ = as.numeric(educ, na.rm = T)) %>%
  group_by(country) %>%
  mutate(educ_mean = mean(educ)) %>%
  ungroup()

utdan_2 <- utdan_1 %>% 
  select(-c(year, educ))

utdan_2 <- utdan_2[!duplicated(utdan_2[c("iso3c")]), ]  %>% 
  arrange(iso3c)

## joint 

BNI<-WDI(
  country = "all",
  indicator = c('NSY'="NY.ADJ.NNAT.GN.ZS"),  
  start = 2000,
  end = 2019,
  extra = TRUE,
  cache = NULL,
  latest = NULL,
  language = "en"
)

BNI <- BNI %>%
  select(country, region, income, iso2c, iso3c, year, NSY) %>% 
  arrange(iso3c, year)


BNI <- BNI %>%
  arrange(iso3c, year) %>%
  mutate(educ = as.numeric(NSY, na.rm = T)) %>%
  group_by(country) %>%
  mutate(NSY_mean = mean(NSY)) %>%
  ungroup()

BNI_1 <- BNI %>% 
  select(-year)

BNI_1 <- BNI_1[!duplicated(BNI_1[c("iso3c")]), ]  %>% 
  arrange(iso3c)

## arbeid

arbeid<-WDI(
  country = "all",
  indicator = c('arbeid'="JI.TLF.TOTL"),  
  start = 2000,
  end = 2019,
  extra = TRUE,
  cache = NULL,
  latest = NULL,
  language = "en"
)


arbeid <- arbeid %>%
  select(country, region, income, iso2c, year, arbeid) %>%
  arrange(iso2c, year) %>%
  dplyr::rename("iso3c" = "iso2c") %>%
  dplyr::mutate(year = as.numeric(year),
                arbeid=as.numeric(arbeid))

arbeid[arbeid == 0] <- NA

arbeid_1 <- arbeid[complete.cases(arbeid$iso3c, arbeid$arbeid),]

arbeid_1 <- arbeid_1[!duplicated(arbeid_1[c("iso3c", "year")]), ] %>%
  arrange(iso3c, year)

Vekstrate_n = arbeid_1 %>%
  arrange(iso3c, year) %>%
  ddply("iso3c", transform,
        t_ar= c(NA,diff(year)),
        arb_kraft_vekst = c(NA,diff(log(arbeid))))


Vekstrate_n <- Vekstrate_n[complete.cases(Vekstrate_n$t_ar, Vekstrate_n$arb_kraft_vekst),]

Vekstrate_n_1 <- Vekstrate_n %>%
  dplyr::mutate(t_ar = as.numeric(t_ar), 
               arb_kraft_vekst = as.numeric(arb_kraft_vekst)) %>%
  dplyr::mutate(arlig_vekstrate_n=arb_kraft_vekst/t_ar)

Vekstrate_n_1 <- Vekstrate_n_1 %>%
  dplyr::group_by(iso3c) %>%
  dplyr::mutate(vekstrate_n_snitt=mean(arlig_vekstrate_n, na.rm = TRUE)) %>%
  dplyr::ungroup()

Vekstrate_n_1 <- Vekstrate_n_1 %>%
  select(country, iso3c, vekstrate_n_snitt)

Vekstrate_n_1 <- Vekstrate_n_1[!duplicated(Vekstrate_n_1["iso3c"]), ] %>%
  arrange(iso3c)

df_1 <- left_join(BNP_both, utdan_2, by = c("country", "region", "income", "iso3c", "iso2c"))

df_2 <- left_join(df_1, BNI_1, by =c("country", "region", "income", "iso3c", "iso2c"))

df_3 <- left_join(df_2, Vekstrate_n_1, by = "iso3c")


df_last <- df_3 %>%
  dplyr::rename(country=country.x) %>%
  select(country, region, income, iso2c,
         iso3c, year, BNP_per_inn, BNP_per_inn_Y0,
         educ_mean, NSY, NSY_mean, vekstrate_n_snitt)



df_rest0<-WDI(
  country = "all",
  indicator = c('poptot'="SP.POP.TOTL", 
                'investering'="NE.GDI.FTOT.KD.ZG",
                'eksport'="NE.EXP.GNFS.KD.ZG", 
                'naturressurser'="NY.ADJ.DRES.GN.ZS",
                'n'="SP.POP.GROW" ),  
  start = 2000,
  end = 2019,
  extra = TRUE,
  cache = NULL,
  latest = NULL,
  language = "en"
)


df_rest1 <- df_rest0[complete.cases(df_rest0$iso3c),] %>%
  arrange(iso3c)

df_rest2 <- df_rest1[!grepl("Aggregates", df_rest1$region),]

df_rest2 <- df_rest2[order(df_rest2$country),]

df_rest2 <- df_rest2 %>%
  select("country", "region", "income", 
         "iso3c", "iso2c", "year", "poptot",
         "investering",
         "eksport","naturressurser","n" )

Bestemmelsesfaktorer <- left_join(df_last, df_rest2, by = c("country", "region", "income", "iso2c", "iso3c", "year"))  


Bestemmelsesfaktorer <- Bestemmelsesfaktorer[complete.cases(Bestemmelsesfaktorer$iso3c),] %>%
  arrange(iso3c)

Vekst_best <- Bestemmelsesfaktorer %>%
  arrange(iso3c, year) %>%
  filter(!country == c("High income", "Low income", " Lower middle income", "Upper middle income")) %>%
  dplyr::group_by(country) %>%
  ddply("iso3c", transform,
        vekst_BNP_per_inn = c(NA, diff(log(BNP_per_inn)))*100) %>%
  dplyr::mutate(vekst_BNP_per_inn = as.numeric(vekst_BNP_per_inn, na.rm = TRUE)) %>%
  dplyr::mutate(snitt_vekst_BNP_per_inn=mean(vekst_BNP_per_inn, na.rm = TRUE),
                snitt_vekstrate_arlig_invets = mean(investering, na.rm = TRUE),
                snitt_reduk_rate_naturres= mean(naturressurser, na.rm = TRUE),
                snitt_vekstrate_arlig_export = mean(eksport, na.rm = TRUE),
                snitt_befolkningsvekstrate = mean(n, na.rm = TRUE)) %>%
  dplyr::ungroup()


Vekst_best1 <- Vekst_best[complete.cases(Vekst_best$country, Vekst_best$income,
                                         Vekst_best$iso3c, Vekst_best$snitt_vekst_BNP_per_inn,
                                         Vekst_best$BNP_per_inn_Y0, Vekst_best$vekstrate_n_snitt,
                                         Vekst_best$snitt_befolkningsvekstrate, Vekst_best$NSY_mean,
                                         Vekst_best$snitt_reduk_rate_naturres, Vekst_best$snitt_vekstrate_arlig_invets,
                                         Vekst_best$snitt_vekstrate_arlig_export,
                                         Vekst_best$educ_mean),]


Vekst_best1 <- Vekst_best1 %>%
  select(country, region, income, iso3c, iso2c, year, poptot, BNP_per_inn,
         BNP_per_inn_Y0, snitt_vekst_BNP_per_inn, vekstrate_n_snitt, snitt_befolkningsvekstrate,
         NSY_mean, snitt_reduk_rate_naturres, snitt_vekstrate_arlig_invets, snitt_vekstrate_arlig_export,
         educ_mean)

Vekst_best2019 <- Vekst_best1 %>%
  arrange(iso3c, year) %>%
  group_by(iso3c) %>%
  slice(n()) %>%
  ungroup()


Vekst_best2019$BNP_per_inn <- as.numeric(Vekst_best2019$BNP_per_inn)




### 

