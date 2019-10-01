#setwd("X:/mediamonitor")
#getwd()

rm(list = ls())

#install.packages(c("Rcpp","readxl", "docstring"))

##Benötigte Librarys
library(dplyr)
library(lubridate)
library(tidyr)
library(readxl)
library(writexl)
library(docstring)
library(rlang)
library(gtools)
MM_MARKE <- "MM_MARKE_2018_01-51"
MM_HANDEL <- "MM_HANDEL_2018_01-39"
jahr_start <- 2016
jahr_ende <- 2018

### Funktionen ####
source("I:/ZEITUNGSMONITOR/Reporting Werbewirkung/FUN_utils.R", encoding = "UTF-8")
source("I:/ZEITUNGSMONITOR/Reporting Werbewirkung/FUN_Tableau_Preprocessing.R", encoding = "UTF-8")
source("wewi_helpers.R", encoding = "UTF-8")


#### Einlesen der Datens#tze und abspeichern in temporaere Verzeichnisse ####
mm <- read_mm(jahr_start = jahr_start, jahr_ende = jahr_ende, marke = T) 
saveRDS(mm, "mm_marke.rds")




mm <- readRDS("mm_marke.rds")

mm$mm16$POLOG7 <- NA
mm$mm17$POLOG7 <- NA
mm$mm18$POLOG7 <- NA
mm$mm16$POLOG2 <- NA
mm$mm17$POLOG2 <- NA
mm$mm18$POLOG2 <- NA
mm$mm18$POLOG2 <- NA

mm$mm17$a01 <- NA
mm$mm18$a01 <- NA
mm$mm18$a01 <- NA

mm <- bind_rows(mm$mm16, mm$mm17, mm$mm18)

wewi_rangreihe <- function(df, 
                           campaign_start, 
                           campaign_ende,
                           kunde,
                           kpi = "ges", 
                           variablen = c("S1", "Altersgruppe"),
                           max_wochen = 5, 
                           generalfilter = NA, 
                           min_fallzahl = 200,
                           sort_prio_value = "absolut",
                           sort_prio_wave = "erfolg"
) {
  df <- df[, c(variablen, kpi, "jahrkw", "gewwoche")]
  #Variablenerstellung
  df$gesamt <- 1
  df$jahrkw <- ordered(df$jahrkw)
  df$jahrkw_num <- as.numeric(df$jahrkw)
  df$leer <- as.factor("Keine Angabe")
  df$leer2 <- as.factor("Keine Angabe")
  #Abfangen Fehler
  if (is.na(generalfilter)) {generalfilter = "gesamt == 1"}
  if (length(variablen) == 2 ) {variablen = c(variablen, "leer")}
  if (length(variablen) == 1 ) {variablen = c(variablen, "leer", "leer2")}
  
  
  jahrkw_start_pos <- which(levels(df$jahrkw) == as.character(campaign_start))
  jahrkw_end_pos <- which(levels(df$jahrkw) == as.character(campaign_ende))
  
  #Definition der Untersuchungszeitraueme Nullmessung, Kampagnenzeitraum, Nachhermessung
  df$cat_kamp <- ""
  df$cat_kamp <- ifelse(df$jahrkw_num >= jahrkw_start_pos & df$jahrkw_num <= jahrkw_end_pos, "Kampagnenzeitraum", df$cat_kamp)
  df$cat_kamp <- ifelse(df$jahrkw_num >= jahrkw_start_pos-max_wochen & df$jahrkw_num <= jahrkw_start_pos-1, "Nullmessung", df$cat_kamp)
  df$cat_kamp <- ifelse(df$jahrkw_num >= jahrkw_end_pos + 1 & df$jahrkw_num <= jahrkw_end_pos + max_wochen, "Erfolgsmessung", df$cat_kamp)
  
  # Datensatz auf Zeitraueme und Generalfilter reduzieren.
  df %>%
    filter(cat_kamp != "") %>%  # Nur die 3 Messergebnisse
    filter(!! parse_expr(generalfilter)) -> df_general # Grundgesamtheitsfilter
  
  counter <- 1
  lvls_1 <- levels(df_general[, variablen[1]])
  coms_1 <- all_combinations(lvls_1)
  lvls_2 <- levels(df_general[, variablen[2]])
  coms_2 <- all_combinations(lvls_2)
  lvls_3 <- levels(df_general[, variablen[3]])
  coms_3 <- all_combinations(lvls_3)
  
  rangreihe <- data.frame(matrix(NA, 1, length(variablen))) 
  colnames(rangreihe) <-variablen
  rangreihe[, "KPI_Null"] <- 0
  rangreihe[, "KPI_Kamp"] <- 0
  rangreihe[, "KPI_Erfo"] <- 0
  rangreihe[, "Fall_Null"] <- 0
  rangreihe[, "Fall_Kamp"] <- 0
  rangreihe[, "Fall_Erfo"] <- 0
  
  
  iterationen <- nrow(coms_1) * nrow(coms_2) * nrow(coms_3)
  # Berechnen der einzelnen Werte für die Rangreihe
  for (var1 in 1: nrow(coms_1)) {
    for (var2 in 1: nrow(coms_2)) {
      for (var3 in 1: nrow(coms_3)) {
        
        v1 <- paste(coms_1[var1, ], collapse = ' & ')
        v2 <- paste(coms_2[var2, ], collapse = ' & ')
        v3 <- paste(coms_3[var3, ], collapse = ' & ')
        v1 <- gsub("& NA", "", v1)
        v2 <- gsub("& NA", "", v2)
        v3 <- gsub("& NA", "", v3)
        
        rangreihe[counter,] <- c(v1, v2, v3)
        
        test2 <- df_general[df_general[, variablen[1]] %in% coms_1[var1, ], ]
        test2 <- test2[test2[, variablen[2]] %in% coms_2[var2, ], ]
        test2 <- test2[test2[, variablen[3]] %in% coms_3[var3, ], ]
        
        
        #dplyr befehl
        test2 %>%
          group_by(cat_kamp) %>%
          summarise(KPI = weighted.mean(!! sym(kpi), w= gewwoche,  na.rm = T), 
                    Fallzahl = sum(gewwoche, na.rm = T)) -> df_final
        
        rangreihe[counter, "KPI_Null"] <- df_final[3,2] *100
        rangreihe[counter, "KPI_Kamp"] <- df_final[2,2] *100
        rangreihe[counter, "KPI_Erfo"] <- df_final[1,2] *100
        rangreihe[counter, "Fall_Null"] <- df_final[3,3] 
        rangreihe[counter, "Fall_Kamp"] <- df_final[2,3] 
        rangreihe[counter, "Fall_Erfo"] <- df_final[1,3] 
        
        counter <- counter + 1
      }
    }
  }
  
  rangreihe <- bildung_deltas(rangreihe, sort_prio_value, sort_prio_wave)
  
  return (rangreihe)
}

df = mm
df2 = mm
max_wochen = 2
campaign_start = 201838
campaign_ende = 201839
kpi = "ges_0408"


generalfilter1 = "Altersgruppe == '14-29 Jahre' | Altersgruppe == '30-49 Jahre'"
generalfilter2 =  "TZ != '0'"
generalfilter3 =  "wlk_tz == 'ja'"
generalfilter4 =  "lg_TZ == 'ja'"
#variablen = c("S1", "Altersgruppe", "wlk_tz")
#variablen = c("Altersgruppe")
variablen = c("fb13c0408")
sort_prio_value = "absolut"
sort_prio_wave = "kampagne"
generalfilter = generalfilter1

start <- timestamp()
test <- wewi_rangreihe(df = mm, max_wochen = max_wochen ,
                       campaign_start = campaign_start, campaign_ende = campaign_ende,
                       kpi = kpi, 
                       generalfilter = NA , variablen = variablen, 
                       sort_prio_value = sort_prio_value,
                       sort_prio_wave = sort_prio_wave)
#test
ende <- timestamp()

View(test)












df <- df1[, c(variablen, kpi, "jahrkw", "gewwoche")]
#Variablenerstellung
df$gesamt <- 1
df$jahrkw <- ordered(df$jahrkw)
df$jahrkw_num <- as.numeric(df$jahrkw)
df$leer <- as.factor("Keine Angabe")
df$leer2 <- as.factor("Keine Angabe")
#Abfangen Fehler
if (is.na(generalfilter)) {generalfilter = "gesamt == 1"}
if (length(variablen) == 2 ) {variablen = c(variablen, "leer")}
if (length(variablen) == 1 ) {variablen = c(variablen, "leer", "leer2")}


jahrkw_start_pos <- which(levels(df$jahrkw) == as.character(campaign_start))
jahrkw_end_pos <- which(levels(df$jahrkw) == as.character(campaign_ende))

#Definition der Untersuchungszeitraueme Nullmessung, Kampagnenzeitraum, Nachhermessung
df$cat_kamp <- ""
df$cat_kamp <- ifelse(df$jahrkw_num >= jahrkw_start_pos & df$jahrkw_num <= jahrkw_end_pos, "Kampagnenzeitraum", df$cat_kamp)
df$cat_kamp <- ifelse(df$jahrkw_num >= jahrkw_start_pos-max_wochen & df$jahrkw_num <= jahrkw_start_pos-1, "Nullmessung", df$cat_kamp)
df$cat_kamp <- ifelse(df$jahrkw_num >= jahrkw_end_pos + 1 & df$jahrkw_num <= jahrkw_end_pos + max_wochen, "Erfolgsmessung", df$cat_kamp)

# Datensatz auf Zeitraueme und Generalfilter reduzieren.
df %>%
  filter(cat_kamp != "") %>%  # Nur die 3 Messergebnisse
  filter(!! parse_expr(generalfilter)) -> df_general # Grundgesamtheitsfilter

generalfilter
head(variablen)

counter <- 1
lvls_1 <- levels(df_general[, variablen[1]])
coms_1 <- all_combinations(lvls_1)
lvls_2 <- levels(df_general[, variablen[2]])
coms_2 <- all_combinations(lvls_2)
lvls_3 <- levels(df_general[, variablen[3]])
coms_3 <- all_combinations(lvls_3)

rangreihe <- data.frame(matrix(NA, 1, length(variablen))) 
colnames(rangreihe) <-variablen
rangreihe[, "KPI_Null"] <- 0
rangreihe[, "KPI_Kamp"] <- 0
rangreihe[, "KPI_Erfo"] <- 0
rangreihe[, "Fall_Null"] <- 0
rangreihe[, "Fall_Kamp"] <- 0
rangreihe[, "Fall_Erfo"] <- 0


iterationen <- nrow(coms_1) * nrow(coms_2) * nrow(coms_3)
# Berechnen der einzelnen Werte für die Rangreihe
for (var1 in 1: nrow(coms_1)) {
  for (var2 in 1: nrow(coms_2)) {
    for (var3 in 1: nrow(coms_3)) {
      print(paste("Berchne Iteration: ", counter, " von ", iterationen, ".", sep = ""))
      
      v1 <- paste(coms_1[var1, ], collapse = ' & ')
      v2 <- paste(coms_2[var2, ], collapse = ' & ')
      v3 <- paste(coms_3[var3, ], collapse = ' & ')
      v1 <- gsub("& NA", "", v1)
      v2 <- gsub("& NA", "", v2)
      v3 <- gsub("& NA", "", v3)
      
      rangreihe[counter,] <- c(v1, v2, v3)
      
      test2 <- df_general[df_general[, variablen[1]] %in% coms_1[var1, ], ]
      test2 <- test2[test2[, variablen[2]] %in% coms_2[var2, ], ]
      test2 <- test2[test2[, variablen[3]] %in% coms_3[var3, ], ]

      #dplyr befehl
      test2 %>%
        group_by(cat_kamp) %>%
        summarise(KPI = weighted.mean(!! sym(kpi), w= gewwoche,  na.rm = T), 
                  Fallzahl = sum(gewwoche, na.rm = T)) -> df_final
      
      rangreihe[counter, "KPI_Null"] <- df_final[3,2] *100
      rangreihe[counter, "KPI_Kamp"] <- df_final[2,2] *100
      rangreihe[counter, "KPI_Erfo"] <- df_final[1,2] *100
      rangreihe[counter, "Fall_Null"] <- df_final[3,3] 
      rangreihe[counter, "Fall_Kamp"] <- df_final[2,3] 
      rangreihe[counter, "Fall_Erfo"] <- df_final[1,3] 
      timestamp()
      counter <- counter + 1
    }
  }
}

rangreihe <- bildung_deltas(rangreihe, sort_prio_value, sort_prio_wave)

rangreihe <- as.data.frame(rangreihe)

