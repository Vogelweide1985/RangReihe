
##Benötigte Librarys
library(dplyr)
library(DBI)
library(RPostgreSQL)
library(tidyr)
library(ZMGeR)
library(gtools)
source("wewi_helpers.r")

# Einlesen der Config Datei mit den Zugangsdaten für den Rapid
config <- config::get()
# Verbindung mit dem Rapid Datenbank aufbauen
con <- dbConnect(dbDriver("PostgreSQL"), host = config$dbhost,   
                 port = config$dbport,   
                 dbname = "Rapid",   
                 user = "postgres",   
                 password = "alpha##23SQL")
# Auslesen des benötigten Rapid Datensatzes
bietfieber <- tbl(con, "Bietfieber") %>% collect()

#Erstellen einer Zufälligen wave variable
bietfieber$wave <- rbinom(nrow(bietfieber), 1, 0.5) +1



#Tests Vars 
df <- bietfieber
wave <- "wave"
kpi <- "awa_1"
rang_vars  <- c("Altersklasse", "Geschlecht", "Taetigkeit")
gew <- "gesamt"
generalfilter <- T
min_fallzahl <- 80
sort_absolut <- T

#Function
#Preprocessing
df <- as.data.frame(df)
df$gesamt <- 1
df <- df[generalfilter, ] # Filtern des Generalfilters
df <- df[, c(kpi, wave, gew, rang_vars)] # Selektieren der relevanten Variablen


df[, rang_vars] <- lapply(df[, rang_vars],  as.factor) # Faktorisierung
ls_lvls <- lapply(df[, rang_vars], function(x) {
  combinations(n=length(levels(x)), r=length(levels(x)),v = levels(x), repeats = T)
  }) # Ziehen aller Level-Kombinationen


df.agg <- df[ df[, names(ls_lvls[1])] %in% ls_lvls[[1]][1, ], ]
#dplyr befehl
df.agg <- df.agg %>%
  group_by(.data[[wave]]) %>%
  summarise(KPI = weighted.mean(.data[[kpi]], w= .data[[gew]],  na.rm = T)*100, 
            Fallzahl = sum(.data[[gew]], na.rm = T),
            Gesamt = 1)
             
test <- gather(df.agg,kpi, value, -wave)
test <- spread(test, kpi, wave)

