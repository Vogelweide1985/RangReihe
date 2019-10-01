
##Benötigte Librarys
library(dplyr)
library(DBI)
library(RPostgreSQL)
library(tidyr)
library(ZMGeR)
library(gtools)
library(purrr)
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
#rang_vars  <- c("Altersklasse", "Geschlecht", "Taetigkeit")
rang_vars  <- c("Altersklasse", "Geschlecht")

test <- rangreihe(df = bietfieber, wave = "wave", kpi = "awa_1",
                  rang_vars = rang_vars, gew = "Gewicht")


