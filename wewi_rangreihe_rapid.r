
##Benötigte Librarys
library(dplyr)
library(DBI)
library(RPostgreSQL)
library(tidyr)
library(ZMGeR)
library(gtools)
library(purrr)

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



#Zusammenfuegen aller Rows
ls_lvls <- lapply(ls_lvls, as.data.frame) 
for (i in 1: length(ls_lvls)) {
  c <- names(ls_lvls[i])
  ls_lvls[[i]] <- unite(ls_lvls[[i]], !!enquo(c), everything(),sep=",")
}


df_sel <- reduce(ls_lvls, crossing) # Erstelllen eines DataFrames mit allen Kombinationen

get_filters <- function(v) {
  unique(unlist(strsplit(v, split = ",")))
}




for (r in 1: nrow(df_sel)) {
  for (c in 1:ncol(df_sel)) {
    
  }
}

df.agg <- df[ df[, names(df_sel[, 3])] %in% get_filters(df_sel[45,3]), ]
#dplyr befehl
df.agg <- df.agg %>%
  group_by(.data[[wave]]) %>%
  summarise(KPI = weighted.mean(.data[[kpi]], w= .data[[gew]],  na.rm = T)*100, 
            Fallzahl = sum(.data[[gew]], na.rm = T),
            Gesamt = 1)



