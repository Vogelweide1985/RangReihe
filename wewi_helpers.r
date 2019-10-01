
get_filters <- function(v) {
  unique(unlist(strsplit(v, split = ",")))
}


rangreihe <- function(df, wave, kpi, rang_vars, gew = NA, generalfilter=T, min_fallzahl = 80 ) {

  #Preprocessing
  df <- as.data.frame(df)
  df <- df[generalfilter, ] # Filtern des Generalfilters
  df <- df[, c(kpi, wave, gew, rang_vars)] # Selektieren der relevanten Variablen
  
  
  df[, rang_vars] <- lapply(df[, rang_vars],  as.factor) # Faktorisierung
  ls_lvls <- lapply(df[, rang_vars], function(x) {
    combinations(n=length(levels(x)), r=length(levels(x)),v = levels(x), repeats = T, set = T)
  }) # Ziehen aller Level-Kombinationen
  
  # TO DO: Entfernen aller dopplungen
  
  #Zusammenfuegen aller Rows
  ls_lvls <- lapply(ls_lvls, as.data.frame) 
  for (i in 1: length(ls_lvls)) {
    c <- names(ls_lvls[i])
    ls_lvls[[i]] <- unite(ls_lvls[[i]], !!enquo(c), everything(),sep=",")
  }
  
  
  df_sel <- reduce(ls_lvls, crossing) # Erstelllen eines DataFrames mit allen Kombinationen
  df_sel$Fallzahl_w1 <- NA
  df_sel$Fallzahl_w2 <- NA
  df_sel$KPI_w1 <- NA
  df_sel$KPI_w2 <- NA
  df_sel$Delta_abs <- NA
  df_sel$Delta_prz <- NA
  
  
  for (r in 1: nrow(df_sel)) {
    #print(r)
    df.agg <- df
    for (c in 1:length(rang_vars)) {
      df.agg <- df.agg[ df[, rang_vars[ c]] %in% get_filters(df_sel[r,c]), ]
    }  
    
    
    df.agg <- df.agg %>%
      group_by(.data[[wave]]) %>%
      summarise(KPI = weighted.mean(.data[[kpi]], w= .data[[gew]],  na.rm = T)*100, 
                Fallzahl = sum(.data[[gew]], na.rm = T))
    
    #Fallzahlpruefung
    if (df.agg[1, 3] < min_fallzahl & df.agg[2, 3] < min_fallzahl) {next}
    
    df_sel[r, "Fallzahl_w1"] <- round(df.agg[1, 3], 2)
    df_sel[r, "Fallzahl_w2"] <- round(df.agg[2, 3], 2)
    df_sel[r, "KPI_w1"] <- round(df.agg[1, 2], 2)
    df_sel[r, "KPI_w2"] <- round(df.agg[2, 2], 2)
    df_sel[r, "Delta_abs"] <- round(df.agg[2, 2] - df.agg[1, 2], 2)
    df_sel[r, "Delta_prz"] <- round((df.agg[2, 2] / df.agg[1, 2] -1) *100, 2)
    
  }
  
  df_sel <- df_sel[!is.na(df_sel$Fallzahl_w1), ]
  
  #Äußerst unschöner Hack auf KOsten von extensiver Rechenzeit, da DUplikate nicht im Vorfeld gefiltert
  df_sel <- df_sel[!duplicated(df_sel$Fallzahl_w1), ]
  df_sel <- arrange(df_sel, desc(Delta_abs))
  
}