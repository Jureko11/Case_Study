#Installationsroutine

  #vergleich mit Liste von benötigten packages und installierten -> Wenn nicht installiert -> installieren
      list_of_req_packages <- c("tidyverse")
      needed_packages <- list_of_req_packages[!(list_of_req_packages %in% installed.packages()[,"Package"])]
      if(length(needed_packages) > 0) install.packages(needed_packages)

  #Laden der Librarys
  library(tidyverse)


#Unwichtige Spalten für Projaktaufgabe: Herstellernummer Fehlerhaft, Fehlerhaft_Datum, Fehlerhaft_Fahrleistung

#Welche Teile / Komponenten benötigt OEM1? OEM1 baut Fahrzeug Typ11 und Typ12

  # Laden der Tabellen

      #Bestandteile Typ11
      bestandteile_typ11 <- read_csv2("./Data/Fahrzeug/Bestandteile_Fahrzeuge_OEM1_Typ11.csv")
    
      #Bestandteile Typ12
      bestandteile_typ12 <- read_csv2("./Data/Fahrzeug/Bestandteile_Fahrzeuge_OEM1_Typ12.csv")
  
  #Herausfinden der Komponenten
  
      #Typ11
      
        #Karosserie
        unique(substr(bestandteile_typ11$ID_Karosserie, start = 1, stop = 2))
        #-> K4
      
        #Schaltung
        unique(substr(bestandteile_typ11$ID_Schaltung, start = 1, stop = 5))
        #-> K3SG1 und K3AG1
      
        #Sitze
        unique(substr(bestandteile_typ11$ID_Sitze, start = 1, stop = 5))
        #-> K2LE1 und K2ST1
        
        #Motor
        unique(substr(bestandteile_typ11$ID_Motor, start = 1, stop = 5))
        #-> K1BE1 und K1DI1
        
      #Typ12
        
        #Karosserie
        unique(substr(bestandteile_typ12$ID_Karosserie, start = 1, stop = 2))
        #-> K5
        
        #Schaltung
        unique(substr(bestandteile_typ12$ID_Schaltung, start = 1, stop = 5))
        #-> K3SG1 & K3AG1 -> gleich wie bei Typ11
        
        #Sitze
        unique(substr(bestandteile_typ12$ID_Sitze, start = 1, stop = 5))
        #-> K2ST1 & K2LE1 -> gleich wie bei Typ11
        
        #Motor
        #Herausfinden welche Komponenten-typ verbaut sind
        unique(substr(bestandteile_typ12$ID_Motor, start = 1, stop = 5))
        #-> K1BE1 & K1DI1 -> gleich wie bei Typ11
        
  #Löschen der Tabellen
  rm(bestandteile_typ11, bestandteile_typ12)
        
      

#Durch Betrachten der Realtionstabellen in Editor der eingebauten Komponenten wird ersichtlich, dass OEM1 folgende Einzelteile benötigt (Spaltennamen):
#T01, T02, T03, T04, T05, T06 (Motorenteile), T11, T12, T14, T13 (Sitzteile), T21, T22, T23, T24, T25 (Schaltungsteile), T30, T31, T32, T33 (Karosserieteile)


#Vorgehen: Buttom-Up Vorgehen (= Zuerst Laden der Teile, dann Laden der relevanten Komponenten, als letztes Geodaten für Karte, der beteiligten Werke)

#Funktionen für Fomatierung der Tabellen

  #Fehler (nicht_verschoben + 2fache Spalten)
  tidy_double_columns <- function(data){
        data_tidy <- data %>%
                          select(c(-X1,-...1, -Fehlerhaft.x, -Fehlerhaft_Datum.x, -Fehlerhaft_Fahrleistung.x,
                                              -Fehlerhaft.y, -Fehlerhaft_Datum.y, -Fehlerhaft_Fahrleistung.y)) %>%
                        
                          unite("Produktionsdatum",Produktionsdatum.x | Produktionsdatum.y, na.rm = TRUE, remove = TRUE) %>% #
                          unite("Werksnummer",Werksnummer.x | Werksnummer.y, na.rm = TRUE, remove = TRUE) %>%
                          unite("Herstellernummer",Herstellernummer.x | Herstellernummer.y, na.rm = TRUE, remove = TRUE) %>%
                          
                          mutate("Werksnummer" = as.numeric(Werksnummer)) %>%
                          mutate("Herstellernummer" = as.numeric(Herstellernummer)) %>%
                          mutate("Produktionsdatum" = as.Date(Produktionsdatum,format = "%Y-%m-%d"))
            
        
        return(data_tidy)
  }
  
  #Fehler (nicht verschoben + 3fache Spalten)
  tidy_triple_columns <- function(data){
    data_tidy <- data %>%
                      select(c(-X1,-...1, -Fehlerhaft.x, -Fehlerhaft_Datum.x, -Fehlerhaft_Fahrleistung.x,
                                                         -Fehlerhaft.y, -Fehlerhaft_Datum.y, -Fehlerhaft_Fahrleistung.y,
                                                         -Fehlerhaft, -Fehlerhaft_Datum, -Fehlerhaft_Fahrleistung)) %>%
                      
                      unite("Produktionsdatum",Produktionsdatum.x | Produktionsdatum, sep = "", na.rm = TRUE, remove = TRUE) %>%
                      unite("Produktionsdatum",Produktionsdatum.y | Produktionsdatum, sep = "", na.rm = TRUE, remove = TRUE) %>%
                      unite("Herstellernummer", Herstellernummer.x | Herstellernummer, sep = "", na.rm = TRUE, remove = TRUE) %>%
                      unite("Herstellernummer", Herstellernummer.y | Herstellernummer, sep = "", na.rm = TRUE, remove = TRUE) %>%
                      unite("Werksnummer",Werksnummer.x | Werksnummer, sep = "", na.rm = TRUE, remove = TRUE) %>%
                      unite("Werksnummer",Werksnummer.y | Werksnummer, sep = "", na.rm = TRUE, remove = TRUE) %>%
                
                      
                      mutate("Herstellernummer" = as.numeric(Herstellernummer)) %>%
                      mutate("Werksnummer" = as.numeric(Werksnummer)) %>%
                      mutate("Produktionsdatum" = as.Date(Produktionsdatum,format = "%Y-%m-%d"))
    
    
    return(data_tidy)
  }
  #Fehler (um eins verschoben + 2fache Spalten)
  tidy_double_columns_p <- function(data){
    data_tidy <- data %>%
                      select(c(-X1, -Fehlerhaft_Datum.x, -Fehlerhaft_Fahrleistung.x,
                                    -Fehlerhaft_Datum.y, -Fehlerhaft_Fahrleistung.y)) %>%
                      
                      unite("Produktionsdatum",Herstellernummer.x | Herstellernummer.y, sep = "", na.rm = TRUE, remove = TRUE) %>%
                      unite("Werksnummer",Fehlerhaft.x | Fehlerhaft.y, sep = "", na.rm = TRUE, remove = TRUE) %>%
                      unite("Herstellernummer",Werksnummer.x | Werksnummer.y, sep = "", na.rm = TRUE, remove = TRUE) %>%

                      mutate("Herstellernummer" = as.numeric(Herstellernummer)) %>%
                      mutate("Werksnummer" = as.numeric(Werksnummer)) %>%
                      mutate("Produktionsdatum" = as.Date(Produktionsdatum,format = "%Y-%m-%d"))
    
    return(data_tidy)
  }
  
  #Fehler (um eins verschoben + 3fache Spalten)
  tidy_triple_columns_p <- function(data){
    data_tidy <- data %>%
                      select(c(-X1, -Fehlerhaft_Datum.x, -Fehlerhaft_Fahrleistung.x,
                                    -Fehlerhaft_Datum.y, -Fehlerhaft_Fahrleistung.y,
                                    -Fehlerhaft_Datum,   -Fehlerhaft_Fahrleistung)) %>%
                      unite("Produktionsdatum",Herstellernummer.x | Herstellernummer, sep = "", na.rm = TRUE, remove = TRUE) %>%
                      unite("Produktionsdatum",Herstellernummer.y | Produktionsdatum, sep = "", na.rm = TRUE, remove = TRUE) %>%
                      unite("Herstellernummer",Werksnummer.x | Werksnummer, sep = "", na.rm = TRUE, remove = TRUE) %>%
                      unite("Herstellernummer",Werksnummer.y | Herstellernummer, sep = "", na.rm = TRUE, remove = TRUE) %>%
                      unite("Werksnummer",Fehlerhaft.x | Fehlerhaft, sep = "", na.rm = TRUE, remove = TRUE) %>%
                      unite("Werksnummer",Fehlerhaft.y | Werksnummer, sep = "", na.rm = TRUE, remove = TRUE) %>%
                      
                      mutate("Herstellernummer" = as.numeric(Herstellernummer)) %>%
                      mutate("Werksnummer" = as.numeric(Werksnummer)) %>%
                      mutate("Produktionsdatum" = as.Date(Produktionsdatum)) 
    
    return(data_tidy)
  }
  
  #Fehler (Produktionstag berechnen)
  tidy_compute_prod_d <- function(data){
    data_tidy <- data %>%
                      mutate("origin_as_date" = as.Date(origin,format = "%d-%m-%Y")) %>%
                      mutate("Produktionsdatum" = as.Date(Produktionsdatum_Origin_01011970, origin = origin_as_date)) %>%
                      
                      select(c(-origin, -Produktionsdatum_Origin_01011970, -origin_as_date))
    
      return(data_tidy)
  }
  
  #Fehler (verschoben + Datum berechnen)
  tidy_compute_prod_d_p <- function(data){
    data_tidy <- data %>%
                mutate("Herstellernummer" = as.numeric(Werksnummer)) %>%
                mutate("Werksnummer" = as.numeric(Fehlerhaft)) %>%
                select(c(-X1, -Fehlerhaft_Datum, -Fehlerhaft_Fahrleistung,-Produktionsdatum_Origin_01011970,-Fehlerhaft)) %>%
                separate(origin, sep = ",", into = c("Produktionsdatum_Origin_01011970", "origin") ) %>%
                mutate("Produktionsdatum_Origin_01011970" = as.numeric(Produktionsdatum_Origin_01011970)) %>%
                mutate("origin" = str_replace_all(origin, "\"0", "0")) %>%
                mutate("origin" = str_replace_all(origin, "0\"", "0")) %>%
                tidy_compute_prod_d() 
    
    return(data_tidy)
  }
  
  #Fehler (wild + 3fach Spalten)
  tidy_triple_columns_w <- function(data){
    data_tidy <- data %>%
                      select(c(-X1, -Fehlerhaft_Datum.x, -Fehlerhaft_Fahrleistung.x,
                                    -Fehlerhaft_Datum.y, -Fehlerhaft_Fahrleistung.y,
                                    -Fehlerhaft_Datum,   -Fehlerhaft_Fahrleistung)) %>%
                      unite("Produktionsdatum", Herstellernummer.x | Herstellernummer, sep = "", na.rm = TRUE, remove = TRUE) %>%
                      unite("Produktionsdatum", Herstellernummer.y | Produktionsdatum, sep = "", na.rm = TRUE, remove = TRUE) %>%
                      unite("Herstellernummer", Werksnummer.x | Werksnummer, sep = "", na.rm = TRUE, remove = TRUE) %>%
                      unite("Herstellernummer", Werksnummer.y | Herstellernummer, sep = "", na.rm = TRUE, remove = TRUE) %>%
                      unite("Werksnummer", Fehlerhaft.x | Fehlerhaft, sep = "", na.rm = TRUE, remove = TRUE) %>%
                      unite("Werksnummer", Fehlerhaft.y | Werksnummer, sep = "", na.rm = TRUE, remove = TRUE) %>%

                      mutate("Herstellernummer" = as.numeric(Herstellernummer)) %>%
                      mutate("Werksnummer" = as.numeric(Werksnummer))
      
      
    
    return(data_tidy)
  }

#Import der Einzelteile
  
  #T01
  #Per Editor festgestellt, dass alles in einer Zeile steht, Eintraege durch " | | " abgetrennt sind (ohne "), Zeileumbrueche per " " 
  
      #Importieren der Tabelle + unnoetige Spalten + verschoben + 3fache Spalten
      T_01 <- read_file("./Data/Einzelteil/Einzelteil_T01.txt") %>%
                          str_replace_all("[:blank:]\\|[:blank:]\\|[:blank:]", "\\,") %>%
                          str_replace_all("\"[:blank:]\"", "\"\n\"") %>%
                          str_replace_all("NA[:blank:]\"", "NA\n\"") %>%
                          str_replace_all("\\[:blank:]\"", "\\,0\n\"") %>%
                          read_delim(delim = ",") %>%
                          
                          unite("ID",Produktionsdatum.x | Produktionsdatum, sep = "", na.rm = TRUE, remove = TRUE) %>%
                          unite("ID",Produktionsdatum.y | ID, sep = "", na.rm = TRUE, remove = TRUE) %>%
                          select(c(-ID_T01.x, -ID_T01.y, -ID_T01)) %>%
                          
                          tidy_triple_columns_p() %>%
                          
                          filter(between(Produktionsdatum, as.Date("2016-01-01"), as.Date("2016-12-31"))) %>%
                          select(-Produktionsdatum)
  
  #T02
  #Per Editor festgestellt, dass alles in einer Zeile steht, Eintraege durch zwei Leerzeichen, Zeilenumbruch per tab und " "
  
      #Importieren der Tabelle + unnoetige Spalten + verschoben + 2fache Spalten
      T_02 <- read_file("./Data/Einzelteil/Einzelteil_T02.txt") %>%
                          str_replace_all("[:blank:]{2}", "\\,") %>%
                          str_replace_all("\"[:blank:]\"", "\"\n\"") %>%
                          str_replace_all("\t", "\n") %>%
                          read_delim(delim = ",") %>%
                          
                          unite("ID",Produktionsdatum.x | Produktionsdatum.y, sep = "", na.rm = TRUE, remove = TRUE) %>%
                          select(c(-ID_T02.x, -ID_T02.y)) %>%
                          
                          tidy_double_columns_p() %>%
                          
                          filter(between(Produktionsdatum, as.Date("2016-01-01"), as.Date("2016-12-31"))) %>%
                          select(-Produktionsdatum)
  
  #T03
  #Per Editor festgestellt, dass alles in einer Zeile steht, Eintraege durch | getrennt sind, Zeilenumbruch per Vertical Tab
  
      #Importieren der Tabelle + unnoetige Spalten + verschoben + Datum berechnen
      T_03 <- read_file("./Data/Einzelteil/Einzelteil_T03.txt") %>%
                          str_replace_all("\\|", "\\,") %>%
                          str_replace_all("\x0B", "\n") %>%
                          read_delim(delim = ",") %>%
                          
                          mutate("ID" = Herstellernummer) %>%
                          select(-ID_T03) %>%
                          
                          tidy_compute_prod_d_p()%>%
                          
                          filter(between(Produktionsdatum, as.Date("2016-01-01"), as.Date("2016-12-31"))) %>%
                          select(-Produktionsdatum)
  
  
  #T04
  
      #Importieren der Tabelle + unnoetige Spalten + Datum berechnen
      T_04 <- read_csv2("./Data/Einzelteil/Einzelteil_T04.csv") %>%
                          select(c(-...1, -X1, -Fehlerhaft_Datum, -Fehlerhaft_Fahrleistung, -Fehlerhaft)) %>%
                          tidy_compute_prod_d() %>%
                          
                          mutate("ID" = ID_T04) %>%
                          select(-ID_T04) %>%
                          
                          filter(between(Produktionsdatum, as.Date("2016-01-01"), as.Date("2016-12-31"))) %>%
                          select(-Produktionsdatum)
  
  #T05
  
      #Importieren der Tabelle + unnoetige Spalten + 2fache Spalten
      T_05 <- read_csv("./Data/Einzelteil/Einzelteil_T05.csv") %>%
                        tidy_double_columns() %>%
                        
                        unite("ID",ID_T05.x | ID_T05.y, sep = "", na.rm = TRUE, remove = TRUE) %>%
                        
                        filter(between(Produktionsdatum, as.Date("2016-01-01"), as.Date("2016-12-31"))) %>%
                        select(-Produktionsdatum)
  
  
  #T06
  
      #Importieren der Tabelle + unnoetige Spalten + Datum berechnen
      T_06 <- read_csv("./Data/Einzelteil/Einzelteil_T06.csv") %>%
                        select(c(-...1, -X1, -Fehlerhaft_Datum, -Fehlerhaft_Fahrleistung, -Fehlerhaft)) %>%
                        tidy_compute_prod_d() %>%
                        
                        mutate("ID" = ID_T06) %>%
                        select(-ID_T06) %>%
                        
                        filter(between(Produktionsdatum, as.Date("2016-01-01"), as.Date("2016-12-31"))) %>%
                        select(-Produktionsdatum)
     
  #T11
  #Per Editor festgestellt, dass alles in einer Zeile steht, Eintraege durch tab getrennt sind, Zeilenumbruch per Form Feed
      
      #Importieren der Tabelle + unnoetige Spalten + verschoben + Datum berechen
      T_11 <- read_file("./Data/Einzelteil/Einzelteil_T11.txt") %>%
                          str_replace_all("\x0C", "\n") %>%
                          str_replace_all("\\t", "\\,") %>%
                          read_delim(delim = ",") %>%
                          
                          mutate("ID" = Herstellernummer) %>%
                          
                          tidy_compute_prod_d_p()%>%
                          
                          filter(between(Produktionsdatum, as.Date("2016-01-01"), as.Date("2016-12-31"))) %>%
                          select(-Produktionsdatum, -ID_T11)
      
  #T12
      
      #Importieren der Tabelle + unnoetige Spalten + 3fach Spalten  
      T_12 <- read_csv2("./Data/Einzelteil/Einzelteil_T12.csv") %>%
                          tidy_triple_columns() %>%
                          unite("ID",ID_T12.x | ID_T12, sep = "", na.rm = TRUE, remove = TRUE) %>%
                          unite("ID",ID_T12.y | ID, sep = "", na.rm = TRUE, remove = TRUE) %>%
                          
                          filter(between(Produktionsdatum, as.Date("2016-01-01"), as.Date("2016-12-31"))) %>%
                          select(-Produktionsdatum)
      
      
  #T13
      
      #Importieren der Tabelle + unnoetige Spalten + Datum berechnen
      T_13 <- read_csv2("./Data/Einzelteil/Einzelteil_T13.csv") %>%
                          select(c(-...1, -X1, -Fehlerhaft_Datum, -Fehlerhaft_Fahrleistung, -Fehlerhaft)) %>%
                          tidy_compute_prod_d() %>%
                          
                          mutate("ID" = ID_T13) %>%
                          filter(between(Produktionsdatum, as.Date("2016-01-01"), as.Date("2016-12-31"))) %>%
                          select(-Produktionsdatum, -ID_T13)
      
  #T14
      
      #Importieren der Tabelle + unnoetige Spalten + Datum berechnen
      T_14 <- read_csv2("./Data/Einzelteil/Einzelteil_T14.csv") %>%
                          select(c(-...1, -X1, -Fehlerhaft_Datum, -Fehlerhaft_Fahrleistung, -Fehlerhaft)) %>%
                          tidy_compute_prod_d() %>%
                          
                          mutate("ID" = ID_T14) %>%
                          filter(between(Produktionsdatum, as.Date("2016-01-01"), as.Date("2016-12-31"))) %>%
                          select(-Produktionsdatum, -ID_T14)
      
  #T15
      
      #Importieren der Tabelle + unnoetige Spalten + 2fach Spalten
      T_15 <- read_csv2("./Data/Einzelteil/Einzelteil_T15.csv") %>%
                          tidy_double_columns() %>%
                          
                          unite("ID",ID_T15.x | ID_T15.y, sep = "", na.rm = TRUE, remove = TRUE) %>%
                          
                          filter(between(Produktionsdatum, as.Date("2016-01-01"), as.Date("2016-12-31"))) %>%
                          select(-Produktionsdatum)
      
  #T21
      
      #Importieren der Tabelle + unnoetige Spalten + Produktionsdatum berechnen
      T_21 <- read_csv2("./Data/Einzelteil/Einzelteil_T21.csv") %>%
                          select(c(-...1, -X1, -Fehlerhaft_Datum, -Fehlerhaft_Fahrleistung, -Fehlerhaft)) %>%
                          tidy_compute_prod_d() %>%
                          
                          mutate("ID" = ID_T21) %>%
                          filter(between(Produktionsdatum, as.Date("2016-01-01"), as.Date("2016-12-31"))) %>%
                          select(-Produktionsdatum, -ID_T21)
      
  #T22
  #Per Editor festgestellt, dass alles in einer Zeile steht, Eintraege mit Tab getrennt sind, Zeilenumbruch per kein Zeichen -> z.B. NA"2" hintereinander (auch" "Fahrleistung""1")
      
      #Importiere der Tabelle + unnoetige Spalten +  wild + 3fache Spalten
      T_22 <- read_file("./Data/Einzelteil/Einzelteil_T22.txt") %>%
                          str_replace_all("\\t", ",") %>%
                          str_replace_all("\"\"", "\"\n\"") %>%
                          str_replace_all("NA\"", "NA\n\"") %>%
                          str_replace_all("\\,0\"", "0\n\"") %>%
                          read_delim(delim = ",") %>%
                          
                          unite("ID",Produktionsdatum.x | Produktionsdatum, sep = "", na.rm = TRUE, remove = TRUE) %>%
                          unite("ID",Produktionsdatum.y | ID, sep = "", na.rm = TRUE, remove = TRUE) %>%

                          select(c(-ID_T22.x, -ID_T22.y)) %>%
                          
                          tidy_triple_columns_w() %>%
                          mutate("Produktionsdatum" = as.Date(Produktionsdatum)) %>%
                          filter(between(Produktionsdatum, as.Date("2016-01-01"), as.Date("2016-12-31"))) %>%
                          select(-Produktionsdatum, -ID_T22)
      
  #T23
      
      #Importieren der Tabelle + unnoetige Spalten + 2fach Spalten
      T_23 <- read_csv2("./Data/Einzelteil/Einzelteil_T23.csv") %>%
                          tidy_double_columns() %>%
                          
                          unite("ID",ID_T23.x | ID_T23.y, sep = "", na.rm = TRUE, remove = TRUE) %>%
                          
                          filter(between(Produktionsdatum, as.Date("2016-01-01"), as.Date("2016-12-31"))) %>%
                          select(-Produktionsdatum)
      
  #T24
  #Per Editor festegestellt, dass alles in einer Zeile steht, Eintraege mit 2 Leerzeichen getrennt sind, Zeilenmbrueche per Form Feed
      
      #Importieren der Tabelle + unnoetige Spalten + verschoben + 3fache Spalten
      T_24 <- read_file("./Data/Einzelteil/Einzelteil_T24.txt") %>%
                          str_replace_all("\x0C", "\n") %>%
                          str_replace_all("[:blank:]{2}", "\\,") %>%
                          read_delim(delim = ",") %>%
                          
                          unite("ID",Produktionsdatum.x | Produktionsdatum, sep = "", na.rm = TRUE, remove = TRUE) %>%
                          unite("ID",Produktionsdatum.y | ID, sep = "", na.rm = TRUE, remove = TRUE) %>%
                          select(c(-ID_T24.x, -ID_T24.y)) %>%
                          
                          tidy_triple_columns_p() %>%
                          
                          filter(between(Produktionsdatum, as.Date("2016-01-01"), as.Date("2016-12-31"))) %>%
                          select(-Produktionsdatum, -ID_T24)
      
  #T25
      
      #Importieren der Tabelle + unnoetige Spalten + Datum berechen
      T_25 <- read_csv("./Data/Einzelteil/Einzelteil_T25.csv") %>%
                        select(c(-...1, -X1, -Fehlerhaft_Datum, -Fehlerhaft_Fahrleistung, -Fehlerhaft)) %>%
                        tidy_compute_prod_d() %>%
                        
                        mutate("ID" = ID_T25) %>%
                        filter(between(Produktionsdatum, as.Date("2016-01-01"), as.Date("2016-12-31"))) %>%
                        select(-Produktionsdatum, -ID_T25)
      
  #T30
          
      #Importieren der Tabelle + unnoetige Spalten + 3fach Spalten
      T_30 <- read_csv("./Data/Einzelteil/Einzelteil_T30.csv") %>%
                          tidy_triple_columns() %>%
                          unite("ID",ID_T30.x | ID_T30, sep = "", na.rm = TRUE, remove = TRUE) %>%
                          unite("ID",ID_T30.y | ID, sep = "", na.rm = TRUE, remove = TRUE) %>%
            
                          filter(between(Produktionsdatum, as.Date("2016-01-01"), as.Date("2016-12-31"))) %>%
                          select(-Produktionsdatum)
      
      
  #T31
  #Per Editor festgestellt, dass alles in einer Zeile steht, einzelne Eintraege mit doppelten Leerzeichen getrennt
      
      #Import der Tabelle + unnoetige Spalten + verschoben + Produktionsdatum berechnen
      T_31 <- read_file("./Data/Einzelteil/Einzelteil_T31.txt") %>%
                          str_replace_all("\x08", "\n") %>%
                          str_replace_all( "[:blank:]{2}", "\\,") %>%
                          read_delim(delim = ",") %>%
                     
                          mutate("ID" = Herstellernummer) %>%
                                      
                          tidy_compute_prod_d_p()%>%
             
                          filter(between(Produktionsdatum, as.Date("2016-01-01"), as.Date("2016-12-31"))) %>%
                          select(-Produktionsdatum, -ID_T31)
                              
  #T32
      
      #Import der Tabelle + unnoetige Spalten + 2fach Spalten
      T_32 <- read_csv2("./Data/Einzelteil/Einzelteil_T32.csv") %>%
                          tidy_double_columns() %>%
                                    
                          unite("ID",ID_T32.x | ID_T32.y, sep = "", na.rm = TRUE, remove = TRUE) %>%
                                      
                          filter(between(Produktionsdatum, as.Date("2016-01-01"), as.Date("2016-12-31"))) %>%
                          select(-Produktionsdatum)
      
  #T33
      
      #Import der Tabelle + unnoetige Spalten + Produktionsdatum berechnen
      T_33 <- read_csv("./Data/Einzelteil/Einzelteil_T33.csv") %>%
                          select(c(-...1, -X1, -Fehlerhaft_Datum, -Fehlerhaft_Fahrleistung, -Fehlerhaft)) %>%
                          tidy_compute_prod_d() %>%
                          
                          mutate("ID" = ID_T33) %>%          
                          filter(between(Produktionsdatum, as.Date("2016-01-01"), as.Date("2016-12-31"))) %>%
                          select(-Produktionsdatum, -ID_T33)
    

#Überprüfen ob Werte = na exestieren in Tabellen
          unique(is.na(T_01)) # -> Nein
          unique(is.na(T_02)) # -> Nein
          unique(is.na(T_03)) # -> Nein
          unique(is.na(T_04)) # -> Nein
          unique(is.na(T_05)) # -> Nein
          unique(is.na(T_06)) # -> Nein
          
          unique(is.na(T_11)) # -> Nein
          unique(is.na(T_12)) # -> Nein
          unique(is.na(T_13)) # -> Nein
          unique(is.na(T_14)) # -> Nein
          unique(is.na(T_15)) # -> Nein
          
          unique(is.na(T_21)) # -> Nein
          unique(is.na(T_22)) # -> Nein
          unique(is.na(T_23)) # -> Nein
          unique(is.na(T_24)) # -> Nein
          unique(is.na(T_25)) # -> Nein
          
          unique(is.na(T_30)) # -> Nein
          unique(is.na(T_31)) # -> Nein
          unique(is.na(T_32)) # -> Nein
          unique(is.na(T_33)) # -> Nein

          
#Teile Tabellen mit Relationtabellen der Komponenten verbinden (in welche Komponente wurden sie eingebaut)
      
    #T01
    #verbaut in Komponente K1BE1 und K1DI1
          
          
      #Importieren der benötigten Komponenten-Relationstabelle + Löschen unnoetiger Spalte
      relation_k1be1 <- read_csv2("./Data/Komponente/Bestandteile_Komponente_K1BE1.csv") %>%
                                    select(-...1)
        
      
      relation_k1di1 <- read_csv2("./Data/Komponente/Bestandteile_Komponente_K1DI1.csv") %>%
                                  select(-...1)
    
      #Verbinden der Tabellen # Löschen unnoetiger Spalten für T01 + Wenn neue Spalte leer -> Zeilen löschen, da Teile in anderen Komponenten verbaut sind, die nicht zu Fahrzeugtypen von OEM1 gehören
      T_01 <- T_01 %>%
                   left_join(relation_k1be1,by = c("ID" = "ID_T1")) %>%
                   select(c(-ID_T2, -ID_T3, -ID_T4)) %>%
                   left_join(relation_k1di1,by = c("ID" = "ID_T1")) %>%
                   select(c(-ID_T2, -ID_T5, -ID_T6)) %>%
                   unite("Komponente_ID",ID_K1BE1 | ID_K1DI1, sep = "", na.rm = TRUE, remove = TRUE) %>%
                   subset(Komponente_ID != "")

    #T02
    #verbaut in Komponente K1BE1 und K1DI1
      
      
      T_02 <- T_02 %>%
                    left_join(relation_k1be1,by = c("ID" = "ID_T2")) %>%
                    select(c(-ID_T1, -ID_T3, -ID_T4)) %>%
                    left_join(relation_k1di1,by = c("ID" = "ID_T2")) %>%
                    select(c(-ID_T1, -ID_T5, -ID_T6)) %>%
                    unite("Komponente_ID",ID_K1BE1 | ID_K1DI1, sep = "", na.rm = TRUE, remove = TRUE) %>%
                    subset(Komponente_ID != "")
    
    #T03
    #verbaut in Komponente K1BE1
      
      T_03 <- T_03 %>%
                    left_join(relation_k1be1,by = c("ID" = "ID_T3")) %>%
                    select(c(-ID_T1, -ID_T2, -ID_T4)) %>%
                    mutate("Komponente_ID" = ID_K1BE1) %>%
                    select(-ID_K1BE1) %>%
                    subset(Komponente_ID != "")
      
    #T04
    #verbaut in Komponente K1BE1
      
      T_04 <- T_04 %>%
                  left_join(relation_k1be1,by = c("ID" = "ID_T4")) %>%
                  select(c(-ID_T1, -ID_T2, -ID_T3)) %>%
                  mutate("Komponente_ID" = ID_K1BE1) %>%
                  select(-ID_K1BE1) %>%
                  subset(Komponente_ID != "")
      
    #T05
    #verbaut in Komponente  K1DI1
      
      T_05 <- T_05 %>%
                  left_join(relation_k1di1,by = c("ID" = "ID_T5")) %>%
                  select(c(-ID_T1, -ID_T2, -ID_T6)) %>%
                  mutate("Komponente_ID" = ID_K1DI1) %>%
                  select(-ID_K1DI1) %>%
                  subset(Komponente_ID != "")
        
    #T06
    #verbaut in Komponente K1DI1
      
      T_06 <- T_06 %>%
                    left_join(relation_k1di1,by = c("ID" = "ID_T6")) %>%
                    select(c(-ID_T1, -ID_T2, -ID_T5)) %>%
                    mutate("Komponente_ID" = ID_K1DI1) %>%
                    select(-ID_K1DI1) %>%
                    subset(Komponente_ID != "")
      
    #Alle Einzelteile der Komponenten K1BE1 und K1DI1 bearbeitet -> Löschen der Realtions-Tabellen
      rm (relation_k1be1, relation_k1di1)
      
    #T11
    #verbaut in K2LE1 und K2ST1
      
      #Importieren der benötigten Komponenten-Relationstabelle + Löschen unnoetiger Spalte
      relation_k2le1 <- read_csv2("./Data/Komponente/Bestandteile_Komponente_K2LE1.csv") %>%
                                  select(-...1)
      
      relation_k2st1 <- read_csv2("./Data/Komponente/Bestandteile_Komponente_K2ST1.csv") %>%
                                  select(c(-X1, -X))
      
      #Verbinden der Tabellen
      T_11 <- T_11 %>%
                      left_join(relation_k2le1,by = c("ID" = "ID_T11")) %>%
                      select(c(-ID_T14, -ID_T15)) %>%
                      left_join(relation_k2st1,by = c("ID" = "ID_T11")) %>%
                      select(c(-ID_T12, -ID_T13)) %>%
                      unite("Komponente_ID",ID_K2LE1 | ID_K2ST1, sep = "", na.rm = TRUE, remove = TRUE) %>%
                      subset(Komponente_ID != "") 
      
    #T12
    #vebaut in K2ST1
      
      T_12 <- T_12 %>%
                    left_join(relation_k2st1,by = c("ID" ="ID_T12")) %>%
                    select(c(-ID_T11, -ID_T13)) %>%
                    mutate("Komponente_ID" = ID_K2ST1) %>%
                    select(-ID_K2ST1) %>%
                    subset(Komponente_ID != "") 
        
    #T13
    #verbaut in K2ST1
      
      T_13 <- T_13 %>%
                    left_join(relation_k2st1,by = c("ID" = "ID_T13")) %>%
                    select(c(-ID_T11, -ID_T12)) %>%
                    mutate("Komponente_ID" = ID_K2ST1) %>%
                    select(-ID_K2ST1) %>%
                    subset(Komponente_ID != "") 
      
    #T14
    #verbaut in K2LE1
      
      T_14 <- T_14 %>%
                    left_join(relation_k2le1,by =c("ID" = "ID_T14")) %>%
                    select(c(-ID_T11, -ID_T15)) %>%
                    mutate("Komponente_ID" = ID_K2LE1) %>%
                    select(-ID_K2LE1) %>%
                    subset(Komponente_ID != "")
      
    #T15
    #verbaut in K2LE1
      
      T_15 <- T_15 %>%
                    left_join(relation_k2le1,by =c("ID" = "ID_T15")) %>%
                    select(c(-ID_T11, -ID_T14)) %>%
                    mutate("Komponente_ID" = ID_K2LE1) %>%
                    select(-ID_K2LE1) %>%
                    subset(Komponente_ID != "")
      
    #Alle Einzelteile der Komponenten K2LE1 und K2ST1 bearbeitet -> Löschen der Realtions-Tabellen
    rm (relation_k2le1, relation_k2st1)
    
    
    #T21    
    #verbaut in K3SG1 und K3AG1
    
      #Importieren der benötigten Komponenten-Relationstabelle + Löschen unnoetiger Spalte
      relation_k3sg1 <- read_csv2("./Data/Komponente/Bestandteile_Komponente_K3SG1.csv") %>%
                                  select(-X1)
      
      relation_k3ag1 <- read_csv2("./Data/Komponente/Bestandteile_Komponente_K3AG1.csv") %>%
                                  select(-X1)
      
      #Verbinden der Tabellen
      T_21 <- T_21 %>%
                      left_join(relation_k3sg1,by = c("ID" = "ID_T21")) %>%
                      select(c(-ID_T22, -ID_T23)) %>%
                      left_join(relation_k3ag1,by = c("ID" = "ID_T21")) %>%
                      select(c(-ID_T24, -ID_T25)) %>%
                      unite("Komponente_ID",ID_K3SG1 | ID_K3AG1, sep = "", na.rm = TRUE, remove = TRUE) %>%
                      subset(Komponente_ID != "")
    #T22
    #verbaut in K3SG1
      
      T_22 <- T_22 %>%
                      left_join(relation_k3sg1,by = c("ID" = "ID_T22")) %>%
                      select(c(-ID_T21, -ID_T23)) %>%
                      mutate("Komponente_ID" = ID_K3SG1) %>%
                      select(-ID_K3SG1) %>%
                      subset(Komponente_ID != "")
                    
    #T23
    #verbaut in K3SG1
      
      
      T_23 <- T_23 %>%
                      left_join(relation_k3sg1,by = c("ID" = "ID_T23")) %>%
                      select(c(-ID_T21, -ID_T22)) %>%
                      mutate("Komponente_ID" = ID_K3SG1) %>%
                      select(-ID_K3SG1) %>%
                      subset(Komponente_ID != "")
      
    #T24
    #verbaut in K3AG1
      
      T_24 <- T_24 %>%
                      left_join(relation_k3ag1,by = c("ID" = "ID_T24")) %>%
                      select(c(-ID_T21, -ID_T25)) %>%
                      mutate("Komponente_ID" = ID_K3AG1) %>%
                      select(-ID_K3AG1) %>%
                      subset(Komponente_ID != "")
      
    #T25
    #verbaut in K3AG1
      
      
      T_25 <- T_25 %>%
                        left_join(relation_k3ag1,by = c("ID" = "ID_T25")) %>%
                        select(c(-ID_T21, -ID_T24)) %>%
                        mutate("Komponente_ID" = ID_K3AG1) %>%
                        select(-ID_K3AG1) %>%
                        subset(Komponente_ID != "")
      
    #Alle Einzelteile der Komponenten K3SG1 und K3AG1 bearbeitet -> Löschen der Realtions-Tabellen
    rm (relation_k3sg1, relation_k3ag1)
    
    #T30
    #verbaut in K4 und K5
    
        #Importieren der benötigten Komponenten-Relationstabelle + Löschen unnoetiger Spalte
        relation_k4 <- read_csv2("./Data/Komponente/Bestandteile_Komponente_K4.csv") %>%
                                  select(-X1)
    
        relation_k5 <- read_csv2("./Data/Komponente/Bestandteile_Komponente_K5.csv") %>%
                                  select(-X1)
        
        #Verbinden der Tabellen
        T_30 <- T_30 %>%
                        left_join(relation_k4,by = c("ID" = "ID_T30")) %>%
                        select(c(-ID_T31, -ID_T32)) %>%
                        left_join(relation_k5,by = c("ID" = "ID_T30")) %>%
                        select(c(-ID_T31, -ID_T33)) %>%
                        unite("Komponente_ID",ID_K4 | ID_K5, sep = "", na.rm = TRUE, remove = TRUE) %>%
                        subset(Komponente_ID != "")
      
    #T31
    #verbaut in K4 und K5
        
        T_31 <- T_31 %>%
                        left_join(relation_k4,by = c("ID" = "ID_T31")) %>%
                        select(c(-ID_T30, -ID_T32)) %>%
                        left_join(relation_k5,by = c("ID" = "ID_T31")) %>%
                        select(c(-ID_T30, -ID_T33)) %>%
                        unite("Komponente_ID",ID_K4 | ID_K5, sep = "", na.rm = TRUE, remove = TRUE) %>%
                        subset(Komponente_ID != "")
        
    #T32
    #verbaut in K4
        
        T_32 <- T_32 %>%
                        left_join(relation_k4,by = c("ID" = "ID_T32")) %>%
                        select(c(-ID_T30, -ID_T31)) %>%
                        mutate("Komponente_ID" = ID_K4) %>%
                        select(-ID_K4) %>%
                        subset(Komponente_ID != "")
        
    #T33
    #verbaut in K5
        
        T_33 <- T_33 %>%
                        left_join(relation_k5,by = c("ID" = "ID_T33")) %>%
                        select(c(-ID_T30, -ID_T31)) %>%
                        mutate("Komponente_ID" = ID_K5) %>%
                        select(-ID_K5) %>%
                        subset(Komponente_ID != "")
        
    #Alle Einzelteile der Komponenten K4 und K5 bearbeitet -> Löschen der Realtions-Tabellen
    rm (relation_k4, relation_k5)   
  
    
#Teile Tabellen mit Komonente Tabelle verbinden (wo wurde Teil in Komponente eingebaut)
    
    #T01
    #verbaut in K1BE1 und K1DI1
    
        #K1BE1
    
          #Importieren der Tabelle + unnoetige Spalten 
          komponente_k1be1 <- read_csv("./Data/Komponente/Komponente_K1BE1.csv") %>%
                                    select(c(ID_Motor, Herstellernummer, Werksnummer)) %>%
                                    mutate("Komponente_ID" = ID_Motor) %>%
                                    mutate("Komponente_Herstellernummer" = Herstellernummer) %>%
                                    mutate("Komponente_Werksnummer" = Werksnummer) %>%
                                    select(-ID_Motor, -Herstellernummer, -Werksnummer)
        #K1DI1
          
          #Importieren der Tabelle + Formatierung der Tabelle (Fehler: unnoetige Spalten + 3fache Spalten)
          komponente_k1di1 <- read_csv("./Data/Komponente/Komponente_K1DI1.csv") %>%
                                  tidy_triple_columns() %>%
                                  unite("Komponente_ID",ID_Motor.x | ID_Motor, sep = "", na.rm = TRUE, remove = TRUE) %>%
                                  unite("Komponente_ID",ID_Motor.y | Komponente_ID, sep = "", na.rm = TRUE, remove = TRUE) %>%
            
                                  mutate("Komponente_Herstellernummer" = Herstellernummer) %>%
                                  mutate("Komponente_Werksnummer" = Werksnummer) %>%
                                  select(-Produktionsdatum, -Herstellernummer, -Werksnummer)
          
        #Verbinden der Tabellen
        T_01 <- T_01 %>%
                          left_join(komponente_k1be1, by = "Komponente_ID") %>%
                          left_join(komponente_k1di1, by = "Komponente_ID") %>%

                          unite("Komponente_Herstellernummer", Komponente_Herstellernummer.x | Komponente_Herstellernummer.y, sep = "", na.rm = TRUE, remove = TRUE) %>%
                          unite("Komponente_Werksnummer", Komponente_Werksnummer.x | Komponente_Werksnummer.y, sep = "", na.rm = TRUE, remove = TRUE) %>%
          
                          mutate("Komponente_Herstellernummer" = as.numeric(Komponente_Herstellernummer)) %>%
                          mutate("Komponente_Werksnummer" = as.numeric(Komponente_Werksnummer) )

          
    #T02
    #verbaut in K1BE1 und K1DI1
        T_02 <- T_02 %>%
                        left_join(komponente_k1be1, by = "Komponente_ID") %>%
                        left_join(komponente_k1di1, by = "Komponente_ID") %>%
                        
                        unite("Komponente_Herstellernummer", Komponente_Herstellernummer.x | Komponente_Herstellernummer.y, sep = "", na.rm = TRUE, remove = TRUE) %>%
                        unite("Komponente_Werksnummer", Komponente_Werksnummer.x | Komponente_Werksnummer.y, sep = "", na.rm = TRUE, remove = TRUE) %>%
          
                        mutate("Komponente_Herstellernummer" = as.numeric(Komponente_Herstellernummer)) %>%
                        mutate("Komponente_Werksnummer" = as.numeric(Komponente_Werksnummer) )
        
    #T03
    #verbaut in K1BE1
        
        T_03 <- T_03 %>%
                        left_join(komponente_k1be1, by = "Komponente_ID")
    
    #T04
    #verbaut in K1BE1
        
        T_04 <- T_04 %>%
                          left_join(komponente_k1be1, by = "Komponente_ID")
        
    #T05
    #verbaut in K1DI1
        
        T_05 <- T_05 %>%
                          left_join(komponente_k1di1, by = "Komponente_ID")
        
    #T06
    #verbaut in K1DI1
        
        T_06 <- T_06 %>%
          left_join(komponente_k1di1, by = "Komponente_ID")
        
    #Löschen der Komponenten-Tabellen
    rm (komponente_k1be1, komponente_k1di1)
          

    #T11
    #verbaut in K2LE1 und K2ST1
    
      
    #K2LE1
    #Per Editor festgetsellt, dass alles in einer Zeile steht, einzelne Eintraege mit II getrennt sind und Zeilenumbrueche mittel Vertical Tab markiert sind
    
      #Import der Tabelle + unnoetige Spalten + verschoben + 2fach Spalten
      komponente_k2le1 <- read_file("./Data/Komponente/Komponente_K2LE1.txt") %>%
                                      str_replace_all("\x0B", "\n") %>%
                                      str_replace_all("II", "\\,") %>%
                                      read_delim(delim = ",") %>%
                                      
                                      unite("Komponente_ID",Produktionsdatum.x | Produktionsdatum.y, sep = "", na.rm = TRUE, remove = TRUE) %>%
                                      select(c(-ID_Sitze.x, -ID_Sitze.y)) %>%
                                      tidy_double_columns_p() %>%
        
                                      mutate("Komponente_Herstellernummer" = Herstellernummer) %>%
                                      mutate("Komponente_Werksnummer" = Werksnummer) %>%
                                      select(-Produktionsdatum, -Herstellernummer, -Werksnummer)
        
      
      
    #K2ST1
    #Per Editor festgestellt, dass einzelne Eintraege mit | getrennt sind
      
      #Error: Error in basename(path) : file name conversion problem -- name too long? wenn man direkt es in pipe einlesen will
      #Wenn man es speichert als textdatei und danach einliest, funktioniert es
      
      #############################################################################################################
      #############################################################################################################
      ####Fehler in Tabelle: z.B. ID = "K2ST1-110-1101-918828" ABER Herstellernummer "109" und Werksnummer 1092####
      ####                                                                                                     ####
      #############################################################################################################
      #############################################################################################################
      
      #Import der Tabelle + unnoetige Spalten + verschoben
      komponente_k2st1 <- read_file("./Data/Komponente/Komponente_K2ST1.txt") %>%
                                      str_replace_all("\\|", ",") %>%
                                      str_replace_all("\\,[:digit:]+\\,\"", "\\,\"") %>%
                                      writeLines("komponente_k2st1_neu.txt")
      
      komponente_k2st1 <-  read_delim("komponente_k2st1_neu.txt", delim = ",") %>%
                                      select(c(ID_Sitze, Herstellernummer, Werksnummer)) %>%
                                      mutate("Komponente_ID" = ID_Sitze) %>%
                                      mutate("Komponente_Herstellernummer" = Herstellernummer) %>%
                                      mutate("Komponente_Werksnummer" = Werksnummer) %>%
                                      select(-ID_Sitze, -Herstellernummer, -Werksnummer)
      
      unlink("komponente_k2sg1_neu.txt")

      #Herausfinden ob Fehler bei Top-Down auch passiert
          
          #Importieren der Typ_11 und Typ_12 Tabelle (Fahrzeuge von OEM1)
          
              typ_11 <- read_csv("./Data/Fahrzeug/Fahrzeuge_OEM1_Typ11.csv") %>%
                select(c(ID_Fahrzeug, Herstellernummer, Werksnummer))
              
          #Importieren der Relationstabellen Fahrzeug-Komponenten
            
              typ_11_relation <- read_csv2("./Data/Fahrzeug/Bestandteile_Fahrzeuge_OEM1_Typ11.csv") %>%
                select(-...1)
          
          #Verbinden der Typ Tabellen mit Relationstabellen
              
              typ_11 <- typ_11 %>%
                left_join(typ_11_relation, by = "ID_Fahrzeug")
      
          
          #Löschen der Relationstabellen
              rm(typ_11_relation)
              
          #Verbinden der Tabellen mit 2kst1 (Fehlerherd)
              
              typ_11 <- typ_11 %>% left_join(komponente_k2st1, by = c("ID_Sitze" = "Komponente_ID"))
          
        #Fehler immernoch vorhanden -> vermerken in Markdown, dass Fehler existiert und ab sofort mit der Komponenten_Werksnummer und Herstellernummer weitergeabreitet wird (ID Fehler ignorieren)
        #Löschen der Tabellen
              rm(typ_11)
              
    #Verbinden der Tabellen
      
      T_11 <- T_11 %>%
        left_join(komponente_k2le1, by = "Komponente_ID") %>%
        left_join(komponente_k2st1, by = "Komponente_ID") %>%
        
        unite("Komponente_Herstellernummer", Komponente_Herstellernummer.x | Komponente_Herstellernummer.y, sep = "", na.rm = TRUE, remove = TRUE) %>%
        unite("Komponente_Werksnummer", Komponente_Werksnummer.x | Komponente_Werksnummer.y, sep = "", na.rm = TRUE, remove = TRUE) %>%
      
        mutate("Komponente_Herstellernummer" = as.numeric(Komponente_Herstellernummer)) %>%
        mutate("Komponente_Werksnummer" = as.numeric(Komponente_Werksnummer) )
    
    #T12
    #verbaut in K2ST1 (gleicher Fehler!)
      
      T_12 <- T_12 %>%
                      left_join(komponente_k2st1, by = "Komponente_ID")
      
    #T13
    #verbaut in K2ST1 (gleicher Fehler!)
      
      T_13 <- T_13 %>%
        left_join(komponente_k2st1, by = "Komponente_ID")
      
    #T14
    #verbaut in K2LE1
      
      T_14 <- T_14 %>%
        left_join(komponente_k2le1, by = "Komponente_ID")
      
    #T15
    #verbaut in K2LE1
      T_15 <- T_15 %>%
        left_join(komponente_k2le1, by = "Komponente_ID")
      
    #Löschen der Komponenten-Tabellen
    rm (komponente_k2st1, komponente_k2le1) 
      
    
    #T21
    #verbaut in K3SG1 und K3AG1
    
        #K3SG1
          #Importieren der Tabelle + Formatierung der Tabelle (Fehler: unnoetige Spalten und Spalten 2fach vorhanden)
          komponente_k3sg1 <- read_csv("./Data/Komponente/Komponente_K3SG1.csv") %>%
                                        tidy_double_columns() %>%
                                        unite("Komponente_ID",ID_Schaltung.x | ID_Schaltung.y, na.rm = TRUE, remove = TRUE) %>%
                                        mutate("Komponente_Herstellernummer" = Herstellernummer) %>%
                                        mutate("Komponente_Werksnummer" = Werksnummer) %>%
                                        select(c(-Herstellernummer, -Werksnummer, -Produktionsdatum))
    
        #K3AG1
          #Importieren der Tabelle + Formatierung der Tabelle (Fehler: unnoetige Spalten und Spalten 3fach vorhanden)
          komponente_k3ag1 <- read_csv("./Data/Komponente/Komponente_K3AG1.csv") %>%
                                        tidy_triple_columns() %>%
                                        unite("Komponente_ID",ID_Schaltung.x | ID_Schaltung, sep = "", na.rm = TRUE, remove = TRUE) %>%
                                        unite("Komponente_ID",ID_Schaltung.y | Komponente_ID, sep = "", na.rm = TRUE, remove = TRUE) %>%
                                        mutate("Komponente_Herstellernummer" = Herstellernummer) %>%
                                        mutate("Komponente_Werksnummer" = Werksnummer) %>%
                                        select(c(-Herstellernummer, -Werksnummer, -Produktionsdatum))
        
        #Verbinden der Tabelle
          
          T_21 <- T_21 %>% 
                          left_join(komponente_k3sg1, by = "Komponente_ID") %>%
                          left_join(komponente_k3ag1, by = "Komponente_ID") %>%
                        
                          unite("Komponente_Herstellernummer", Komponente_Herstellernummer.x | Komponente_Herstellernummer.y, sep = "", na.rm = TRUE, remove = TRUE) %>%
                          unite("Komponente_Werksnummer", Komponente_Werksnummer.x | Komponente_Werksnummer.y, sep = "", na.rm = TRUE, remove = TRUE) %>%
            
                          mutate("Komponente_Herstellernummer" = as.numeric(Komponente_Herstellernummer)) %>%
                          mutate("Komponente_Werksnummer" = as.numeric(Komponente_Werksnummer) )
          
        #T22
        #verbaut in K3SG1
          
          T_22 <- T_22 %>%
                      left_join(komponente_k3sg1, by = "Komponente_ID")
          
        #T23
        #verbaut in K3SG1
          
          T_23 <- T_23 %>%
            left_join(komponente_k3sg1, by = "Komponente_ID")
          
        #T24
        #verbaut in K3AG1
          
          T_24 <- T_24 %>%
            left_join(komponente_k3ag1, by = "Komponente_ID")
          
        #T25
        #verbaut in K3AG1
          
          T_25 <- T_25 %>%
            left_join(komponente_k3ag1, by = "Komponente_ID")
          
        #Löschen der Tabellen
        rm(komponente_k3ag1, komponente_k3sg1)
        
        #T30
        #verbaut in K4 und K5
        
            #K4
            #Importieren der Komponenten + Formatieren der Tabelle (Fehler: unnoetige Spalten und Spalten 2fach vorhanden)
            komponente_k4 <- read_csv2("./Data/Komponente/Komponente_K4.csv") %>%
                                        tidy_double_columns() %>%
                                        unite("Komponente_ID",ID_Karosserie.x | ID_Karosserie.y, na.rm = TRUE, remove = TRUE) %>%
                                        mutate("Komponente_Herstellernummer" = Herstellernummer) %>%
                                        mutate("Komponente_Werksnummer" = Werksnummer) %>%
                                        select(c(-Herstellernummer, -Werksnummer, -Produktionsdatum))
            
            #K5
            #Importieren der Komponenten + Formatierung der Tabelle(Fehler: unnoetige Spalten + 2fache Spalten)
            komponente_k5 <- read_csv("./Data/Komponente/Komponente_K5.csv")%>% 
                                        tidy_double_columns() %>%
                                        unite("Komponente_ID",ID_Karosserie.x | ID_Karosserie.y, na.rm = TRUE, remove = TRUE) %>%
                                        mutate("Komponente_Herstellernummer" = Herstellernummer) %>%
                                        mutate("Komponente_Werksnummer" = Werksnummer) %>%
                                        select(c(-Herstellernummer, -Werksnummer, -Produktionsdatum))
            
            #Verbinden der Tabellen
            T_30 <- T_30 %>% 
                          left_join(komponente_k4, by = "Komponente_ID") %>%
                          left_join(komponente_k5, by = "Komponente_ID") %>%
                          
                          unite("Komponente_Herstellernummer", Komponente_Herstellernummer.x | Komponente_Herstellernummer.y, sep = "", na.rm = TRUE, remove = TRUE) %>%
                          unite("Komponente_Werksnummer", Komponente_Werksnummer.x | Komponente_Werksnummer.y, sep = "", na.rm = TRUE, remove = TRUE) %>%
                          mutate("Komponente_Herstellernummer" = as.numeric(Komponente_Herstellernummer)) %>%
                          mutate("Komponente_Werksnummer" = as.numeric(Komponente_Werksnummer) )
                          
      #T31
      #verbaut in K4 und K5
    
          T_31 <- T_31 %>% 
                          left_join(komponente_k4, by = "Komponente_ID") %>%
                          left_join(komponente_k5, by = "Komponente_ID") %>%
                          
                          unite("Komponente_Herstellernummer", Komponente_Herstellernummer.x | Komponente_Herstellernummer.y, sep = "", na.rm = TRUE, remove = TRUE) %>%
                          unite("Komponente_Werksnummer", Komponente_Werksnummer.x | Komponente_Werksnummer.y, sep = "", na.rm = TRUE, remove = TRUE) %>%
                          mutate("Komponente_Herstellernummer" = as.numeric(Komponente_Herstellernummer)) %>%
                          mutate("Komponente_Werksnummer" = as.numeric(Komponente_Werksnummer) )
          
      #T32
      #verbaut in K4
          
          T_32 <- T_32 %>%
            left_join(komponente_k4, by = "Komponente_ID")
          
      #T33
      #verbaut in K5
          
          T_33 <- T_33 %>%
            left_join(komponente_k5, by = "Komponente_ID")    
          
      
      #Löschen der Komponenten Tabellen
      rm(komponente_k4, komponente_k5)
      
#Importieren der Geodaten
      
      #Tier 1 + Löschen unnoetiger Spalten + Umbennen zu Laengengrad + unnötige NA Zeilen (+letzte Zeile mit Wert 2193), löschen da unnoetig + Verbessern der Geodaten und ins richtige Format bringen (oftmals sind Koordinaten zu kruz -> 0 muss angehängt werden)
      tier1_geo <- read_csv2("./Data/Geodaten/Tier1_Werke_2017-07-11_v1.2_TrR.csv") %>%
        select(c(-PLZ, -ORT)) %>%
        rename(Laengengrad = 3) %>%
        drop_na() %>%
        mutate(Breitengrad = replace(Breitengrad, Breitengrad == 5068766, 50687660)) %>%
        mutate(Breitengrad = replace(Breitengrad, Breitengrad == 5005266, 50052660)) %>%
        mutate(Breitengrad = replace(Breitengrad, Breitengrad == 5225431, 52254310)) %>%
        mutate(Laengengrad = replace(Laengengrad, Laengengrad == 102389, 10238900)) %>%
        mutate(Laengengrad = replace(Laengengrad, Laengengrad == 1008458, 10084580)) %>%
        mutate(Laengengrad = replace(Laengengrad, Laengengrad == 1080846, 10808460)) %>%
        mutate(Breitengrad = round(Breitengrad / 1000000, 4)) %>%
        mutate(Laengengrad = round(Laengengrad / 1000000, 4))
        
      
      
      #Tier2 + Löschen unnoetiger Spalten + Umbennen zu Laengengrad + unnötige NA Spalten + Verbessern der Geodaten und ins richtige Format bringen
      tier2_geo <- read_csv2("./Data/Geodaten/Tier2_Werke_2017-07-11_v1.2_TrR.csv") %>%
        rename(Laengengrad = 5) %>%
        select(c(Werk, Breitengrad, Laengengrad)) %>%
        mutate(Breitengrad = replace(Breitengrad, Breitengrad == 503485, 50348500)) %>%
        mutate(Breitengrad = replace(Breitengrad, Breitengrad == 503485, 50348500)) %>%
        mutate(Breitengrad = replace(Breitengrad, Breitengrad == 4940027, 49400270)) %>%
        mutate(Breitengrad = replace(Breitengrad, Breitengrad == 4986667, 49866670)) %>%
        mutate(Breitengrad = replace(Breitengrad, Breitengrad == 5127126, 51271260)) %>%
        mutate(Breitengrad = replace(Breitengrad, Breitengrad == 1086667, 10866670)) %>%
        mutate(Breitengrad = replace(Breitengrad, Breitengrad == 5216291, 52162910)) %>% 
        mutate(Breitengrad = replace(Breitengrad, Breitengrad == 4768308, 47683080)) %>% 
        mutate(Breitengrad = replace(Breitengrad, Breitengrad == 5118429, 51184290)) %>%
        mutate(Laengengrad = replace(Laengengrad, Laengengrad == 1032755, 10327550)) %>%
        mutate(Laengengrad = replace(Laengengrad, Laengengrad == 1032755, 10327550)) %>%
        mutate(Laengengrad = replace(Laengengrad, Laengengrad == 1086667, 10866670)) %>% 
        
        mutate(Breitengrad = round(Breitengrad / 1000000, 4)) %>%
        mutate(Laengengrad = round(Laengengrad / 1000000, 4))
        

      
#Zusammenfügen aller Einzelteil Tabellen + #Verbinden der Einzelteil Tabelle mit Geodaten
      
Einzelteile_gesamt = bind_rows(T_01, T_02, T_03, T_04, T_05, T_06, T_11, T_12, T_13, T_14, T_15, T_21, T_22, T_23, T_24, T_25, T_30, T_31, T_32, T_33) %>%
                                left_join(tier2_geo, by = c("Werksnummer" = "Werk")) %>%
                                left_join(tier1_geo, by = c("Komponente_Werksnummer" = "Werk")) %>%
                                rename(Breitengrad = Breitengrad.x) %>%
                                rename(Laengengrad= Laengengrad.x) %>%
                                rename(Komponente_Breitengrad = Breitengrad.y) %>%
                                rename(Komponente_Laengengrad= Laengengrad.y)

#Löschen der Geo Tabellen
rm(tier1_geo, tier2_geo)

#Exportieren der fertigen Tabelle
write_csv(Einzelteile_gesamt, "shiny_datensatz.csv")

Einzelteile_gesamt_104 <- Einzelteile_gesamt %>%
  filter(Komponente_Herstellernummer == 104)
          
          
          
                                      
      