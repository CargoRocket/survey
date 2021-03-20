if(file.exists(here("data", "responses.csv"))){
  data <- read_csv(file.path("data", "responses.csv"))
  emails <- data[!is.na(data$`deine E-Mail Adresse`), "deine E-Mail Adresse"]
  data <- data[, 3:45]
  
  write_csv(data, file.path("data", "data_raw.csv"))
  write_csv(emails, file.path("data", "email_addresses_betatest.csv"))
  unlink(file.path("data", "responses.csv"))
  
  # TODO: anonymisieren für Open Data
  # Stadt nur > 5 Antworten
  # Other löschen
  # Freies Textfeld löschen?
}

data_raw <- read_csv(file.path("data", "data_raw.csv"))

set_label(data_raw) <- colnames(data_raw)

data <- data_raw %>% 
  rename("Fahrradtyp" = "Ich fahre:\n",
         "Nutzung" = "Wofür nutzt du das Lastenfahrrad hauptsächlich?",
         "Lastenradtyp" = "Was für ein Lastenrad nutzt du hauptsächlich?",
         "Routing" = "Welches Routing verwendest du hauptsächlich zum (Lasten-)Fahrradfahren?",
         "Zufriedenheit_Routing" = "Wie zufrieden bist du mit dem Routing?",
         "Stadt" = "In welcher Stadt fährst du hauptsächlich (Lasten-)Fahrrad?",
         "Haeufigkeit_Nutzung" = "Nutzt du das (Lasten-)Fahrrad regelmäßig in deinem Alltag oder unregelmäßig für einzelne Gelegenheiten?",
         "Haeufigkeit_unbekannte_Routen" = "Wie häufig fährst du dir bisher unbekannte Routen mit dem (Lasten-)Fahrrad?",
         "Eantrieb" = "Hat dein (Lasten-)Fahrrad einen Elektro-Antrieb?",
         "Zufriedenheit_Infrastruktur" = "Wie zufrieden bist du insgesamt mit der Fahrradinfrastruktur in dem Gebiet, in dem du alltäglich fährst?",
         "Umweg_Stau_Autoverkehr" = "nicht / kaum passierbarer Stau durch Autoverkehr",
         "Umweg_schmaler_Radweg" = "schmale Radwege (< 1,5 m)",
         "Umweg_Kopfsteinpflaster" = "Kopfsteinplaster",
         "Umweg_unebener_Belag" = "unebener Straßenbelag (z.B. durch Wurzeln und Schlaglöcher)",
         "Umweg_Bordsteine" = "Bordsteine",
         "Umweg_Poller" = "Poller / Drängelgitter",
         "Umweg_Fussverkehr" = "Fußverkehr (z.B. geteilter Fuß- und Radweg)",
         "Umweg_Baustellen" = "Baustellen auf der Fahrbahn",
         "Umweg_Steigung" = "Steigung", 
         "Umweg_Kreuzung" = "Gefährliche Kreuzungen",
         "Umweg_kein_Schutzstreifen" = "Hauptstraßen ohne Schutzstreifen (dichtes, schnelles Überholen / Drängeln)",
         "Umweg_Gegenverkehr" = "schlecht passierbarer Gegenverkehr (z.B. Einbahnstraßen frei für Fahrräder)",
         "Umweg_Fussgaengerzone" = "Fußgängerzone: Radfahrende müssen absteigen",
         "Freq_Stau_Autoverkehr" = "nicht / kaum passierbarer Stau durch Autoverkehr_1",
         "Freq_schmaler_Radweg" = "schmale Radwege (< 1,5 m)_1",
         "Freq_Kopfsteinplaster" = "Kopfsteinplaster_1",
         "Freq_unebener_Belag" = "unebener Straßenbelag (z.B. durch Wurzeln und Schlaglöcher)_1",
         "Freq_Bordsteine" = "Bordsteine_1",
         "Freq_Poller" = "Poller / Drängelgitter_1",
         "Freq_Fussverkehr" = "Fußverkehr (z.B. geteilter Fuß- und Radweg)_1",
         "Freq_Baustellen" = "Baustellen auf der Fahrbahn_1",
         "Freq_Steigung" = "Steigung_1", 
         "Freq_Kreuzung" = "gefährliche Kreuzungen",
         "Freq_kein_Schutzstreifen" = "Hauptstraßen ohne Schutzstreifen (dichtes, schnelles Überholen / Drängeln)_1",
         "Freq_Gegenverkehr" = "schlecht passierbarer Gegenverkehr (z.B. Einbahnstraßen frei für Fahrräder)_1",
         "Freq_Fussgaengerzone" = "Fußgängerzone: Radfahrende müssen absteigen_1",
         "sonstige_Hindernisse" = "Gibt es sonstige Probleme oder Hindernisse beim (Lasten-)Radfahren, die hier nicht berücksichtigt wurden?",
         "Geschlecht" = "Dein Geschlecht",
         "Alter" = "Dein Alter",
         "Beta-Test" = "Möchtest du dich als Beta-Tester\\*in für unser Lastenrad-Routing eintragen?") %>% 
  # mutate(Nutzung = coalesce(Nutzung, Other), .after = "Other") %>%
  mutate(Typ_all = coalesce(Lastenradtyp, Other_1, Fahrradtyp), .after = "Fahrradtyp") %>% 
  mutate(Routing = coalesce(Routing, Other_2)) %>% 
  select(-Other, -Other_1, -Other_2, -Lastenradtyp) %>% 
  mutate(Typ_all = ifelse(Typ_all == "filibus", "Frontloader (einspurig)", Typ_all)) %>% ## weise "other" Angaben zu
  mutate(Typ_all = ifelse(Typ_all == "ONO Pioneers Edition", "Schwerlast (mehrspurig)", Typ_all)) %>% 
  mutate(Typ_all = ifelse(Typ_all == "Zu gleichen Teilen A und F", "Anhänger", Typ_all)) %>% 
  mutate(Nutzung = ifelse(Nutzung == "privater Transport von Einkäufen / Gegenständen / Haustieren", "privater Transport Gegenstände", Nutzung)) %>% 
  mutate(Nutzung = ifelse(Nutzung == "gewerbliche Nutzung  (z.B. Lieferdienst)", "gewerbliche Nutzung", Nutzung)) %>% 
  mutate(Stadt = stringr::str_to_title(Stadt)) %>% 
  mutate(Stadt = ifelse(Stadt == "Frankfurt/M.", "Frankfurt Am Main", Stadt)) %>% 
  mutate(Stadt = ifelse(Stadt == "Harburg/Hamburg", "Hamburg", Stadt)) %>% 
  mutate(Stadt = ifelse(Stadt == "Hamburg Und Unzu", "Hamburg", Stadt)) %>% 
  mutate(Stadt = ifelse(Stadt == "Münster I. W.", "Münster", Stadt))

stadt_kleiner_5 <- data %>% 
  tabyl(Stadt) %>% 
  arrange(desc(n)) %>% 
  filter(n < 5) %>% 
  pull(Stadt)

data <- data %>% 
  mutate(Stadt = ifelse(Stadt %in% stadt_kleiner_5, "Sonstige", Stadt))


recode_umweg <- function(x) {
  return(recode(x, 
                "0 min" = "0", 
                "2 min" = "2",
                "4 min" = "4",
                "6 min" = "6",
                "8 min" = "8",
                "10 min oder mehr" = "10"))
}

# labels werden überschrieben mit recode_umweg. Quick fix: speichern und wieder setzen
temp_labels <- get_label(data[ , grepl( "Umweg_" , names( data ) ) ])

data$Umweg_Stau_Autoverkehr <- recode_umweg(data$Umweg_Stau_Autoverkehr) %>% as.numeric()
data$Umweg_schmaler_Radweg <- recode_umweg(data$Umweg_schmaler_Radweg)%>% as.numeric()
data$Umweg_Kopfsteinpflaster <- recode_umweg(data$Umweg_Kopfsteinpflaster)%>% as.numeric()
data$Umweg_unebener_Belag <- recode_umweg(data$Umweg_unebener_Belag)%>% as.numeric()
data$Umweg_Bordsteine <- recode_umweg(data$Umweg_Bordsteine)%>% as.numeric()
data$Umweg_Poller <- recode_umweg(data$Umweg_Poller)%>% as.numeric()
data$Umweg_Fussverkehr <- recode_umweg(data$Umweg_Fussverkehr)%>% as.numeric()
data$Umweg_Baustellen <- recode_umweg(data$Umweg_Baustellen)%>% as.numeric()
data$Umweg_Steigung <- recode_umweg(data$Umweg_Steigung)%>% as.numeric()
data$Umweg_Kreuzung <- recode_umweg(data$Umweg_Kreuzung)%>% as.numeric()
data$Umweg_kein_Schutzstreifen <- recode_umweg(data$Umweg_kein_Schutzstreifen)%>% as.numeric()
data$Umweg_Gegenverkehr <- recode_umweg(data$Umweg_Gegenverkehr)%>% as.numeric()
data$Umweg_Fussgaengerzone <- recode_umweg(data$Umweg_Fussgaengerzone)%>% as.numeric()

set_label(data[ , grepl( "Umweg_" , names( data ) ) ]) <- temp_labels

