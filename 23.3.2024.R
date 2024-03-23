#### 1.Setup ####

# Zurücksetzen der Arbeitsumgebung:
rm(list = ls())

# Installation und Laden der fuer die Datenanalyse verwendeten Pakte
packages <- c("cowplot","patchwork","RColorBrewer","tidyverse")

for (package in packages) {
  if (!package %in% installed.packages()) {
    install.packages(package)
  }
  library(package, character.only = TRUE)
}

# Festlegen des Arbeitsverzeichnis:
setwd("C:/Users/thoma/Documents/Data")

# Import des Chapell Hill Expert Survey Datensatz fuer Parteien (online unter: https://www.chesdata.eu/ches-europe):
ches_party_2019 <- read_csv("CHES_Data_v3.csv") |>
  zap_label() |>
  select(year,
         country_id = country,
         ches_id = party_id,
         manifesto_id = cmp_id,
         party_abb = party,
         rile = lrgen,
         rile_econ = lrecon,
         rile_cult = galtan,
         expert) |>
  filter(year == 2019 & country_id == 3) |>
  select(-year,-country_id)

# Imports des Manifesto Projects Datensatz fuer Parteien (online unter: https://manifesto-project.wzb.eu/datasets):
mp_party_2017 <- read_csv("MP_Data_v2023a.csv") |>
  zap_label() |>
  select(date,
         country_id = country,
         manifesto_id = party,
         party_abb = partyabbrev,
         # sozio-ökonomische Positionen:
         #   sozio-ökonomisch linke Positionen:
         pos_mark_regul = per403,
         pos_plan_econ = per404,
         pos_demand_econ = per409,
         pos_control_econ = per412,
         pos_national_econ = per413,
         pos_protection = per406,
         pos_welfare_expand = per504,
         #   sozio-ökonomisch rechte Positionen:
         pos_mark_econ = per401,
         pos_supply_econ = per402,
         pos_privat_econ = per4011,
         neg_protection = per407,
         pos_welfare_limit = per504,
         # sozio-kulturelle Postionen:
         #   sozio-kulturell linke Positionen:
         neg_national_way = per602,
         neg_tradition = per604,
         pos_multicult = per607,
         #   sozio-kulturell rechte Positionen:
         pos_national_way = per601,
         pos_tradition = per603,
         neg_multicult = per608,
         # gesellschaftliche Zielgruppen:
         #   linke gesellschaftliche Zielgruppen
         pos_labour = per701,
         pos_professional = per703,
         pos_minority = per704,
         #   rechte gesellschaftliche Zielgruppen
         neg_labour = per702,
         pos_farmer = per703) |>
  mutate(year = as.numeric(str_extract(as.character(date),"\\d{4}"))) |>
  relocate(year, .before = date) |>
  filter(year == 2017 & country_id == 41) |>
  select(-date,-year,-country_id)

# Import der German Longitudinal Elections Study (GLES) 2009-2017 (online unter: https://search.gesis.org/research_data/ZA6835)
gles_2021 <- read_spss("GLES_Nachwahl_Data_v2021.sav") |>
  zap_label() |>
  select(wahl = q114ba, # -99 = keine Angabe, -98 = weiß nicht, -97 = trifft nicht zu, -94 = nicht in Auswahlgesamtheit -83 = ungültig wählen, 1 = CDU/CSU, 4 = SPD, 5 = FDP, 6 = GRÜNE, 7 = DIE LINKE, 206 = NPD, 215 = Piraten, 322 = AfD, 801 = sonstige
         rile_selbst = q37, # -99 = keine Angabe, -98 = weiß nicht, -97 = trifft nicht zu, 1 = links [...] 11 = rechts 
         rile_cdu = q35b, # -99 = keine Angabe, -98 = weiß nicht, -97 = trifft nicht zu, -71 = kenne den Begriff/die Begriffe nicht, 1 = links [...] 11 = rechts
         rile_csu = q35c, #...
         rile_spd = q35d, #...
         rile_fdp = q35e, #...
         rile_gruene = q35f, #...
         rile_die_linke = q35g, #...
         rile_afd = q35h) #...

#### 3. Datenbereinigung: ####

# filtern fehlender Werte (GLES):
filter_values_gles <- c(-99,-98,-97,-94,-93,-92,-91,-86,-85,-84,-83,-82,-81,-73,-72,-71)

#filtern fehlender Werte für GLES 2017:
gles_2021 <- gles_2021 |>
  filter(if_all(everything(), ~ !. %in% filter_values_gles))

#### 4. Datenmanipulation: ####

ches_party_2019 <- ches_party_2019 |>
  # Faktorisieren der Variablen, bei denen dies sinnvoll erscheint:
  mutate(party_abb = factor(case_when(party_abb == "GRUNEN" ~ "DIE GRÜNEN",
                                      party_abb == "LINKE" ~ "DIE LINKE",
                                      TRUE ~ party_abb))
  )

ches_party_2019 <- ches_party_2019 |>
  # Erstellen eines Eintrags für die gemittelten Werte für CDU und CSU:
  rbind(ches_party_2019 |>
          filter(party_abb %in% c("CDU","CSU")) |>
          summarise(across(rile:rile_cult, ~ mean(., na.rm = TRUE))) |>
          mutate(ches_id = 301,
                 manifesto_id = 41521,
                 party_abb = "CDU/CSU",
                 expert = 21) |>
          relocate(c(ches_id,manifesto_id,party_abb), .before = rile)
  )

mp_party_2017 <- mp_party_2017 |>
  # Faktorisieren der Variablen, bei denen dies sinnvoll erscheint:
  mutate(party_abb = factor(case_when(party_abb == "90/Greens" ~ "DIE GRÜNEN",
                                      party_abb == "LINKE" ~ "DIE LINKE",
                                      TRUE ~ party_abb)),
         # Erstellen einer Variable, die angibt, wie häufig die Partei sozio-ökonomisch linke Forderungen stellt:
         le_econ = pos_mark_regul + pos_plan_econ + pos_demand_econ + pos_control_econ + pos_national_econ + pos_protection + pos_welfare_expand,
         # Erstellen einer Variable, die angibt, wie häufig die Partei sozio-ökonomisch rechte Forderungen stellt:
         ri_econ = pos_mark_econ + pos_supply_econ + pos_welfare_limit + pos_privat_econ + neg_protection,
         # Erstellen einer Variable, die angibt, ob die Partei insgesamt häufiger sozio-ökonomisch linke oder rechte Forderungen stellt:
         rile_econ = ri_econ - le_econ,
         # Erstellen einer Variablen, die angibt, wie häufig die Partei sozio-kulturell linke Forderungen stellt:
         le_cult = neg_national_way + neg_tradition + pos_multicult,
         # Erstellen einer Variablen, die angibt, wie häufig die Partei sozio-kulturell rechte Forderungen stellt:
         ri_cult = pos_national_way + pos_tradition + neg_multicult, 
         # Erstellen einer Variablen, die angibt, ob die Partei insgesamt häufiger sozio-kulturell linke oder rechte Forderungen stellt:
         rile_cult = ri_cult - le_cult,
         # Erstellen einer Variable, die angibt ob die Partei insgesamt häufiger linke oder rechte Forderungen stellt:
         rile = ri_econ + ri_cult - le_econ - le_cult)

# Vereinigung des Datensatz Chapell Hill Expert Survey Datensatz mit dem Manifesto Project Datensatz:
ches_2019_mp_2017_party <- ches_party_2019 |>
  left_join(mp_party_2017,
            by = c("manifesto_id","party_abb"),
            suffix = c("_ches","_mp")
  )

ches_2019_mp_2017_party <- ches_2019_mp_2017_party |>
  mutate(party_abb = fct_reorder(party_abb,rile_ches, .desc = TRUE))

gles_2021 <- gles_2021 |>
  # Umformatierung des Datensatzes von einem wide- zu eine long-format:
  pivot_longer(cols = rile_cdu:rile_afd,
               names_to = "party_abb",
               values_to = "rile_party") |>
  mutate(party_abb = str_extract(party_abb, "(?![rile_])\\w*"))

gles_2021 <- gles_2021 |>
  # Faktorisieren der Variablen, bei denen dies sinnvoll erscheint:
  mutate(wahl = factor(case_when(wahl == 1 ~ "CDU/CSU",
                                 wahl == 4 ~ "SPD",
                                 wahl == 5 ~ "FDP",
                                 wahl == 6 ~ "DIE GRÜNEN",
                                 wahl == 7 ~ "DIE LINKE",
                                 wahl == 322 ~ "AfD",
                                 TRUE ~ "Nichtwähler/Sonstige")
  ),
  party_abb =  factor(case_when(party_abb == "cdu" ~ "CDU",
                                party_abb == "csu" ~ "CSU",
                                party_abb == "spd" ~ "SPD",
                                party_abb == "fdp" ~ "FDP",
                                party_abb == "gruene" ~ "DIE GRÜNEN",
                                party_abb == "die_linke" ~ "DIE LINKE",
                                TRUE ~ "AfD"),
                      levels = c("AfD","CSU","CDU","FDP","SPD","DIE GRÜNEN","DIE LINKE")
  ),
  rile_party_trans = (9/10) * rile_party + (1 - (9/10) * 1)
  )

#### 5. Datenvisualisierung: ####

# Festlegen des Standard-themes:
theme_set(theme_classic() +
            theme(plot.title = element_text(size = 25, face = "bold", hjust = 0.5),
                  plot.subtitle = element_text(size = 22.5, face = "italic", hjust = 0.5),
                  plot.caption = element_text(size = 20, face = "italic"),
                  legend.title = element_text(size = 20, face = "bold", hjust = 0.5),
                  legend.text = element_text(size = 17.5, hjust = 0.5),
                  axis.title = element_text(size = 20, face = "bold"),
                  axis.text = element_text(size = 17.5),
                  panel.grid.major.y = element_line(linewidth = 1)
            )
)

# Erstellen von FArbpalleten für die Visualisierung I:
partei_farben1 <- c("CDU" = "#000000","CSU" = "#008ac5","SPD" = "#e2001a","DIE GRÜNEN" = "#1fa12d","FDP" = "#ffed00",
                    "AfD" = "#009fe1","DIE LINKE" = "#6f003c","Freie Wähler" = "#f29204","SSW" = "#003c8f","Sonstige" = "grey25")

partei_farben2 <- c("CDU/CSU" = "#000000","SPD" = "#e2001a","DIE GRÜNEN" = "#1fa12d","FDP" = "#ffed00","AfD" = "#009fe1",
                    "DIE LINKE" = "#6f003c","Freie Wähler" = "#f29204","SSW" = "#003c8f","Sonstige" = "grey25")

# Erstellen von Hilfsfunktionen für die Einfärbung nach Parteifarben:
partei_farben_funktion1 <- function(x) {
  case_when(x == "CDU" ~ partei_farben1["CDU"],
            x == "CSU" ~ partei_farben1["CSU"],
            x == "SPD" ~ partei_farben1["SPD"],
            x == "DIE GRÜNEN" ~ partei_farben1["DIE GRÜNEN"],
            x == "FDP" ~ partei_farben1["FDP"],
            x == "DIE LINKE" ~ partei_farben1["DIE LINKE"],
            x == "AfD" ~ partei_farben1["AfD"],
            TRUE ~ "grey25")
}

# Erstellen einer Hilfsfunktion zur eindimensionalen Visualisierung der links-rechts-Verortung gemäß Wahlprogrammen (eindimensional):
ri_vs_le1 <- function(df, variable1, variable2, farbe = partei_farben1, subtitle = "",
                      xlabs = "", ylabs = "", caption = "", text_size = 5, text_abstand = 2.5) {
  variable1 <- enquo(variable1)
  variable2 <- enquo(variable2)
  
    ggplot(df, aes(x = !!variable1, y = !!variable2, colour = party_abb)) +
      geom_point(size = 4) +
      geom_vline(xintercept = 5, linewidth = 1, linetype = "dashed", colour = "grey25") +
      scale_colour_manual(values = farbe) +
      scale_x_continuous(breaks = c(1,2.5,5,7.5,10),
                         labels = c(1,2.5,5,7.5,10),
                         limits = c(0.75,10.25)) +
      guides(colour = "none") +
      labs(subtitle = subtitle,
           x = xlabs,
           y = ylabs,
           caption = caption) +
      theme(axis.title.y = element_blank())
}

# Visualisierung der Links-Rechts-Verortung von Parteien gemäß Experten und gemäß Wählern:
plot6.1 <- ches_2019_mp_2017_party |>
  filter(!party_abb %in% c("CSU","CDU","Piraten","DieTier")) |>
  ri_vs_le1(rile_ches, party_abb, farbe = partei_farben2,
            subtitle = "Eindimensionale Verortung\n(Expertenbefragung, 2020)",
            xlab = "politische Ausrichtung*",
            caption = "*1 = sehr links [...] 10 = sehr rechts
            Quelle: CHES 2019")

gles_2021_summary <- gles_2021 |>
  filter(wahl != "Nichtwähler/Sonstige") |>
  group_by(wahl, party_abb) |>
  summarise(rile_party_mean = mean(rile_party))

plot6.2 <- ri_vs_le1(gles_2021_summary,
                     rile_party_mean, wahl,
                     partei_farben_funktion1(gles_2021_summary$party_abb),
                     subtitle = "Eindimensionale Verortung\n(Wählerbefragung, 2021)",
                     x = "politische Verortung (1-10)*",
                     caption = "*1 = links [...] 10 = rechts
                     Quelle: GLES 2021")

plot6.1 + plot6.2 +
  plot_annotation("Links-Rechts-Ausrichtung: Expertenbefragung vs. Wählerbefragung")