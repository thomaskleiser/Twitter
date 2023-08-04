#### 1. Laden der fuer die Datenanalyse relevanten Pakte ####

library(readr)
library(readxl)
library(tidyverse)
library(psych)

#### 2. Import des fuer die Datenanalyse relevanten Datensaetze: ####

# Festlegung des Arbeitsverzeichnis:
setwd("C:/Users/thoma/Documents/Data")

# Import des Datensatz fuer demokratische- und autokratische Regime (online unter:):
vdem <- read_csv("V-Dem_Full_StateYear_Data_v13.csv") %>%
  select(year,
         state_id = country_id,
         state_name = country_name,
         state_abb = country_text_id,
         liberal_democracy = v2x_libdem,
         type_of_regime = v2x_regime
         ) %>%
  filter(year >= 1960) %>%
  mutate(state_name = case_when(state_name == "Burma/Myanmar" ~ "Myanmar",
                                state_name == "The Gambia" ~ "Gambia",
                                state_name == "United States of America" ~ "United States",
                                state_name == "Palestine/West Bank" ~ "Palestine",
                                state_name == "Palestine/Gaza" ~ "Palestine",
                                TRUE ~ state_name)
         )

# Import des Datensatz fuer das Bruttoinlandproduk (BIP) (online unter: https://data.worldbank.org/indicator/NY.GDP.PCAP):
wb_gdp_per_capita <- read_csv("Worldbank_GDP_per_Capita_v2022.csv", skip=4) %>%
  pivot_longer(cols=5:67,
               values_to="gdp_per_capita",
               names_to="year") %>%
  select(year,
         state_name = "Country Name",
         gdp_per_capita) %>%
  mutate(year = as.numeric(year),
         state_name = case_when(state_name == "Cabo Verde" ~ "Cape Verde",
                                state_name == "Congo, Dem. Rep." ~ "Democratic Republic of the Congo",
                                state_name == "Congo, Rep." ~ "Republic of the Congo",
                                state_name == "Cote d'Ivoire" ~ "Ivory Coast",
                                state_name == "Egypt, Arab Rep." ~ "Egypt",
                                state_name == "Hong Kong SAR, China" ~ "Hong Kong",
                                state_name == "Iran, Islamic Rep." ~ "Iran",
                                state_name == "Korea, Dem. People's Rep." ~ "North Korea",
                                state_name == "Korea, Rep." ~ "South Korea",
                                state_name == "Kyrgyz Republic" ~ "Kyrgyzstan",
                                state_name == "Lao PDR" ~ "Laos",
                                state_name == "Russian Federation" ~ "Russia",
                                state_name == "Slovak Republic" ~ "Slovakia",
                                state_name == "Syrian Arab Republic" ~ "Syria",
                                state_name == "Turkiye" ~ "Turkey",
                                state_name == "Venezuela, RB" ~ "Venezuela",
                                state_name == "West Bank and Gaza" ~ "Palestine",
                                state_name == "Yemen, Rep." ~ "Yemen",
                                TRUE ~ state_name
                                ),
         gdp_per_capita = gdp_per_capita / 1000
         )

# Vereinigung des Datensatz fuer das Bruttoinlandprodukt mit dem Datensatz fuer demokratische und autokratische Regime:
vdem <- vdem %>%
  left_join(wb_gdp_per_capita,
            by=c("year",
                 "state_name")
            )

# Import des Datensatz fuer Korruption (online unter: https://www.transparency.de/fileadmin/Redaktion/Aktuelles/2023/CPI2022_Results.xlsx):
ti_corruption <- read_excel("TI_CPI_Data_v2022.xlsx", skip=2) %>%
  select(state_name = "Country / Territory",
         cpi = "CPI score 2022") %>%
  mutate(year = 2022,
         state_name = case_when(state_name == "Cabo Verde" ~ "Cape Verde",
                                state_name == "Congo" ~ "Republic of the Congo",
                                state_name == "Cote d'Ivoire" ~ "Ivory Coast",
                                state_name == "Guinea Bissau" ~ "Guinea-Bissau",
                                state_name == "Korea, North" ~ "North Korea",
                                state_name == "Korea, South" ~ "South Korea",
                                state_name == "United States of America" ~ "United States",
                                TRUE ~ state_name)
         ) %>%
  relocate(year, .before=state_name)

# Vereinigung des Datensatz fuer Korrupton mit dem Datensatz fuer demokratische und autokratische Regime:
vdem <- left_join(ti_corruption,
                  by=c("year",
                       "state_name")
                  )

#### 2. Erstellung der themes: ####

# ... fuer Saeulendiagramme:
theme_kupfer1 <- theme(plot.title = element_text(size = 25, face = "bold", hjust = 0.5),
                       plot.subtitle = element_text(size = 20, hjust = 0.5),
                       plot.caption = element_text(size = 15),
                       axis.title.x = element_blank(),
                       axis.title.y = element_text(size = 15, face = "bold"),
                       axis.text.x = element_text(size = 12.5, face = "bold", hjust = 0.5),
                       axis.text.y = element_text(size = 12.5, face = "bold"),
                       axis.line = element_line(color = "black"),
                       legend.title = element_text(size = 15, face = "bold", hjust = 0.5),
                       legend.text = element_text(size = 15),
                       legend.background = element_rect(color = "black"),
                       panel.grid.major.x = element_blank(),
                       panel.grid.major.y = element_line(color = "grey20"),
                       panel.grid.minor = element_blank(),
                       panel.background = element_blank()
)

# ... fuer Streudiagramme:
theme_kupfer3 <- theme(plot.title = element_text(size = 25, face = "bold", hjust = 0.5),
                       plot.subtitle = element_text(size = 20, hjust = 0.5),
                       plot.caption = element_text(size = 15),
                       axis.title = element_text(size = 15, face = "bold"),
                       axis.text = element_text(size = 12.5, face = "bold"),
                       axis.line = element_line(color = "black"),
                       legend.title = element_text(face = "bold", hjust = 0.5),
                       legend.background = element_rect(color = "black"),
                       panel.grid.major = element_line(color = "grey20"),
                       panel.grid.minor = element_blank(),
                       panel.background = element_blank()
)

#### 4. Datenvisualisierung: ####

# Visualisierung des BIPs nach Regiemtyp:
vdem %>%
  mutate(type_of_regime = factor(type_of_regime,
                                 levels = c(0,1,2,3),
                                 labels = c("closed autocracy",
                                            "electoral autocracy",
                                            "electoral democracy",
                                            "liberal democracy")
                                 )
         ) %>%
  filter(year == 2022) %>%
  ggplot( aes(x = type_of_regime, y = gdp_per_capita, fill = type_of_regime) ) +
    geom_boxplot() +
    labs(title = "The lie of wealth through oppression I",
         subtitle = "GDP per capita per regime type (2022)",
         y = "GDP per Capita (1000$)",
         caption = "Source: V-dem Institute; Worldbank") + 
    theme_kupfer1 +
    theme(legend.position = "none")

# Visualisierung des Zusammenhang des liberalen Demokratieindex und BIP:
vdem %>%
  ggplot( aes(x = liberal_democracy, y = gdp_per_capita) ) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_point(alpha = 0.25) +
  labs(title = "The lie of wealth through oppression II",
       subtitle = "linear relation between liberal democracy index and GDP per capita (1960-2022)",
       x = "Liberal Democracy Index (0.0-1.0)*",
       y = "GDP per Capita (1000$)",
       caption = "*0.0 = fully autocratic    1.0 = fully democratic
       r = 0.4941    p = 2.2e-16") +
  theme_kupfer3

# Visualisierung des BIP von
vdem %>% filter(year == 2022 &
                state_name %in% c("China","France","Germany","Norway","United States",
                                  "United Kingdom")
                ) %>%
  mutate() %>%
  ggplot( aes(x = reorder(state_name,desc(gdp_per_capita)), y = gdp_per_capita) ) +
  geom_bar(stat = "identity",
           fill = brewer.pal(9,"Set1")[2],
           width = 0.75) +
  labs(title = "The lie of wealth through oppression III",
       subtitle = "GDP per capita in major western democracies ... and China (2022)",
       y = "GDP per Capita (1000$)",
       caption = "Source: Worldbank") +
  theme_kupfer1 +
  theme(legend.position = "none")

#### 5. Datenanalyse: ####

# Berechnung der Korrelationsstaerke und -signifikanz zwischen liberalen Demokratieindex und BIP:
cor.test(vdem$liberal_democracy,vdem$gdp_per_capita)

# Anmerkung: Ein Korrealtionskoeffizient von 0.4941 spricht fuer einen mittelstarken Zusammenhang.
# Ein P-Wert nahe Null spricht fuer einen hoechst signifikanten Zusammenhang (die Wahrscheinlichkeit, dass die Korrelation zufaellig zustandekommt tendiert gegen Null)