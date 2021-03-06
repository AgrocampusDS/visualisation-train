# package loading
library(readr)
library(tidyverse)

# data processing
train <- read_csv("data/Regularities_by_liaisons_Trains_France.csv")
coord <- read_delim("data/Coordonnees.csv", delim = ";", escape_double = FALSE,
                    trim_ws = TRUE)

# add  a d season fields
train <- train %>% 
  mutate(
    Nb_circulations = `Number of expected circulations`-`Number of cancelled trains`,
    # % of late trains (on arrival)
    Percent_trains_late = `Number of trains late on arrival`/Nb_circulations,
    # Season from month
    Season = case_when(Month %in% 2:4 ~ "A",
                       Month %in% 5:7 ~ "B",
                       Month %in% 8:10 ~ "C",
                       Month %in% c(11, 12, 1) ~ "D"),
    # Number of late trains on arrival by reason
    Nb_late_external_causes = `Number of trains late on arrival`*`% trains late due to external causes (weather, obstacles, suspicious packages, malevolence, social movements, etc.)`,
    Nb_late_infrastructure = `Number of trains late on arrival`*`% trains late due to railway infrastructure (maintenance, works)`,
    Nb_late_traffic_mgmt = `Number of trains late on arrival`*`% trains late due to traffic management (rail line traffic, network interactions)`,
    Nb_late_rolling_stock = `Number of trains late on arrival`*`% trains late due to rolling stock`,
    Nb_late_station_mgmt = `Number of trains late on arrival`*`% trains late due to station management and reuse of material`,
    Nb_late_passengers = `Number of trains late on arrival`*`% trains late due to passenger traffic (affluence, PSH management, connections)`
  )

month.name.fr <- c("Janvier", "Février", "Mars", "Avril", "Mai", "Juin", "Juillet",
                   "Août", "Septembre", "Octobre", "Novembre", "Décembre")
season.name.fr <- c("Printemps", "Été", "Automne", "Hiver")




## add date and liaison fields
train <- train %>% 
  mutate(Liaison = paste(`Departure station`, `Arrival station`, sep = " - "),
         Month = as.factor(Month),
         Year = as.factor(Year))

# Nombre de mois sur la période (jan. 2015 - jun. 2020)
train %>% 
  select(Year, Month) %>% 
  mutate(MonthYr = paste(Month, Year)) %>% 
  select(MonthYr) %>% 
  unique() %>% nrow()

# Nombre de gares (LGV)
train %>% 
  select(`Departure station`) %>% 
  unique() %>% nrow()

# Nombre de liaisons par mois
train %>% 
  group_by(Year, Month) %>% 
  summarise(Nb = n())

# Liaisons < 66 sur la période (n'apparaissent pas tous les mois)
liaisons_occasional <- train %>%
  mutate(Liaison = paste(`Departure station`, `Arrival station`, sep = " - ")) %>% 
  group_by(Liaison) %>% 
  summarise(Nb_mois = n()) %>% 
  filter(Nb_mois < 66) %>% 
  select(Liaison) %>% pull()



# Évolution saisonnière des retards
df_dep <- train %>% 
  filter(str_detect(`Departure station`, "PARIS")) %>% 
  left_join(coord, by = c("Arrival station" = "Departure station")) %>% 
  group_by(Season, Direction) %>% 
  summarise(Retards = mean(Percent_trains_late_dep, na.rm = TRUE))
df_arr <- train %>% 
  filter(str_detect(`Departure station`, "PARIS")) %>% 
  left_join(coord, by = c("Arrival station" = "Departure station")) %>% 
  group_by(Season, Direction) %>% 
  summarise(Retards = mean(Percent_trains_late_arr, na.rm = TRUE))
df_dep_arr <- rbind(df_dep, df_arr)
df_dep_arr$Gare <- as.factor(c(rep("En gare de départ", 16), rep("En gare d'arrivée", 16)))

df_dep_arr %>% 
  # diagramme en barres
  ggplot() + aes(x = Season, y = Retards*100, fill = tolower(Direction)) +
  geom_bar(stat = "identity", position='dodge', width = 0.5) +
  scale_x_discrete(labels = season.name.fr) +
  ylab(element_blank()) + xlab(element_blank()) +
  labs(title = "Trains en retard (en %)",
       caption = "Période : janvier 2015 à juin 2020") +
  theme(plot.title = element_text(size = 12, face = "bold.italic", hjust = 0.5),
        plot.caption = element_text(size = 8, face = "italic", hjust = 1),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(vjust = 0.5),
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line(color = 'grey', linetype = 'dotted'),
        panel.grid.major.x = element_blank(),
        axis.ticks.x = element_blank()) +
  # scale_fill_brewer(palette="Paired")  # colorblind -friendly palette
  # scale_fill_brewer(palette="Dark2)
  scale_fill_viridis(discrete=TRUE) +
  facet_wrap(~Gare, nrow = 2, strip.position = "right")




# nombre de trajets par direction
train %>% 
  filter(str_detect(`Departure station`, "PARIS")) %>% 
  left_join(coord, by = c("Arrival station" = "Departure station")) %>% 
  group_by(Direction) %>% 
  summarise(Nb_trajets = sum(Nb_circulations, na.rm = TRUE))



# Évolution saisonnière des retards selon leur motif
df_circu <- train %>%
  filter(str_detect(`Departure station`, "PARIS")) %>%
  left_join(coord, by = c("Arrival station" = "Departure station")) %>%
  select(`Arrival station`, Season, Direction, Nb_circulations) %>% 
  group_by(Season, Direction) %>% 
  summarise(Nb_trajets = sum(Nb_circulations, na.rm = TRUE))

df_graph2 <- train %>%
  filter(str_detect(`Departure station`, "PARIS")) %>%
  left_join(coord, by = c("Arrival station" = "Departure station")) %>%
  select(`Arrival station`, Season, Direction,
         Nb_late_external_causes,
         Nb_late_infrastructure,
         Nb_late_traffic_mgmt,
         Nb_late_rolling_stock,
         Nb_late_station_mgmt,
         Nb_late_passengers) %>% 
  # pivot dataframe to switch causes from 6 columns to 6 row per individual
  pivot_longer(cols = -c(`Arrival station`, Season, Direction)) %>% 
  mutate(Motif = case_when(name=="Nb_late_external_causes"~"Causes extérieures",
                           name=="Nb_late_infrastructure"~"Infrastructure (maintenance, travaux)",
                           name=="Nb_late_traffic_mgmt"~"Gestion du trafic (voie ferrée)",
                           name=="Nb_late_rolling_stock"~"Matériel roulant",
                           name=="Nb_late_station_mgmt"~"Gestion en gare, réutilisation de matériel",
                           name=="Nb_late_passengers"~"Prise en compte voyageurs")) %>% 
  group_by(Season, Direction, Motif) %>% 
  summarise(Retards = sum(value, na.rm = TRUE)) %>% 
  left_join(df_circu, by = c("Season", "Direction")) %>% 
  mutate(Perc_retard = Retards/Nb_trajets)
  
df_graph2 %>% 
  # diagramme en barres
  ggplot() + aes(x = Season, y = Perc_retard*100, fill = tolower(Direction)) +
  geom_bar(stat = "identity", position='dodge', width = 0.5) +
  scale_x_discrete(labels = season.name.fr) +
  # scale_y_continuous(labels = c(0, 100, 200, 300)) +
  ylab(element_blank()) + xlab(element_blank()) +
  labs(title = "Trains en retard selon le motif (en %)",
       caption = "Période : janvier 2015 à juin 2020") +
  theme(plot.title = element_text(size = 12, face = "bold.italic", hjust = 0.5),
        plot.caption = element_text(size = 8, face = "italic", hjust = 1),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(vjust = 0.5),
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line(color = 'grey', linetype = 'dotted'),
        panel.grid.major.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_fill_brewer(palette="Dark2") +
  facet_wrap(~Motif, scales = "fixed")


# # Évolution mensuelle des retards
# train %>% 
#   # conservations des liaisons fréquentes (au moins une par mois)
#   filter(!Liaison %in% liaisons_occasional) %>% 
#   group_by(Month) %>% 
#   summarise(Retards = mean(Percent_trains_late, na.rm = TRUE)) %>%
#   # diagramme en barres
#   ggplot() + aes(x = Month, y = Retards*100) +
#   geom_bar(stat = "identity", width = 0.5, fill = "#0088CE", alpha = 1) +
#   scale_x_discrete(labels = month.name.fr) +
#   ylab(element_blank()) + xlab(element_blank()) +
#   labs(title = "Trains en retards à la gare d'arrivée (en %)",
#        caption = "Période : janvier 2015 à juin 2020") +
#   theme(plot.title = element_text(size = 12, face = "bold.italic", hjust = 0.5),
#         plot.caption = element_text(size = 8, face = "italic", hjust = 1),
#         axis.text.x = element_text(angle = 40, vjust = 0.5),
#         panel.background = element_rect(fill = "white"),
#         panel.grid = element_line(color = 'grey', linetype = 'dotted'),
#         panel.grid.major.x = element_blank(),
#         axis.ticks.x = element_blank())


# # Répartition mensuelle des retards
# train %>%
#   # conservations des liaisons fréquentes (au moins une par mois)
#   filter(!Liaison %in% liaisons_occasional) %>%
#   group_by(Month, Liaison) %>%
#   summarise(Delay = mean(`Average delay of late arriving trains (min)`, na.rm = TRUE)) %>%
#   # boxplot par mois
#   ggplot() + aes(x = Month, y = Delay) +
#   geom_boxplot(aes(group = Month), outlier.shape = 19, color = "#0088CE") +
#   scale_x_discrete(labels = month.name.fr) +
#   ylab("Délai (en minutes)") + xlab(element_blank()) +
#   labs(title = "Répartition du délai moyen des trains en retard à la gare d'arrivée",
#        caption = "Période : janvier 2015 à juin 2020") +
#   theme(plot.title = element_text(size = 12, face = "bold.italic", hjust = 0.5),
#         plot.caption = element_text(size = 8, face = "italic", hjust = 1),
#         axis.text.x = element_text(angle = 40, vjust = 0.5),
#         panel.background = element_rect(fill = "white"),
#         panel.grid = element_line(color = 'grey', linetype = 'dotted'),
#         axis.ticks.x = element_blank())


# # Évolution mensuelle des annulations
# train %>% 
#   # conservations des liaisons fréquentes (au moins une par mois)
#   filter(!Liaison %in% liaisons_occasional) %>% 
#   group_by(Month) %>% 
#   summarise(Cancel = mean(`Number of cancelled trains`, na.rm = TRUE)) %>%
#   # diagramme en barres
#   ggplot() + aes(x = Month, y = Cancel) +
#   geom_bar(stat = "identity", width = 0.5, fill = "#0088CE", alpha = 1) +
#   scale_x_discrete(labels = month.name.fr) +
#   ylab(element_blank()) + xlab(element_blank()) +
#   labs(title = "Nombre moyen de trains annulés",
#        caption = "Période : janvier 2015 à juin 2020") +
#   theme(plot.title = element_text(size = 12, face = "bold.italic", hjust = 0.5),
#         plot.caption = element_text(size = 8, face = "italic", hjust = 1),
#         axis.text.x = element_text(angle = 40, vjust = 0.5),
#         panel.background = element_rect(fill = "white"),
#         panel.grid = element_line(color = 'grey', linetype = 'dotted'),
#         panel.grid.major.x = element_blank(),
#         axis.ticks.x = element_blank())


