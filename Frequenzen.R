####################################################
#### Keto Schumacher, UZH                       ####
#### Capstone Course Media Representation       ####
#### Erklaerungsmodell                          ####
####################################################

#### Preparations ####

# Cleaning
rm(list=ls())
detach("package:MASS", unload=TRUE)

# define stringsAsFactors = FALSE as default
options(stringsAsFactors = FALSE)

# Loading Packages
library(tidyverse)
library(tictoc)
library(quanteda)

# Loading the Data
# mentions
df_names_parties_etc_per_doc <- readRDS("~/capstone_share/Names_Full_and_Lastnames/df_names_parties_etc_per_doc.rds")

# dictionary
df_names_dict_national_roles <- readRDS("~/capstone_share/Names_Full_and_Lastnames/df_names_dict_national_roles.rds")

# information on texts ad publications
df_texts_full_and_lastnames <- readRDS("~/capstone_share/Names_Full_and_Lastnames/df_texts_full_and_lastnames.rds")%>%
  rename(document = doc_id)

# information on media where the texts are published
df_medien_teilnehmer_typ <- readRDS("~/capstone_share/df_medien_teilnehmer_typ.rds")%>%
  rename(so = "Kürzel")

# Data wrangling
# summarise the number of kommissionspräsidentschaften
df_names_dict_national_roles2 <- df_names_dict_national_roles %>%
  group_by(year,
           Regex_Bruno,
           name,
           geburtsjahr,
           dict,
           Funktion,
           CouncilName,
           year_joined_parliament,
           year_joined_council,
           year_left,
           Kanton_Abkuerzung,
           parteiname,
           geschlecht,
           Fraktion_Abkuerzung
           )%>%
  summarise(Sonderrollen_Anzahl = n())%>%
  ungroup()

# recode missings
df_names_dict_national_roles2 <- df_names_dict_national_roles2 %>%
  mutate(Funktion =  replace_na(Funktion, "Keine_Sonderrolle"))

# transform to wide 
df_names_dict_national_roles_spread <- df_names_dict_national_roles2 %>%
  spread(value = Sonderrollen_Anzahl, key = Funktion, fill = 0)

# calculate political age (Amtsdauer im Parlament)
df_names_dict_national_roles_spread$political_age <- df_names_dict_national_roles_spread$year - df_names_dict_national_roles_spread$year_joined_parliament

# calculate the age of the politician
df_names_dict_national_roles_spread$age <- df_names_dict_national_roles_spread$year - df_names_dict_national_roles_spread$geburtsjahr

# if a person changed legislative chamber, the entry for the council in which the MP served shorter in the year of change is deleted
df_names_dict_national_roles_spread <- df_names_dict_national_roles_spread %>%
  filter(!((CouncilName == "Ständerat" & dict == "Personenmarker_Andrea_Claudio_Caroni" & year == 2015)|
             (CouncilName == "Nationalrat" & dict == "Personenmarker_Christian_Levrat" & year == 2012)|
             (CouncilName == "Ständerat" & dict == "Personenmarker_Daniel_Jositsch" & year == 2015)|
             (CouncilName == "Ständerat" & dict == "Personenmarker_Olivier_Français" & year == 2015)|
             (CouncilName == "Ständerat" & dict == "Personenmarker_Philipp_Müller" & year == 2015)|
             (CouncilName == "Ständerat" & dict == "Personenmarker_Ruedi_Noser" & year == 2015)))

# add text information
# check names
names(df_names_parties_etc_per_doc)
names(df_texts_full_and_lastnames)

# left join by doc_id
df_names_parties_etc_per_doc_texts <- left_join(df_names_parties_etc_per_doc, df_texts_full_and_lastnames)

# add information on text category
df_names_parties_etc_per_doc_texts_nomatch <- anti_join(df_names_parties_etc_per_doc_texts, df_medien_teilnehmer_typ)

# which do not match?
table(df_names_parties_etc_per_doc_texts_nomatch$so)

# recode existing
df_medien_teilnehmer_typ$so <- df_medien_teilnehmer_typ$so%>%
  recode(`SOZ / SOZM` = "SOZM", `BLZ / BZM` = "BZM")

# add new entry
# check which variables are needed
names(df_medien_teilnehmer_typ)

df_medium_nlz <- data.frame("Partner" = "NZZ Mediengruppe", "Split" = "CH Media",
                            "Produkte" = "Neue Luzerner Zeitung", "so"= "NLZ",
                            "Erscheinung" = "Tag", "Überregional" = 1, "foeg_2017" = "6.77", 
                            "Typ" = "Tageszeitung")

df_medien_teilnehmer_typ <- bind_rows(df_medien_teilnehmer_typ, df_medium_nlz)

# check again
df_names_parties_etc_per_doc_texts_nomatch <- anti_join(df_names_parties_etc_per_doc_texts, df_medien_teilnehmer_typ)

# titles not in list of titles
not <- which(!unique(df_names_parties_etc_per_doc_texts$so)%in%unique(df_medien_teilnehmer_typ$so))
unique(df_names_parties_etc_per_doc_texts$so)[not]

# no articles from which titles
not <- which(!unique(df_medien_teilnehmer_typ$so)%in%unique(df_names_parties_etc_per_doc_texts$so))
unique(df_medien_teilnehmer_typ$so)[not]

# seems good
# join finally
df_names_parties_etc_per_doc_texts <- left_join(df_names_parties_etc_per_doc_texts, df_medien_teilnehmer_typ)

# issues with party changes: check which politicians are affected, then recode manually
# rules used: use national party membership e.g. BastA in canton of Basel is a member of the GPS
# if a party change occured during the observation period, code accordingly
# if a party change occured before observation period, use latest party membership
df_names_dict_national_roles$dict[replace_na(df_names_dict_national_roles$mehr_partei, 0) ==1] %>%
  unique()

df_names_party_changes <- df_names_dict_national_roles %>%
  filter(mehr_partei==1)

# Thomas Müller: SVP, changed Party before our timeframe (2011), but there are other Thomas Müllers in our dataset, hence remove entry with CVP

"Personenmarker_Thomas_Müller_CVP"
df_names_parties_etc_per_doc_texts <- df_names_parties_etc_per_doc_texts%>%
  filter(dict!="Personenmarker_Thomas_Müller_CVP")

df_names_dict_national_roles_spread <- df_names_dict_national_roles_spread%>%
  filter(dict!="Personenmarker_Thomas_Müller_CVP")

# calculate occurences per year per person
# create year
df_names_parties_etc_per_doc_texts$year <- stringr::str_extract(df_names_parties_etc_per_doc_texts$document,
                                                          ".*?(?=_.*)")%>%
  as.numeric()

# add binary variable indicating whether it is tamedia or not
df_names_parties_etc_per_doc_texts <- df_names_parties_etc_per_doc_texts%>%
  mutate(Tamedia = ifelse(Partner == "Tamedia AG", 1, 0))

# summarise on basis of year per person per media:
df_occurrences_per_year_media <- df_names_parties_etc_per_doc_texts%>%
  group_by(dict,
           NAME, 
           vorname,
           geburtsjahr,
           name,
           year,
           Typ,
           Partner,
           Tamedia,
           foeg_2017,
           Erscheinung,
           Produkte,
           Überregional,
           Split)%>%
  summarise(mentions_year = sum(count), articles_year = n())%>%
  ungroup()

# turn the dictionary from a person-year-basis into a person-year-media-basis
# replicate data.frame with roles and data.frame with types to match and bind the columns
df_names_dict_national_roles_spread_rep <- df_names_dict_national_roles_spread%>%
  slice(rep(row_number(), nrow(df_medien_teilnehmer_typ)))%>%
  arrange(name, year)

# repeat the media participant names multiple times
df_medien_teilnehmer_typ_rep <- df_medien_teilnehmer_typ%>%
  mutate(Tamedia = ifelse(Partner == "Tamedia AG", 1, 0))%>%
  arrange(so)%>%
  slice(rep(row_number(), nrow(df_names_dict_national_roles_spread_rep)/nrow(df_medien_teilnehmer_typ)))

# bind the columns together
df_names_dict_national_roles_media <- bind_cols(df_names_dict_national_roles_spread_rep, df_medien_teilnehmer_typ_rep)

# merge
df_occurrences_per_year_media_roles <- left_join(df_names_dict_national_roles_media, 
                                                 df_occurrences_per_year_media)%>%
  mutate(mentions_year = replace_na(mentions_year, 0), articles_year = replace_na(articles_year, 0))

# summarise the data
names(df_occurrences_per_year_media_roles)[!names(df_occurrences_per_year_media_roles)%in%c("Regex_Bruno", "mentions_year", "articles_year")]

df_occurrences_per_year_media_roles <- df_occurrences_per_year_media_roles %>%
  group_by(dict,
           geburtsjahr,
           name,
           year,
           dict,
           CouncilName,
           year_joined_parliament,
           year_joined_council,
           year_left,
           Kanton_Abkuerzung,
           parteiname,
           geschlecht,
           `Fraktionschef/in`,       
          `Keine_Sonderrolle`,
          `Kommissionspräsident/in`,
          `Parteipräsidium`,
          age,
          political_age,
          Typ,
          Partner,
          Tamedia,
          foeg_2017,
          Erscheinung,
          Produkte,
          Überregional,
          Split
           )%>%
  summarise(mentions_year = sum(mentions_year), articles_year=sum(articles_year))%>%
  ungroup()

# remove Bundesräte, add information on speech region and location of large newspapers
df_occurrences_per_year_media_roles <- df_occurrences_per_year_media_roles%>%
  filter(CouncilName!="Bundesrat")%>%
  mutate(StandortZeitung = ifelse(Kanton_Abkuerzung%in%c("AG", "BS", "BE", "ZH", "SG", "LU", "SO"),
                                  1, 0),
         Deutschsprachig = ifelse(Kanton_Abkuerzung%in%c("VD", "NE", "GE", "JU", "TI", "FR"),
                                  0, 1))

# account for effect of BR-Wahlen
# Parmelin: Gewählt am 09.12.2015, Amtsantritt 2016, Kandidierende: Aeschi
# Berset: Gewählt am 14.12.2011, Amtsantritt 2012
# Cassis: Gewählt am 20.09.2017, Amtsantritt 01.11.2017; Kandidierende: Isabelle Moret
# Amherd: Gewählt am 05.12.18, Amtsantritt 2019
# KKS: Gewählt am 05.12.18, Amtsantritt 2019, Kandidierende: Hans Wicki

df_occurrences_per_year_media_roles$BRW <- 0 
df_occurrences_per_year_media_roles$BRW[df_occurrences_per_year_media_roles$year == 2015 & df_occurrences_per_year_media_roles$dict == "Personenmarker_Guy_Parmelin"] <- 1
df_occurrences_per_year_media_roles$BRW[df_occurrences_per_year_media_roles$year == 2015 & df_occurrences_per_year_media_roles$dict == "Personenmarker_Thomas_Aeschi"] <- 1
df_occurrences_per_year_media_roles$BRW[df_occurrences_per_year_media_roles$year == 2017 & df_occurrences_per_year_media_roles$dict == "Personenmarker_Ignazio_Cassis"] <- 1
df_occurrences_per_year_media_roles$BRW[df_occurrences_per_year_media_roles$year == 2017 & df_occurrences_per_year_media_roles$dict == "Personenmarker_Isabelle_Moret"] <- 1
df_occurrences_per_year_media_roles$BRW[df_occurrences_per_year_media_roles$year == 2018 & df_occurrences_per_year_media_roles$dict == "Personenmarker_Viola_Amherd"] <- 1
df_occurrences_per_year_media_roles$BRW[df_occurrences_per_year_media_roles$year == 2018 & df_occurrences_per_year_media_roles$dict == "Personenmarker_Karin_Keller_Sutter"] <- 1
df_occurrences_per_year_media_roles$BRW[df_occurrences_per_year_media_roles$year == 2018 & df_occurrences_per_year_media_roles$dict == "Personenmarker_Hans_Wicki"] <- 1


# choose relevant base categories
df_occurrences_per_year_media_roles$geschlecht <- df_occurrences_per_year_media_roles$geschlecht %>%
  dplyr::recode_factor(`2` = "maennlich", `1` = "weiblich")%>%
  relevel(ref = "maennlich")

df_occurrences_per_year_media_roles$parteiname <- df_occurrences_per_year_media_roles$parteiname %>%
  as.factor()%>%
  relevel(ref = "CVP")

df_occurrences_per_year_media_roles$CouncilName <- df_occurrences_per_year_media_roles$CouncilName %>%
  as.factor()%>%
  relevel(ref = "Nationalrat")

df_occurrences_per_year_media_roles$Kanton_Abkuerzung <- df_occurrences_per_year_media_roles$Kanton_Abkuerzung %>%
  as.factor()%>%
  relevel(ref = "ZH")

# # exclude parties without variation
df_occurrences_per_year_media_roles <- df_occurrences_per_year_media_roles %>%
  filter(!parteiname%in%c("-", "MCR", "PdA/PST", "CSP"))

# plot distributions as densities
# for mentions
ggplot(data= df_occurrences_per_year_media_roles, aes(x=mentions_year, group = geschlecht, fill = geschlecht))+geom_density(alpha=0.25)

# for articles
ggplot(data= df_occurrences_per_year_media_roles, aes(x=articles_year, group = geschlecht, fill = geschlecht))+geom_density(alpha=0.25)

# summarising on the higher levels of aggregation as well 
# media type
df_occurrences_per_year_type_roles <- df_occurrences_per_year_media_roles %>%
  group_by(dict,
           geburtsjahr,
           name,
           year,
           CouncilName,
           year_joined_parliament,
           year_joined_council,
           year_left,
           Kanton_Abkuerzung,
           parteiname,
           geschlecht,
           BRW,
           `Fraktionschef/in`,       
           `Keine_Sonderrolle`,
           `Kommissionspräsident/in`,
           `Parteipräsidium`,
           age,
           political_age,
           Typ,
           StandortZeitung,
           Deutschsprachig
  )%>%
  summarise(mentions_year = sum(mentions_year), articles_year=sum(articles_year))%>%
  ungroup()

# tamedia or not 
df_occurrences_per_year_tamedia_roles <- df_occurrences_per_year_media_roles %>%
  group_by(dict,
           geburtsjahr,
           name,
           year,
           dict,
           CouncilName,
           year_joined_parliament,
           year_joined_council,
           year_left,
           Kanton_Abkuerzung,
           parteiname,
           geschlecht,
           BRW,
           `Fraktionschef/in`,       
           `Keine_Sonderrolle`,
           `Kommissionspräsident/in`,
           `Parteipräsidium`,
           age,
           political_age,
           Tamedia,
           StandortZeitung,
           Deutschsprachig
  )%>%
  summarise(mentions_year = sum(mentions_year), articles_year=sum(articles_year))%>%
  ungroup()

# per person per year 
df_occurrences_per_year_roles <- df_occurrences_per_year_media_roles %>%
  group_by(dict,
           geburtsjahr,
           name,
           year,
           dict,
           CouncilName,
           year_joined_parliament,
           year_joined_council,
           year_left,
           Kanton_Abkuerzung,
           parteiname,
           geschlecht,
           BRW,
           `Fraktionschef/in`,       
           `Keine_Sonderrolle`,
           `Kommissionspräsident/in`,
           `Parteipräsidium`,
           age,
           political_age,
           StandortZeitung,
           Deutschsprachig
  )%>%
  summarise(mentions_year = sum(mentions_year), articles_year=sum(articles_year))%>%
  ungroup()

# check if there are people which occurr more than once in a given year
df_doublenames <- table(df_occurrences_per_year_tamedia_roles$year, df_occurrences_per_year_tamedia_roles$name)%>%
  as.data.frame()%>%
  filter(Freq>2)

####################################
##### DESCRIPTIVES / PLOTTING ######
####################################

df_mentions_by_sex <- df_occurrences_per_year_roles %>%
  #filter(Deutschsprachig==1)%>%
  group_by(year, parteiname, geschlecht)%>%
  summarise(durschnittliche_Nennungen = mean(mentions_year),
            mentions_year = sum(mentions_year), 
            articles_year = sum(articles_year),
            anzahl_personen = n())

df_mentions_by_party <- df_occurrences_per_year_roles %>% 
  #filter(Deutschsprachig==1)%>%
  group_by(year, parteiname)%>%
  summarise(alle_personen = n(),
            mentions_year_all = sum(mentions_year),
            articles_year_all = sum(articles_year))

df_years_party_sex <- data.frame(parteiname = rep(unique(df_mentions_by_party$parteiname), each = 14),
                                 year = rep(2012:2018, length(unique(df_mentions_by_party$parteiname))*2),
                                 geschlecht = rep(c(rep("maennlich",7),rep("weiblich",7)), length(unique(df_mentions_by_party$parteiname))))

df_mentions_by_sex <- left_join(df_years_party_sex, df_mentions_by_sex)%>%
  left_join(df_mentions_by_party)%>%
  mutate(mentions_year = replace_na(mentions_year, 0),
         articles_year = replace_na(articles_year, 0),
         anzahl_personen = replace_na(anzahl_personen, 0))

df_mentions_by_sex <- df_mentions_by_sex %>%
  mutate(anteil_sitze = 100*anzahl_personen/alle_personen,
         anteil_nennungen = 100*mentions_year/mentions_year_all,
         anteil_artikel = 100*articles_year/articles_year_all)%>%
  #filter(geschlecht == "weiblich")%>%
  dplyr::select(year, geschlecht, parteiname, anteil_sitze, anteil_artikel, anteil_nennungen, durschnittliche_Nennungen)


df_sex_years_party_sel <- df_mentions_by_sex%>%
  filter(parteiname %in% c("FDP", 
                           "GPS", 
                           "SVP", 
                           "BDP", 
                           "SP",
                           "EDU",
                           "CVP",
                           "GLP",
                           "EVP"))

df_sex_years_party_sel$parteiname <- factor(df_sex_years_party_sel$parteiname, levels = c("SP", "GPS", "GLP",
                                                                                          "EVP","CVP", "BDP",
                                                                                          "FDP", "SVP", "EDU"))
df_sex_years_party_sel_plotting <- df_sex_years_party_sel %>%
  filter(geschlecht == "weiblich")%>%
  gather(value = "Anteil", key =  "Wovon", anteil_sitze:anteil_nennungen)

#png("~/Capstone/Plots/Final/Anteile_Nennungen_Parlament.png", width = 700, height = 600)
ggplot(df_sex_years_party_sel_plotting[df_sex_years_party_sel_plotting$Wovon%in%c("anteil_nennungen", "anteil_sitze"), ], aes(x = as.numeric(year), y = Anteil))+
  geom_point(aes(x = as.numeric(year), y=Anteil, shape = Wovon), size = 2)+
  ylim(0,100)+
  facet_wrap(~parteiname)+
  theme_light()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x = "Jahr", y = "Prozentualer Anteil von Politikerinnen")+
  scale_shape_manual(name = "Frauenanteil an...", labels = c("Namensnennungen In Zeitungen", "Sitzen im Bundeshaus"), values = c(2,1))
#dev.off()

#png("~/Capstone/Plots/Final/Anteile_Artikel_Parlament.png", width = 700, height = 600)
ggplot(df_sex_years_party_sel_plotting[df_sex_years_party_sel_plotting$Wovon%in%c("anteil_artikel", "anteil_sitze"), ], aes(x = as.numeric(year), y = Anteil))+
  geom_point(aes(x = as.numeric(year), y=Anteil, shape = Wovon), size = 2)+
  ylim(0,100)+
  facet_wrap(~parteiname)+
  theme_light()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x = "Jahr", y = "Prozentualer Anteil von Politikerinnen")+
  scale_shape_manual(name = "Frauenanteil an...", labels = c("In Artikel vorkommenden Personen", "Sitzen im Bundeshaus"), values = c(2,1))
#dev.off()

#png("~/Capstone/Plots/Final/Anteile_Kombiniert_Parlament.png", width = 1200, height = 600)
ggplot(df_sex_years_party_sel_plotting, aes(x = as.numeric(year), y = Anteil))+
  geom_point(aes(x = as.numeric(year), y=Anteil, shape = Wovon), size = 2)+
  ylim(0,100)+
  facet_wrap(~parteiname, nrow = 2)+
  theme_light()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x = "Jahr", y = "Prozentualer Anteil von Politikerinnen")+
  scale_shape_manual(name = "Frauenanteil an...", labels = c("In Artikel vorkommenden Personen","Namensnennungen In Zeitungen", "Sitzen im Bundeshaus"), values = c(2,1,4))
#dev.off()

#### start modelling ####
# restricting the data-frma eto only the big parties: SVP, SP, FDP, CVP, GPS
# for the other parties, there sometimes are to few observations per category

df_occurrences_per_year_roles_big <- df_occurrences_per_year_roles %>%
  filter(parteiname %in% c("SVP", "SP", "FDP", "CVP", "GPS"))

library(MASS)

# Selecting the correct model for our count data: Poisson, Quasi-Poisson or negative Binomial (NB)?
# first for mentions
model.poisson <- glm(mentions_year ~ BRW + StandortZeitung + CouncilName + parteiname*geschlecht*Deutschsprachig + political_age*geschlecht*parteiname +`Fraktionschef/in`*Deutschsprachig + `Fraktionschef/in`*parteiname + `Fraktionschef/in`*geschlecht + `Parteipräsidium`*Deutschsprachig + `Parteipräsidium`*parteiname + `Parteipräsidium`*geschlecht + `Kommissionspräsident/in`*Deutschsprachig*geschlecht + `Kommissionspräsident/in`*parteiname,
                     data=df_occurrences_per_year_roles_big, family=poisson)
summary(model.poisson)

model.quasi <- glm(mentions_year ~ BRW + StandortZeitung + CouncilName + parteiname*geschlecht*Deutschsprachig + political_age*geschlecht*parteiname +`Fraktionschef/in`*Deutschsprachig + `Fraktionschef/in`*parteiname + `Fraktionschef/in`*geschlecht + `Parteipräsidium`*Deutschsprachig + `Parteipräsidium`*parteiname + `Parteipräsidium`*geschlecht + `Kommissionspräsident/in`*Deutschsprachig*geschlecht + `Kommissionspräsident/in`*parteiname,
                   data=df_occurrences_per_year_roles_big, family=quasipoisson)
summary(model.quasi)

library(MASS)
model.nb <- glm.nb(mentions_year ~ BRW + StandortZeitung + CouncilName + parteiname*geschlecht*Deutschsprachig + political_age*geschlecht*parteiname +`Fraktionschef/in`*Deutschsprachig + `Fraktionschef/in`*parteiname + `Fraktionschef/in`*geschlecht + `Parteipräsidium`*Deutschsprachig + `Parteipräsidium`*parteiname + `Parteipräsidium`*geschlecht + `Kommissionspräsident/in`*Deutschsprachig*geschlecht + `Kommissionspräsident/in`*parteiname,
                   data=df_occurrences_per_year_roles_big)
summary(model.nb)

# testing for overdispersion of the data.
library(pscl)
odTest(model.nb)
# Overdispersion is present, no poisson model should be used

# now checking whether the mean-variance relationship fits besser a quasi poisson or a NB model
mean.var.plot = function(model.poisson,model.nb){
  xb = predict(model.nb)
  g = cut(xb, breaks=unique(quantile(xb,seq(0,1,0.1))))
  m = tapply(model.poisson$y, g, mean)
  v = tapply(model.poisson$y, g, var)
  pr <- residuals(model.poisson,"pearson")
  phi <- sum(pr^2)/df.residual(model.poisson)
  x = seq(min(m),max(m),length.out = 500)
  line.data = data.frame(x=rep(x,2),y=c(x*phi,x*(1+x/model.nb$theta)),
                         model=c(rep("Q. Poisson",length(x)),rep("Neg. Binomial",length(x))))
  library(ggplot2)
  ggplot() + geom_point(aes(x=m,y=v)) + 
    geom_line(aes(x=x,y=y,linetype=model),data=line.data) + 
    theme_bw() + theme(panel.background = element_rect(rgb(.95,.95,.95))) +
    ylab("variance") + xlab("mean") +
    scale_linetype_manual(values = c("solid","dashed")) +
    ggtitle("Mean-Variance Relationship") 
}

mean.var.plot(model.poisson, model.nb)
# The plot shows the mean-variance relationship is better approximated by a NB model

# second for articles
model.poisson <- glm(articles_year ~ BRW + StandortZeitung + CouncilName + parteiname*geschlecht*Deutschsprachig + political_age*geschlecht*parteiname +`Fraktionschef/in`*Deutschsprachig + `Fraktionschef/in`*parteiname + `Fraktionschef/in`*geschlecht + `Parteipräsidium`*Deutschsprachig + `Parteipräsidium`*parteiname + `Parteipräsidium`*geschlecht + `Kommissionspräsident/in`*Deutschsprachig*geschlecht + `Kommissionspräsident/in`*parteiname,
                     data=df_occurrences_per_year_roles_big, family=poisson)
summary(model.poisson)

model.quasi <- glm(articles_year ~ BRW + StandortZeitung + CouncilName + parteiname*geschlecht*Deutschsprachig + political_age*geschlecht*parteiname +`Fraktionschef/in`*Deutschsprachig + `Fraktionschef/in`*parteiname + `Fraktionschef/in`*geschlecht + `Parteipräsidium`*Deutschsprachig + `Parteipräsidium`*parteiname + `Parteipräsidium`*geschlecht + `Kommissionspräsident/in`*Deutschsprachig*geschlecht + `Kommissionspräsident/in`*parteiname,
                   data=df_occurrences_per_year_roles_big, family=quasipoisson)
summary(model.quasi)

library(MASS)
model.nb <- glm.nb(articles_year ~ BRW + StandortZeitung + CouncilName + parteiname*geschlecht*Deutschsprachig + political_age*geschlecht*parteiname +`Fraktionschef/in`*Deutschsprachig + `Fraktionschef/in`*parteiname + `Fraktionschef/in`*geschlecht + `Parteipräsidium`*Deutschsprachig + `Parteipräsidium`*parteiname + `Parteipräsidium`*geschlecht + `Kommissionspräsident/in`*Deutschsprachig*geschlecht + `Kommissionspräsident/in`*parteiname,
                   data=df_occurrences_per_year_roles_big)
summary(model.nb)

# testing for overdispersion of the data.
library(pscl)
odTest(model.nb)
# Overdispersion is present, no poisson model should be used

# now checking whether the mean-variance relationship fits besser a quasi poisson or a NB model
mean.var.plot = function(model.poisson,model.nb){
  xb = predict(model.nb)
  g = cut(xb, breaks=unique(quantile(xb,seq(0,1,0.1))))
  m = tapply(model.poisson$y, g, mean)
  v = tapply(model.poisson$y, g, var)
  pr <- residuals(model.poisson,"pearson")
  phi <- sum(pr^2)/df.residual(model.poisson)
  x = seq(min(m),max(m),length.out = 500)
  line.data = data.frame(x=rep(x,2),y=c(x*phi,x*(1+x/model.nb$theta)),
                         model=c(rep("Q. Poisson",length(x)),rep("Neg. Binomial",length(x))))
  library(ggplot2)
  ggplot() + geom_point(aes(x=m,y=v)) + 
    geom_line(aes(x=x,y=y,linetype=model),data=line.data) + 
    theme_bw() + theme(panel.background = element_rect(rgb(.95,.95,.95))) +
    ylab("variance") + xlab("mean") +
    scale_linetype_manual(values = c("solid","dashed")) +
    ggtitle("Mean-Variance Relationship") 
}

mean.var.plot(model.poisson, model.nb)
# The plot shows the mean-variance relationship is better approximated by a NB model

#### Final Models ####
# one model for articles, with explanatory variables:
# Bundesratswahl
# Standort Zeitung
# Council Name
# Party
# Sex
# German-speaking Canton
# political experience
# Special roles:
# - FraktionspräsidentIn
# - KommissionspräsidentIn
# - ParteipräsidentIn
# where the effect of party changes based on:
# - sex
# - German-Speaking Canton
# - German-Speaking Canton and Sex
# - All special roles
# - political experience
# - political experience and sex
# and the effect of sex changes based on:
# - party
# - party and German-speaking canton
# - German-speaking Kanton
# - political experience and party
# - political experience
# - all special roles
#   - Kommissionspräsidentschaft and german-speaking Canton

# articles
mod_negbin_classic <- glm.nb(articles_year ~ BRW + StandortZeitung + CouncilName + parteiname*geschlecht*Deutschsprachig + political_age*geschlecht*parteiname +`Fraktionschef/in`*Deutschsprachig + `Fraktionschef/in`*parteiname + `Fraktionschef/in`*geschlecht + `Parteipräsidium`*Deutschsprachig + `Parteipräsidium`*parteiname + `Parteipräsidium`*geschlecht + `Kommissionspräsident/in`*Deutschsprachig*geschlecht + `Kommissionspräsident/in`*parteiname,
                             data = df_occurrences_per_year_roles_big)
summary(mod_negbin_classic)

# mentions
mod_negbin_classic_mentions <- glm.nb(mentions_year ~ BRW + StandortZeitung + CouncilName + parteiname*geschlecht*Deutschsprachig + political_age*geschlecht*parteiname +`Fraktionschef/in`*Deutschsprachig + `Fraktionschef/in`*parteiname + `Fraktionschef/in`*geschlecht + `Parteipräsidium`*Deutschsprachig + `Parteipräsidium`*parteiname + `Parteipräsidium`*geschlecht + `Kommissionspräsident/in`*Deutschsprachig*geschlecht + `Kommissionspräsident/in`*parteiname,
                                      data = df_occurrences_per_year_roles_big)
summary(mod_negbin_classic_mentions)

# articles simplified
mod_negbin_simple <- glm.nb(articles_year ~ BRW + StandortZeitung + CouncilName + parteiname*geschlecht*Deutschsprachig + political_age*geschlecht*parteiname +`Fraktionschef/in`*Deutschsprachig + Parteipräsidium*Deutschsprachig + `Parteipräsidium`*parteiname +  `Kommissionspräsident/in`*Deutschsprachig*geschlecht,
                             data = df_occurrences_per_year_roles_big)
summary(mod_negbin_simple)
lmtest::coeftest(mod_negbin_simple, vcov = sandwich)
lmtest::waldtest(mod_negbin_classic, mod_negbin_simple)
lmtest::waldtest(mod_negbin_classic)
lmtest::lrtest(mod_negbin_classic, mod_negbin_simple)

anova(mod_negbin_simple)
anova(mod_negbin_classic)
# mentions simplified
mod_negbin_simple_mentions <- glm.nb(mentions_year ~ BRW + StandortZeitung + CouncilName + parteiname*geschlecht*Deutschsprachig + political_age*geschlecht*parteiname +`Fraktionschef/in`*Deutschsprachig + Parteipräsidium*Deutschsprachig + `Parteipräsidium`*parteiname +  `Kommissionspräsident/in`*Deutschsprachig*geschlecht,
                                     data = df_occurrences_per_year_roles_big)
summary(mod_negbin_simple_mentions)
lmtest::coeftest(mod_negbin_simple_mentions, vcov = sandwich)

plot(mod_negbin_simple)
plot(mod_negbin_classic)
plot(mod_negbin_classic_mentions)
plot(mod_negbin_simple_mentions)

influential <- df_occurrences_per_year_roles_big[c(480, 939, 752, 339, 1463, 549), ]
influential_mentions <- df_occurrences_per_year_roles_big[c(601, 611, 339, 752), ]

# excluding influential observations from SP
mod_negbin_restricted_mentions <- glm.nb(mentions_year ~ BRW + StandortZeitung + CouncilName + parteiname*geschlecht*Deutschsprachig + political_age*geschlecht*parteiname +`Fraktionschef/in`*Deutschsprachig + Parteipräsidium*Deutschsprachig + `Parteipräsidium`*parteiname +  `Kommissionspräsident/in`*Deutschsprachig*geschlecht,
                                data = df_occurrences_per_year_roles_big[!df_occurrences_per_year_roles_big$dict%in%c("Personenmarker_Paul_Rechsteiner", "Personenmarker_Cédric_Wermuth", "Personenmarker_Alexander_Tschäppät"), ])
summary(mod_negbin_restricted_mentions)

mod_negbin_restricted <- glm.nb(articles_year ~ BRW + StandortZeitung + CouncilName + parteiname*geschlecht*Deutschsprachig + political_age*geschlecht*parteiname +`Fraktionschef/in`*Deutschsprachig + Parteipräsidium*Deutschsprachig + `Parteipräsidium`*parteiname +  `Kommissionspräsident/in`*Deutschsprachig*geschlecht,
                                data = df_occurrences_per_year_roles_big[!df_occurrences_per_year_roles_big$dict%in%c("Personenmarker_Paul_Rechsteiner", "Personenmarker_Cédric_Wermuth", "Personenmarker_Alexander_Tschäppät"), ])
summary(mod_negbin_restricted)

# only Tamedia
df_occurrences_per_year_roles_big_tamedia <- df_occurrences_per_year_tamedia_roles %>%
  filter(parteiname %in% c("SVP", "SP", "FDP", "CVP", "GPS") & Tamedia == 1)

mod_negbin_simple_mentions_tamedia <- glm.nb(mentions_year ~ BRW + StandortZeitung + CouncilName + parteiname*geschlecht*Deutschsprachig + political_age*geschlecht*parteiname +`Fraktionschef/in`*Deutschsprachig + Parteipräsidium*Deutschsprachig + `Parteipräsidium`*parteiname +  `Kommissionspräsident/in`*Deutschsprachig*geschlecht,
                                     data = df_occurrences_per_year_roles_big_tamedia)
summary(mod_negbin_simple_mentions_tamedia)

mod_negbin_simple_tamedia <- glm.nb(articles_year ~ BRW + StandortZeitung + CouncilName + parteiname*geschlecht*Deutschsprachig + political_age*geschlecht*parteiname +`Fraktionschef/in`*Deutschsprachig + Parteipräsidium*Deutschsprachig + `Parteipräsidium`*parteiname +  `Kommissionspräsident/in`*Deutschsprachig*geschlecht,
                                             data = df_occurrences_per_year_roles_big_tamedia)
summary(mod_negbin_simple_tamedia)

sink("~/Capstone/Plots/Final/regressions_table.txt", append = F)
stargazer::stargazer(type = "latex", 
                     header = F,
                     mod_negbin_classic,
                     mod_negbin_simple,
                     mod_negbin_simple_tamedia,
                     mod_negbin_classic_mentions,
                     mod_negbin_simple_mentions,
                     mod_negbin_simple_mentions_tamedia,
                     column.labels =  rep(c("Volles Model", "Beschränktes Modell", "Nur Tamedia"), 2), 
                     dep.var.labels = c("Artikel Pro Jahr", "Nennungen Pro Jahr"))
sink()

# installing latest version from github
#devtools::install_github("benjaminschlegel/glm.predict")
library(glm.predict)
# sind die Unterschiede zwischen einer Nationalrätin und einem Nationalrat signifikant?
# Simulation für NICHT-Teilnehmende an einer Bundesratswahl, welche aus einem Standortkanton einer Zeitung kommen, die im Nationalrat sitzen,
# für alle Parteien, für beide Geschlechter, die aus der Deutschschweiz stammen, mit durchschnittlicher politischer Erfahrung,
# die nicht Fraktionschefin oder Chef sind, keine Partei präsideren sowie auch keine Kommission
# mentions
df_prediction_nb_disc_simple_mentions <- predicts(model = mod_negbin_simple_mentions, values = "0;1;F(1);F;F;1;median;0;0;0", position = 5, sim.count = 100000, set.seed = 12345)

png("~/Capstone/Plots/Final/Disc_Change_Mentions.png", width = 700, height = 600)
ggplot(df_prediction_nb_disc_simple_mentions,aes(x=parteiname)) + geom_point(aes(y=dc_mean)) +
  geom_errorbar(aes(ymax=dc_upper,ymin=dc_lower)) + 
  geom_hline(yintercept = 0,linetype="dashed",col="red") +
  theme_light() + ylab("Nennungen Mann - Nennungen Frau") + xlab("Partei")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

# articles
df_prediction_nb_disc_simple_articles <- predicts(model = mod_negbin_simple, values = "0;1;F(1);F;F;1;median;0;0;0", position = 5, sim.count = 100000, set.seed = 12345)

png("~/Capstone/Plots/Final/Disc_Change_Articles.png", width = 700, height = 600)
ggplot(df_prediction_nb_disc_simple_articles,aes(x=parteiname)) + geom_point(aes(y=dc_mean)) +
  geom_errorbar(aes(ymax=dc_upper,ymin=dc_lower)) + 
  geom_hline(yintercept = 0,linetype="dashed",col="red") +
  theme_light() + ylab("Artikel Mann - Artikel Frau") + xlab("Partei")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

# restricted model excluding Rechsteiner, Tschäppät, Wermuth
df_prediction_nb_disc_restricted_mentions <- predicts(model = mod_negbin_restricted_mentions, values = "0;1;F(1);F;F;1;median;0;0;0", position = 5, sim.count = 100000, set.seed = 12345)

png("~/Capstone/Plots/Final/Disc_Change_Restricted_Mentions.png", width = 700, height = 600)
ggplot(df_prediction_nb_disc_restricted_mentions,aes(x=parteiname)) + geom_point(aes(y=dc_mean)) +
  geom_errorbar(aes(ymax=dc_upper,ymin=dc_lower)) + 
  geom_hline(yintercept = 0,linetype="dashed",col="red") +
  theme_light() + ylab("Nennungen Mann - Nennungen Frau") + xlab("Partei")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

df_prediction_nb_disc_restricted <- predicts(model = mod_negbin_restricted, values = "0;1;F(1);F;F;1;median;0;0;0", position = 5, sim.count = 100000, set.seed = 12345)

png("~/Capstone/Plots/Final/Disc_Change_Restricted.png", width = 700, height = 600)
ggplot(df_prediction_nb_disc_restricted,aes(x=parteiname)) + geom_point(aes(y=dc_mean)) +
  geom_errorbar(aes(ymax=dc_upper,ymin=dc_lower)) + 
  geom_hline(yintercept = 0,linetype="dashed",col="red") +
  theme_light() + ylab("Artikel Mann - Artikel Frau") + xlab("Partei")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

# just predictions
# mentions
df_prediction_nb_simple_mentions <- predicts(model = mod_negbin_simple_mentions, values = "0;1;F(1);F;F;1;median;0;0;0", sim.count = 100000, set.seed = 12345)

png("~/Capstone/Plots/Final/Prediction_Mentions.png", width = 700, height = 600)
ggplot(df_prediction_nb_simple_mentions,aes(x=parteiname, colour = geschlecht))  + geom_point(aes(y=mean)) +
  geom_errorbar(aes(ymax=upper,ymin=lower))+
  scale_color_manual(values = c("weiblich" = "#f81233", "maennlich" = "#48839c"), labels =c("Politiker", "Politikerinnen"), name = " ")+
  theme_light() + ylab("Anzahl Nennungen") + xlab("Partei")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

# articles
df_prediction_nb_simple_articles <- predicts(model = mod_negbin_simple, values = "0;1;F(1);F;F;1;median;0;0;0", sim.count = 100000, set.seed = 12345)

png("~/Capstone/Plots/Final/Prediction_Articles.png", width = 700, height = 600)
ggplot(df_prediction_nb_simple_articles,aes(x=parteiname, colour = geschlecht)) + geom_point(aes(y=mean)) +
  geom_errorbar(aes(ymax=upper,ymin=lower))+
  scale_color_manual(values = c("weiblich" = "#f81233", "maennlich" = "#48839c"), labels =c("Politiker", "Politikerinnen"), name = " ")+
  theme_light() + ylab("Anzahl Artikel") + xlab("Partei")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

# just predictions restricted model
# mentions restricted model
df_prediction_nb_restricted_mentions <- predicts(model = mod_negbin_restricted_mentions, values = "0;1;F(1);F;F;1;median;0;0;0", sim.count = 100000, set.seed = 12345)

png("~/Capstone/Plots/Final/Prediction_Mentions_Restricted.png", width = 700, height = 600)
ggplot(df_prediction_nb_restricted_mentions,aes(x=parteiname, colour = geschlecht))  + geom_point(aes(y=mean)) +
  geom_errorbar(aes(ymax=upper,ymin=lower))+
  scale_color_manual(values = c("weiblich" = "#f81233", "maennlich" = "#48839c"), labels =c("Politiker", "Politikerinnen"), name = " ")+
  theme_light() + ylab("Anzahl Nennungen") + xlab("Partei")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

# articles restricted model
df_prediction_nb_restricted_articles <- predicts(model = mod_negbin_restricted, values = "0;1;F(1);F;F;1;median;0;0;0", sim.count = 100000, set.seed = 12345)

png("~/Capstone/Plots/Final/Prediction_Articles_Restricted.png", width = 700, height = 600)
ggplot(df_prediction_nb_restricted_articles,aes(x=parteiname, colour = geschlecht)) + geom_point(aes(y=mean)) +
  geom_errorbar(aes(ymax=upper,ymin=lower))+
  scale_color_manual(values = c("weiblich" = "#f81233", "maennlich" = "#48839c"), labels =c("Politiker", "Politikerinnen"), name = " ")+
  theme_light() + ylab("Anzahl Artikel") + xlab("Partei")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

# only Tamedia
# mentions
df_prediction_nb_disc_simple_mentions_tamedia <- predicts(model = mod_negbin_simple_mentions_tamedia, values = "0;1;F(1);F;F;1;median;0;0;0", position = 5, sim.count = 100000, set.seed = 12345)

png("~/Capstone/Plots/Final/Disc_Change_Mentions_Tamedia.png", width = 700, height = 600)
ggplot(df_prediction_nb_disc_simple_mentions_tamedia,aes(x=parteiname)) + geom_point(aes(y=dc_mean)) +
  geom_errorbar(aes(ymax=dc_upper,ymin=dc_lower)) + 
  geom_hline(yintercept = 0,linetype="dashed",col="red") +
  theme_light() + ylab("Nennungen Mann - Nennungen Frau") + xlab("Partei")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

# articles
df_prediction_nb_disc_simple_tamedia <- predicts(model = mod_negbin_simple_tamedia, values = "0;1;F(1);F;F;1;median;0;0;0", position = 5, sim.count = 100000, set.seed = 12345)

png("~/Capstone/Plots/Final/Disc_Change_Articles_Tamedia.png", width = 700, height = 600)
ggplot(df_prediction_nb_disc_simple_tamedia,aes(x=parteiname)) + geom_point(aes(y=dc_mean)) +
  geom_errorbar(aes(ymax=dc_upper,ymin=dc_lower)) + 
  geom_hline(yintercept = 0,linetype="dashed",col="red") +
  theme_light() + ylab("Artikel Mann - Artikel Frau") + xlab("Partei")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

# just predictions
# mentions
df_prediction_nb_simple_mentions_tamedia <- predicts(model = mod_negbin_simple_mentions_tamedia, values = "0;1;F(1);F;F;1;median;0;0;0", sim.count = 100000, set.seed = 12345)

png("~/Capstone/Plots/Final/Prediction_Mentions_Tamedia.png", width = 700, height = 600)
ggplot(df_prediction_nb_simple_mentions_tamedia,aes(x=parteiname, colour = geschlecht))  + geom_point(aes(y=mean)) +
  geom_errorbar(aes(ymax=upper,ymin=lower))+
  scale_color_manual(values = c("weiblich" = "#f81233", "maennlich" = "#48839c"), labels =c("Politiker", "Politikerinnen"), name = " ")+
  theme_light() + ylab("Anzahl Nennungen") + xlab("Partei")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

# articles tamedia
df_prediction_nb_simple_articles_tamedia <- predicts(model = mod_negbin_simple_tamedia, values = "0;1;F(1);F;F;1;median;0;0;0", sim.count = 100000, set.seed = 12345)

png("~/Capstone/Plots/Final/Prediction_Articles_Tamedia.png", width = 700, height = 600)
ggplot(df_prediction_nb_simple_articles_tamedia,aes(x=parteiname, colour = geschlecht)) + geom_point(aes(y=mean)) +
  geom_errorbar(aes(ymax=upper,ymin=lower))+
  scale_color_manual(values = c("weiblich" = "#f81233", "maennlich" = "#48839c"), labels =c("Politiker", "Politikerinnen"), name = " ")+
  theme_light() + ylab("Anzahl Artikel") + xlab("Partei")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

# continuous prediction over a range of political experience
# articles
df_prediction_nb_experience <- predicts(model = mod_negbin_simple, values = "0;1;F(1);F;F;1;0-25;0;0;0", sim.count = 100000, set.seed = 12345)

png("~/Capstone/Plots/Final/Articles_Experience.png", width = 700, height = 600)
ggplot(df_prediction_nb_experience, aes(x=political_age)) + geom_line(aes(y=mean, x=political_age, colour = geschlecht)) +
  geom_ribbon(aes(ymax=upper,ymin=lower, x= political_age, fill = geschlecht), alpha = 0.2) + 
  theme_light() + ylab("Artikel pro Jahr") +
  xlab("Jahre seit erster Wahl ins Bundeshaus") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~parteiname)+
  scale_fill_manual(values = c("#48839c", "#f81233"),
                    name = " ",
                    labels = c("Politiker","Politikerinnen"))+
  scale_colour_manual(values = c("#48839c", "#f81233"),
                      name = " ",
                      labels = c("Politiker","Politikerinnen"))
dev.off()

# mentions
df_prediction_nb_experience_mentions <- predicts(model = mod_negbin_simple_mentions, values = "0;1;F(1);F;F;1;0-25;0;0;0", sim.count = 100000, set.seed = 12345)

png("~/Capstone/Plots/Final/Mentions_Experience.png", width = 700, height = 600)
ggplot(df_prediction_nb_experience_mentions, aes(x=political_age)) + geom_line(aes(y=mean, x=political_age, colour = geschlecht)) +
  geom_ribbon(aes(ymax=upper,ymin=lower, x= political_age, fill = geschlecht), alpha = 0.2) + 
  theme_light() + ylab("Nennungen pro Jahr") +
  xlab("Jahre seit erster Wahl ins Bundeshaus") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~parteiname)+
  scale_fill_manual(values = c("#48839c", "#f81233"),
                    name = " ",
                    labels = c("Politiker","Politikerinnen"))+
  scale_colour_manual(values = c("#48839c", "#f81233"),
                      name = " ",
                      labels = c("Politiker","Politikerinnen"))
dev.off()

# difference over a range of values
# mentions
df_prediction_nb_experience_disc_mentions <- predicts(model = mod_negbin_simple_mentions, values = "0;1;F(1);F;F;1;0-25;0;0;0", position= 5, sim.count = 100000, set.seed = 12345)

png("~/Capstone/Plots/Final/Disc_Change_Mentions_Experience.png", width = 700, height = 600)
ggplot(df_prediction_nb_experience_disc_mentions ,aes(x=political_age)) + geom_line(aes(y=dc_mean, x=political_age)) +
  geom_ribbon(aes(ymax=dc_upper,ymin=dc_lower, x= political_age), alpha = 0.2) + 
  geom_hline(yintercept = 0,linetype="dashed",col="red") +
  theme_light() + ylab("Nennungen Mann -  Nennungen Frau") +
  xlab("Jahre seit erster Wahl ins Bundeshaus") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~parteiname)
dev.off()

# articles
df_prediction_nb_experience_disc <- predicts(model = mod_negbin_simple, values = "0;1;F(1);F;F;1;0-25;0;0;0", position= 5, sim.count = 100000, set.seed = 12345)

png("~/Capstone/Plots/Final/Disc_Change_Articles_Experience.png", width = 700, height = 600)
ggplot(df_prediction_nb_experience_disc ,aes(x=political_age)) + geom_line(aes(y=dc_mean, x=political_age)) +
  geom_ribbon(aes(ymax=dc_upper,ymin=dc_lower, x= political_age), alpha = 0.2) + 
  geom_hline(yintercept = 0,linetype="dashed",col="red") +
  theme_light() + ylab("Artikel Mann - Artikel Frau") +
  xlab("Jahre seit erster Wahl ins Bundeshaus") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~parteiname)
dev.off()

##### Getting a Table with the most named Politicians #####
# sumarise mentions
df_mentions_overall <- df_names_parties_etc_per_doc %>%
  group_by(name, geschlecht)%>%
  summarise(Nennungen = sum(count))%>%
  ungroup()%>%
  arrange(desc(Nennungen))%>%
  rowid_to_column("Platz")

# get top 10
df_topmentions <- head(df_mentions_overall, 10)

# get follow-up of women
df_mention_follow_up <- df_mentions_overall %>%
  filter(geschlecht == 1) %>%
  head(13)%>%
  tail(10)

# combine
df_topmentions <- df_topmentions %>%
  bind_rows(df_mention_follow_up)%>%
  mutate(name = str_replace_all(string = name, pattern = "(?<=[:alpha:])\\.(?=[:alpha:])", replacement = "-"))%>%
  dplyr::select(Platz = Platz, Name = name, Nennungen = Nennungen)

sink("~/Capstone/Plots/Final/topmentions_table.txt", append = F)
stargazer::stargazer(df_topmentions,
                     summary = F,
                     header = F,
                     title = "Meistgenannte Politikerinnen und Politiker"
                     )
sink()

##### prepare Graphs for all politicians ####
# read in csv-data on women elected in cantonal parliaments
library(readr)
df_cantonal_elections <- read_csv2("Capstone/Data_Women/190721_Frauenanteil_bfs_adjusted.csv", 
                                      trim_ws = TRUE, na = c("", "NA", "*", "…"))%>%
  drop_na(Kanton, Wahljahr)

# replace NA with 0
df_cantonal_elections[is.na(df_cantonal_elections)] <- 0

# make long data.frame out of it,
# completing the missing years
df_cantonal_elections_long <- df_cantonal_elections%>%
  filter(FDP!="F", Kanton != "Schweiz")%>%
  dplyr::select(-(X67:X69))%>%
  gather(key = "Partei", value = "Sitze", FDP:Total_3)%>%
  mutate(Sitze = as.numeric(Sitze),
         Wahljahr = as.numeric(Wahljahr))%>%
  unique()%>%
  mutate(Jahr = Wahljahr)%>%
  complete(Jahr, nesting(Partei, Kanton))

# filling in the missing values from the last elections
df_cantonal_elections_long_filled <- df_cantonal_elections_long %>%
  group_by(Partei, Kanton)%>%
  fill(Sitze, Wahljahr)%>%
  ungroup()%>%
  filter(Jahr>=2012, !Partei%in%c("Total", "Total_1", "Total_2", "Total_3"))%>%
  mutate(geschlecht = ifelse(
    str_detect(Partei, "_1"),
    "Politiker",
    "Politikerinnen"
    ),
    Partei = str_remove_all(Partei, "_1"))%>%
  mutate(Partei = recode(Partei, GP = "GPS", LP = "FDP"))

# adding the data on the national parliament
df_national_mps <- df_names_dict_national_roles_spread %>%
  dplyr::select(Jahr = year, Partei = parteiname, geschlecht)%>%
  group_by_all()%>%
  summarise(Sitze = n())%>%
  ungroup()%>%
  mutate(geschlecht = recode(geschlecht, "2" = "Politiker", "1" = "Politikerinnen"))

df_elected_seatshares <- bind_rows(df_national_mps, df_cantonal_elections_long_filled)

#summarising the values
# based on sex, party and year
df_elected_seatshares_summarised_sex <- df_elected_seatshares %>%
  group_by(Partei, Jahr, geschlecht)%>%
  summarise(Sitze = sum(Sitze))%>%
  ungroup()

# only based on party and year
df_elected_seatshares_summarised <- df_elected_seatshares %>%
  group_by(Partei, Jahr)%>%
  summarise(Sitze_Total = sum(Sitze))%>%
  ungroup()

# join
df_elected_seatshares_summarised_sex <- left_join(df_elected_seatshares_summarised_sex,
                                                  df_elected_seatshares_summarised)

# calculate proportion
df_elected_seatshares_summarised_sex <- df_elected_seatshares_summarised_sex %>%
  mutate(Anteil_Sitze = (Sitze/Sitze_Total)*100)

# summarising the texts
df_texts_preparing <- df_names_parties_etc_per_doc%>%
  dplyr::select(geschlecht, year, count, parteiname)%>%
  mutate(geschlecht = recode(geschlecht, "2" = "Politiker", "1" = "Politikerinnen"),
         parteiname = recode(parteiname, "glp"="GLP",
                "PdA/PST" = "PdA",
                "LPS/PLS" = "FDP",
                "LDP" = "FDP",
                "LPS" = "FDP",
                "UDC" = "SVP")) # make sex and party uniform for joining later

df_sex_years_party <- df_texts_preparing%>%
  group_by(geschlecht, year, parteiname)%>%
  summarise(sum = sum(count))%>%
  ungroup()

df_years_party <- df_sex_years_party%>%
  group_by(year, parteiname)%>%
  summarise(year_total = sum(sum))%>%
  ungroup()

df_sex_years_party <- df_sex_years_party %>%
  left_join(df_years_party)

df_sex_years_party <- df_sex_years_party %>%
  mutate(rel = (sum/year_total)*100,
         year = as.numeric(year))%>%
  rename(Jahr = year, Partei = parteiname)

df_sex_years_party_seats_articles <- left_join(df_elected_seatshares_summarised_sex, df_sex_years_party)

df_sex_years_party_sel <- df_sex_years_party_seats_articles%>%
  filter(Partei %in% c("FDP", 
                           "GPS", 
                           "SVP", 
                           "BDP", 
                           "SP",
                           "EDU",
                           "CVP",
                           "GLP",
                           "EVP"))

df_sex_years_party_sel$Partei <- factor(df_sex_years_party_sel$Partei, levels = c("SP", "GPS", "GLP",
                                                                                          "EVP","CVP", "BDP",
                                                                                          "FDP", "SVP", "EDU"))

df_sex_years_party_sel <- df_sex_years_party_sel %>% 
  filter(geschlecht == "Politikerinnen")%>%
  rename(Anteil_Nennungen = rel)%>%
  gather(value = "Anteil", key = "Wovon", c("Anteil_Nennungen", "Anteil_Sitze"))%>%
  mutate(Bundesrat = "Mit Bundesräten")

# summarising the texts excluding BR
federal_councillors <- df_names_dict_national_roles_spread %>%
  filter(CouncilName == "Bundesrat")%>%
  dplyr::select(dict, year)

df_texts_preparing_BR <- df_names_parties_etc_per_doc%>%
  mutate(year = as.numeric(year))%>%
  anti_join(federal_councillors)%>%
  dplyr::select(geschlecht, year, count, parteiname)%>%
  mutate(geschlecht = recode(geschlecht, "2" = "Politiker", "1" = "Politikerinnen"),
         parteiname = recode(parteiname, "glp"="GLP",
                             "PdA/PST" = "PdA",
                             "LPS/PLS" = "FDP",
                             "LDP" = "FDP",
                             "LPS" = "FDP",
                             "UDC" = "SVP")) # make sex and party uniform for joining later

df_sex_years_party_BR <- df_texts_preparing_BR%>%
  group_by(geschlecht, year, parteiname)%>%
  summarise(sum = sum(count))%>%
  ungroup()

df_years_party_BR <- df_sex_years_party_BR%>%
  group_by(year, parteiname)%>%
  summarise(year_total = sum(sum))%>%
  ungroup()

df_sex_years_party_BR <- df_sex_years_party_BR %>%
  left_join(df_years_party_BR)

df_sex_years_party_BR <- df_sex_years_party_BR %>%
  mutate(rel = (sum/year_total)*100,
         year = as.numeric(year))%>%
  rename(Jahr = year, Partei = parteiname)

df_sex_years_party_seats_articles_BR <- left_join(df_elected_seatshares_summarised_sex, df_sex_years_party_BR)

df_sex_years_party_sel_BR <- df_sex_years_party_seats_articles_BR%>%
  filter(Partei %in% c("FDP", 
                       "GPS", 
                       "SVP", 
                       "BDP", 
                       "SP",
                       "EDU",
                       "CVP",
                       "GLP",
                       "EVP"))

df_sex_years_party_sel_BR$Partei <- factor(df_sex_years_party_sel_BR$Partei, levels = c("SP", "GPS", "GLP",
                                                                                  "EVP","CVP", "BDP",
                                                                                  "FDP", "SVP", "EDU"))

df_sex_years_party_sel_BR <- df_sex_years_party_sel_BR %>% 
  filter(geschlecht == "Politikerinnen")%>%
  rename(Anteil_Nennungen = rel)%>%
  gather(value = "Anteil", key = "Wovon", c("Anteil_Nennungen", "Anteil_Sitze"))%>%
  mutate(Bundesrat = "Ohne Bundesräte")


df_sex_years_party_sel <- bind_rows(df_sex_years_party_sel, df_sex_years_party_sel_BR)

png("~/Capstone/Plots/Final//Anteil_Namen_Gesamtschweiz.png", width = 1000, height = 600)
ggplot(df_sex_years_party_sel, aes(x = as.numeric(Jahr), y = Anteil))+
  geom_smooth(data =df_sex_years_party_sel[df_sex_years_party_sel$Wovon == "Anteil_Sitze", ],
              aes(y = Anteil, x= as.numeric(Jahr)), 
              se = F, 
              colour = "grey", 
              alpha = 0.5,
              method = lm)+
  geom_point(aes(shape = Wovon))+
  ylim(0,100)+
  facet_grid(Bundesrat~Partei)+
  theme_light()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x = "Jahr", y = "Prozentualer Frauenanteil")+
  scale_shape_manual(name = "Frauenanteil an...", labels = c("Namensnennungen In Zeitungen", "Sitzen in allen untersuchten Gremien"), values = c(1,4))
dev.off()