#####################################
#APS: exploration and wrangling     #
#####################################

#### Preparations ####

# Cleaning workspace
rm(list=ls())

# define stringsAsFactors = FALSE as default
options(stringsAsFactors = FALSE)

#libraries
library(quanteda)
library(tidyverse)
library(readtext)
library(stringi)

#load data
aps <- read_csv("~/APS/files.csv")
#aps2 <- read_csv("~/APS/files2.csv")

#### Inspect data ####
head(aps)#looks like we can use that
#head(aps2)#raw texttitles

#get rid of newspapers we don't need
aps_smd <- aps[ ! aps$newspaper_name %in% c("SN", "NZZ", "CdT", "LM",
                                            "24H", "WB", "LMD", "Exp", "QJ", "Lar", "CaF", "cdt",
                                            "GdP", "ARCinfo", "BauernZeitung", "Cdt", "SRF", "lib",
                                            "20VD", "Lib", "TG", "NF", "LT"), ]

#get same newspaper abbreviations as SMD-Corpus
aps_smd$so <- recode(aps_smd$newspaper_name,
                     SGT ="SGT", APZ ="APPZ", TZ = "TZ", OSSO = "OAS", LZ = "NLZ",
                     ZGZ = "ZUGZ", ZG = "ZUGZ", URZ = "URZ", NW = "NIW", OW ="OBW", AZ = "AZM",
                     BL = "BLZ", SZ = "SOZ", Blick = "BLI", BaM = "BLIA", `So-Bli` = "SBLI",
                     SGL = "SOS", SOGL = "SOS", SGR = "SOS", `20SG` = "ZWA", `20ZH` = "ZWA", `20LU` = "ZWA",
                     `20BE` = "ZWA", `20BS` = "ZWA", BZ = "BZ", Mag = "TAM", SoZ ="TAS", TA = "TA",
                     Bund = "BU", BaZ = "BAZ", WoZ = "WoZ", WW = "WEW")

#remove columns we don't need
aps_smd[c(4:7,9, 10, 12)] <- NULL


#Extraxt title keywords
aps_smd$keyword <- aps_smd$filename%>%
  str_extract(pattern = paste0("(?<=_(", paste0(unique(aps$newspaper_name), collapse = "|"), ")_(\\s?))[a-zA-Z0-9\u0080-\\uFFFF]+?(?=([:punct:]|\\s))"))

# uniformize special characters
aps_smd$keyword <- stri_trans_nfc(aps_smd$keyword)

#Extract topic
aps_smd$topic <- aps_smd$filename%>%
 str_extract(pattern = "[:digit:]{2,}")


write_csv(aps_smd, "~/APS/aps_smd_all.csv")

##############################################
#### SMD Corpus prep APS-matching         ####
##############################################

#### Preparations ####

# Cleaning
rm(list=ls())

# define stringsAsFactors = FALSE as default
options(stringsAsFactors = FALSE)

#libraries
library(tidyverse)
library(quanteda)
library(readtext)

#load data
df_texts_full_and_lastnames <- readRDS("~/capstone_share/Names_Full_and_Lastnames/df_texts_full_and_lastnames.rds")
dfmat_sex_fullnames <- readRDS("~/capstone_share/Names_Full_and_Lastnames/dfmat_sex_fullnames.rds")

#### Coombine dfm and text-data.frame to get gender variable ####

#convert dfm to data.frame and take only texts with >0 matches
gender <- convert(dfm_subset(dfmat_sex_fullnames, rowSums(dfmat_sex_fullnames)>0), to = "data.frame")
names(gender) <- c("doc_id", "maennlich", "weiblich")

#merge
df_texts_gender <- merge(df_texts_full_and_lastnames, gender, by = "doc_id")

saveRDS(df_texts_gender, "~/APS/df_texts_gender.rds")

#### bring to same format as APS-corpus ####

#date to same format
df_texts_gender$date <- df_texts_gender$pubDateTime%>%
  str_extract(pattern = "[:digit:]{4}[:punct:][:digit:]{2}[:punct:][:digit:]{2}")

#remove newspapers not included in APS corpus
df_texts_gender <- df_texts_gender[ ! df_texts_gender$so %in% 
                                      c("WZ", "WEOB", "TBT", "NLZS", "ZUGP", "ZUGB", "GTB",
                                        "BT", "LTZ", "OLT", "SAW", "ZOF", "BLIAO", "SHZ", "SHZO",
                                        "BEO", "BEOO", "BIZ", "BIZO", "BOL", "BOLF", "BOLM",
                                        "BOLMF", "GP", "PME", "SCHB", "SI", "SIO", "LAL", "SCHV",
                                        "SIS", "TELE", "TVLL", "TVS", "TVZW", "TVV", "TVHU",
                                        "TVT", "ONA", "LINT", "BUET", "ZWAF", "ZWAO", "FEM",
                                        "SF", "ANNA", "BEOL", "LAT", "LB", "ENC", "FUW", "FUWO",
                                        "TVGL", "NNBE", "NNBS", "NNBU", "NNTA", "TASI", "THT",
                                        "ZHUL", "ZSZ", "TAZT", "REPU", "BIT"
                                      ), ]

#recode those with multiple shortnames to one option only
df_texts_gender$so <- recode(df_texts_gender$so, BZM = "BLZ", SOZM = "SOZ", LZ ="NLZ", AZ = "AZM")


saveRDS(df_texts_gender, "~/APS/df_texts_gender_SMD_all.rds")

##############################################
#### APS-SMD matching, inconclusives out  ####
##############################################

#### Preparations ####

# Cleaning
rm(list=ls())

# define stringsAsFactors = FALSE as default
options(stringsAsFactors = FALSE)

#libraries
library(tidyverse)
library(stringi)

#load data
df_texts_gender_SMD <- readRDS("~/APS/df_texts_gender_SMD_all.rds")%>%
  .[! is.na(.$date) , ]

aps_smd <- read_csv("~/APS/aps_smd_all.csv")%>%
  .[! is.na(.$keyword),]

### Matching ###

##preparations
# load data, make sure both have same encoding, make all letters lower-case
df_texts_gender_SMD$ht <- stri_trans_nfc(df_texts_gender_SMD$ht)%>%
  str_to_lower(locale = "UTF-8")

aps_smd$keyword <- aps_smd$keyword%>%
  str_to_lower(locale = "UTF-8")


#create vectors for date and newspaper source
date <- unique(df_texts_gender_SMD$date)
so <- unique(df_texts_gender_SMD$so)

#create new column for topic and a spare one for loop
df_texts_gender_SMD$topic <- NA
df_texts_gender_SMD$test <- NA

#look at articles with:
## 1. same date
## 2. same source
## 3. same character strings
## 4. mark articles that match multiple times

for(i in date){
  for (j in so){
    aps_i <- aps_smd[aps_smd$date == i & aps_smd$so == j , ]
    for(k in 1:nrow(aps_i)){
      smd_i <- df_texts_gender_SMD[df_texts_gender_SMD$date == i & df_texts_gender_SMD$so == j , ]
      for(l in 1:nrow(smd_i)){ 
        ifelse(smd_i$ht[l] %>% str_detect(aps_i$keyword[k]), 
               ifelse(is.na(df_texts_gender_SMD$topic[df_texts_gender_SMD$doc_id == smd_i$doc_id[l]]),
                      df_texts_gender_SMD$topic[df_texts_gender_SMD$doc_id == smd_i$doc_id[l]]  <- aps_i$topic_ref[k],
                      df_texts_gender_SMD$topic[df_texts_gender_SMD$doc_id == smd_i$doc_id[l]]  <- "inconclusive"),
               df_texts_gender_SMD$test[l] <- NA
        )
      }
    }
  }
}
df_texts_gender_SMD$test <- NULL

#keep only those which matched exactly one time
df_topic_match <- df_texts_gender_SMD[ ! df_texts_gender_SMD$topic %in% NA , ]
df_topic_match <- df_topic_match[! df_topic_match$topic %in% "inconclusive" ,]

saveRDS(df_topic_match, "~/APS/df_topic_match_all_sorted.rds")

##############################################
#### Recode Topics                        ####
##############################################

#### Preparations ####

# Cleaning
rm(list=ls())

# define stringsAsFactors = FALSE as default
options(stringsAsFactors = FALSE)

#libraries
library(tidyverse)

#load data
df_topic_match_recoded <- readRDS("~/APS/df_topic_match_all_sorted.rds")

#### Recode topics ####

#length(unique(df_topic_match$topic))
#[1] 435
# we have far too many topics to make a reasonable statement.
# Solution: don't look at sub-topics. To do that, topics have to be recoded on higher level
# Coding according to APS-Documentation

df_topic_match_recoded$topic[df_topic_match_recoded$topic == "3"] <- '30'
df_topic_match_recoded$topic[df_topic_match_recoded$topic == "4"] <- '40'

df_topic_match_recoded$topic_re <- df_topic_match_recoded$topic%>%
  str_extract(pattern = "^[:digit:]{2}(?=[:digit:]{0,2})")%>%
  str_replace_all("^3[:digit:]", "30")%>%
  str_replace_all("^4[:digit:]", "40")%>%
  recode('10' = "Staats- und Sozialordnung der Schweiz", '11' = "Institutionen", '12' = "Aussenpolitik",
         '13' = "Landesverteidigung", '14' = "Finanz- und Geldpolitik", '15' = "Wirtschaft und Infrastruktur",
         '16' = "Sozial- und Bevölkerungspolitik", '17' = "Erziehungs- Bildungswesen, Kulturpolitik, Medienpolitik",
         '24' = "Kantonale Finanz- und Geldpolitik", '27' = "Erziehungs- und Bildungswesen, Kulturpolitik, Medienpolitik (kantonal)",
         '30' = "Parteien und Parteiensystem", '20' = "Staats- und Sozialordnung (kantonal)", '21' = "Kantonale Institutionen",
         '22' = "Beziehungen zwischen den Kantonen", '40' = "Verbände, Interessenorganisationen und Vereinigungen",
         '25' = "Kantonswirtschaft und kantonale Infrastruktur", '26' = "Kantonale Sozial- und Bevölkerungspolitik")

#now I want a column that does not draw a difference between kantonal and schweizweit
df_topic_match_recoded$topic_eq <- df_topic_match_recoded$topic_re%>%
  recode('Kantonale Finanz- und Geldpolitik' = "Finanz- und Geldpolitik", 'Staats- und Sozialordnung (kantonal)' = "Staats- und Sozialordnung",
         'Staats- und Sozialordnung der Schweiz' = "Staats- und Sozialordnung",
         'Kantonale Institutionen' = "Institutionen", 'Kantonswirtschaft und kantonale Infrastruktur' = "Wirtschaft und Infrastruktur",
         'Kantonale Sozial- und Bevölkerungspolitik' = "Sozial- und Bevölkerungspolitik",
         'Erziehungs- und Bildungswesen, Kulturpolitik, Medienpolitik (kantonal)' = "Erziehungs- Bildungswesen, Kulturpolitik, Medienpolitik")

#### alternative solution: recode topics that occur more than 400 times on lowest level
df_topic_match_recoded$topic_low <- df_topic_match_recoded$topic%>%
  recode('1272' = "Asylrecht, Flüchtlinge", '34' = "Parteien: SVP", '1121' = "Eidgenössische Wahlen", '161' = "Migrationsfragen",
         '1211' = "Europäische Organisationen", '2112' = "Wahlen in Kantonsregierungen", '2712' = "Grundschule, Gymnasium (kantonal)",
         '1112' = "Bundesratswahlen", '33' = "Parteien: SP", '112' = "Parlament (national)", '1641' = "Erste Säule (AHV/IV)",
         '142' = "Direkte Steuern (national)", '1753' = "Radio und Fernsehen", '15323' = "Strassenbau und -verkehr (national)",
         '25323' = "Strassenbau und -verkehr (kantonal)", '2111' = "Kantonale Verwaltung", '2113' = "Kantonsregierung, Stadtregierung",
         '133' = "Bewaffnung und Ausrüstung", '32' = "Parteien: FDP", '111' = "Regierung (national)", '162' = "Familienpolitik",
         '212' = "Kantonsparlament, Stadtparlament", '31' = "Parteien: CVP", '1531' = "Energie", '2662' = "Kantonales Spitalwesen",
         '1571' = "Geld- und Kapitalmarkt, Banken", '211' = "Kantonsregierung, Stadtregierung", '242' = "Direkte Steuern (kantonal)",
         '1661' = "Kranken- und Umfallversicherung", '15321' = "Eisenbahn", '25320' = "Öffentlicher Verkehr (kantonal)" )

#remove rows with topic 91 - I have no idea what it is and the APS documentation does not help either
df_topic_match_recoded <- df_topic_match_recoded[ ! df_topic_match_recoded$topic %in% c('91', '1', '2', "Beziehungen zwischen den Kantonen") ,]

# save
saveRDS(df_topic_match_recoded, "~/APS/df_topic_match_recoded_all_sorted.rds")

############################################################
#### Topic Classifier Training                          ####
############################################################

#### Preparations ####

# Cleaning
rm(list=ls())

# define stringsAsFactors = FALSE as default
options(stringsAsFactors = FALSE)

#libraries
library(tidyverse)
library(quanteda)


# load data
df_topic_match_recoded_all <- readRDS("~/APS/df_topic_match_recoded_all_sorted.rds")

##### Create training and test set #####

# to only look at recoded topics with distinction between kantonal and schweizweit, get rid of other topic variables
df_topic_match_recoded_all$topic <- NULL
df_topic_match_recoded_all$topic_re <- NULL
# combine "Verbände", "Parteien", "Institutionen" to one topic
df_topic_match_recoded_all$topic_eq[df_topic_match_recoded_all$topic_eq == "Verbände, Interessenorganisationen und Vereinigungen"] <- "Parteien und Parteiensystem, Verbände, Interessensorganisationen"
df_topic_match_recoded_all$topic_eq[df_topic_match_recoded_all$topic_eq == "Parteien und Parteiensystem"] <- "Parteien und Parteiensystem, Verbände, Interessensorganisationen"
df_topic_match_recoded_all$topic_eq[df_topic_match_recoded_all$topic_eq == "Parteien und Parteiensystem, Verbände, Interessensorganisationen"] <- "Institutionen"

# get sample
set.seed(123)
df_texts <-  df_topic_match_recoded_all[! df_topic_match_recoded_all$topic_eq == "Beziehungen zwischen den Kantonen",]%>%
  .[sample(nrow(.), 46000), ]

# create corpus
quanteda_options(threads = 10)
corpus_texts <- corpus(df_texts)

# create ID variable for subsetting
docvars(corpus_test, "id") <- 1:ndoc(corpus_test)

# create test set
dfmat_test_set <- corpus_texts %>%
  corpus_subset(id <= 2000) %>%
  dfm(remove = c(stopwords("de"), "dass"), remove_numbers = TRUE, remove_punct = TRUE)%>%
  dfm(remove = "[:punct:]")%>%
  dfm_wordstem(language = "de")

# training set
dfmat_training_set <- corpus_texts %>%
  corpus_subset(id > 2000) %>%
  dfm(remove = c(stopwords("de"), "dass"), remove_numbers = TRUE, remove_punct = TRUE)%>%
  dfm(remove = "[:punct:]")%>%
  dfm_wordstem(language = "de")

#### Train classifier for "topic_eq" prediction ####
tmod_nb <- textmodel_nb(dfmat_training_set, docvars(dfmat_training_set, "topic_eq"))
summary(tmod_nb)

#### Apply classifier to test set and create cross-table ####

#predict class in test set
tmod_pred <- predict(tmod_nb, dfmat_test_set, force = TRUE)

# create cross-table
tab <- table(actual = docvars(dfmat_test_set, "topic_eq"),
             predicted = tmod_pred)

saveRDS(tmod_nb, "~/APS/tmod_nb_short.rds")

#### How reliable does it work? ####
library(caret)

performance_eq_all <- confusionMatrix(tab)

performance_eq_all$byClass[, "F1"]


############################################################
#### Apply Topic Classifier to corpus                   ####
############################################################

#### Preparations ####

# Cleaning
rm(list=ls())

# define stringsAsFactors = FALSE as default
options(stringsAsFactors = FALSE)

# libraries
library(tidyverse)
library(quanteda)

# load data
tmod_nb <- readRDS("~/APS/tmod_nb_short.rds")
corpus_texts <- readRDS("~/capstone_share/Names_Full_and_Lastnames/corp_texts_full_and_lastnames.rds")

#### Prepare data
quanteda_options(threads = 6)

# create dfm, remove stopwords, numbers and punctuation, stem words
dfmat_texts <- corpus_texts %>%
  dfm(remove = c(stopwords("de"), "dass"), remove_numbers = TRUE, remove_punct = TRUE)%>%
  dfm_wordstem(language = "de")

#### Apply classifier
# predict topic
pred_topic <- predict(tmod_nb, dfmat_texts, force = TRUE)

# ad information to dataframe
df_texts_full_and_lastnames <- readRDS("~/capstone_share/Names_Full_and_Lastnames/df_texts_full_and_lastnames.rds")
df_texts_full_and_lastnames$topic <- pred_topic

saveRDS(df_texts_full_and_lastnames, "~/capstone_share/df_texts_full_and_lastnames_topic_short.rds")

############################################################
#### Plots                                              ####
############################################################

#### Preparations ####

# Cleaning
rm(list=ls())


library(tidyverse)

# load data
df_names_parties_etc_per_doc <- readRDS("~/capstone_share/Names_Full_and_Lastnames/df_names_parties_etc_per_doc.rds")
names_dict_national_roles <- readRDS("~/capstone_share/Names_Full_and_Lastnames/df_names_dict_national_roles.rds")

#### Remove Bundesräte
# get information of year a article was printed
df_names_parties_etc_per_doc$year <- df_names_parties_etc_per_doc$document%>%
  str_extract("^[:digit:]{4}")

# merge dataframes
names(df_names_parties_etc_per_doc)
names(names_dict_national_roles)
df_merge <- names_dict_national_roles%>%
  select(name, dict, Regex_Bruno, year, CouncilName)

df_national <- merge(df_names_parties_etc_per_doc, df_merge)

df_wo_br <- anti_join(df_names_parties_etc_per_doc, df_national)

# remove Bundeskanzler und Bundesräte
df_national <- df_national%>%
  .[! is.na(.$CouncilName),]%>%
  .[! .$CouncilName == "Bundesrat",]


df_wo_br$CouncilName <- NA
df_wo_br <- rbind(df_wo_br, df_national)

# count of women and men per article
df_geschlecht <- df_wo_br%>%
  group_by(document, geschlecht)%>%
  summarise(count = sum(count))%>%
  ungroup()

df_geschlecht <- spread(data = df_geschlecht, key = geschlecht, value = count, fill = 0)
names(df_geschlecht) <- c("document", "weiblich", "maennlich")

# Share of women per text
df_geschlecht$w_share <- df_geschlecht$weiblich / (df_geschlecht$weiblich + df_geschlecht$maennlich)

# merge with topic
df_topic <- readRDS("~/capstone_share/df_texts_full_and_lastnames_topic_short.rds")%>%
  select(doc_id, topic, so)
names(df_topic) <- c("document", "topic", "so")

df_geschlecht <- merge(df_geschlecht, df_topic)

# merge with newspaper infos
df_newspaper <- readRDS("~/capstone_share/df_medien_teilnehmer_typ.rds")%>%
  select(Kürzel, Partner, Typ, foeg_2017)
names(df_newspaper) <- c("so", "Verlag", "typ", "foeg_2017")

df_geschlecht <- merge(df_geschlecht, df_newspaper)

# tamedia or not?
df_geschlecht$tamedia <- 0
df_geschlecht$tamedia[df_geschlecht$Verlag == "Tamedia AG"] <- 1


saveRDS(df_geschlecht, "~/df_geschlecht.rds")

# Estimate share of women per article for all newspapers
topic1 <- lm(w_share ~ -1 + topic, data = df_geschlecht)

df_topiclm <- as.data.frame(matrix(NA, nrow = 8, ncol = 5))
names(df_topiclm) <- c("topic", "coef", "se", "ciinf", "cisup")
df_topiclm$topic <- c("Aussenpolitik", "Erziehungs- Bildungswesen, Kulturpolitik, Medienpolitik",
                      "Finanz- und Geldpolitik", "Institutionen, Parteien und Verbände", "Landesverteidigung",
                      "Sozial- und Bevölkerungspolitik", "Staats- und Sozialordnung", "Wirtschaft und Infrastruktur")
df_topiclm$coef <- topic1$coef
df_topiclm$se <- summary(topic1)$coefficients[,2]

# calculate confidence interval
df_topiclm$ciinf <- df_topiclm$coef - qnorm(0.975)*df_topiclm$se
df_topiclm$cisup <- df_topiclm$coef + qnorm(0.975)*df_topiclm$se
df_topiclm$tamedia <- "Alle"

# Estimate share of women per article only for Tamedia outlets
topic_ta <- lm(w_share ~ -1 + topic, data = df_geschlecht[df_geschlecht$Verlag == "Tamedia AG",])
df_topiclm_ta <- as.data.frame(matrix(NA, nrow = 8, ncol = 5))
names(df_topiclm_ta) <- c("topic", "coef", "se", "ciinf", "cisup")

df_topiclm_ta$topic <- c("Aussenpolitik", "Erziehungs- Bildungswesen, Kulturpolitik, Medienpolitik",
                         "Finanz- und Geldpolitik", "Institutionen, Parteien und Verbände", "Landesverteidigung",
                         "Sozial- und Bevölkerungspolitik", "Staats- und Sozialordnung", "Wirtschaft und Infrastruktur")

df_topiclm_ta$coef <- topic_ta$coef
df_topiclm_ta$se <- summary(topic_ta)$coefficients[,2]
# calculate confidence interval
df_topiclm_ta$ciinf <- df_topiclm_ta$coef - qnorm(0.975)*df_topiclm_ta$se
df_topiclm_ta$cisup <- df_topiclm_ta$coef + qnorm(0.975)*df_topiclm_ta$se
df_topiclm_ta$tamedia <- "Tamedia"

# combine all newspapers and only tamedia estimates
df_plot_topic <- rbind(df_topiclm, df_topiclm_ta)

# order according to women share in all newspapers
df_plot_topic$topic <- factor(df_plot_topic$topic,
                              levels = unique(df_plot_topic$topic[order(df_plot_topic$coef[df_plot_topic$tamedia == "Alle"])],
                                              decreasing = FALSE))

# Plot
ggplot(df_plot_topic) + aes(x=topic, y = 100*coef,
                            ymin = 100*ciinf, ymax = 100*cisup, color = tamedia) +
  geom_point() +
  geom_pointrange() +
  coord_flip() +
  scale_color_manual(labels = c("Alle", "Tamedia"), values = c("black", "#FF0050"))+
  labs(title = "Anteil an Namensnennungen von Politikerinnen in Artikeln nach Thema",
       subtitle = "Durchschnitt auf Artikelebene",
       x = "", y = "Prozent", color = "Verlag")+
  theme_light()+
  theme(axis.text.y = element_text(size = 11))


#### APS matched articles -> estimate for topic codification on lowest level 
df_topic_match_recoded_all_sorted <- readRDS("~/APS/df_topic_match_recoded_all_sorted.rds")

df_geschlecht_aps <- df_geschlecht%>%
  select(document, w_share)
names(df_geschlecht_aps) <- c("doc_id", "w_share")

# Prepare Data
df_topic_match_media <- merge(df_topic_match_recoded_all_sorted, df_geschlecht_aps)

# get information on newspapers
df_medien_teilnehmer_typ <- readRDS("~/capstone_share/df_medien_teilnehmer_typ.rds")%>%
  select(Partner, Kürzel, Typ)
names(df_medien_teilnehmer_typ) <- c("Verlag", "so", "typ")

# recode newspaper shortnames with more than one option how it's written
df_medien_teilnehmer_typ$so <- df_medien_teilnehmer_typ$so%>%
  recode('SOZ / SOZM' = "SOZ", 'BLZ / BZM' = "BLZ")

# merge
df_topic_match_media <- merge(df_topic_match_media, df_medien_teilnehmer_typ)

saveRDS(df_topic_match_media, "~/APS/df_topic_match_media.rds")

# Estimates for all newspapers (in APS corpus)
lm_topic_d <- lm(w_share ~ -1 + as.factor(topic_low), df_topic_match_media)

# Extract values
topic_d <- names(lm_topic_d$coefficients[402:431])%>%
  str_replace("as.factor\\(topic_low\\)", "")

df_topiclm_d <- as.data.frame(matrix(NA, nrow = length(topic_d), ncol = 5))
names(df_topiclm_d) <- c("topic", "coef", "se", "ciinf", "cisup")
df_topiclm_d$topic <- topic_d
df_topiclm_d$coef <- lm_topic_d$coef[402:431]
se_daily <- summary(lm_topic_d)$coefficients[,2]
df_topiclm_d$se <- se_daily[402:431]
# calculate confidence interval
df_topiclm_d$ciinf <- df_topiclm_d$coef - qnorm(0.975)*df_topiclm_d$se
df_topiclm_d$cisup <- df_topiclm_d$coef + qnorm(0.975)*df_topiclm_d$se
df_topiclm_d$tamedia <- "Alle"

#### Estimates for only tamedia
lm_topic_d_ta <- lm(w_share ~ -1 + as.factor(topic_low), df_topic_match_media[df_topic_match_media$Verlag == "Tamedia AG",])

# ectract values
topic_d_ta <- names(lm_topic_d_ta$coefficients[374:403])%>%
  str_replace("as.factor\\(topic_low\\)", "")
df_topiclm_d_ta <- as.data.frame(matrix(NA, nrow = length(topic_d), ncol = 6))
names(df_topiclm_d_ta) <- c("topic", "coef", "se", "ciinf", "cisup", "tamedia")
df_topiclm_d_ta$topic <- topic_d
df_topiclm_d_ta$coef <- lm_topic_d_ta$coef[374:403]
se_d_ta <- summary(lm_topic_d_ta)$coefficients[,2]
df_topiclm_d_ta$se <- se_d_ta[374:403]
# calculate confidence interval
df_topiclm_d_ta$ciinf <- df_topiclm_d_ta$coef - qnorm(0.975)*df_topiclm_d_ta$se
df_topiclm_d_ta$cisup <- df_topiclm_d_ta$coef + qnorm(0.975)*df_topiclm_d_ta$se
df_topiclm_d_ta$tamedia <- "Tamedia"

# combine estimates for all and tamedia outlets
df_plot_topic_d <- rbind(df_topiclm_d, df_topiclm_d_ta)

# order according to all
df_plot_topic_d$topic <- factor(df_plot_topic_d$topic,
                                levels = unique(df_plot_topic_d$topic[order(df_plot_topic_d$coef[df_plot_topic_d$tamedia == "Alle"])],
                                                decreasing = FALSE))

# shorten to 25 most frequent topics instead of 30 for better readability of the plot
df_topic_match_recoded_all_sorted <- readRDS("~/APS/df_topic_match_recoded_all_sorted.rds")
df_topic_freq <- df_topic_match_recoded_all_sorted$topic_low%>%
  table()%>%
  as.data.frame()

names(df_topic_freq) <- c("topic", "freq")
df_topic_freq <- df_topic_freq[405:434,]%>%
  .[order(.$freq),]

df_plot_topic_d <- df_plot_topic_d[! df_plot_topic_d$topic %in% df_topic_freq$topic[1:5] , ]

# Plot
ggplot(df_plot_topic_d) + aes(x=topic, y = 100*coef,
                              ymin = 100*ciinf, ymax = 100*cisup, color = tamedia)  +
  geom_point() +
  geom_pointrange() +
  scale_color_manual(labels = c("Alle", "Tamedia"), values = c("black", "#FF0050"))+
  coord_flip() +
  labs(title = "Anteil an Namensnennungen von Politikerinnen in Artikeln nach Thema",
       subtitle = "Durchschnitt auf Artikelebene, nur 25 häufigsten Themen",
       x = "", y = "Prozent", color = "Verlag")+
  theme_light()+
  theme(axis.text.y = element_text(size = 11))


#### Women share per topic per party

# get party names for parties that are of interest
partei <- c("SP", "GPS", "GLP", "EVP", "CVP", "BDP", "FDP", "SVP", "EDU")

# calculate name calls of men and women per party per topic
df_topic_parties <- df_wo_br%>%
  merge(df_topic)%>%
  select(document, count, geschlecht, parteiname, topic)%>%
  group_by(parteiname, topic, geschlecht)%>%
  summarise(count = sum(count))%>%
  .[.$parteiname %in% partei,]

# relevel factors for better readability of plot
df_topic_parties$geschlecht <- as.factor(df_topic_parties$geschlecht)
df_topic_parties$geschlecht <- relevel(df_topic_parties$geschlecht, "2")
df_topic_parties$topic <- factor(df_topic_parties$topic, levels = rev(levels(df_topic_parties$topic)))
df_topic_parties$parteiname <- as.factor(df_topic_parties$parteiname)
df_topic_parties$parteiname <- factor(df_topic_parties$parteiname,
                                      levels = c("SP", "GPS", "GLP", "EVP","CVP","BDP", "FDP", "SVP", "EDU"))

# Plot
ggplot(df_topic_parties, aes(x = topic, y = count, fill = geschlecht)) + 
  geom_bar(position = position_fill(), stat = "identity")+
  scale_y_continuous(labels = scales::percent_format())+
  labs(y = "", x = "", title = "Anteil an Namensnennungen",
       subtitle = "Pro Partei und Thema",
       fill = "")+
  scale_fill_manual(labels = c("Männer", "Frauen"), values = c("#48839c","#f81233"))+
  coord_flip()+
  facet_wrap(~parteiname)+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 7))

