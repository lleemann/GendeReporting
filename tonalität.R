# Capstone Course UZH ##########
# Politikerinnen in den Medien #
# Sentiment Analysis ###########
# Eliane Antonia Müller ########
################################


# packages
library(dplyr)
library(ggplot2)
library(glm.predict) # devtools::install_github("benjaminschlegel/glm.predict")
library(quanteda)
library(readr)
library(stringr)
library(tictoc)
library(tidyr)



### KWICS ###

## Preparation

# load dictionary with names of politicians
NamesDict <- readRDS("~/capstone_share/
                     Names_Full_and_Lastnames/names_dict_corrected.rds")


# build corpus
df_texts_full_and_lastnames <- readRDS("~/
                                       capstone_share/Names_Full_and_Lastnames/
                                       df_texts_full_and_lastnames.rds")
corp_final <- readRDS("~/capstone_share/Names_Full_and_Lastnames/
                      corp_texts_full_and_lastnames.rds")


## Female Kwics

# dictionary with only females
NamesDict_fem <- subset(NamesDict, NamesDict$geschlecht == 1, select = 7)


# kwic for female politicians
quanteda_options(threads = 20)

tic()
kwic_fem <- kwic(corp_final, pattern = NamesDict_fem$dict, 30)
toc()

quanteda_options(threads = 8)


# add metadata to kwic
kwic_fem_doc <- kwic_fem
names(kwic_fem_doc)[names(kwic_fem_doc) == 'docname'] <- 'doc_id'
kwic_fem_all <- merge(kwic_fem_doc, df_texts_full_and_lastnames, by = "doc_id")
rm(kwic_fem_doc)


# combine pre keyword post
kwic_fem_all$keytext <- paste(kwic_fem_all$pre, kwic_fem_all$keyword)
kwic_fem_all$keytext <- paste(kwic_fem_all$keytext, kwic_fem_all$post)
kwic_fem_all <- kwic_fem_all[,-c(4:6)]
kwic_fem_all <- kwic_fem_all[, c(1:3, 18, 4:17)]


# rename columns
names(kwic_fem_all)[names(kwic_fem_all) == 'text'] <- 'complete_text'
names(kwic_fem_all)[names(kwic_fem_all) == 'keytext'] <- 'text'


# kwic to corpus
corp_fem <- corpus(kwic_fem, split_context = FALSE, extract_keyword = TRUE)
corp_fem_all <- corpus(kwic_fem_all)


## Male Kwics

# dictionary with only males
NamesDict_male <- subset(NamesDict, NamesDict$geschlecht == 2, select = 7)


# kwic for male politicians
quanteda_options(threads = 20)

tic()
kwic_male <- kwic(corp_final, pattern = NamesDict_male$dict, 30)
toc()

quanteda_options(threads = 8)


# add metadata to kwic
kwic_male_doc <- kwic_male
names(kwic_male_doc)[names(kwic_male_doc) == 'docname'] <- 'doc_id'
kwic_male_all <- merge(kwic_male_doc, df_texts_full_and_lastnames, by = "doc_id")
rm(kwic_male_doc)


# combine pre keyword post
kwic_male_all$keytext <- paste(kwic_male_all$pre, kwic_male_all$keyword)
kwic_male_all$keytext <- paste(kwic_male_all$keytext, kwic_male_all$post)
kwic_male_all <- kwic_male_all[,-c(4:6)]
kwic_male_all <- kwic_male_all[, c(1:3, 18, 4:17)]


# rename columns
names(kwic_male_all)[names(kwic_male_all) == 'text'] <- 'complete_text'
names(kwic_male_all)[names(kwic_male_all) == 'keytext'] <- 'text'


# kwic to corpus
corp_male <- corpus(kwic_male, split_context = FALSE, extract_keyword = TRUE)
corp_male_all <- corpus(kwic_male_all)


### DICTIONARIES ###

## Merge Dictionaries

# load dictionary files
both_dictionaries <- read_delim("~/objects/both dictionaries.csv", 
                                ";", escape_double = FALSE, col_names = FALSE, trim_ws = 
                                  TRUE)
nrc_eng_ger <- read_delim("objects/nrc_eng_ger.csv", ";", escape_double = FALSE, 
                          col_types = cols(X13 = col_skip()), trim_ws = TRUE)


# rename columns
colnames(both_dictionaries) <- c("word", "sentiment")
nrc_ger <- nrc_eng_ger[,-1]
colnames(nrc_ger) <- c("word", "positive", "negative", "anger", "anticipation", "disgust",
                       "fear", "joy", "sadness", "surprise", "trust")


# merge the two dictionaries
dict_fin <- merge(both_dictionaries, nrc_ger, by = "word")


# split german sentiment
dict_fin$pos[dict_fin$sentiment > 0] <- 1
dict_fin$pos[dict_fin$sentiment < 0] <- 0
dict_fin$neg[dict_fin$sentiment < 0] <- 1
dict_fin$neg[dict_fin$sentiment > 0] <- 0

dict_fin <- dict_fin[,-2]


# reshape dataframe
dict_end <- gather(dict_fin, key = "sentiment", value = "value", 2:13)


# get rid of superfluous sentiments
dict_end$sentiment[dict_end$value==0] <- NA
dict_end <- na.omit(dict_end)
dict_end <- dict_end[,1:2]


# build dictionary
both_dict <- as.dictionary(dict_end)


# remove items
rm(dict_end, dict_fin, both_dictionaries, nrc_ger, nrc_eng_ger)


### SENTIMENT ANALYSIS ###

## Female Corpus

# sentiment analysis
quanteda_options(threads = 8)

tic()
corp_fem_both_30 <- dfm(corp_fem_all, dictionary = both_dict)
toc()


# to dataframe
quanteda_options(threads = 2)

corp_fem_both_30_df <- convert(corp_fem_both_30, to = "data.frame")


# calculate sentiment
corp_fem_both_30_df$sentiment <- log((corp_fem_both_30_df$positive+0.5)/
                                       (corp_fem_both_30_df$negative+0.5))


# extract doc ids
corp_fem_both_30_df$doc_id <- str_extract_all(corp_fem_both_30_df$document,
                                              "^201._output.csv.[:alnum:]{1,6}")
corp_fem_both_30_df$doc_id <- as.character(corp_fem_both_30_df$doc_id)


# add metadata
req_meta <- df_texts_full_and_lastnames[,c(1,3,4,6)]
both_fem_df_30 <- left_join(corp_fem_both_30_df, req_meta, by = "doc_id")


# add keyword-names to dataframe
both_fem_df_30$name <- kwic_fem$pattern


# remove items
rm(corp_fem_both_30, corp_fem_both_30_df)


## Male Corpus

# sentiment analysis
quanteda_options(threads = 8)

tic()
corp_male_both_30 <- dfm(corp_male_all, dictionary = both_dict)
toc()


# to dataframe
quanteda_options(threads = 2)

corp_male_both_30_df <- convert(corp_male_both_30, to = "data.frame")


# calculate sentiment
corp_male_both_30_df$sentiment <- log((corp_male_both_30_df$positive+0.5)/
                                        (corp_male_both_30_df$negative+0.5))


# extract doc ids
corp_male_both_30_df$doc_id <- str_extract_all(corp_male_both_30_df$document, 
                                               "^201._output.csv.[:alnum:]{1,6}")
corp_male_both_30_df$doc_id <- as.character(corp_male_both_30_df$doc_id)


# add metadata
req_meta <- df_texts_full_and_lastnames[,c(1,3,4,6)]
both_male_df_30 <- left_join(corp_male_both_30_df, req_meta, by = "doc_id")


# add keyword-names to dataframe
both_male_df_30$name <- kwic_male$pattern


# remove items
rm(corp_male_both_30_df, corp_male_both_30)


## Add Metadata

# read df with names and roles
df_names_dict_national_roles_spread_corrected <- readRDS("~/capstone_share/
                                                         Names_Full_and_Lastnames/
                                                         df_names_dict_national_roles
                                                         _spread_corrected.rds")


# add column for merging
df_names_dict_national_roles_spread_corrected$name_year <- paste(df_names_dict_national_roles_spread_corrected$dict,
                                                                 df_names_dict_national_roles_spread_corrected$year,
                                                                 sep = "_")


# subset male and female dfs
nat_roles_fem <- subset(df_names_dict_national_roles_spread_corrected,
                        df_names_dict_national_roles_spread_corrected$geschlecht == 1)
nat_roles_male <- subset(df_names_dict_national_roles_spread_corrected,
                         df_names_dict_national_roles_spread_corrected$geschlecht == 2)


# add columns for merging
both_fem_df_30_new$Jahr <- str_extract_all(both_fem_df_30_new$document, "^[:digit:]{4}", 
                                           simplify = T)%>%as.numeric()
both_fem_df_30_new$name_year <- paste(both_fem_df_30_new$name, both_fem_df_30_new$Jahr, 
                                      sep = "_")

both_male_df_30_new$Jahr <- str_extract_all(both_male_df_30_new$document, "^[:digit:]{4}", 
                                            simplify = T)%>%as.numeric()
both_male_df_30_new$name_year <- paste(both_male_df_30_new$name, both_male_df_30_new$Jahr, 
                                       sep = "_")


# merge dfs
df_both_fem_meta <- left_join(both_fem_df_30_new, nat_roles_fem, by = "name_year")

df_both_male_meta <- left_join(both_male_df_30_new, nat_roles_male, by = "name_year")


# add geschlecht
df_both_fem_meta$sex <- 1
df_both_male_meta$sex <- 0


## Recalculate Sentiment for Better Understanding

# Female Dataframe
df_both_fem_meta$senti_sum <- df_both_fem_meta$positive + df_both_fem_meta$negative

df_both_fem_meta$senti_sum <- df_both_fem_meta$senti_sum + 0.0000000000000000001

df_both_fem_meta$senti_share_pos <- (df_both_fem_meta$positive/
                                       df_both_fem_meta$senti_sum) * 100
df_both_fem_meta$senti_share_neg <- (df_both_fem_meta$negative/
                                       df_both_fem_meta$senti_sum) * 100


# Male Dataframe
df_both_male_meta$senti_sum <- df_both_male_meta$positive + df_both_male_meta$negative

df_both_male_meta$senti_sum <- df_both_male_meta$senti_sum + 0.0000000000000000001

df_both_male_meta$senti_share_pos <- (df_both_male_meta$positive/
                                        df_both_male_meta$senti_sum) * 100
df_both_male_meta$senti_share_neg <- (df_both_male_meta$negative/
                                        df_both_male_meta$senti_sum) * 100


### REGRESSION MODELS ###

## Merging Both Dataframes

# combine female & male dataframes
df_reg_model <- rbind(df_both_fem_meta, df_both_male_meta)


## Recode Parties - Left-Right & Liberal-Conservative

# recode parteiname
df_reg_model$parteiname[df_reg_model$parteiname == "BastA"] <- "GPS"


# parties on left-right & liberal-conservative spectrum
df_reg_model$partyspec[df_reg_model$parteiname == "-"] <- NA
df_reg_model$partyspec[df_reg_model$parteiname == "BDP"] <- 2
df_reg_model$partyspec[df_reg_model$parteiname == "CSP"] <- NA
df_reg_model$partyspec[df_reg_model$parteiname == "CVP"] <- 2
df_reg_model$partyspec[df_reg_model$parteiname == "EVP"] <- 1
df_reg_model$partyspec[df_reg_model$parteiname == "FDP"] <- 2
df_reg_model$partyspec[df_reg_model$parteiname == "GLP"] <- 2
df_reg_model$partyspec[df_reg_model$parteiname == "GPS"] <- 1
df_reg_model$partyspec[df_reg_model$parteiname == "LDP"] <- NA
df_reg_model$partyspec[df_reg_model$parteiname == "Lega"] <- 3
df_reg_model$partyspec[df_reg_model$parteiname == "LPS"] <- NA
df_reg_model$partyspec[df_reg_model$parteiname == "MCR"] <- NA
df_reg_model$partyspec[df_reg_model$parteiname == "PdA/PST"] <- 1
df_reg_model$partyspec[df_reg_model$parteiname == "SP"] <- 1
df_reg_model$partyspec[df_reg_model$parteiname == "SVP"] <- 3


## Model 1

# Regression
sentsha_reg <- glm(senti_share_pos ~ sex * Jahr)
summary(sentsha_reg)


# Prediction
ssr_predicts <- predicts(sentsha_reg, "0-1; 2012-2018", sim.count = 100000, 
                          set.seed = 12345)


## Model 2

# Regression
emoreg <- glm(emotions ~ sex * Jahr)
summary(emoreg)


# Prediction
er_predicts <- predicts(emoreg, "0-1; 2012-2018", sim.count = 100000, 
                         set.seed = 12345)


### PLOTS ###

## Model 1

# recode sex
ssr_predicts$sex[ssr_predicts$sex == 0] <- "male"
ssr_predicts$sex[ssr_predicts$sex == 1] <- "female"


# plot
ggplot(ssr5_predicts, aes(ssr5_predicts$Jahr)) +
  geom_ribbon(aes(ymin = ssr5_predicts$lower, ymax = ssr5_predicts$upper, 
                  fill = as.factor(ssr5_predicts$sex), alpha = 0.3), show.legend = F) +
  geom_line(aes(y = ssr5_predicts$mean, color = as.factor(ssr5_predicts$sex))) +
  scale_color_manual(values = c("#f81233", "#48839c"), 
                     labels = c("Politikerinnen", "Politiker"), name = "Geschlecht") +
  scale_fill_manual(values = c("#f81233", "#48839c"), 
                    labels = c("Politikerinnen", "Politiker"), name = "Geschlecht") +
  xlab("Jahr") +
  ylab("Anteil positiver Wörter") +
  theme_light()


## Model 2

# recode sex
er_predicts$sex[er_predicts$sex == 0] <- "male"
er_predicts$sex[er_predicts$sex == 1] <- "female"


# plot
ggplot(er3_predicts, aes(er3_predicts$Jahr)) +
  geom_ribbon(aes(ymin = er3_predicts$lower, ymax = er3_predicts$upper, 
                  fill = as.factor(er3_predicts$sex), alpha = 0.3), show.legend = F) +
  geom_line(aes(y = er3_predicts$mean, color = as.factor(er3_predicts$sex))) +
  scale_color_manual(values = c("#f81233", "#48839c"), 
                     labels = c("Politikerinnen", "Politiker"), name = "Geschlecht") +
  scale_fill_manual(values = c("#f81233", "#48839c"), 
                    labels = c("Politikerinnen", "Politiker"), name = "Geschlecht") +
  xlab("Jahr") +
  ylab("Anzahl emotional konnotierter Wörter") +
  theme_light()


### COMPARISON TAMEDIA & OTHERS ###

## Preparation

# read media dataframe
medien <- readRDS("~/capstone_share/df_medien_teilnehmer_typ.rds")
names(medien)[names(medien) == 'Kürzel'] <- 'so'


# influence of TA & nTA
df_reg <- left_join(df_reg_model, medien, by = 'so')

df_reg$ta[df_reg$Partner == "Tamedia AG"] <- 1
df_reg$ta[df_reg$Partner != "Tamedia AG"] <- 0


## Models

# attach column names
attach(df_reg)


# regression
ta_mod1 <- glm(emotions ~ ta * sex)
ta_mod2 <- glm(senti_share_pos ~ ta * sex)


# prediction
ta_predicts1 <- predicts(ta_mod1, "0-1;0-1")
ta_predicts2 <- predicts(ta_mod2, "0-1;0-1")


### PLOTS ###

## Model 1

# recode sex
ta_predicts1$sex[ta_predicts1$sex == 0] <- "Politiker"
ta_predicts1$sex[ta_predicts1$sex == 1] <- "Politikerinnen"


# plot
ggplot(data = ta_predicts1) +
  geom_pointrange(aes(x = ta_predicts1$sex, ymin = ta_predicts1$lower, 
                      ymax = ta_predicts1$upper, y = ta_predicts1$mean, 
                      color = as.factor(ta_predicts1$ta))) +
  scale_color_manual(values = c("black", "#FF0050"), labels = c("nicht Tamedia", "Tamedia"), 
                     name = "Verlagshäuser") +
  guides(color = guide_legend(reverse = TRUE)) +
  xlab("Geschlecht") +
  ylab("Anzahl emotional konnotierte Wörter") +
  theme_light()


## Model 2

# recode sex
ta_predicts2$sex[ta_predicts2$sex == 0] <- "Politiker"
ta_predicts2$sex[ta_predicts2$sex == 1] <- "Politikerinnen"


# plot
ggplot(data = ta_predicts2) +
  geom_pointrange(aes(x = ta_predicts2$sex, ymin = ta_predicts2$lower, 
                      ymax = ta_predicts2$upper, y = ta_predicts2$mean, 
                      color = as.factor(ta_predicts2$ta))) +
  scale_color_manual(values = c("black", "#FF0050"), labels = c("nicht Tamedia", "Tamedia"), 
                     name = "Verlagshäuser") +
  guides(color = guide_legend(reverse = TRUE)) +
  xlab("Geschlecht") +
  ylab("Anteil positiver Wörter") +
  theme_light()



############################################################################################