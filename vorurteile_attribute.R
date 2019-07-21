#### Preparations ####

# Cleaning
rm(list=ls())

# define stringsAsFactors = FALSE as default
options(stringsAsFactors = FALSE)

# loading packages

library(quanteda)
library(readr)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(esquisse)
library(xtable)

# load dictionary with names of politicians
NamesDict <- readRDS("~/capstone_share/Names_Full_and_Lastnames/names_dict.rds")

# maximize quanteda power
quanteda_options("threads" = 8)

# dictionary with only females
NamesDict_fem <- subset(NamesDict, NamesDict$geschlecht == 1, select = 7)

# dictionary with only males
NamesDict_male <- subset(NamesDict, NamesDict$geschlecht == 2, select = 7)

# load names
df_texts_full_and_lastnames <- readRDS("~/capstone_share/Names_Full_and_Lastnames/df_texts_full_and_lastnames.rds")

# build corpus
corp_final <- corpus(df_texts_full_and_lastnames)

# KWIC female
kwic_fem <- kwic(corp_final, pattern = NamesDict_fem$dict, 5)

# KWIC male
kwic_male <- kwic(corp_final, pattern = NamesDict_male$dict, 5)


# Preparation for later subsetting in order to sort by publisher


# add information on media where the texts are published
df_medien_teilnehmer_typ <- readRDS("~/capstone_share/df_medien_teilnehmer_typ.rds")%>%
  rename(so = "Kürzel")
df_texts_full_and_lastnames <- readRDS("~/capstone_share/Names_Full_and_Lastnames/df_texts_full_and_lastnames.rds")%>%
  select(doc_id, so, pubDateTime, nz)

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

# join finally with whatever you have
df_texts_infos <- left_join(df_texts_full_and_lastnames, df_medien_teilnehmer_typ)

docvars(Kwic_lookup_attri_fem) <- left_join(docvars(Kwic_lookup_attri_fem), df_texts_infos)
docvars(Kwic_lookup_attri_femSum) <- left_join(docvars(Kwic_lookup_attri_femSum), df_texts_infos)
docvars(Kwic_lookup_attri_male) <- left_join(docvars(Kwic_lookup_attri_male), df_texts_infos)
docvars(Kwic_lookup_attri_maleSum) <- left_join(docvars(Kwic_lookup_attri_maleSum), df_texts_infos)

docvars(Kwic_lookup_look_fem) <- left_join(docvars(Kwic_lookup_look_fem), df_texts_infos)
docvars(Kwic_lookup_look_femSum) <- left_join(docvars(Kwic_lookup_look_femSum), df_texts_infos)
docvars(Kwic_lookup_look_male) <- left_join(docvars(Kwic_lookup_look_male), df_texts_infos)
docvars(Kwic_lookup_look_maleSum) <- left_join(docvars(Kwic_lookup_look_maleSum), df_texts_infos)

docvars(dfm_attri_full_fem) <- left_join(docvars(dfm_attri_full_fem), df_texts_infos)




#### ATTRIBUTES - Preparation ####

# loading attributes dictionary 
Attribute_dictionary <- read_csv("capstone_share/Attribute_dictionary.csv", 
                                 locale = locale(encoding = "WINDOWS-1252"))

# Attributes 
# select only negations starting with "nicht" including a space
words_to_compound <- Attribute_dictionary%>%apply(2, str_subset, pattern = "^(nicht).*\\s")%>%unlist()

# select only negations starting with "kein" including a space
words_to_compound <- Attribute_dictionary%>%apply(2, str_subset, pattern = "^(kein).*\\s")%>%unlist()

# select only negations starting with "keine" including a space
words_to_compound <- Attribute_dictionary%>%apply(2, str_subset, pattern = "^(keine).*\\s")%>%unlist()

# convert data.frame to dictionary
df_pre_dict_att <- Attribute_dictionary%>%gather(na.rm = T) # from wide to long
colnames(df_pre_dict_att) <- c("sentiment", "word") # adjust colnames
dict_attr <- as.dictionary(df_pre_dict_att) # create dictionary



## Female
# combine pre keyword post 
kwic_fem_chris$keytext <- paste(kwic_fem_chris$pre, kwic_fem_chris$keyword)
kwic_fem_chris$keytext <- paste(kwic_fem_chris$keytext, kwic_fem_chris$post)

# create data frame
df_key_doc_fem <- data.frame(text=kwic_fem_chris$keytext, doc_id = kwic_fem_chris$docname)

# convert to corpus
KwicCorp_fem <- corpus(df_key_doc_fem)

# tokenize
Tok_Kwic_fem <- tokens(KwicCorp_fem)

# apply dictionary
Kwic_lookup_attri_fem <- tokens_lookup(Tok_Kwic_fem, dictionary = dict_attr, levels = 1)

# restructure tokens object in preparation for later subsetting
vec_doc_ids <- docnames(Kwic_lookup_attri_fem)%>%
  str_extract(pattern = ".*\\.[:digit:]*?(a?)(?=\\.[:digit:])")
vec_doc_ids[is.na(vec_doc_ids)] <- docnames(Kwic_lookup_attri_fem)[is.na(vec_doc_ids)]
docvars(Kwic_lookup_attri_fem, "doc_id") <- vec_doc_ids

# create dfm
attri_dfm_fem <- dfm(Kwic_lookup_attri_fem)



## Male 
# combine pre keyword post 
kwic_male_chris$keytext <- paste(kwic_male_chris$pre, kwic_male_chris$keyword)
kwic_male_chris$keytext <- paste(kwic_male_chris$keytext, kwic_male_chris$post)

# create data frame
df_key_doc_male <- data.frame(text=kwic_male_chris$keytext, doc_id = kwic_male_chris$docname)

# convert to corpus
KwicCorp_male <- corpus(df_key_doc_male)

# tokenize
Tok_Kwic_male <- tokens(KwicCorp_male)

# apply dictionary
Kwic_lookup_attri_male <- tokens_lookup(Tok_Kwic_male, dictionary = dict_attr, levels = 1)

# restructure tokens object in preparation for later subsetting
vec_doc_ids <- docnames(Kwic_lookup_attri_male)%>%
  str_extract(pattern = ".*\\.[:digit:]*?(a?)(?=\\.[:digit:])")
vec_doc_ids[is.na(vec_doc_ids)] <- docnames(Kwic_lookup_attri_male)[is.na(vec_doc_ids)]
docvars(Kwic_lookup_attri_male, "doc_id") <- vec_doc_ids

# create dfm
attri_dfm_male <- dfm(Kwic_lookup_attri_male)


##### Attribute Analysis ##### 

# Create dfs for attribute results
df_attri_fem <- as.data.frame(attri_dfm_fem)
df_attri_male <- as.data.frame(attri_dfm_male)

# T-TESTS
# t-Test for political craftsmanship
t.test(df_attri_fem$polcraft_pos,df_attri_male$polcraft_pos,var.equal= FALSE, paired=FALSE)
# t-Test for vigourousness
t.test(df_attri_fem$vigour_pos,df_attri_male$vigour_pos, var.equal=FALSE, paired=FALSE) # ***
# t-Test for integrity
t.test(df_attri_fem$integrit_pos,df_attri_male$integrit_pos, var.equal=FALSE, paired=FALSE) # **
# t-Test for continuity
t.test(df_attri_fem$contin_pos,df_attri_male$contin_pos, var.equal=FALSE, paired=FALSE)
# t-Test for communicative skills
t.test(df_attri_fem$comm_pos,df_attri_male$comm_pos, var.equal=FALSE, paired=FALSE)
# t-Test for responsiveness
t.test(df_attri_fem$respons_pos,df_attri_male$respons_pos, var.equal=FALSE, paired=FALSE)



# Plot preparations

sex <- c(rep("Politikerinnen", 6), rep("Politiker", 6))
attribut <- rep(c("Politisches Geschick", "Durchsetzungsfähigkeit", "Integrität", "Kontinuität", "Kommunikative Fähigkeiten", "Ansprechbarkeit"), 2) 


# generate data points for graph 

# Female
scores <- c(
  mean(df_attri_fem$polcraft_pos)*1000,
  mean(df_attri_fem$vigour_pos)*1000, ##
  mean(df_attri_fem$integrit_pos)*1000, ##
  mean(df_attri_fem$contin_pos)*1000,
  mean(df_attri_fem$comm_pos)*1000, 
  mean(df_attri_fem$respons_pos)*1000,
  
 # Male
  mean(df_attri_male$polcraft_pos)*1000,
  mean(df_attri_male$vigour_pos)*1000, ##
  mean(df_attri_male$integrit_pos)*1000, ##
  mean(df_attri_male$contin_pos)*1000,
  mean(df_attri_male$comm_pos)*1000,
  mean(df_attri_male$respons_pos)*1000)

# Calculate error to produce graph error bars 

# Error terms
error_fem_polcraft_pos <- qt(0.975,df=length(df_attri_fem$polcraft_pos)-1)*sd(df_attri_fem$polcraft_pos)/sqrt(length(df_attri_fem$polcraft_pos))*1000
error_fem_vigour_pos <- qt(0.975,df=length(df_attri_fem$vigour_pos)-1)*sd(df_attri_fem$vigour_pos)/sqrt(length(df_attri_fem$vigour_pos)) *1000
error_fem_integrit_pos <- qt(0.975,df=length(df_attri_fem$integrit_pos)-1)*sd(df_attri_fem$integrit_pos)/sqrt(length(df_attri_fem$integrit_pos)) *1000
error_fem_contin_pos <- qt(0.975,df=length(df_attri_fem$contin_pos)-1)*sd(df_attri_fem$contin_pos)/sqrt(length(df_attri_fem$contin_pos)) *1000
error_fem_comm_pos <- qt(0.975,df=length(df_attri_fem$comm_pos)-1)*sd(df_attri_fem$comm_pos)/sqrt(length(df_attri_fem$comm_pos)) *1000
error_fem_respons_pos <- qt(0.975,df=length(df_attri_fem$respons_pos)-1)*sd(df_attri_fem$respons_pos)/sqrt(length(df_attri_fem$respons_pos)) *1000
error_male_polcraft_pos <- qt(0.975,df=length(df_attri_male$polcraft_pos)-1)*sd(df_attri_male$polcraft_pos)/sqrt(length(df_attri_male$polcraft_pos)) *1000
error_male_vigour_pos <- qt(0.975,df=length(df_attri_male$vigour_pos)-1)*sd(df_attri_male$vigour_pos)/sqrt(length(df_attri_male$vigour_pos)) *1000
error_male_integrit_pos <- qt(0.975,df=length(df_attri_male$integrit_pos)-1)*sd(df_attri_male$integrit_pos)/sqrt(length(df_attri_male$integrit_pos)) *1000
error_male_contin_pos <- qt(0.975,df=length(df_attri_male$contin_pos)-1)*sd(df_attri_male$contin_pos)/sqrt(length(df_attri_male$contin_pos)) *1000
error_male_comm_pos <- qt(0.975,df=length(df_attri_male$comm_pos)-1)*sd(df_attri_male$comm_pos)/sqrt(length(df_attri_male$comm_pos)) *1000
error_male_respons_pos <- qt(0.975,df=length(df_attri_male$respons_pos)-1)*sd(df_attri_male$respons_pos)/sqrt(length(df_attri_male$respons_pos)) *1000


left_bound <- c(
  # Female Attri Left Errors
  left_error_fem_polcraft_pos <- mean(df_attri_fem$polcraft_pos)*1000 - error_fem_polcraft_pos,
  left_error_fem_vigour_pos <- mean(df_attri_fem$vigour_pos)*1000- error_fem_vigour_pos,
  left_error_fem_integrit_pos <- mean(df_attri_fem$integrit_pos)*1000- error_fem_integrit_pos,
  left_error_fem_contin_pos <- mean(df_attri_fem$contin_pos)*1000- error_fem_contin_pos,
  left_error_fem_comm_pos <- mean(df_attri_fem$comm_pos)*1000- error_fem_comm_pos,
  left_error_fem_respons_pos <- mean(df_attri_fem$respons_pos)*1000- error_fem_respons_pos,
  # Male Attri Left Errors
  left_error_male_polcraft_pos <- mean(df_attri_male$polcraft_pos)*1000- error_male_polcraft_pos,
  left_error_male_vigour_pos <- mean(df_attri_male$vigour_pos)*1000- error_male_vigour_pos,
  left_error_male_integrit_pos <- mean(df_attri_male$integrit_pos)*1000- error_male_integrit_pos,
  left_error_male_contin_pos <- mean(df_attri_male$contin_pos)*1000- error_male_contin_pos,
  left_error_male_comm_pos <- mean(df_attri_male$comm_pos)*1000- error_male_comm_pos,
  left_error_male_respons_pos <- mean(df_attri_male$respons_pos)*1000- error_male_respons_pos)

right_bound <- c(
  # Female Attri Right Errors
  right_error_fem_polcraft_pos <- mean(df_attri_fem$polcraft_pos)*1000+ error_fem_polcraft_pos,
  right_error_fem_vigour_pos <- mean(df_attri_fem$vigour_pos)*1000+ error_fem_vigour_pos,
  right_error_fem_integrit_pos <- mean(df_attri_fem$integrit_pos)*1000+ error_fem_integrit_pos,
  right_error_fem_contin_pos <- mean(df_attri_fem$contin_pos)*1000+ error_fem_contin_pos,
  right_error_fem_comm_pos <- mean(df_attri_fem$comm_pos)*1000+ error_fem_comm_pos,
  right_error_fem_respons_pos <- mean(df_attri_fem$respons_pos)*1000+ error_fem_respons_pos,
  # Male Attri Right Errors
  right_error_male_polcraft_pos <- mean(df_attri_male$polcraft_pos)*1000+ error_male_polcraft_pos,
  right_error_male_vigour_pos <- mean(df_attri_male$vigour_pos)*1000+ error_male_vigour_pos,
  right_error_male_integrit_pos <- mean(df_attri_male$integrit_pos)*1000+ error_male_integrit_pos,
  right_error_male_contin_pos <- mean(df_attri_male$contin_pos)*1000+ error_male_contin_pos,
  right_error_male_comm_pos <- mean(df_attri_male$comm_pos)*1000+ error_male_comm_pos,
  right_error_male_respons_pos <- mean(df_attri_male$respons_pos)*1000+ error_male_respons_pos)


#### Attribute Analysis for Tamedia Subset #### 

## subset dataframes to tamedia only
fem_tamedia <-Kwic_lookup_attri_fem %>%
  tokens_subset(Partner == "Tamedia AG")%>%
  dfm()%>%
  convert(to = "data.frame")


male_tamedia <-Kwic_lookup_attri_male %>%
  tokens_subset(Partner == "Tamedia AG")%>%
  dfm()%>%
  convert(to = "data.frame")

# t-Test for political craftsmanship
t.test(fem_tamedia$polcraft_pos,male_tamedia$polcraft_pos,var.equal= TRUE, paired=FALSE)
# t-Test for vigourousness
t.test(fem_tamedia$vigour_pos,male_tamedia$vigour_pos, var.equal=FALSE, paired=FALSE) 
# t-Test for integrity
t.test(fem_tamedia$integrit_pos,male_tamedia$integrit_pos, var.equal=FALSE, paired=FALSE)
# t-Test for continuity
t.test(fem_tamedia$contin_pos,male_tamedia$contin_pos, var.equal=FALSE, paired=FALSE)
# t-Test for communicative skills
t.test(fem_tamedia$comm_pos,male_tamedia$comm_pos, var.equal=FALSE, paired=FALSE)
# t-Test for responsiveness
t.test(fem_tamedia$respons_pos,male_tamedia$respons_pos, var.equal=TRUE, paired=FALSE)


# Plot preparations

sex_TA <- c(rep("Politikerinnen", 6), rep("Politiker", 6))
attribut_TA <- rep(c("Politisches Geschick", "Durchsetzungsfähigkeit", "Integrität", "Kontinuität", "Kommunikative Fähigkeiten", "Ansprechbarkeit"), 2) 

# generate data points for graph 

# Female 
scores_TA <- c(
  mean(fem_tamedia$polcraft_pos)*1000,
  mean(fem_tamedia$vigour_pos)*1000, ##
  mean(fem_tamedia$integrit_pos)*1000, ##
  mean(fem_tamedia$contin_pos)*1000,
  mean(fem_tamedia$comm_pos)*1000, 
  mean(fem_tamedia$respons_pos)*1000,
  
  # Male 
  mean(male_tamedia$polcraft_pos)*1000,
  mean(male_tamedia$vigour_pos)*1000, ##
  mean(male_tamedia$integrit_pos)*1000, ##
  mean(male_tamedia$contin_pos)*1000,
  mean(male_tamedia$comm_pos)*1000,
  mean(male_tamedia$respons_pos)*1000)

# Calculate error to produce graph error bars 

## Error terms
error_fem_polcraft_pos_TA <- qt(0.95,df=length(fem_tamedia$polcraft_pos)-1)*sd(fem_tamedia$polcraft_pos)/sqrt(length(fem_tamedia$polcraft_pos))*1000
error_fem_vigour_pos_TA <- qt(0.95,df=length(fem_tamedia$vigour_pos)-1)*sd(fem_tamedia$vigour_pos)/sqrt(length(fem_tamedia$vigour_pos)) *1000
error_fem_integrit_pos_TA <- qt(0.95,df=length(fem_tamedia$integrit_pos)-1)*sd(fem_tamedia$integrit_pos)/sqrt(length(fem_tamedia$integrit_pos)) *1000
error_fem_contin_pos_TA <- qt(0.95,df=length(fem_tamedia$contin_pos)-1)*sd(fem_tamedia$contin_pos)/sqrt(length(fem_tamedia$contin_pos)) *1000
error_fem_comm_pos_TA <- qt(0.95,df=length(fem_tamedia$comm_pos)-1)*sd(fem_tamedia$comm_pos)/sqrt(length(fem_tamedia$comm_pos)) *1000
error_fem_respons_pos_TA <- qt(0.95,df=length(fem_tamedia$respons_pos)-1)*sd(fem_tamedia$respons_pos)/sqrt(length(fem_tamedia$respons_pos)) *1000

error_male_polcraft_pos_TA <- qt(0.95,df=length(male_tamedia$polcraft_pos)-1)*sd(male_tamedia$polcraft_pos)/sqrt(length(male_tamedia$polcraft_pos)) *1000
error_male_vigour_pos_TA <- qt(0.95,df=length(male_tamedia$vigour_pos)-1)*sd(male_tamedia$vigour_pos)/sqrt(length(male_tamedia$vigour_pos)) *1000
error_male_integrit_pos_TA <- qt(0.95,df=length(male_tamedia$integrit_pos)-1)*sd(male_tamedia$integrit_pos)/sqrt(length(male_tamedia$integrit_pos)) *1000
error_male_contin_pos_TA <- qt(0.95,df=length(male_tamedia$contin_pos)-1)*sd(male_tamedia$contin_pos)/sqrt(length(male_tamedia$contin_pos)) *1000
error_male_comm_pos_TA <- qt(0.95,df=length(male_tamedia$comm_pos)-1)*sd(male_tamedia$comm_pos)/sqrt(length(male_tamedia$comm_pos)) *1000
error_male_respons_pos_TA <- qt(0.95,df=length(male_tamedia$respons_pos)-1)*sd(male_tamedia$respons_pos)/sqrt(length(male_tamedia$respons_pos)) *1000


left_bound_TA <- c(
  # Female Attri Left Errors
  left_error_fem_polcraft_pos_TA <- mean(fem_tamedia$polcraft_pos)*1000 - error_fem_polcraft_pos_TA,
  left_error_fem_vigour_pos_TA <- mean(fem_tamedia$vigour_pos)*1000- error_fem_vigour_pos_TA,
  left_error_fem_integrit_pos_TA <- mean(fem_tamedia$integrit_pos)*1000- error_fem_integrit_pos_TA,
  left_error_fem_contin_pos_TA <- mean(fem_tamedia$contin_pos)*1000- error_fem_contin_pos_TA,
  left_error_fem_comm_pos_TA <- mean(fem_tamedia$comm_pos)*1000- error_fem_comm_pos_TA,
  left_error_fem_respons_pos_TA <- mean(fem_tamedia$respons_pos)*1000- error_fem_respons_pos_TA,
  # Male Attri Left Errors
  left_error_male_polcraft_pos_TA <- mean(male_tamedia$polcraft_pos)*1000- error_male_polcraft_pos_TA,
  left_error_male_vigour_pos_TA <- mean(male_tamedia$vigour_pos)*1000- error_male_vigour_pos_TA,
  left_error_male_integrit_pos_TA <- mean(male_tamedia$integrit_pos)*1000- error_male_integrit_pos_TA,
  left_error_male_contin_pos_TA <- mean(male_tamedia$contin_pos)*1000- error_male_contin_pos_TA,
  left_error_male_comm_pos_TA <- mean(male_tamedia$comm_pos)*1000- error_male_comm_pos_TA,
  left_error_male_respons_pos_TA <- mean(male_tamedia$respons_pos)*1000- error_male_respons_pos_TA)


right_bound_TA <- c(
  # Female Attri Right Errors
  right_error_fem_polcraft_pos <- mean(fem_tamedia$polcraft_pos)*1000+ error_fem_polcraft_pos_TA,
  right_error_fem_vigour_pos <- mean(fem_tamedia$vigour_pos)*1000+ error_fem_vigour_pos_TA,
  right_error_fem_integrit_pos <- mean(fem_tamedia$integrit_pos)*1000+ error_fem_integrit_pos_TA,
  right_error_fem_contin_pos <- mean(fem_tamedia$contin_pos)*1000+ error_fem_contin_pos_TA,
  right_error_fem_comm_pos <- mean(fem_tamedia$comm_pos)*1000+ error_fem_comm_pos_TA,
  right_error_fem_respons_pos <- mean(fem_tamedia$respons_pos)*1000+ error_fem_respons_pos_TA,
  
  # Male Attri Right Errors
  right_error_male_polcraft_pos <- mean(male_tamedia$polcraft_pos)*1000+ error_male_polcraft_pos_TA,
  right_error_male_vigour_pos <- mean(male_tamedia$vigour_pos)*1000+ error_male_vigour_pos_TA,
  right_error_male_integrit_pos <- mean(male_tamedia$integrit_pos)*1000+ error_male_integrit_pos_TA,
  right_error_male_contin_pos <- mean(male_tamedia$contin_pos)*1000+ error_male_contin_pos_TA,
  right_error_male_comm_pos <- mean(male_tamedia$comm_pos)*1000+ error_male_comm_pos_TA,
  right_error_male_respons_pos <- mean(male_tamedia$respons_pos)*1000+ error_male_respons_pos_TA)



#### Looks Preparation #####


# loading dictionary
looks_dictionary <- read_csv("capstone_share/looks.csv", 
                             locale = locale(encoding = "WINDOWS-1252"))

# select only negations starting with "nicht" including a space
words_to_compound <- looks_dictionary%>%apply(2, str_subset, pattern = "^(nicht).*\\s")%>%unlist()

# convert data.frame to dictionary
df_pre_dict_look <- looks_dictionary%>%gather(na.rm = T) # from wide to long
colnames(df_pre_dict_look) <- c("sentiment", "word") # adjust colnames
dict_look <- as.dictionary(df_pre_dict_look) # create dictionary


## Female Looks
# combine pre keyword post
kwic_fem_chris$keytext <- paste(kwic_fem_chris$pre, kwic_fem_chris$keyword)
kwic_fem_chris$keytext <- paste(kwic_fem_chris$keytext, kwic_fem_chris$post)

# convert to data frame
df_key_doc_fem <- data.frame(text=kwic_fem_chris$keytext, doc_id = kwic_fem_chris$docname)

# convert to corpus
KwicCorp_fem <- corpus(df_key_doc_fem)

# tokenize 
Tok_Kwic_fem <- tokens(KwicCorp_fem)

# apply dictionary
Kwic_lookup_look_fem <- tokens_lookup(Tok_Kwic_fem, dictionary = dict_look, levels = 1)

# restructure tokens object in preparation for later subsetting
vec_doc_ids <- docnames(Kwic_lookup_look_fem)%>%
  str_extract(pattern = ".*\\.[:digit:]*?(a?)(?=\\.[:digit:])")
vec_doc_ids[is.na(vec_doc_ids)] <- docnames(Kwic_lookup_look_fem)[is.na(vec_doc_ids)]
docvars(Kwic_lookup_look_fem, "doc_id") <- vec_doc_ids

# create dfm
look_dfm_fem <- dfm(Kwic_lookup_look_fem)



### Male Looks
# combine pre keyword post
kwic_male_chris$keytext <- paste(kwic_male_chris$pre, kwic_male_chris$keyword)
kwic_male_chris$keytext <- paste(kwic_male_chris$keytext, kwic_male_chris$post)

# convert to data frame
df_key_doc_male <- data.frame(text=kwic_male_chris$keytext, doc_id = kwic_male_chris$docname)

# convert to corpus
KwicCorp_male <- corpus(df_key_doc_male)

# tokenize 
Tok_Kwic_male <- tokens(KwicCorp_male)

# apply dictionary
Kwic_lookup_look_male <- tokens_lookup(Tok_Kwic_male, dictionary = dict_look, levels = 1)

# restructure tokens object in preparation for later subsetting
vec_doc_ids <- docnames(Kwic_lookup_look_male)%>%
  str_extract(pattern = ".*\\.[:digit:]*?(a?)(?=\\.[:digit:])")
vec_doc_ids[is.na(vec_doc_ids)] <- docnames(Kwic_lookup_look_male)[is.na(vec_doc_ids)]
docvars(Kwic_lookup_look_male, "doc_id") <- vec_doc_ids


# create dfm
look_dfm_male <- dfm(Kwic_lookup_look_male)



#### Look Analysis #####


## Create dfs for look results
df_look_fem <- as.data.frame(look_dfm_fem)
df_look_male <- as.data.frame(look_dfm_male)


## T-TESTS
# t-Test for clothing
t.test(df_look_fem$kleidung,df_look_male$kleidung, var.equal=FALSE, paired=FALSE)
# t-Test for hair
t.test(df_look_fem$haar,df_look_male$haar, var.equal=FALSE, paired=FALSE)
# t-Test for body type
t.test(df_look_fem$körperstatur,df_look_male$körperstatur, var.equal=FALSE, paired=FALSE)
# t-Test for eyes
t.test(df_look_fem$augen,df_look_male$augen, var.equal=FALSE, paired=FALSE)


# Plot preparations

sex_look <- c("Politikerinnen", "Politiker","Politikerinnen", "Politiker", "Politikerinnen", "Politiker","Politikerinnen", "Politiker")
attribut_look <- c("Kleidung", "Kleidung", "Haare", "Haare", "Körperstatur", "Körperstatur", "Augen", "Augen") 

# generate data points for graph 

scores_look <- c(
  
  mean(df_look_fem$kleidung)*1000, ##
  mean(df_look_male$kleidung)*1000, ##
  mean(df_look_fem$haar)*1000,
  mean(df_look_male$haar)*1000,
  mean(df_look_fem$körperstatur)*1000,
  mean(df_look_male$körperstatur)*1000,
  mean(df_look_fem$augen)*1000,
  mean(df_look_male$augen)*1000)

# Calculate error to produce graph error bars 

# Error terms
error_fem_kleidung <- qt(0.975,df=length(df_look_fem$kleidung)-1)*sd(df_look_fem$kleidung)/sqrt(length(df_look_fem$kleidung))*1000
error_fem_haar <- qt(0.975,df=length(df_look_fem$haar)-1)*sd(df_look_fem$haar)/sqrt(length(df_look_fem$haar)) *1000
error_fem_körperstatur <- qt(0.975,df=length(df_look_fem$körperstatur)-1)*sd(df_look_fem$körperstatur)/sqrt(length(df_look_fem$körperstatur)) *1000
error_fem_augen <- qt(0.975,df=length(df_look_fem$augen)-1)*sd(df_look_fem$augen)/sqrt(length(df_look_fem$augen)) *1000

error_male_kleidung <- qt(0.975,df=length(df_look_male$kleidung)-1)*sd(df_look_male$kleidung)/sqrt(length(df_look_male$kleidung))*1000
error_male_haar <- qt(0.975,df=length(df_look_male$haar)-1)*sd(df_look_male$haar)/sqrt(length(df_look_male$haar)) *1000
error_male_körperstatur <- qt(0.975,df=length(df_look_male$körperstatur)-1)*sd(df_look_male$körperstatur)/sqrt(length(df_look_male$körperstatur)) *1000
error_male_augen <- qt(0.975,df=length(df_look_male$augen)-1)*sd(df_look_male$augen)/sqrt(length(df_look_male$augen)) *1000

left_bound_look <- c(
  # Look Left Errors
  left_error_fem_kleidung <- mean(df_look_fem$kleidung)*1000 - error_fem_kleidung,
  left_error_male_kleidung <- mean(df_look_male$kleidung)*1000 - error_male_kleidung,
  left_error_fem_haar <- mean(df_look_fem$haar)*1000- error_fem_haar,
  left_error_male_haar <- mean(df_look_male$haar)*1000- error_male_haar,
  left_error_fem_körperstatur <- mean(df_look_fem$körperstatur)*1000- error_fem_körperstatur,
  left_error_male_körperstatur <- mean(df_look_male$körperstatur)*1000- error_male_körperstatur,
  left_error_fem_augen <- mean(df_look_fem$augen)*1000- error_fem_augen,
  left_error_male_augen <- mean(df_look_male$augen)*1000- error_male_augen)


right_bound_look <- c(
  #  Look Right Errors
  right_error_fem_kleidung <- mean(df_look_fem$kleidung)*1000 + error_fem_kleidung,
  right_error_male_kleidung <- mean(df_look_male$kleidung)*1000 + error_male_kleidung,
  right_error_fem_haar <- mean(df_look_fem$haar)*1000 + error_fem_haar,
  right_error_male_haar <- mean(df_look_male$haar)*1000 + error_male_haar,
  right_error_fem_körperstatur <- mean(df_look_fem$körperstatur)*1000 + error_fem_körperstatur,
  right_error_male_körperstatur <- mean(df_look_male$körperstatur)*1000 + error_male_körperstatur,
  right_error_fem_augen <- mean(df_look_fem$augen)*1000 + error_fem_augen,
  right_error_male_augen <- mean(df_look_male$augen)*1000 + error_male_augen)


#### Look Analysis for Tamedia Subset #####

## subset dataframes to tamedia only


fem_tamedia_looks <-Kwic_lookup_look_fem %>%
  tokens_subset(Partner == "Tamedia AG")%>%
  dfm()%>%
  convert(to = "data.frame")

fem_tamedia_Sum_looks <-Kwic_lookup_look_femSum %>%
  tokens_subset(Partner == "Tamedia AG")%>%
  dfm()%>%
  convert(to = "data.frame")

male_tamedia_looks <-Kwic_lookup_look_male %>%
  tokens_subset(Partner == "Tamedia AG")%>%
  dfm()%>%
  convert(to = "data.frame")

male_tamedia_Sum_looks <-Kwic_lookup_look_maleSum %>%
  tokens_subset(Partner == "Tamedia AG")%>%
  dfm()%>%
  convert(to = "data.frame")

# T-TESTS
t.test(fem_tamedia_looks$kleidung,male_tamedia_looks$kleidung,var.equal= TRUE, paired=FALSE)
t.test(fem_tamedia_looks$haar,male_tamedia_looks$haar, var.equal=FALSE, paired=FALSE) 
t.test(fem_tamedia_looks$körperstatur,male_tamedia_looks$körperstatur, var.equal=FALSE, paired=FALSE) 
t.test(fem_tamedia_looks$augen,male_tamedia_looks$augen, var.equal=FALSE, paired=FALSE) 


# Plot reparations
sex_look_TA <- c("Politikerin", "Politiker","Politikerin", "Politiker", "Politikerin", "Politiker","Politikerin", "Politiker")
attribut_look_TA <- c("Kleidung", "Kleidung", "Haare", "Haare", "Körperstatur", "Körperstatur", "Augen", "Augen") 


# generate data points for graph 

scores_look_TA <- c(
  
  mean(fem_tamedia_looks$kleidung)*1000, ##
  mean(male_tamedia_looks$kleidung)*1000, ##
  mean(fem_tamedia_looks$haar)*1000,
  mean(male_tamedia_looks$haar)*1000,
  mean(fem_tamedia_looks$körperstatur)*1000,
  mean(male_tamedia_looks$körperstatur)*1000,
  mean(fem_tamedia_looks$augen)*1000,
  mean(male_tamedia_looks$augen)*1000)


# Calculate error to produce graph error bars 

# Error terms
error_fem_kleidung_TA <- qt(0.975,df=length(fem_tamedia_looks$kleidung)-1)*sd(fem_tamedia_looks$kleidung)/sqrt(length(fem_tamedia_looks$kleidung))*1000
error_fem_haar_TA <- qt(0.975,df=length(fem_tamedia_looks$haar)-1)*sd(fem_tamedia_looks$haar)/sqrt(length(fem_tamedia_looks$haar)) *1000
error_fem_körperstatur_TA <- qt(0.975,df=length(fem_tamedia_looks$körperstatur)-1)*sd(fem_tamedia_looks$körperstatur)/sqrt(length(fem_tamedia_looks$körperstatur)) *1000
error_fem_augen_TA <- qt(0.975,df=length(fem_tamedia_looks$augen)-1)*sd(fem_tamedia_looks$augen)/sqrt(length(fem_tamedia_looks$augen)) *1000

error_male_kleidung_TA <- qt(0.975,df=length(male_tamedia_looks$kleidung)-1)*sd(male_tamedia_looks$kleidung)/sqrt(length(male_tamedia_looks$kleidung))*1000
error_male_haar_TA <- qt(0.975,df=length(male_tamedia_looks$haar)-1)*sd(male_tamedia_looks$haar)/sqrt(length(male_tamedia_looks$haar)) *1000
error_male_körperstatur_TA <- qt(0.975,df=length(male_tamedia_looks$körperstatur)-1)*sd(male_tamedia_looks$körperstatur)/sqrt(length(male_tamedia_looks$körperstatur)) *1000
error_male_augen_TA <- qt(0.975,df=length(male_tamedia_looks$augen)-1)*sd(male_tamedia_looks$augen)/sqrt(length(male_tamedia_looks$augen)) *1000

left_bound_look_TA <- c(
  # Left Errors
  left_error_fem_kleidung_TA <- mean(fem_tamedia_looks$kleidung)*1000 - error_fem_kleidung_TA,
  left_error_male_kleidung_TA <- mean(male_tamedia_looks$kleidung)*1000 - error_male_kleidung_TA,
  left_error_fem_haar_TA <- mean(fem_tamedia_looks$haar)*1000- error_fem_haar_TA,
  left_error_male_haar_TA <- mean(male_tamedia_looks$haar)*1000- error_male_haar_TA,
  left_error_fem_körperstatur_TA <- mean(fem_tamedia_looks$körperstatur)*1000- error_fem_körperstatur_TA,
  left_error_male_körperstatur_TA <- mean(male_tamedia_looks$körperstatur)*1000- error_male_körperstatur_TA,
  left_error_fem_augen_TA <- mean(fem_tamedia_looks$augen)*1000- error_fem_augen_TA,
  left_error_male_augen_TA <- mean(male_tamedia_looks$augen)*1000- error_male_augen_TA)


right_bound_look_TA <- c(
  # Right Errors
  right_error_fem_kleidung_TA <- mean(fem_tamedia_looks$kleidung)*1000 + error_fem_kleidung_TA,
  right_error_male_kleidung_TA <- mean(male_tamedia_looks$kleidung)*1000 + error_male_kleidung_TA,
  right_error_fem_haar_TA <- mean(fem_tamedia_looks$haar)*1000 + error_fem_haar_TA,
  right_error_male_haar_TA <- mean(male_tamedia_looks$haar)*1000 + error_male_haar_TA,
  right_error_fem_körperstatur_TA <- mean(fem_tamedia_looks$körperstatur)*1000 + error_fem_körperstatur_TA,
  right_error_male_körperstatur_TA <- mean(male_tamedia_looks$körperstatur)*1000 + error_male_körperstatur_TA,
  right_error_fem_augen_TA <- mean(fem_tamedia_looks$augen)*1000 + error_fem_augen_TA,
  right_error_male_augen_TA <- mean(male_tamedia_looks$augen)*1000 + error_male_augen_TA)



#### Plot Creation and Output, all analyses ####

# Attributes all publishers 

df_plotting <- data.frame(sex, attribut, scores, left_bound, right_bound)

g1 <- ggplot(df_plotting, aes(x = attribut, colour = sex))+ scale_color_manual(values=c("#48839c", "#f81233")) + theme_light()+
  geom_point(aes(x = attribut, y = scores, colour = sex))+ geom_errorbar(aes(ymin=left_bound, ymax=right_bound, x = attribut), width=0.4)+
  xlab("") + ylab("Keywords pro 1000 Namensnennungen")+ labs(color="")+ theme(text = element_text(size=15),axis.text.x = element_text(angle=45, hjust=1))

g1


# Attributes Tamedia specific
df_plotting_TA <- data.frame(sex_TA, attribut_TA, scores_TA, left_bound_TA, right_bound_TA)

g1_TA <- ggplot(df_plotting_TA, aes(x = attribut_TA, colour = sex_TA))+ scale_color_manual(values=c("#48839c", "#f81233")) + theme_light()+
  geom_point(aes(x = attribut_TA, y = scores_TA, colour = sex_TA))+ geom_errorbar(aes(ymin=left_bound_TA, ymax=right_bound_TA, x = attribut_TA),width=0.4) + xlab("") + ylab("Keywords pro 1000 Namensnennungen")+ labs(color="")+ theme(text = element_text(size=15),
                                                                                                                                                                                                                                         axis.text.x = element_text(angle=45, hjust=1))
g1_TA


# Looks all publishers
df_plotting_look <- data.frame(sex_look, attribut_look, scores_look, left_bound_look, right_bound_look)

g1_look <- ggplot(df_plotting_look, aes(x = attribut_look, colour = sex_look))+ scale_color_manual(values=c("#48839c", "#f81233")) + theme_light()+
  geom_point(aes(x = attribut_look, y = scores_look, colour = sex_look))+ geom_errorbar(aes(ymin=left_bound_look, ymax=right_bound_look, x = attribut_look), width=0.4) + xlab("") + ylab("Keywords pro 1000 Namensnennungen")+ labs(color="")+theme(text = element_text(size=15),
                                                                                                                                                                                                                                                     axis.text.x = element_text(angle=0, hjust=1))

g1_look


# Looks Tamedia specific
df_plotting_look_TA <- data.frame(sex_look_TA, attribut_look_TA, scores_look_TA, left_bound_look_TA, right_bound_look_TA)

g1_look_TA <- ggplot(df_plotting_look_TA, aes(x = attribut_look_TA, colour = sex_look_TA))+ scale_color_manual(values=c("#48839c", "#f81233")) + theme_light()+
  geom_point(aes(x = attribut_look_TA, y = scores_look_TA, colour = sex_look_TA))+ geom_errorbar(aes(ymin=left_bound_look_TA, ymax=right_bound_look_TA, x = attribut_look_TA), width=0.2)+ xlab("") + ylab("Keywords pro 1000 Namensnennungen")+ labs(color="")+ggtitle("Anzahl erwähnte Äusserlichkeiten pro 1000 Namensnennungen (Tamedia)")+theme(text = element_text(size=17))

g1_look_TA

