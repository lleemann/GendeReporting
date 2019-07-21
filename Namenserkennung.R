################################################
#### Capstone Course Media Representation   ####
#### Compounding the Names to single tokens ####
################################################

#### Preparations ####

# Cleaning
rm(list=ls())

# define stringsAsFactors = FALSE as default
options(stringsAsFactors = FALSE)

# Loading Packages
library(tidyverse)
library(readtext)
library(readxl)
library(doParallel)
library(foreach)
library(tictoc)

# Loading Data

# Cantonal Data
candidatesKW <- read.csv2("~/capstone_share/Data_Names/Data_BfS/KW_KANDIDATENSTIMMEN_KT.csv", fileEncoding = "ISO-8859-1")

# National Data
candidatesNR <- read.csv2("~/capstone_share/Data_Names/Data_BfS/NRW_KANDIDATENSTIMMEN_KT.csv", fileEncoding = "ISO-8859-1")

# MPs
membersParliament <- read_excel("~/capstone_share/Data_Names/Parlamentsdienste/Ratsmitglieder_1848_DE.xlsx")
##### Getting Candidate Names ####

# constructing year for KW
candidatesKW$year <- candidatesKW$voting_id%>%
  str_extract(pattern="[:digit:]{4}")%>%
  as.numeric()

# constructing year for NR
candidatesNR$year <- candidatesNR$voting_id%>%
  str_extract(pattern="[:digit:]{4}")%>%
  as.numeric()

# constructing year for MPs
membersParliament$year_joined <- membersParliament$DateJoining%>%
  str_extract(pattern="[:digit:]{4}")%>%
  as.numeric()

membersParliament$year_left <- membersParliament$DateLeaving%>%
  str_extract(pattern="[:digit:]{4}")%>%
  as.numeric()

membersParliament$year_left[is.na(membersParliament$year_left)] <- 9999

# recode year of birth to something useful
membersParliament$geburtsjahr <- membersParliament$DateOfBirth%>%
  str_extract(pattern="[:digit:]{4}")%>%
  as.numeric()

candidatesKW$geburtsjahr <- candidatesKW$geburtsjahr%>%
  str_extract(pattern="[:digit:]{4}")%>%
  as.numeric()

candidatesNR$geburtsjahr <- candidatesNR$geburtsjahr%>%
  str_extract(pattern="[:digit:]{4}")%>%
  as.numeric()

# filter relevant candidates
# election year, and being elected
relevantcandidatesKW <- dplyr::filter(candidatesKW, year >= 2008 & gewaehlt == 1)
relevantcandidatesNR <- dplyr::filter(candidatesNR, year >= 2010 & gewaehlt == 1)
relevantMPs <- dplyr::filter(membersParliament, year_left >= 2000) # everyone that left since 2000

# select relevant variables: last Name, first Name, sex, party name, year of birth
namKW <- relevantcandidatesKW %>% select(NAME, vorname, geschlecht, parteiname, geburtsjahr)
namNR <- relevantcandidatesNR %>% select(NAME, vorname, geschlecht, parteiname, geburtsjahr)

namMP <- relevantMPs %>% select(LastName, FirstName, GenderAsString, PartyAbbreviation, geburtsjahr)

# recode Gender
namMP$GenderAsString <- recode(namMP$GenderAsString, "m" = 2, "f" = 1)

# make the column names uniform
names(namMP) <- names(namNR)

# combining national, cantonal and parliamentary data
nam<- namKW %>% rbind(namMP) %>% rbind(namNR)

#recoding party names
unique(nam$parteiname) # All Party names
nam$parteiname <- car::recode(nam$parteiname, "
                              c('SVP/UDC', 'SVP/UDC')= 'SVP';
                              c('SP/PS', 'SP') = 'SP';
                              c('CVP/PDC', 'CVP', 'CVPO') = 'CVP';
                              c('FDP/PLR (PRD)', 'FDP', 'FDP-Liberale') = 'FDP';
                              c('GPS/PES', 'GPS','GB') = 'GPS';
                              c('EVP/PEV', 'EVP') = 'EVP';
                              c('EDU/UDF', 'EDU') = 'EDU';
                              c('SD/DS', 'SD') = 'SD';
                              c('GLP/PVL', 'glp') = 'GLP';
                              c('BDP/PBD', 'BDP') = 'BDP';
                              c('CSPO', 'csp-ow', 'CSP', 'CSP/PCS') = 'CSP'
                              ")
unique(nam$parteiname)# All Party names -> further suggetions?
table(nam$parteiname)

#remove errors in encoding which are displayed as ? or similar
nam$vorname <- nam$vorname%>%
  str_replace_all("\\.", replacement = " ")%>%
  str_replace_all(pattern = "[:punct:]", replacement = ".")%>%
  str_squish()

nam$NAME <- nam$NAME%>%
  str_replace_all("\\.", replacement = " ")%>%
  str_replace_all(pattern = "[:punct:]", replacement = ".")%>%
  str_squish()

# create full name
nam$name <- paste(nam$vorname, nam$NAME)

# create dictionary entry
nam$dict <- paste(nam$vorname, nam$NAME, sep = "_")%>%
  str_replace_all("\\s|[:punct:]", "_")

# preparing the regex: taking only the first first name
vornamen <- nam$vorname%>%
  str_split(pattern = "\\s", simplify = T)

# splitting the last name up
nachnamen1 <-nam$NAME%>%
  str_split(pattern = "\\s|[:punct:]", simplify = T)

# get length of names
nachnamencount <- nachnamen1%>%
  apply(MARGIN = 2, FUN = str_length)

# delete all names which consist only of one letter or are titles e.g. "von"
nachnamen1[nachnamencount<2|nachnamen1=="de"|nachnamen1=="De"|nachnamen1=="von"|nachnamen1=="Von"|nachnamen1=="la"|nachnamen1=="La"|nachnamen1=="der"|nachnamen1=="Der"|nachnamen1=="Van"|nachnamen1=="van"|nachnamen1=="di"|nachnamen1=="da"|nachnamen1=="Di"|nachnamen1=="Da"] <- ""

# create regex, which lists every part of a name as part separated by |
nachnamen1 <- nachnamen1%>%
  apply(1, FUN = paste0, collapse = "|")%>%
  str_replace_all(pattern = "\\|{2,}", replacement = "|")%>%
  str_remove_all(pattern = "\\|{2,4}|(^\\|)|(\\|$)")

# create regex, which takes the whole last name, parts separated by .
nachnamen2 <-nam$NAME%>%
  str_split(pattern = "\\s|[:punct:]")%>%
  sapply(FUN = paste0, collapse = ".")%>%
  str_replace_all(pattern = "(\\|){2}", replacement = "|")%>%
  str_remove_all(pattern = "\\.{2,4}|(^\\|)|(\\|$)")

# paste the two regexes behind each other in the case that they differ
nachnamen <- ifelse(nachnamen1==nachnamen2, nachnamen1,
                    paste0(nachnamen1, "|", nachnamen2))

# create the final search regex
nam$Regex_Bruno <- paste0("(",
                          vornamen[ ,1],
                          ")",
                          ".{0,15}?",
                          "[^_\\-]",
                          "(",
                          nachnamen,
                          ")(s)?(?![[:alpha:]_\\-])"
)

# get unique versions of the names
namunique <- unique(nam)

# test the Regex
vec_testsring <- c("Bundesrat Ueli Maurer",
                   "Bundesrätin Karin Keller-Sutter",
                   "Die Konditorin Karin Sutter",
                   "Nationalrätin Susanne Leutenegger Oberholzer",
                   "Kantonalpolitikerin Susanne Oberholzer",
                   "Susanne_Oberholzer_Glatt Susanne Oberholzer Susanne_Leutenegger_Oberholzer Susanne Leutenegger Oberholzer",
                   "Die Moderatorin Susanne Wille")

df_test_regexes <- namunique %>%
  filter(dict%in%c("Ueli_Maurer", "Karin_Keller_Sutter", "Susanne_Oberholzer", "Susanne_Leutenegger_Oberholzer"))%>%
  arrange(dict)

vec_regexes <- df_test_regexes$dict
names(vec_regexes) <- df_test_regexes$Regex_Bruno
vec_regexes


str_replace_all(string =  vec_testsring, vec_regexes)

# Seems not to work quite fine

# remove duplicate entries, if they differ only in the year of birth and one of them is NA
namunique <- namunique %>% 
  group_by(NAME, vorname, geschlecht, parteiname, name, dict, Regex_Bruno) %>%
  filter(if(length(geburtsjahr)>1){
    !is.na(geburtsjahr)
  }else{
    T
  }
  )%>%
  ungroup()


# test whether we have names in our data.set which match two people
Nachnamen <- namunique$NAME%>%
  str_split(pattern = "\\s|[:punct:]", simplify = T)
Vornamen <- namunique$vorname%>%
  str_split(pattern = "\\s|[:punct:]", simplify = T)

# get length of names
nachnamencount <- Nachnamen%>%
  apply(MARGIN = 2, FUN = str_length)

# delete all names which consist only of one letter or are titles e.g. "von"
Nachnamen[nachnamencount<2|Nachnamen=="de"|Nachnamen=="De"|Nachnamen=="von"|Nachnamen=="Von"|Nachnamen=="la"|Nachnamen=="La"|Nachnamen=="der"|Nachnamen=="Der"|Nachnamen=="Van"|Nachnamen=="van"|Nachnamen=="di"|Nachnamen=="da"|Nachnamen=="Di"|Nachnamen=="Da"] <- ""

# set empty character fields to NA
Nachnamen[Nachnamen==""] <- NA
Vornamen[Vornamen==""] <- NA

# Paste all possible combinations to new columns in the data.frame
namunique$matches_1 <- paste(Vorname = Vornamen[ ,1], Nachnamen[, 1])

namunique$matches_2 <- str_c(Vornamen[ ,1], Nachnamen[, 2], sep = " ")

namunique$matches_3 <- str_c(Vornamen[ ,1], Nachnamen[, 3], sep = " ")

namunique$matches_4 <- str_c(Vornamen[ ,1], Nachnamen[, 4], sep = " ")

# create large vector with all matched names
AllUniqueNames <- c(namunique$matches_1, namunique$matches_2, namunique$matches_3, namunique$matches_4)%>%
  na.omit()

# create frequency table with all names, select only those which occurr more than once
TableAllUniqueNames <- as.matrix(table(AllUniqueNames))
multipleNames <- data.frame("Name" = rownames(TableAllUniqueNames)[which(TableAllUniqueNames>1)],
                            "Frequenz" = TableAllUniqueNames[which(TableAllUniqueNames>1)])

# filter the data.frame for multiple matches
test <- filter(namunique, matches_1%in%multipleNames$Name|matches_2%in%multipleNames$Name|matches_3%in%multipleNames$Name|matches_4%in%multipleNames$Name)

#write_csv(test, "~/capstone_share/mehrfachmatches_neu.csv")

# dealing with critical matches:
# 1. identify which still match more than once
# 2. Add party regex to all matching more than once
# 3. recombine with entries which became singe matches
# 4. anti-join all multiple matches from original dict
# 5. rbind the new entries below

# 1. identify which still match more than once
df_mehrfach_nennungen_bearb <- read_csv2("capstone_share/mehrfach_nennungen_bearb.csv",
                                         trim_ws = TRUE)

# create large vector with all matched names
vec_mehrfach <- c(df_mehrfach_nennungen_bearb$matches_1,
                  df_mehrfach_nennungen_bearb$matches_2,
                  df_mehrfach_nennungen_bearb$matches_3,
                  df_mehrfach_nennungen_bearb$matches_4)%>%
  na.omit()

# create frequency table with all names, select only those which occurr more than once
mat_mehrfach <- as.matrix(table(vec_mehrfach))
df_mehrfach <- data.frame("Name" = rownames(mat_mehrfach)[which(mat_mehrfach>1)],
                          "Frequenz" = mat_mehrfach[which(mat_mehrfach>1)])

# filter the data.frame for multiple matches
df_immernoch_mehrfach <- filter(df_mehrfach_nennungen_bearb,
                                matches_1%in%df_mehrfach$Name|matches_2%in%df_mehrfach$Name|matches_3%in%df_mehrfach$Name|matches_4%in%df_mehrfach$Name)

# get only the resolved cases by anti-joining
df_mehrfach_nennungen_bearb <- anti_join(df_mehrfach_nennungen_bearb, df_immernoch_mehrfach)

# export the data.frame
# write_csv(df_immernoch_mehrfach, "capstone_share/immernoch_mehrfach.csv")

# checked by hand for conflicts. re-import
df_immernoch_mehrfach_bearbeitet <- read_csv("capstone_share/Data_Names/Mehrfachnennungen/immernoch_mehrfach_bearbeitet_6.csv")%>%
  mutate(Loeschen = replace_na(Loeschen, 0),
         Keine_Partyregex = replace_na(Keine_Partyregex, 0))%>% # replace na with 0 for relevant variables
  filter(Loeschen != 1) # delete the ones which can be deleted (e.g. people that were included with more than one party even though their party change occured before our time period)

# add other names which were contained in the original dictionary more than once
df_mehrfach_nennungen_bearb <- bind_rows(df_immernoch_mehrfach_bearbeitet, df_mehrfach_nennungen_bearb)%>%
  mutate(Keine_Partyregex = replace_na(Keine_Partyregex, 1),
         Voller_Nachname = replace_na(Voller_Nachname, 0),
         Voller_Vorname = replace_na(Voller_Vorname, 0),
         Nicht_Zeitraum = replace_na(Nicht_Zeitraum, 0))


# make new regex
# preparing the regex: taking only the first first name
vornamen <- df_mehrfach_nennungen_bearb$vorname%>%
  str_replace_all(pattern = "/", replacement = "|")%>%
  str_split(pattern = "\\s", simplify = T)

# splitting the last name up
nachnamen1 <-df_mehrfach_nennungen_bearb$NAME%>%
  str_split(pattern = "\\s|[:punct:]", simplify = T)

# get length of names
nachnamencount <- nachnamen1%>%
  apply(MARGIN = 2, FUN = str_length)

# delete all names which consist only of one letter or are titles e.g. "von"
nachnamen1[nachnamencount<2|nachnamen1=="de"|nachnamen1=="De"|nachnamen1=="von"|nachnamen1=="Von"|nachnamen1=="la"|nachnamen1=="La"|nachnamen1=="der"|nachnamen1=="Der"|nachnamen1=="Van"|nachnamen1=="van"|nachnamen1=="di"|nachnamen1=="da"|nachnamen1=="Di"|nachnamen1=="Da"] <- ""

# create regex, which lists every part of a name as part separated by |
nachnamen1 <- nachnamen1%>%
  apply(1, FUN = paste0, collapse = "|")%>%
  str_replace_all(pattern = "\\|{2,}", replacement = "|")%>%
  str_remove_all(pattern = "\\|{2,4}|(^\\|)|(\\|$)")

# create regex, which takes the whole last name, parts separated by .
nachnamen2 <-df_mehrfach_nennungen_bearb$NAME%>%
  str_split(pattern = "\\s|[:punct:]")%>%
  sapply(FUN = paste0, collapse = ".")%>%
  str_replace_all(pattern = "(\\|){2}", replacement = "|")%>%
  str_remove_all(pattern = "\\.{2,4}|(^\\|)|(\\|$)")

# paste the two regexes behind each other in the case that they differ
nachnamen <- ifelse(df_mehrfach_nennungen_bearb$Voller_Nachname==1,
                    nachnamen2,
                    ifelse(nachnamen1==nachnamen2,
                           nachnamen1,
                           paste0(nachnamen1, "|", nachnamen2)
                    )
)

# handling first names
vec_vornamen_ganz <-df_mehrfach_nennungen_bearb$vorname%>%
  str_split(pattern = "\\s|[:punct:]")%>%
  sapply(FUN = paste0, collapse = ".")%>%
  str_remove_all(pattern = "\\.{2,4}|(^\\|)|(\\|$)")

vec_vornamen <- ifelse(df_mehrfach_nennungen_bearb$Voller_Vorname==1,
                       vec_vornamen_ganz,
                       vornamen[ ,1]
)

# create the final search regex
df_mehrfach_nennungen_bearb$Regex_Bruno <- paste0("(",
                                                  vec_vornamen,
                                                  ")",
                                                  ".{0,15}?",
                                                  "[^_\\-]",
                                                  "(",
                                                  nachnamen,
                                                  ")(s)?(?![[:alpha:]_\\-])"
)

# add party information
# split data.frame
df_nicht_mehr_mehrfach <- df_mehrfach_nennungen_bearb%>%
  filter(Keine_Partyregex==1) # sort those out, that do not need an addition of a party regex

df_mehrfach_nennungen_bearb <- df_mehrfach_nennungen_bearb%>%
  filter(Keine_Partyregex==0) # those that need an addition of a party regex

# just an example to estimate length for lookaround
c("-Aussenpolitikerin", "-Nationalrätin",
  "-Ständerätin", "-Umweltpolitikerin",
  "-Mandatsträgerin", "-Shooting Star",
  "-Fraktionspräsidentin", "-gesundheitspolitikerin")%>%
  str_length()%>%max()+1

# 2. Add party regex to all matching more than once
# get party names
parteien <- df_mehrfach_nennungen_bearb$parteiname

# look at parties
parteien %>% table()

#deal with names which are often not only used as acronyms
parteien[parteien == "GPS"] <- "Grüne|GPS"
parteien[parteien == "GLP"] <- "Grünliberale|GLP"
parteien <- parteien%>%
  str_replace_all("/", "\\|")%>%
  str_replace_all("\\!", "\\.")

# make regex for parties
partyregex <- paste("(([:punct:]|\\s)(",
                    parteien,
                    "|",
                    str_to_lower(parteien),
                    "|",
                    str_to_upper(parteien),
                    ")([:punct:]|\\s))")%>%
  str_remove_all("\\s")


# create new regex to get the ambigous entries sorted out
Regex_Bruno_v2 <- paste0("(?<=(",partyregex,".{0,25}))",
                         df_mehrfach_nennungen_bearb$Regex_Bruno,
                         "|",
                         str_extract(df_mehrfach_nennungen_bearb$Regex_Bruno, ".*(?=\\(\\?\\!)"),
                         "(?=(.{0,25}",
                         partyregex,
                         "))(?![:alpha:])")

# create new name keys
Names_Dict_2 <- paste(df_mehrfach_nennungen_bearb$dict, df_mehrfach_nennungen_bearb$parteiname, sep = "_")

# set values in data.frame
df_mehrfach_nennungen_bearb$dict <- Names_Dict_2
df_mehrfach_nennungen_bearb$Regex_Bruno <- Regex_Bruno_v2

# 3. recombine with entries which became singe matches
df_mehrfach_handled<- rbind(df_nicht_mehr_mehrfach, df_mehrfach_nennungen_bearb)

# 4. anti-join all multiple matches from original dict
df_einfach <- anti_join(namunique, test)

# 5. rbind the new entries below
df_all_names <- bind_rows(df_einfach,
                          df_mehrfach_handled)

#ensure, that each entry only exists once
df_all_names <- df_all_names %>%
  unique()

# add signifier to dictionary
df_all_names <- df_all_names%>%
  mutate(dict = str_c("Personenmarker", dict, sep = "_"))

# change the name of niklaus-samuel gugger to find also nik gugger
df_all_names$Regex_Bruno[df_all_names$dict == "Personenmarker_Niklaus_Samuel_Gugger"] <- str_replace(string = df_all_names$Regex_Bruno[df_all_names$dict == "Personenmarker_Niklaus_Samuel_Gugger"],
                                                                                                     pattern = "\\(Niklaus.Samuel\\)",
                                                                                                     replacement = "(Niklaus.Samuel|Nik)")

# test the regex again
df_test_regexes <- df_all_names %>%
  filter(dict%in%c("Personenmarker_Ueli_Maurer",
                   "Personenmarker_Karin_Keller_Sutter",
                   "Personenmarker_Susanne_Oberholzer",
                   "Personenmarker_Susanne_Leutenegger_Oberholzer",
                   "Personenmarker_Niklaus_Samuel_Gugger"))%>%
  arrange(desc(dict))

vec_regexes <- df_test_regexes$dict
names(vec_regexes) <- df_test_regexes$Regex_Bruno
vec_regexes

vec_testsring <- c(vec_testsring, "Der EVP-Nationalrat und Restaurantbesitzer Niklaus-Samuel Gugger wird in den Zeitungen meist Nik Gugger genannt")

str_replace_all(string =  vec_testsring, vec_regexes)

df_test_regexes <- df_all_names %>%
  filter(dict%in%c("Personenmarker_Ueli_Maurer",
                   "Personenmarker_Karin_Keller_Sutter",
                   "Personenmarker_Susanne_Oberholzer",
                   "Personenmarker_Susanne_Leutenegger_Oberholzer",
                   "Personenmarker_Niklaus_Samuel_Gugger"))%>%
  arrange(dict)

vec_regexes <- df_test_regexes$dict
names(vec_regexes) <- df_test_regexes$Regex_Bruno
vec_regexes


str_replace_all(string =  vec_testsring, vec_regexes)

# the order in which the names are supplied seems to make the difference
# hence, it is necessary to sort by full lastnames and full firstnames

df_all_names <- df_all_names%>%
  mutate(Voller_Vorname = replace_na(Voller_Vorname, 0), Voller_Nachname = replace_na(Voller_Nachname, 0))%>%
  arrange(desc(Voller_Nachname), desc(Voller_Vorname))

# another option would be to restrict the number of free characters to the maximum of length complete first name - length first name used

saveRDS(df_all_names, "~/capstone_share/Names_Full_and_Lastnames/names_dict.rds")

##### correcting the dictionary parties ####

#### Preparations ####

# Cleaning
rm(list=ls())

# define stringsAsFactors = FALSE as default
options(stringsAsFactors = FALSE)

# Loading Packages
library(tidyverse)

#### Loading Data ####
# names Dictionary
df_names_dict <- readRDS("~/capstone_share/Names_Full_and_Lastnames/names_dict.rds")

#### Get misleading party codes

# create vector with unidentifiable parties
uncparty <- c("Übrige/Autres", "43", "41", "42", "44", "42", "-")

# remove whitespaces from partynames
df_names_dict$parteiname <- str_squish(df_names_dict$parteiname)

# get all dubious parties
findparty <- df_names_dict%>%
  subset(parteiname %in% uncparty)

#### get correct party names (with some help from mighty google)
df_names_dict$parteiname[df_names_dict$name == "Florian Alter"] <- "AdG"
df_names_dict$parteiname[df_names_dict$name == "Olivier Battaglia"] <- "LDP"
df_names_dict$parteiname[df_names_dict$name == "Barbara Dahinden.Zahner"] <- "CSP"
df_names_dict$parteiname[df_names_dict$name == "Jean.Henri Dumont"] <- "SP"
df_names_dict$parteiname[df_names_dict$name == "Anna Eugster"] <- "CVP"
df_names_dict$parteiname[df_names_dict$name == "Regula Gerig.Bucher"] <- "CSP"
df_names_dict$parteiname[df_names_dict$name == "Robert Gisler"] <- "FDP"
df_names_dict$parteiname[df_names_dict$name == "Stefan Gisler Schäfer"] <- "Al"
df_names_dict$parteiname[df_names_dict$name == "Madeline Heiniger"] <- "AdG"
df_names_dict$parteiname[df_names_dict$name == "Helmut Hersberger"] <- "FDP"
df_names_dict$parteiname[df_names_dict$name == "Helen Keiser.Fürrer"] <- "CSP"
df_names_dict$parteiname[df_names_dict$name == "Axel Marion"] <- "CVP"
df_names_dict$parteiname[df_names_dict$name == "Toni Moser.Stadelmann"] <- "SP"
df_names_dict$parteiname[df_names_dict$name == "Christian Schäli"] <- "CSP"
df_names_dict$parteiname[df_names_dict$name == "Elisabeth Schnyder"] <- "SVP"
df_names_dict$parteiname[df_names_dict$name == "Vroni Straub.Müller"] <- "CSP"
df_names_dict$parteiname[df_names_dict$name == "Jaap van Dam"] <- "SP"
df_names_dict$parteiname[df_names_dict$name == "Joe Vogler"] <- "CSP"
df_names_dict$parteiname[df_names_dict$name == "Stéphanie Vuichard"] <- "GPS"
df_names_dict$parteiname[df_names_dict$name == "Silvia Zbinden"] <- "CSP"
df_names_dict$parteiname[df_names_dict$name == "Tabea Zimmermann Gibson"] <- "GPS"
df_names_dict$parteiname[df_names_dict$name == "Sylvie Bonvin.Sansonnens"] <- "GPS"
df_names_dict$parteiname[df_names_dict$name == "Nicolas Pasquier"] <- "GPS"
df_names_dict$parteiname[df_names_dict$name == "Romain Schaer"] <- "SVP"
df_names_dict$parteiname[df_names_dict$name == "Guido Etterlin"] <- "SP"
df_names_dict$parteiname[df_names_dict$name == "Robert Gisler"] <- "FDP"
df_names_dict$parteiname[df_names_dict$name == "Birgitta Michel Thenen"] <- "SP"
df_names_dict$parteiname[df_names_dict$name == "Jonathan Prelicz"] <- "SP"
df_names_dict$parteiname[df_names_dict$name == "Elsbeth Anderegg Marty"] <- "SP"
df_names_dict$parteiname[df_names_dict$name == "Bettina Eschmann"] <- "SP"
df_names_dict$parteiname[df_names_dict$name == "Erika Weber"] <- "SP"
df_names_dict$parteiname[df_names_dict$name == "Antoine Chaix"] <- "SP"
df_names_dict$parteiname[df_names_dict$name == "Prisca Bünter"] <- "SP"
df_names_dict$parteiname[df_names_dict$name == "Luka Markic´"] <- "SP"
df_names_dict$parteiname[df_names_dict$name == "Guy Tomaschett"] <- "SP"
df_names_dict$parteiname[df_names_dict$name == "Eros Nicola Mellini"] <- "SVP"
df_names_dict$parteiname[df_names_dict$name == "Orlando Del Don"] <- "SVP"
df_names_dict$parteiname[df_names_dict$name == "Paolo Pamini"] <- "SVP"
df_names_dict$parteiname[df_names_dict$name == "Annalise Russi"] <- "GPS"
df_names_dict$parteiname[df_names_dict$name == "Alf Arnold Rosenkranz"] <- "GPS"
df_names_dict$parteiname[df_names_dict$name == "Beatrice Bünter"] <- "SP"
df_names_dict$parteiname[df_names_dict$name == "Pia Tresch"] <- "SP"
df_names_dict$parteiname[df_names_dict$name == "Armin Braunwalder Epp"] <- "SP"
df_names_dict$parteiname[df_names_dict$name == "Christoph Schillig"] <- "GPS"
df_names_dict$parteiname[df_names_dict$name == "Alex Inderkum"] <- "SP"
df_names_dict$parteiname[df_names_dict$name == "Nina Marty"] <- "SP"
df_names_dict$parteiname[df_names_dict$name == "Adriano Prandi"] <- "SP"
df_names_dict$parteiname[df_names_dict$name == "Nora Sommer"] <- "SP"
df_names_dict$parteiname[df_names_dict$name == "Christiane Jaquet.Berger"] <- "PdA"
df_names_dict$parteiname[df_names_dict$name == "Gérald Cretegny"] <- "CVP"
df_names_dict$parteiname[df_names_dict$name == "Marcelle Monnet Terrettaz"] <- "SP"
df_names_dict$parteiname[df_names_dict$name == "Camille Carron"] <- "GPS"
df_names_dict$parteiname[df_names_dict$name == "Sonia Z.Graggen.Salamin"] <- "SP"
df_names_dict$parteiname[df_names_dict$name == "Francine Zufferey Molina"] <- "SP"
df_names_dict$parteiname[df_names_dict$name == "Anne.Christine Bagnoud.Essellier"] <- "SP"
df_names_dict$parteiname[df_names_dict$name == "Olivier Salamin"] <- "SP"
df_names_dict$parteiname[df_names_dict$name == "Werner Jordan"] <- "SP"
df_names_dict$parteiname[df_names_dict$name == "Blaise Carron"] <- "SP"
df_names_dict$parteiname[df_names_dict$name == "Margaux Dubuis"] <- "SP"
df_names_dict$parteiname[df_names_dict$name == "Jolanda Spiess.Hegglin"] <- "GPS"

saveRDS(df_names_dict, "~/capstone_share/Names_Full_and_Lastnames/names_dict_corrected.rds")

##### applying the compounder ####

# load Data

df_filtered <- readRDS("~/capstone_share/df_texts_complete_all.rds")
df_all_names <- readRDS("~/capstone_share/Names_Full_and_Lastnames/names_dict_corrected.rds")

# create "manual" dictionary
# as named character vector
New_try_Bruno <- str_c(" ", df_all_names$dict, " ") #add whitespace to make sure that the token will be recognized later on as single token 
names(New_try_Bruno) <- df_all_names$Regex_Bruno

#### Apply in parallel ####
cores <- detectCores()-4 # number of cores - cores not to use
articles <- nrow(df_filtered) # number of articles
result <- rep(NA, articles)

texts <- df_filtered$text

max <- length(texts)/cores
x <- seq_along(texts)
textsprepared <- split(texts, ceiling(x/max))

# define a core cluster
cl <- makeCluster(cores)

# register the core cluster for use with foreach
registerDoParallel(cl)

# exporting necessary functions
clusterExport(cl, c("%>%"))

tic("Bruno_Parallel")
result <- foreach(k = 1:length(textsprepared), .combine =  'c', .packages=c("stringr"))%dopar% {
           str_replace_all(string = textsprepared[[k]], New_try_Bruno)
         }
stopCluster(cl)
#
toc()

df_filtered$text <- result

saveRDS(df_filtered, "~/capstone_share/Names_Full_and_Lastnames/df_texts_fullnames.rds")

#### Transforming to quanteda corpus ####
library(quanteda)
quanteda_options(threads = 20)
quanteda_options("threads")

corp_filtered <- df_filtered %>% 
  corpus()

saveRDS(corp_filtered, "~/capstone_share/Names_Full_and_Lastnames/corp_texts_fullnames.rds")

toks_filtered <- corp_filtered%>%
  tokens()

saveRDS(toks_filtered, "~/capstone_share/Names_Full_and_Lastnames/toks_texts_fullnames.rds")

#### Last Names to Keys ####

#### Preparations ####

# Cleaning
rm(list=ls())

# define stringsAsFactors = FALSE as default
options(stringsAsFactors = FALSE)

# Loading Packages
library(tidyverse)
library(tictoc)
library(foreach)
library(doParallel)
library(quanteda)
quanteda_options(threads = 20)

#### Loading Data ####
# Names
NamesDict <- readRDS("~/capstone_share/Names_Full_and_Lastnames/names_dict_corrected.rds")

# Texts
df_textsCompounded <- readRDS("~/capstone_share/Names_Full_and_Lastnames/df_texts_fullnames.rds")

# tokens
toks_comp <- readRDS("~/capstone_share/Names_Full_and_Lastnames/toks_texts_fullnames.rds")

#### know which texts contain which names ####

# constructing a gendered dictionary
dict <- dictionary(list(maennlich = NamesDict$dict[NamesDict$geschlecht == 2], weiblich = NamesDict$dict[NamesDict$geschlecht == 1]), tolower = F)

# constructing a dfm with counts of male / female names
geschlecht_dfm  <- dfm(toks_comp, dictionary  = dict, verbose = TRUE, tolower = F)

# subsetting the documents based on the occurence of names (at least one name is required)
toks_comp_reduced <- toks_comp %>%
  tokens_subset(rowSums(geschlecht_dfm)>0)

# constructing a dfm of names and documents in which they occurr
name_dfm <- dfm(toks_comp_reduced, select = dict, verbose = TRUE, tolower = F)

# check whether there are Personenmarker which are not in the dictionary
test_dfm <- dfm(toks_comp_reduced, select = "Personenmarker_*", verbose = TRUE, tolower = F)

!names(test_dfm)%in%names(name_dfm)

# saveRDS(name_dfm, "~/capstone_share/Names_Full_and_Lastnames/dfmat_names_prov.rds")
# name_dfm <- readRDS("~/capstone_share/Names_Full_and_Lastnames/dfmat_names_prov.rds")

# converting this dfm to a tidy data.frame
df_tidy_names <- tidytext::tidy(name_dfm)

# taking the occurring names as a filter for the names
NamesUsed <- filter(NamesDict,
                    dict%in%unique(df_tidy_names$term))

#### preparing the search and replacement operation ####
# preparing the regex: splitting the last name up
nachnamen1 <-NamesUsed$NAME%>%
  str_split(pattern = "\\s|[:punct:]", simplify = T)

# get length of names
nachnamencount <- nachnamen1%>%
  apply(MARGIN = 2, FUN = str_length)

# delete all names which consist only of one letter or are titles e.g. "von"
nachnamen1[nachnamencount<2|nachnamen1=="de"|nachnamen1=="De"|nachnamen1=="von"|nachnamen1=="Von"|nachnamen1=="la"|nachnamen1=="La"|nachnamen1=="der"|nachnamen1=="Der"|nachnamen1=="Van"|nachnamen1=="van"|nachnamen1=="di"|nachnamen1=="da"|nachnamen1=="Di"|nachnamen1=="Da"] <- ""

# create regex, which lists every part of a name as part separated by |
nachnamen1 <- nachnamen1%>%
  apply(1, FUN = paste0, collapse = "|")%>%
  str_replace_all(pattern = "\\|{2,}", replacement = "|")%>%
  str_remove_all(pattern = "\\|{2,4}|(^\\|)|(\\|$)")

# create regex, which takes the whole last name, parts separated by .
nachnamen2 <-NamesUsed$NAME%>%
  str_split(pattern = "\\s|[:punct:]")%>%
  sapply(FUN = paste0, collapse = ".")%>%
  str_replace_all(pattern = "(\\|){2}", replacement = "|")%>%
  str_remove_all(pattern = "\\.{2,4}|(^\\|)|(\\|$)")

# paste the two regexes behind each other in the case that they differ
nachnamen <- ifelse(nachnamen1==nachnamen2, nachnamen1,
                    paste0(nachnamen1, "|", nachnamen2))

# create the final search regex
NamesUsed$Regex_Lastnames <- paste0("(?<=\\s)(",
                                    nachnamen,
                                    ")(s)?(?![[:alpha:]_\\-])"
)


# # replace 0 with NA
# df_dfmat_names[df_dfmat_names == 0] <- NA
# 
# # convert data from wide to long
# df_names_documents <-  gather(df_dfmat_names,
#                               key = "dict",
#                               value = "count",
#                               na.rm = T,
#                               -document)

df_tidy_names <- rename(df_tidy_names, "dict" = "term")

# merge data
df_Names_Documents <- merge(df_tidy_names,
                            NamesUsed,
                            by = "dict",
                            all.y =T)

# filter texts by doc.ids
df_texts_politicians <- filter(df_textsCompounded,doc_id%in%unique(df_tidy_names$document))

# set.seed(1234)
# df_texts_politicians_samp <- sample_n(df_texts_politicians, 10000) 
# df_texts_politicians_samp_s <- df_texts_politicians_samp
# df_texts_politicians_samp_p <- df_texts_politicians_samp
# 
# tic("sequential")
# #### replacing the names ####
# # replace last names by keys
# for(i in 1:nrow(df_texts_politicians_samp_s)){
#   doc_id_i <- df_texts_politicians_samp_s$doc_id[i] # get doc.id
#   
#   iterdata <- filter(df_Names_Documents, document == doc_id_i)%>% # filter for doc_id
#     select(dict, Regex_Lastnames) # select only relevant columns
#   
#   pattern_replacement <- iterdata$dict # create character vector
#   
#   names(pattern_replacement) <- iterdata$Regex_Lastnames # name the character vector
#   
#   df_texts_politicians_samp_s$text[i] <- df_texts_politicians_samp_s$text[i] %>%
#     str_replace_all(pattern_replacement)
#   
#   #cat(i, "of", nrow(df_texts_politicians_samp), "articles edited \n")
# }
# toc()

# do the same stuff in parallel
tic("parallel")
#### Apply in parallel ####
quanteda_options(threads = 2)

# prepare by creating a list of named character vectors
names_docs <- str_c(" ", df_Names_Documents$dict, " ")

names(names_docs) <- df_Names_Documents$Regex_Lastnames

ls_names_vectors <- split(names_docs, df_Names_Documents$document)

cores <- detectCores()-4 # number of cores - cores not to use
articles <- nrow(df_texts_politicians) # number of articles
result <- rep(NA, articles)

# define a core cluster
cl <- makeCluster(cores)

# register the core cluster for use with foreach
registerDoParallel(cl)

# exporting necessary functions
clusterExport(cl, c("%>%"))

result <- foreach(i = 1:nrow(df_texts_politicians), .combine =  'c', .packages=c("stringr"))%dopar% {
  df_texts_politicians$text[i] %>%
    str_replace_all(ls_names_vectors[[which(df_texts_politicians$doc_id[i]==names(ls_names_vectors))]])
}
stopCluster(cl)
#


df_texts_politicians$text <- result

toc()

#### saving the output ####
saveRDS(df_texts_politicians, "~/capstone_share/Names_Full_and_Lastnames/df_texts_full_and_lastnames.rds")
saveRDS(df_dfmat_names, "~/capstone_share/Names_Full_and_Lastnames/df_dfmat_fullnames.rds")
saveRDS(geschlecht_dfm, "~/capstone_share/Names_Full_and_Lastnames/dfmat_sex_fullnames.rds")

##### converting to corpus and tokenizing ####

# Cleaning
rm(list=ls())

# define stringsAsFactors = FALSE as default
options(stringsAsFactors = FALSE)

# Loading Packages
library(tidyverse)
library(tictoc)
library(quanteda)

#### Loading Data ####

df_texts_full_and_lastnames <- readRDS("~/capstone_share/Names_Full_and_Lastnames/df_texts_full_and_lastnames.rds")

#### creating corpus and tokenizing ####

# maximize performance
quanteda_options(threads = 18)
quanteda_options("threads")

# creating corpus
corp_final <- df_texts_full_and_lastnames %>%
  corpus()

# saving
saveRDS(corp_final, "~/capstone_share/Names_Full_and_Lastnames/corp_texts_full_and_lastnames.rds")

# tokenizing
toks_final <- corp_final%>%
  tokens()

# saving
saveRDS(toks_final, "~/capstone_share/Names_Full_and_Lastnames/toks_texts_full_and_lastnames.rds")

###### Creating information on which politician (from which party) is in which text #####
# Cleaning
rm(list=ls())

# define stringsAsFactors = FALSE as default
options(stringsAsFactors = FALSE)

# Loading Packages
library(tidyverse)
library(tictoc)
library(quanteda)
quanteda_options(threads = 12)

#### Loading Data ####
# Names
NamesDict <- readRDS("~/capstone_share/Names_Full_and_Lastnames/names_dict_corrected.rds")

# tokens
toks_full <- readRDS("~/capstone_share/Names_Full_and_Lastnames/toks_texts_full_and_lastnames.rds")

#### know which texts contain which names ####
# constructing a gendered dictionary
dict <- dictionary(list(maennlich = NamesDict$dict[NamesDict$geschlecht == 2], weiblich = NamesDict$dict[NamesDict$geschlecht == 1]), tolower = F)

# constructing a dfm with counts of male / female names
geschlecht_dfm  <- dfm(toks_full, dictionary  = dict, verbose = TRUE, tolower = F)

# constructing a dfm of names and documents in which they occurr
name_dfm <- dfm(toks_full, select = dict, verbose = TRUE, tolower = F)

# converting this dfm to a tidy data.frame
df_tidy_names <- tidytext::tidy(name_dfm)

# taking the occurring names as a filter for the names
NamesUsed <- filter(NamesDict,dict%in%unique(df_tidy_names$term))

# rename variables to fit
df_tidy_names <- rename(df_tidy_names, "dict" = "term")

# merge data
df_Names_Documents <- merge(df_tidy_names,
                            NamesUsed,
                            by = "dict",
                            all.y =T)

#### saving the output ####
saveRDS(df_Names_Documents, "~/capstone_share/Names_Full_and_Lastnames/df_names_parties_etc_per_doc.rds")
saveRDS(name_dfm, "~/capstone_share/Names_Full_and_Lastnames/dfmat_full_and_lastnames.rds")
saveRDS(geschlecht_dfm, "~/capstone_share/Names_Full_and_Lastnames/dfmat_sex_full_and_lastnames.rds")

#### Compunding names in legends ####
#### Preparations ####

# Cleaning
rm(list=ls())

# define stringsAsFactors = FALSE as default
options(stringsAsFactors = FALSE)

# Loading Packages
library(tidyverse)
library(readtext)
library(readxl)
library(doParallel)
library(foreach)
library(tictoc)

##### applying the compounder ####

# load Data
df_all_names <- readRDS("~/capstone_share/Names_Full_and_Lastnames/names_dict_corrected.rds")

df_full_and_lastnames <- readRDS("~/capstone_share/Names_Full_and_Lastnames/df_texts_full_and_lastnames.rds")

# create "manual" dictionary
# as named character vector
New_try_Bruno <- str_c(" ", df_all_names$dict, " ") #add whitespace to make sure that the token will be recognized later on as single token 
names(New_try_Bruno) <- df_all_names$Regex_Bruno

#### Apply in parallel ####
cores <- detectCores()-4 # number of cores - cores not to use
articles <- nrow(df_full_and_lastnames) # number of articles
result <- rep(NA, articles)

texts <- replace_na(df_full_and_lastnames$lg, " ")

max <- length(texts)/cores
x <- seq_along(texts)
textsprepared <- split(texts, ceiling(x/max))

# define a core cluster
cl <- makeCluster(cores)

# register the core cluster for use with foreach
registerDoParallel(cl)

# exporting necessary functions
clusterExport(cl, c("%>%"))

tic("Bruno_Parallel")
result <- foreach(k = 1:length(textsprepared), .combine =  'c', .packages=c("stringr"))%dopar% {
  str_replace_all(string = textsprepared[[k]], New_try_Bruno)
}
stopCluster(cl)
#
toc()

df_full_and_lastnames$lg <- result

saveRDS(df_full_and_lastnames, "~/capstone_share/Names_Full_and_Lastnames/df_legends_fullnames.rds")

#### Transforming to quanteda corpus ####
library(quanteda)
quanteda_options(threads = 20)
quanteda_options("threads")

corp_filtered <- df_full_and_lastnames %>% 
  corpus(text_field = "lg")

saveRDS(corp_filtered, "~/capstone_share/Names_Full_and_Lastnames/corp_legends_fullnames.rds")

toks_filtered <- corp_filtered%>%
  tokens()

saveRDS(toks_filtered, "~/capstone_share/Names_Full_and_Lastnames/toks_legends_fullnames.rds")

#### Adding Control Variables ####
#### Preparations ####

# Cleaning
rm(list=ls())

# define stringsAsFactors = FALSE as default
options(stringsAsFactors = FALSE)

# Loading Packages
library(tidyverse)
library(tictoc)
library(readxl)
library(quanteda)
#quanteda_options(threads = 6)

#### Loading Data ####
# Names Dictionary
df_names_dict <- readRDS("~/capstone_share/Names_Full_and_Lastnames/names_dict_corrected.rds")

# Controls
df_sonderrollen <- read_csv("capstone_share/Data_Names/Kontrols/Kontrols_ Sonderrollen.csv")

# Parliament Data
df_parldata <- read_excel("~/capstone_share/Data_Names/Parlamentsdienste/Ratsmitglieder_1848_DE.xlsx")

#df_names_parties_doc <- readRDS("~/capstone_share/Names_Full_and_Lastnames/df_names_parties_etc_per_doc.rds")

#### Making data more usefull ####
# constructing year for MPs
df_parldata$year_joined <- df_parldata$DateJoining%>%
  str_extract(pattern="[:digit:]{4}")%>%
  as.numeric()

df_parldata$year_left <- df_parldata$DateLeaving%>%
  str_extract(pattern="[:digit:]{4}")%>%
  as.numeric()

df_parldata$year_left[is.na(df_parldata$year_left)] <- 9999

# recode year of birth to something useful
df_parldata$geburtsjahr <- df_parldata$DateOfBirth%>%
  str_extract(pattern="[:digit:]{4}")%>%
  as.numeric()

# remove trailing whitespaces
df_parldata$FirstName <- str_squish(df_parldata$FirstName)
df_parldata$LastName <- str_squish(df_parldata$LastName)

# construct fullname as an ID
df_parldata$Fullname <- str_c(df_parldata$FirstName, df_parldata$LastName, sep = " ")

# get only first part of lastname
nachnamen1 <- str_split(df_parldata$LastName, pattern = "(\\s|[:punct:])", simplify = T)

# get length of names
nachnamencount <- nachnamen1%>%
  apply(MARGIN = 2, FUN = str_length)

# replace titles etc
nachnamen1[nachnamencount<2|nachnamen1=="de"|nachnamen1=="Auf"|nachnamen1=="Ab"|nachnamen1=="De"|nachnamen1=="von"|nachnamen1=="Von"|nachnamen1=="la"|nachnamen1=="La"|nachnamen1=="der"|nachnamen1=="Der"|nachnamen1=="Van"|nachnamen1=="van"|nachnamen1=="di"|nachnamen1=="da"|nachnamen1=="Di"|nachnamen1=="Da"] <- ""

# select only real parts of names
df_parldata$LastName1 <- ifelse(nachnamen1[,1]=="",
                                ifelse(nachnamen1[,2]=="",
                                       nachnamen1[,3],
                                       nachnamen1[,2]),
                                nachnamen1[,1])

df_parldata$Fullname1 <- str_c(df_parldata$FirstName, df_parldata$LastName1, sep = " ")

# get only first part of first name
vornamen1 <- str_split(df_parldata$FirstName, pattern = "(\\s)", simplify = T) # split only on whitespace, becaue hyphenated names are typically kept

# get length of names
vornamencount <- vornamen1%>%
  apply(MARGIN = 2, FUN = str_length)

# replace titles etc
vornamen1[vornamencount<3] <- ""

# select only real parts of first names
df_parldata$FirstName1 <- ifelse(vornamen1[,1]=="",
                                 ifelse(vornamen1[,2]=="",
                                        vornamen1[,3],
                                        vornamen1[,2]),
                                 vornamen1[,1])

df_parldata$Fullname1 <- str_c(df_parldata$FirstName1, df_parldata$LastName1, sep = " ")

# filter the parliamentary data 
df_parldata1 <- df_parldata%>%
  filter(year_left>=2012)%>% #only take relevant politicians
  group_by(FirstName1, LastName, geburtsjahr, CouncilName)%>% # group to find most usefull occurence
  filter(year_left == max(year_left)&year_joined == max(year_joined))%>% # find only latest entry
  select(-Active)%>%
  unique()%>%
  ungroup()

# add correct date joining:
# 1. overall date joining
df_parldata2 <- df_parldata%>%
  filter(Fullname1%in%df_parldata1$Fullname1)%>% #only take relevant politicians
  group_by(FirstName1, LastName1, geburtsjahr)%>% # group to find most usefull occurence
  filter(year_joined == min(year_joined))%>% # find only latest entry
  ungroup() %>%
  select(-Active)%>%
  select(LastName1, year_joined, Fullname1, CantonAbbreviation, PartyAbbreviation, geburtsjahr, FirstName1)%>%
  unique()

# 2. function-specific date joining
df_parldata3 <- df_parldata%>%
  filter(Fullname1%in%df_parldata1$Fullname1)%>% #only take relevant politicians
  group_by(FirstName1, LastName1, geburtsjahr, CouncilName)%>% # group to find most usefull occurence
  filter(year_joined == min(year_joined))%>% # find only latest entry
  ungroup() %>%
  select(-Active)%>%
  select(LastName1, year_joined, Fullname1, CantonAbbreviation, PartyAbbreviation, geburtsjahr, CouncilName, FirstName1)%>%
  unique()

# check whether there are entries with no match
no_match <- anti_join(df_parldata1, df_parldata2, by = c("FirstName1", "LastName1", "CantonAbbreviation", "Fullname1"))
no_match <- anti_join(df_parldata1, df_parldata3, by = c("FirstName1", "LastName1", "CantonAbbreviation", "Fullname1"))

# add overall joining date
df_parldata_final <- left_join(df_parldata1, df_parldata2,
                               by= c("FirstName1", "LastName1", "CantonAbbreviation", "Fullname1",  "CantonAbbreviation", "geburtsjahr")
                               , suffix = c("_left", "_joined"))

# add function-specific joining date
df_parldata_final <- left_join(df_parldata_final, df_parldata3,
                               by= c("FirstName1", "LastName1", "CantonAbbreviation", "Fullname1",  "CantonAbbreviation", "geburtsjahr", "CouncilName")
)

# Check for multiple occurences
df_parldata_final$Fullname1%>%
  table()%>%
  sort(decreasing = T)

# Repeat data.frame 7 times
df_parldata_final2 <- df_parldata_final%>%
  slice(rep(row_number(), 7))

# add year
df_parldata_final2$year <- c(rep(2012, nrow(df_parldata_final)),
                             rep(2013, nrow(df_parldata_final)),
                             rep(2014, nrow(df_parldata_final)),
                             rep(2015, nrow(df_parldata_final)),
                             rep(2016, nrow(df_parldata_final)),
                             rep(2017, nrow(df_parldata_final)),
                             rep(2018, nrow(df_parldata_final)))

# calculate difference between current year and joining date
df_parldata_final2$joindiff <- df_parldata_final2$year - df_parldata_final2$year_joined

# filter only for people in the council
df_parldata_final2 <- df_parldata_final2%>%
  filter(joindiff>=0)

# calculate difference between current year and leaving date
df_parldata_final2$leavediff <- df_parldata_final2$year - df_parldata_final2$year_left

# filter only for active politicians
df_parldata_final2 <- df_parldata_final2 %>%
  filter(leavediff<=0)

# ensure that there are no duplicates
df_sonderrollen <- df_sonderrollen%>%
  unique()

# rename data to match
df_sonderrollen <- df_sonderrollen%>%
  rename(NAME = Nachname, vorname = Vorname, year = Jahr)

df_parldata_final2 <- df_parldata_final2%>%
  rename(NAME = LastName, vorname = FirstName)

# select ony relevant entries
df_sonderrollen <- df_sonderrollen %>%
  filter(year>=2012&year<2019)

# check whether the names are common between the two data.frames
df_missing <- anti_join(df_sonderrollen, df_parldata_final2)

# recode Filippo Lomardi and Marianne Streiff-Feller
df_sonderrollen$vorname[df_sonderrollen$NAME =="Lombardi"] <- "Filippo"
df_sonderrollen$NAME[df_sonderrollen$NAME =="Streiff"&df_sonderrollen$vorname=="Marianne"] <- "Streiff-Feller"

# check again whether the names are common between the two data.frames
df_missing <- anti_join(df_sonderrollen, df_parldata_final2)

# seems ok now
df_merged <- left_join(df_parldata_final2, df_sonderrollen)

# test whether there are any missings when merging with dict data
df_missing <- anti_join(df_merged, df_names_dict)

# create a new regex
# preparing the regex: taking only the first first name
vornamen <- df_names_dict$vorname%>%
  str_split(pattern = "\\s", simplify = T)

# splitting the last name up
nachnamen1 <-df_names_dict$NAME%>%
  str_split(pattern = "\\s|[:punct:]", simplify = T)

# get length of names
nachnamencount <- nachnamen1%>%
  apply(MARGIN = 2, FUN = str_length)

# delete all names which consist only of one letter or are titles e.g. "von"
nachnamen1[nachnamencount<2|nachnamen1=="de"|nachnamen1=="Auf"|nachnamen1=="Ab"|nachnamen1=="De"|nachnamen1=="von"|nachnamen1=="Von"|nachnamen1=="la"|nachnamen1=="La"|nachnamen1=="der"|nachnamen1=="Der"|nachnamen1=="Van"|nachnamen1=="van"|nachnamen1=="di"|nachnamen1=="da"|nachnamen1=="Di"|nachnamen1=="Da"] <- ""

# create regex, which lists every part of a name as part separated by |
nachnamen1 <- nachnamen1%>%
  apply(1, FUN = paste0, collapse = "|")%>%
  str_replace_all(pattern = "\\|{2,}", replacement = "|")%>%
  str_remove_all(pattern = "\\|{2,4}|(^\\|)|(\\|$)")

# create regex, which takes the whole last name, parts separated by .
nachnamen2 <-df_names_dict$NAME%>%
  str_split(pattern = "\\s|[:punct:]")%>%
  sapply(FUN = paste0, collapse = ".")%>%
  str_replace_all(pattern = "(\\|){2}", replacement = "|")%>%
  str_remove_all(pattern = "\\.{2,4}|(^\\|)|(\\|$)")

# paste the two regexes behind each other in the case that they differ
nachnamen <- ifelse(nachnamen1==nachnamen2, nachnamen1,
                    paste0(nachnamen1, "|", nachnamen2))

# create the final search regex
df_names_dict$Regex_Roles <- paste0("(",
                                    vornamen[ ,1],
                                    ")",
                                    ".{1,15}",
                                    "(",
                                    nachnamen,
                                    ").",
                                    df_names_dict$geburtsjahr
)

# get a unique set of names, multiply by number of rows in dictionary
df_to_be_matched_roles <- df_merged%>%
  select(Fullname, geburtsjahr)%>%
  unique()%>%
  slice(rep(row_number(), nrow(df_names_dict)))%>%
  arrange(Fullname)

# extend the names with birth years
df_to_be_matched_roles$Fullname_birthyear <- str_c(df_to_be_matched_roles$Fullname,
                                                   df_to_be_matched_roles$geburtsjahr,
                                                   sep = " ")

# repeat the names dictionary multiple times
df_to_be_matched_dict <- df_names_dict%>%
  select(dict, Regex_Roles, geburtsjahr)%>%
  arrange(dict)%>%
  slice(rep(row_number(), nrow(df_to_be_matched_roles)/nrow(df_names_dict)))

# bind the columns together
df_fully_matched <- bind_cols(df_to_be_matched_dict, df_to_be_matched_roles)

# detect the matches
df_fully_matched <- df_fully_matched%>%
  filter(str_detect(string = df_fully_matched$Fullname_birthyear, pattern = df_fully_matched$Regex_Roles))

# check whether there are MPs which are no longer discovered
df_missing <- anti_join(df_merged, df_fully_matched)

# fathi derder and eveline widmer-schlumpf are no longer discovered. They are not in the main dictionary, hence the original dictionary needs to be inspected.

# merge id for merging
df_merged <- left_join(df_merged, df_fully_matched)

# merge finally
df_final <- left_join(df_merged,
                      df_names_dict,
                      by = c("dict", "Regex_Roles", "geburtsjahr"),
                      suffix = c("_Roles", "_Old_Dict"))

# remove ignazio cassis as BR in 2017, because he only entered into the BR in November
df_final <- df_final %>%
  filter(!(Fullname1=="Ignazio Cassis"&CouncilName=="Bundesrat"&year==2017))


# rename the variables to something usefull
df_validation <- df_final%>%
  rename(NAME = NAME_Old_Dict,
         vorname = vorname_Old_Dict,
         Regex_Bruno = Regex_Bruno,
         year = year,
         geburtsjahr = geburtsjahr,
         year_joined_parliament = year_joined_joined,
         year_joined_council = year_joined,
         diff_leaving_council = leavediff,
         diff_entering_council = joindiff,
         Funktion = Funktion,
         `Kommission_Fraktion` = `Kommission/Fraktion`)

df_final <- df_validation%>%
  select(name,
         NAME,
         vorname,
         dict,
         geburtsjahr,
         geburtstag = DateOfBirth,
         parteiname,
         Kanton = CantonName,
         Kanton_Abkuerzung = CantonAbbreviation,
         geschlecht,
         GenderAsString,
         Regex_Bruno,
         mehr_partei,
         CouncilName,
         Fraktion = ParlGroupName,
         Fraktion_Abkuerzung = ParlGroupAbbreviation,
         year_joined_parliament,
         year_joined_council,
         year_left,
         year,
         diff_leaving_council,
         diff_entering_council,
         Funktion,
         Kommission_Fraktion
  )

# save the output
saveRDS(df_validation, "~/capstone_share/Names_Full_and_Lastnames/df_validation_names_dict_national_roles.rds")
saveRDS(df_final, "~/capstone_share/Names_Full_and_Lastnames/df_names_dict_national_roles.rds")

#### Transforming the dictionary from Long to Wide #####
#### Preparations ####

# Cleaning
rm(list=ls())

# define stringsAsFactors = FALSE as default
options(stringsAsFactors = FALSE)

# Loading Packages
library(tidyverse)
library(tictoc)
library(quanteda)

# Loading the Data
# dictionary
df_names_dict_national_roles <- readRDS("~/capstone_share/Names_Full_and_Lastnames/df_names_dict_national_roles.rds")

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

# issues with party changes: check which politicians are affected, then recode manually
# rules used: use national party membership e.g. BastA in canton of Basel is a member of the GPS
# if a party change occured during the observation period, code accordingly
# if a party change occured before observation period, use latest party membership
df_names_dict_national_roles$dict[replace_na(df_names_dict_national_roles$mehr_partei, 0) ==1] %>%
  unique()

df_names_party_changes <- df_names_dict_national_roles %>%
  filter(mehr_partei==1)

# Thomas Müller: SVP, changed Party before our timeframe (2011), but there are other Thomas Müllers in our dataset, hence remove entry with CVP
df_names_dict_national_roles_spread <- df_names_dict_national_roles_spread%>%
  filter(dict!="Personenmarker_Thomas_Müller_CVP")

# remove Bundesräte, add information on speech region and location of large newspapers
df_names_dict_national_roles_spread <- df_names_dict_national_roles_spread%>%
  mutate(StandortZeitung = ifelse(Kanton_Abkuerzung%in%c("AG", "BS", "BE", "ZH", "SG", "LU", "SO"),
                                  1, 0),
         Deutschsprachig = ifelse(Kanton_Abkuerzung%in%c("VD", "NE", "GE", "JU", "TI"),
                                  0, 1))

# account for effect of BR-Wahlen
# Parmelin: Gewählt am 09.12.2015, Amtsantritt 2016, Kandidierende: Aeschi
# Berset: Gewählt am 14.12.2011, Amtsantritt 2012
# Cassis: Gewählt am 20.09.2017, Amtsantritt 01.11.2017; Kandidierende: Isabelle Moret
# Amherd: Gewählt am 05.12.18, Amtsantritt 2019
# KKS: Gewählt am 05.12.18, Amtsantritt 2019, Kandidierende: Hans Wicki

df_names_dict_national_roles_spread$BRW <- 0 
df_names_dict_national_roles_spread$BRW[df_names_dict_national_roles_spread$year == 2015 & df_names_dict_national_roles_spread$dict == "Personenmarker_Guy_Parmelin"] <- 1
df_names_dict_national_roles_spread$BRW[df_names_dict_national_roles_spread$year == 2015 & df_names_dict_national_roles_spread$dict == "Personenmarker_Thomas_Aeschi"] <- 1
df_names_dict_national_roles_spread$BRW[df_names_dict_national_roles_spread$year == 2017 & df_names_dict_national_roles_spread$dict == "Personenmarker_Ignazio_Cassis"] <- 1
df_names_dict_national_roles_spread$BRW[df_names_dict_national_roles_spread$year == 2017 & df_names_dict_national_roles_spread$dict == "Personenmarker_Isabelle_Moret"] <- 1
df_names_dict_national_roles_spread$BRW[df_names_dict_national_roles_spread$year == 2018 & df_names_dict_national_roles_spread$dict == "Personenmarker_Viola_Amherd"] <- 1
df_names_dict_national_roles_spread$BRW[df_names_dict_national_roles_spread$year == 2018 & df_names_dict_national_roles_spread$dict == "Personenmarker_Karin_Keller_Sutter"] <- 1
df_names_dict_national_roles_spread$BRW[df_names_dict_national_roles_spread$year == 2018 & df_names_dict_national_roles_spread$dict == "Personenmarker_Hans_Wicki"] <- 1

saveRDS(df_names_dict_national_roles_spread, "~/capstone_share/Names_Full_and_Lastnames/df_names_dict_national_roles_spread_corrected.rds")

##### Producing KWIK #####

##### ELIANE ####
rm(list = ls())
library(quanteda)
library(tictoc)

# load dictionary with names of politicians
NamesDict <- readRDS("~/capstone_share/Names_Full_and_Lastnames/names_dict_corrected.rds")


# load corpus
corp_final <- readRDS("~/capstone_share/Names_Full_and_Lastnames/corp_texts_full_and_lastnames.rds")


# dictionary with only females
NamesDict_fem <- subset(NamesDict, NamesDict$geschlecht == 1, select = 7)


# kwic for female politicians
quanteda_options(threads = 20)

tic()
kwic_fem <- kwic(corp_final, pattern = NamesDict_fem$dict, 30)
toc()


# save kwic
saveRDS(kwic_fem, file = "~/capstone_share/kwic_fem.rds")

###

# dictionary with only males
NamesDict_male <- subset(NamesDict, NamesDict$geschlecht == 2, select = 7)


# kwic for male politicians
quanteda_options(threads = 20)

tic()
kwic_male <- kwic(corp_final, pattern = NamesDict_male$dict, 30)
toc()


# save kwic
saveRDS(kwic_male, file = "~/capstone_share/kwic_male.rds")


##### CHRIS ####
rm(list = ls())
library(quanteda)

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

# KWIC NAMES
kwic_fem <- kwic(corp_final, pattern = NamesDict_fem$dict, 5)
#KWIC male
kwic_male <- kwic(corp_final, pattern = NamesDict_male$dict, 5)

saveRDS(kwic_male, file = "~/capstone_share/kwic_male_chris.rds")
saveRDS(kwic_fem, file = "~/capstone_share/kwic_fem_chris.rds")
