library(httr2)
library(purrr)
library(tidyverse)
library(jsonlite)
library(rcrossref)
library(citecorp)
library(roadoi)
library(rorcid)
library(forcats)
library(janitor)
library(viridisLite)
library(tidyjson)


# Query Customization -----------------------------------------------------

# ROR ID for Oklahoma State University + dataset resource type
ror_query <- request("https://api.datacite.org/dois?affiliation-id=https%3A%2F%2Fror.org%2F01g9vbr38&resource-type-id=dataset&affiliation=true&page[size]=1000")
# &publisher=true - provides full complement of metadata for publisher (if available)


# "Oklahoma State University" as creator affiliation + dataset resource type
textstring_query <- request("https://api.datacite.org/dois?query=creators.affiliation.name:%22Oklahoma%20State%20University%22*&resource-type-id=dataset&affiliation=true&page[size]=1000")
# &publisher=true - provides full complement of metadata for publisher (if available)

# send query to DataCite REST API
send_query <- textstring_query %>% 
  req_method("GET") %>%
  req_perform(.)


# Extract Desired Data ----------------------------------------------------


### Query Metadata ----------------------------------------------------------

# pull metadata from query
parse_meta <- resp_body_json(send_query) %>% 
  pluck("meta")

# write_json(parse_meta, "data/raw/dataciteOKstate_meta_20240822.json")


##### Extract Metadata Summaries ----------------------------------------------

dcJSONmeta <- tidyjson::read_json("data/raw/dataciteOKState_meta_20240822.json", format = "json") %>% 
  gather_object("metaID") %>% 
  json_types() %>% 
  select(-document.id)

# quick count of prevalence of repositories (using clients, providers)
clientMeta <- dcJSONmeta %>% 
  filter(metaID == "clients") %>% 
  gather_array("clientIndex") %>% 
  gather_object() %>% 
  filter(name %in% c("title","count")) %>% 
  gather_array()

providerMeta <- dcJSONmeta %>% 
  filter(metaID == "providers") %>% 
  gather_array("providerIndex") %>% 
  gather_object() %>% 
  filter(name %in% c("title","count")) %>% 
  gather_array()

# quick count of affiliations
affilationMeta <- dcJSONmeta %>% 
  filter(metaID == "affiliations") %>% 
  gather_array("affilIndex") %>% 
  gather_object() %>% 
  filter(name %in% c("title","count")) %>% 
  gather_array()


### Query Data --------------------------------------------------------------

# pluck "data" portion from query
parse_query <- resp_body_json(send_query) %>%
  pluck("data") 

# write_json(parse_query,"data/raw/dataciteOKState_20240822.json")
# dcJSON <- tidyjson::read_json("data/raw/dataciteOKState.json", format = "json") %>% 
dcJSON <- tidyjson::read_json("data/raw/dataciteOKState_20240822.json", format = "json") %>%
  gather_array("indexID") %>% 
  select(-document.id) 

dcDatasets <- dcJSON %>% 
  enter_object("attributes") %>% 
  gather_object()

##### Extract Dataset DOIs -------------------------------------------------------------

dcDOI <- dcDatasets %>% 
    filter(name == "doi") %>% # keep only rows with DOI data
    enter_object() %>% 
    gather_array() %>% 
    mutate(doi = as.character(..JSON)) %>% # create "doi" column
    select(indexID,doi) # remove unnecessary columns


##### Extract Author Metadata -------------------------------------------------

# extract metadata provided on creators
dcAuthorMetadata <- dcDatasets %>%
  dplyr::filter(name == "creators") %>% 
  gather_array("creatorIndex") %>%
  gather_object("nameMetadata") 

####### Author Information

# extract authors (1 per row)
dcAuthorNames <- dcAuthorMetadata %>% 
    filter(nameMetadata == "name") %>% 
    enter_object() %>% 
    gather_array("nameIndex") %>% 
    mutate(authorName = as.character(..JSON)) %>% 
    select(indexID,creatorIndex,authorName)

# extract author family name (1 per row)
dcAuthorFamily <- dcAuthorMetadata %>% 
  filter(nameMetadata == "familyName") %>% 
  gather_array("nameIndex") %>% 
  mutate(familyName = as.character(..JSON)) %>% 
  select(indexID,creatorIndex,familyName)

# extract author given name (1 per row)
dcAuthorGiven <- dcAuthorMetadata %>% 
  filter(nameMetadata == "givenName") %>% 
  gather_array("nameIndex") %>% 
  mutate(givenName = as.character(..JSON)) %>% 
  select(indexID,creatorIndex,givenName)

# combine authors into single list per dataset
dcAuthorList <- dcAuthorNames %>% 
  group_by(indexID) %>% 
  summarize(authorList = paste(authorName, collapse = "|"))


####### Affiliation Information

# extract author affiliation names
dcAuthorAffil <- dcAuthorMetadata %>% 
    filter(nameMetadata == "affiliation") %>% 
    gather_array("affiliationIndex") %>% 
    gather_object("affilMetadata") %>% 
    filter(affilMetadata == "name") %>% 
    gather_array() %>% 
    mutate(authorAffil = as.character(..JSON)) %>% 
    select(indexID,creatorIndex,affiliationIndex,authorAffil)

# extract affiliation IDs
dcAffilID <- dcAuthorMetadata %>% 
  filter(nameMetadata == "affiliation") %>% 
  gather_array("affiliationIndex") %>% 
  gather_object("affilMetadata") %>% 
  filter(affilMetadata == "affiliationIdentifier") %>% 
  gather_array() %>% 
  mutate(affilID = as.character(..JSON)) %>% 
  select(indexID,creatorIndex,affiliationIndex,affilID)

# extract affiliation schema type
dcAffilIDtype <- dcAuthorMetadata %>% 
  filter(nameMetadata == "affiliation") %>% 
  gather_array("affiliationIndex") %>% 
  gather_object("affilMetadata") %>% 
  filter(affilMetadata == "affiliationIdentifierScheme") %>% 
  gather_array() %>% 
  mutate(affilIDtype = as.character(..JSON)) %>% 
  select(indexID,creatorIndex,affiliationIndex,affilIDtype)

# combine affiliations into single list per author per dataset
dcAffilList <- dcAuthorAffil %>% 
  group_by(indexID,creatorIndex) %>% 
  summarize(affilList = paste(authorAffil, collapse = "|"))

####### Researcher IDs (ORCIDs)

# extract author identifiers
dcAuthorID <- dcAuthorMetadata %>% 
    filter(nameMetadata == "nameIdentifiers") %>% 
    gather_array("identifierIndex") %>% 
    gather_object("identifierMetadata") %>% 
    filter(identifierMetadata == "nameIdentifier") %>% 
    gather_array() %>% 
    mutate(authorID = as.character(..JSON),
           authorID = ifelse(str_detect(string = authorID, # keep only ORCID iD
                                        pattern = "https://orcid.org/"),
                             substr(authorID,19,37),authorID)) %>% 
    select(indexID,creatorIndex,identifierIndex,authorID)

# extract type of Author ID
dcAuthorIDtype <- dcAuthorMetadata %>% 
  filter(nameMetadata == "nameIdentifiers") %>% 
  gather_array("identifierIndex") %>% 
  gather_object("identifierMetadata") %>% 
  filter(identifierMetadata == "nameIdentifierScheme") %>% 
  gather_array() %>% 
  mutate(authorIDtype = as.character(..JSON)) %>% 
  select(indexID,creatorIndex,identifierIndex,authorIDtype)

####### Join Author Metadata

# important to have unjoined dataframe as .x and joined dataframe as .y
# hence full_join(dcAuthorFamily, ., ...)
# . represents joined dataframe after each pipe
authorMeta_full <- dcDOI %>%
  dplyr::full_join(dcAuthorNames, by = c("indexID")) %>%
  dplyr::full_join(dcAuthorFamily,., by = c("indexID","creatorIndex")) %>%
  dplyr::full_join(dcAuthorGiven,., by = c("indexID","creatorIndex")) %>%
  dplyr::full_join(dcAuthorID,., by = c("indexID","creatorIndex")) %>%
  dplyr::full_join(dcAuthorIDtype,.,by = c("indexID","creatorIndex","identifierIndex")) %>% 
  dplyr::full_join(dcAuthorAffil,., by = c("indexID","creatorIndex")) %>%
  dplyr::full_join(dcAffilID,., by = c("indexID","creatorIndex","affiliationIndex")) %>% 
  dplyr::full_join(dcAffilIDtype,., by = c("indexID","creatorIndex","affiliationIndex")) %>% 
  dplyr::select(doi,authorName,authorID,authorIDtype,authorAffil,
                affilID,affilIDtype,familyName,givenName,
                indexID,creatorIndex,affiliationIndex,identifierIndex)

# verifying no unusual encoding (typically unknown or UTF-8)
Encoding(authorMeta_full$authorName) %>% 
  unique()

write_csv(authorMeta_full,"data/processed/authorMetadata.csv")
# NOTE: need to open specifying UTF-8 character encoding


##### Author & Affiliation Summaries ------------------------------------------

# count unique author-affiliation combinations
authorMeta_full %>% 
  select(authorName,authorAffil) %>% 
  distinct() %>% 
  count(authorAffil) %>% 
  arrange(desc(n))

sum(str_detect(authorMeta_full$authorAffil,
           pattern = "Oklahoma State University"), na.rm = TRUE)

# filter for string "Oklahoma State University" to find non-exact matches
authorMeta_full %>% 
  select(authorName,authorAffil) %>% 
  distinct() %>%
  count(authorAffil) %>% 
  filter(grepl("Oklahoma State University",authorAffil)) %>% 
  arrange(desc(n))

# affiliation identifiers (ROR and GRID in this case)
authorMeta_full %>% 
  count(affilIDtype)

# number of DOIs per author
# DOES NOT account for name variation
authorMeta_full %>% 
  select(doi,authorName) %>% 
  distinct() %>% 
  count(authorName) %>% 
  arrange(desc(n))



##### Extract Pub Year, Publisher, Usage Stats, Title ------------------------------------------

####### Publication year
dcPubYear <- dcDatasets %>% 
    dplyr::filter(name == "publicationYear") %>% 
    enter_object() %>% 
    gather_array() %>% 
    mutate(publicationYear = as.numeric(..JSON)) %>% 
    select(indexID,publicationYear)

dcPubYear %>% 
  count(publicationYear) 

####### Publisher
# "publisher" = who published item

dcPublisher <- dcDatasets %>% 
    filter(name == "publisher") %>% 
    gather_array("publisherMetadata") %>% 
    mutate(publisherID = as.character(..JSON)) %>% 
    select(indexID,publisherID)

dcPublisher %>% 
  count(publisherID) %>% 
  arrange(desc(n))


####### View Counts

dcViews <- dcDatasets %>% 
    filter(name == "viewCount") %>% 
    enter_object() %>% 
    gather_array() %>% 
    mutate(viewCount = as.numeric(..JSON)) %>% 
    select(indexID,viewCount)

# DOIs with non-zero number of views
sum(dcViews$viewCount > 0)

####### Download Counts

dcDownloads <- dcDatasets %>% 
    filter(name == "downloadCount") %>% 
    enter_object() %>% 
    gather_array() %>% 
    mutate(downloadCount = as.numeric(..JSON)) %>% 
    select(indexID,downloadCount)

# DOIs with non-zero number of downloads
sum(dcDownloads$downloadCount > 0)

####### Citation Counts

dcCitations <- dcDatasets %>% 
    filter(name == "citationCount") %>% 
    enter_object() %>% 
    gather_array() %>% 
    mutate(citationCount = as.numeric(..JSON)) %>% 
    select(indexID,citationCount)

# DOIs with non-zero number of citations
sum(dcCitations$citationCount > 0)

dcCitations %>% 
  count(citationCount)

####### Title

dcTitle <- dcDatasets %>% 
    filter(name == "titles") %>% 
    gather_array("titleIndex") %>% 
    gather_object("title") %>%
    filter(title == "title") %>% 
    gather_array() %>% 
    mutate(title = as.character(..JSON)) %>%
    select(indexID,titleIndex,title) %>% 
    filter(titleIndex == 1)

# how many duplicated titles?
sum(duplicated(dcTitle$title))

##### Join Single Line Items --------------------------------------------------

datasetMetadata <- dcDOI %>% 
  full_join(dcTitle, by = "indexID") %>% 
  full_join(dcPubYear,., by = "indexID") %>% 
  full_join(dcPublisher,., by = "indexID") %>% # how get client ID?
  full_join(dcViews,., by = "indexID") %>% 
  full_join(dcDownloads,., by = "indexID") %>% 
  full_join(dcCitations,., by = "indexID") %>% 
  select(doi, publicationYear, publisherID,
         title, viewCount, downloadCount, citationCount,
         indexID,titleIndex)

Encoding(datasetMetadata$title) %>% 
  unique()

write_csv(datasetMetadata,"data/processed/datasetMetadata.csv") 


# Extract Multiple Line Items ---------------------------------------------


### Copyright ---------------------------------------------------------------

dcRights <- dcDatasets %>% 
    filter(name == "rightsList") %>% 
    gather_array("rightsIndex") %>% 
    gather_object("rights") %>% 
    gather_array() %>% 
    filter(rights == "rights") %>% 
    mutate(copyright = as.character(..JSON)) %>%
    select(indexID,rightsIndex,copyright)

# NOTE: ends with 2 rights sometimes (OA + CC license)


# Extract Funder Information ------------------------------------------------------

# extract metadata provided on fundingReferences
dcFunderMetadata <- dcDatasets %>%
  dplyr::filter(name == "fundingReferences") %>% 
  gather_array("funderIndex") %>% 
  gather_object("funderMetadata")


### Funder Name -------------------------------------------------------------

dcFunderName <- dcFunderMetadata %>% 
    filter(funderMetadata == "funderName") %>% 
    enter_object() %>% 
    gather_array() %>% 
    mutate(funderName = as.character(..JSON)) %>% 
    select(indexID,funderIndex,funderName)

### Funder ID -------------------------------------------------------------

dcFunderID <- dcFunderMetadata %>% 
  filter(funderMetadata == "funderIdentifier") %>% 
  enter_object() %>% 
  gather_array() %>% 
  mutate(funderID = as.character(..JSON)) %>% 
  select(indexID,funderIndex,funderID)

### Funder ID Type -------------------------------------------------------------

dcFunderIDtype <- dcFunderMetadata %>% 
  filter(funderMetadata == "funderIdentifierType") %>% 
  enter_object() %>% 
  gather_array() %>% 
  mutate(funderIDtype = as.character(..JSON)) %>% 
  select(indexID,funderIndex,funderIDtype)

### Award Number ------------------------------------------------------------

dcFunderAwardNum <- dcFunderMetadata %>% 
    filter(funderMetadata == "awardNumber") %>% 
    enter_object() %>% 
    gather_array() %>% 
    mutate(awardNumber = as.character(..JSON)) %>% 
    select(indexID,funderIndex,awardNumber)

### Award Title ------------------------------------------------------------

dcFunderAwardTitle <- dcFunderMetadata %>% 
    filter(funderMetadata == "awardTitle") %>% 
    enter_object() %>% 
    gather_array() %>% 
    mutate(awardTitle = as.character(..JSON)) %>% 
    select(indexID,funderIndex,awardTitle)

# Join Funder Information -------------------------------------------------

funderMeta_full <- dcDOI %>% 
  full_join(dcFunderName, by = "indexID") %>% 
  full_join(dcFunderAwardNum,., by = c("indexID","funderIndex")) %>% 
  full_join(dcFunderAwardTitle,., by = c("indexID","funderIndex")) %>% 
  full_join(dcFunderID,., by = c("indexID","funderIndex")) %>% 
  full_join(dcFunderIDtype,., by = c("indexID","funderIndex")) %>% 
  select(doi, funderName, funderID, funderIDtype,
         awardNumber, awardTitle,
         indexID, funderIndex)

# which funding agencies are represented?
funderMeta_full %>% 
  count(funderName) %>% 
  arrange(desc(n))

# which funder ID types are used?
funderMeta_full %>% 
  count(funderIDtype) 


Encoding(funderMeta_full$funderName) %>% 
  unique()

write_csv(funderMeta_full,"data/processed/funderMetadata.csv")

# Extracted Related Works -------------------------------------------------

# extract metadata provided on relatedIdentifiers
dcRelatedWorksMetadata <- dcDatasets %>%
  dplyr::filter(name == "relatedIdentifiers") %>% 
  gather_array("relatedIndex") %>% 
  gather_object("relatedMetadata")


### Relation Type -----------------------------------------------------------

dcRelationType <- dcRelatedWorksMetadata %>% 
    filter(relatedMetadata == "relationType") %>% 
    enter_object() %>% 
    gather_array() %>% 
    mutate(relationType = as.character(..JSON)) %>% 
    select(indexID,relatedIndex,relationType)


### Related Identifier ------------------------------------------------------

dcRelatedID <- dcRelatedWorksMetadata %>% 
    filter(relatedMetadata == "relatedIdentifier") %>% 
    enter_object() %>% 
    gather_array() %>% 
    mutate(relatedWorkID = as.character(..JSON)) %>% 
    select(indexID,relatedIndex,relatedWorkID)

# Join Related Works ------------------------------------------------------

relatedMeta_full <- dcDOI %>% 
  full_join(dcRelationType, by = "indexID") %>% 
  full_join(dcRelatedID,., by = c("indexID","relatedIndex")) %>% 
  select(doi,relatedWorkID,relationType,indexID,relatedIndex)

write_csv(relatedMeta_full,"data/processed/relatedWorkMetadata.csv")

# "dates"
# "registered"


# Combine All Data ---------------------------------------------

allMetadataEx <- authorMeta_full %>% 
  full_join(datasetMetadata, join_by(doi,indexID)) %>% 
  full_join(funderMeta_full,., join_by(doi,indexID)) %>% 
  full_join(relatedMeta_full,.,join_by(doi,indexID)) %>% 
  select(doi,publicationYear,publisherID,
         authorName,authorID,authorIDtype,
         authorAffil,affilID,affilIDtype,
         familyName,givenName,title,
         funderName,funderID,funderIDtype,awardNumber,awardTitle,
         viewCount,downloadCount,citationCount,
         relatedWorkID,relationType,
         indexID,relatedIndex,funderIndex,creatorIndex,affiliationIndex,
         identifierIndex,titleIndex)

write_csv(allMetadataEx,"data/processed/fullMetadata.csv")


# Read in Data ------------------------------------------------------------

datasetMeta <- read_csv("data/processed/datasetMetadata.csv")
funderMeta <- read_csv("data/processed/funderMetadata.csv")
authorMeta <- read_csv("data/processed/authorMetadata.csv")
relatedMeta <- read_csv("data/processed/relatedWorkMetadata.csv")
fullMeta <- read_csv("data/processed/fullMetadata.csv")

# Summarize and Visualize -------------------------------------------------

### Datasets per Publisher --------------------------------------------------

unique(datasetMeta$publisherID)

# ESS-DIVE = Environmental System Science Data Infrastructure for a Virtual Ecosystem
# NOTE: renaming will be specific to each institutional dataset

datasetMeta %>% 
  mutate(publisherID = ifelse(publisherID %in% c("Environmental System Science Data Infrastructure for a Virtual Ecosystem; Effects of Hurricane Disturbance and Increased Temperature on Carbon Cycling and Storage of a Puerto Rican Forest: A Mechanistic Investigation of Above- and Belowground Processes",
                                             "Environmental System Science Data Infrastructure for a Virtual Ecosystem; Vegetation Survival-Mortality (SUMO)"),"ESS-DIVE",
                            ifelse(publisherID == "ICPSR - Interuniversity Consortium for Political and Social Research",
                                   "ICPSR",publisherID))) %>% 
  count(publisherID) %>% 
  mutate(publisherID = reorder(publisherID, -n)) %>% 
  ggplot(aes(x = publisherID,
             y = n,
             fill = publisherID)) +
  geom_bar(stat = "identity",
           color = "black") +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.text.x = element_blank(),
        axis.title.x = element_blank()) +
  geom_text(aes(label = n),
            vjust = -0.5) +
  ylim(0,165) +
  labs(y = "Number of Datasets",
       fill = "Publisher") +
  guides(fill = guide_legend(nrow = 5))


### Datasets per Publication Year -------------------------------------------

datasetMeta %>% 
  count(publicationYear) %>% 
  ggplot(aes(x = publicationYear,
             y = n)) +
  geom_line()

# year + publisher
datasetMeta %>% 
  mutate(publisherID = ifelse(publisherID %in% c("Environmental System Science Data Infrastructure for a Virtual Ecosystem; Effects of Hurricane Disturbance and Increased Temperature on Carbon Cycling and Storage of a Puerto Rican Forest: A Mechanistic Investigation of Above- and Belowground Processes",
                                             "Environmental System Science Data Infrastructure for a Virtual Ecosystem; Vegetation Survival-Mortality (SUMO)"),"ESS-DIVE",
                            ifelse(publisherID == "ICPSR - Interuniversity Consortium for Political and Social Research",
                                   "ICPSR",publisherID))) %>% 
  group_by(publicationYear,publisherID) %>% 
  count() %>% 
  ggplot(aes(x = publicationYear,
             y = n,
             fill = publisherID)) +
  geom_bar(stat = "identity",
           color = "black") +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 5))

### View Counts -------------------------------------------------------------

datasetMeta %>% 
  mutate(publisherID = ifelse(publisherID %in% c("Environmental System Science Data Infrastructure for a Virtual Ecosystem; Effects of Hurricane Disturbance and Increased Temperature on Carbon Cycling and Storage of a Puerto Rican Forest: A Mechanistic Investigation of Above- and Belowground Processes",
                                             "Environmental System Science Data Infrastructure for a Virtual Ecosystem; Vegetation Survival-Mortality (SUMO)"),"ESS-DIVE",
                            ifelse(publisherID == "ICPSR - Interuniversity Consortium for Political and Social Research",
                                   "ICPSR",publisherID))) %>% 
  ggplot(aes(x = viewCount,
             fill = publisherID)) +
  geom_histogram(color = "black") +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 5))

### Download Counts ---------------------------------------------------------

datasetMeta %>% 
  mutate(publisherID = ifelse(publisherID %in% c("Environmental System Science Data Infrastructure for a Virtual Ecosystem; Effects of Hurricane Disturbance and Increased Temperature on Carbon Cycling and Storage of a Puerto Rican Forest: A Mechanistic Investigation of Above- and Belowground Processes",
                                             "Environmental System Science Data Infrastructure for a Virtual Ecosystem; Vegetation Survival-Mortality (SUMO)"),"ESS-DIVE",
                            ifelse(publisherID == "ICPSR - Interuniversity Consortium for Political and Social Research",
                                   "ICPSR",publisherID))) %>% 
  ggplot(aes(x = downloadCount,
             fill = publisherID)) +
  geom_histogram(color = "black") +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 5))

### Citation Counts ---------------------------------------------------------

datasetMeta %>% 
  mutate(publisherID = ifelse(publisherID %in% c("Environmental System Science Data Infrastructure for a Virtual Ecosystem; Effects of Hurricane Disturbance and Increased Temperature on Carbon Cycling and Storage of a Puerto Rican Forest: A Mechanistic Investigation of Above- and Belowground Processes",
                                             "Environmental System Science Data Infrastructure for a Virtual Ecosystem; Vegetation Survival-Mortality (SUMO)"),"ESS-DIVE",
                            ifelse(publisherID == "ICPSR - Interuniversity Consortium for Political and Social Research",
                                   "ICPSR",publisherID))) %>% 
  ggplot(aes(x = citationCount,
             fill = publisherID)) +
  geom_bar(color = "black") +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 5))


# Funder Summary -----------------------------------------------------------------

unique(funderMeta$funderName) # 97*

# *barring further de-duplication
# NOTE: lots of * that throw things off (corrected in OpenRefine)

funderMeta %>% 
  dplyr::select(funderName,indexID) %>% 
  distinct() %>% # 342 unique funder-dataset combos
  count(funderName) %>% 
  arrange(desc(n))

162/261 # 62% have no funder listed

41/180 # NSF listed as funder ~22% of the time

96 # 96 distinct funding agencies listed


# Author Summary ----------------------------------------------------------

unique(authorMeta$authorAffil)
unique(authorMeta$authorName)
# NOTE: using OpenRefine to clean

authorMeta %>% 
  dplyr::select(authorName,authorAffil) %>% 
  distinct() %>% # 1120 unique authors* 
  count(authorAffil) %>% 
  arrange(desc(n))

# *barring further de-duplication with name standardization

52/1120 # only 4.6% of authors did not have listed affiliation
226/1120 # 20% of authors were affiliated with OSU


# Quick Stats -------------------------------------------------------------

# where are they publishing?
datasetMeta %>% 
  count(publisherID) %>% 
  arrange(desc(n))

# dataset citation
datasetMeta %>% 
  group_by(publisherID,citationCount) %>% 
  count() %>% 
  arrange(desc(citationCount))

# dataset downloads
datasetMeta %>% 
  group_by(publisherID,downloadCount) %>% 
  count() %>% 
  arrange(desc(downloadCount)) %>% 
  print(n = 61)

# dataset views
datasetMeta %>% 
  group_by(publisherID,viewCount) %>% 
  count() %>% 
  arrange(desc(viewCount)) %>% 
  print(n = 125)


# Metadata Quality --------------------------------------------------------

### Author (ORCID) ----------------------------------------------------------


authorIDcount <- allMetadataEx %>% 
  select(doi,publisherID,authorIDtype) %>% 
  distinct()

authorPID_only <- authorIDcount %>% 
  filter(!is.na(authorIDtype))

# total number of DOIs from each publisher
authorIDcount %>% 
  select(doi,publisherID,authorIDtype) %>%
  arrange(authorIDtype) %>% # should ensure ORCID is default, rather than NA
  filter(!duplicated(doi)) %>% # remove duplicated DOI entries
  count(publisherID,authorIDtype)


### Affiliation (ROR, GRID) -------------------------------------------------

affilIDcount <- allMetadataEx %>% 
  select(doi,publisherID,affilIDtype) %>% 
  distinct()

affilPID_only <- affilIDcount %>% 
  filter(!is.na(affilIDtype))

# total number of DOIs from each publisher
affilIDcount %>% 
  select(doi,publisherID,affilIDtype) %>%
  arrange(affilIDtype) %>% # should ensure ROR/GRID is default, rather than NA
  filter(!duplicated(doi)) %>% # remove duplicated DOI entries
  count(publisherID,affilIDtype)


### Funder (ROR, Crossref Funder ID) -------------------------------------------------

funderIDcount <- allMetadataEx %>% 
  select(doi,publisherID,funderIDtype) %>% 
  distinct()

funderPID_only <- funderIDcount %>% 
  filter(!is.na(funderIDtype))

# total number of DOIs from each publisher
funderIDcount %>% 
  select(doi,publisherID,funderIDtype) %>%
  arrange(funderIDtype) %>% # should ensure ROR/Crossref is default, rather than NA
  filter(!duplicated(doi)) %>% # remove duplicated DOI entries
  count(publisherID,funderIDtype)


# Join PID Metadata -------------------------------------------------------

metadataPID <- authorPID_only %>% 
  full_join(affilPID_only,by = c("doi","publisherID")) %>% 
  full_join(funderPID_only,.,by = c("doi","publisherID")) %>% 
  select(doi,publisherID,authorIDtype,affilIDtype,funderIDtype) %>% 
  arrange(doi,affilIDtype,authorIDtype,funderIDtype)

# pull out DOIs from PID records
PID_DOIs <- unique(metadataPID$doi)
  

noPID <- allMetadataEx %>% 
  select(doi,publisherID,funderIDtype,authorIDtype,affilIDtype) %>% 
  filter(is.na(funderIDtype)) %>% 
  filter(is.na(affilIDtype)) %>% 
  filter(is.na(authorIDtype)) %>% 
  filter(!doi %in% PID_DOIs) %>% 
  distinct()

metadataPID_quant <- metadataPID %>%
  full_join(noPID) %>% 
  mutate(authPID = ifelse(!is.na(authorIDtype),"Y","N"),
         affPID = ifelse(!is.na(affilIDtype),"Y","N"),
         fundPID = ifelse(!is.na(funderIDtype),"Y","N"))
# !!!!!!!!!!!!!! #
# 2 DOIs with duplicate records (GRID & ROR)
# 10.6084/m9.figshare.16786857
# 10.6084/m9.figshare.16786857.v1

# Author PIDs
metadataPID_quant %>% 
  group_by(publisherID) %>%
  summarize(numY = sum(authPID == "Y"),
            numN = sum(authPID == "N"),
            propY = sum(authPID == "Y")/n())

metadataPID_quant %>% 
  group_by(publisherID,authorIDtype) %>% 
  count(authorIDtype) %>% 
  # filter(publisherID %in% c("Dryad","Zenodo","figshare",
  #                         "ICPSR - Interuniversity Consortium for Political and Social Research")) %>% 
  mutate(publisherID = ifelse(publisherID == "ICPSR - Interuniversity Consortium for Political and Social Research",
                              "ICPSR",
                              ifelse(str_detect(publisherID,
                                                pattern = "Environmental System Science Data Infrastructure for a Virtual"),
                                                "ESS-DIVE",publisherID))) %>% 
  ggplot(aes(x = reorder(publisherID,-n),
             y = n,
             fill = authorIDtype)) +
  geom_bar(stat = "identity",
           color = "black") +
  labs(x = "",
       y = "Number of Datasets",
       fill = "",
       title = "Presence of at Least 1 Author PID") +
  scale_fill_manual(values = c("ORCID" = "#FFC20A","Other" = "#0C7BDC",
                               "NA" = "gray80")) +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1),
        text = element_text(size = 16),
        legend.position = "bottom")

ggsave("figures/authorPIDs.png",plot = last_plot(),
       width = 8, height = 8,dpi = 300)

# Affiliation PIDs
metadataPID_quant %>% 
  group_by(publisherID) %>%
  summarize(numY = sum(affPID == "Y"),
            numN = sum(affPID == "N"),
            propY = sum(affPID == "Y")/n())

metadataPID_quant %>% 
  group_by(publisherID,affilIDtype) %>% # to distinguish ID types
  count(affilIDtype) %>% 
  # filter(publisherID %in% c("Dryad","Zenodo","figshare",
  #                           "ICPSR - Interuniversity Consortium for Political and Social Research")) %>% 
  mutate(publisherID = ifelse(publisherID == "ICPSR - Interuniversity Consortium for Political and Social Research",
                              "ICPSR",
                              ifelse(str_detect(publisherID,
                                                pattern = "Environmental System Science Data Infrastructure for a Virtual"),
                                     "ESS-DIVE",publisherID))) %>% 
  ggplot(aes(x = reorder(publisherID,-n),
             y = n,
             fill = affilIDtype)) +
  geom_bar(stat = "identity",
           color = "black") +
  labs(x = "",
       y = "Number of Datasets",
       fill = "",
       title = "Presence of at Least 1 Affiliation PID") +
  scale_fill_manual(values = c("ROR" = "#FFC20A","GRID" = "#0C7BDC",
                               "NA" = "gray80")) +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1),
        text = element_text(size = 16),
        legend.position = "bottom")

ggsave("figures/affilPIDs.png",plot = last_plot(),
       width = 8, height = 8,dpi = 300)

# Funder PIDs
metadataPID_quant %>% 
  group_by(publisherID) %>%
  summarize(numY = sum(fundPID == "Y"),
            numN = sum(fundPID == "N"),
            propY = sum(fundPID == "Y")/n())

metadataPID_quant %>% 
  group_by(publisherID,funderIDtype) %>% # to distinguish ID types
  count(funderIDtype) %>% 
  # filter(publisherID %in% c("Dryad","Zenodo","figshare",
  #                           "ICPSR - Interuniversity Consortium for Political and Social Research")) %>% 
  mutate(publisherID = ifelse(publisherID == "ICPSR - Interuniversity Consortium for Political and Social Research",
                              "ICPSR",
                              ifelse(str_detect(publisherID,
                                                pattern = "Environmental System Science Data Infrastructure for a Virtual"),
                                     "ESS-DIVE",publisherID))) %>% 
  ggplot(aes(x = reorder(publisherID,-n),
             y = n,
             fill = funderIDtype)) +
  geom_bar(stat = "identity",
           color = "black") +
  labs(x = "",
       y = "Number of Datasets",
       fill = "",
       title = "Presence of at Least 1 Funder PID") +
  scale_fill_manual(values = c("ROR" = "#FFC20A","Crossref Funder ID" = "#0C7BDC",
                               "NA" = "gray80")) +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1),
        text = element_text(size = 16),
        legend.position = "bottom")

ggsave("figures/funderPIDs.png",plot = last_plot(),
       width = 8, height = 8,dpi = 300)
