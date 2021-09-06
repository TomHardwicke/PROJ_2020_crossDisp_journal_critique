# script to compile list of articles to check for PPPR in practice

library(tidyverse)
library(here)
library(readxl)
`%notin%` <- Negate(`%in%`)

load(here('data','processed','dataPolicy.Rdata'))

# get list of journals that offer PPPR
ppprJournals <- policyData %>%
  filter(anyPPPR == 'YES') %>%
  count(journal, PPPR_name) %>%
  filter(!is.na(PPPR_name)) %>%
  group_by(journal) %>% 
  summarise(PPPR_names = paste(PPPR_name, collapse=" //// "))

# ppprJournals <- ppprJournals[sample(1:nrow(ppprJournals)), ] # randomly shuffle dataframe rows
# write_csv(ppprJournals, here('data','raw','stage2Articles','ppprJournals.csv'))

for(thisJournal in ppprJournals$journal){
  print(thisJournal)
  d <- read_csv(here('data','raw','stage2Articles','fromWoS',paste0(thisJournal,'.csv')), col_types = cols(.default = "c")) # read in articles for this journal
  jrnl <- str_replace(thisJournal,' ','_') # make journal name for id
  
  d <- d %>% 
    filter(across(any_of("DT"), ~.x %notin% # if DT column (article type) exists
                    c('Correction',
                      'Retraction',
                      'News Item',
                      'Book Review',
                      'Meeting Abstract',
                      'Biographical-Item'))) %>% # exclude these article types
    rownames_to_column('id') %>%
    mutate(id = paste0(jrnl,'_',id), # create unique ID for each article based on journal name and row number
           doi = paste0('https://doi.org/',DI),
           authors = AU,
           title = TI,
           exclusion = NA,
           `exclusion explanation` = NA,
           `Is article itself PPPR` = NA,
           `Is article linked to PPPR` = NA,
           `Additional notes` = NA) %>%
    select(id,doi,authors,title,exclusion,`exclusion explanation`,`Is article itself PPPR`,`Is article linked to PPPR`,`Additional notes`, everything())
  set.seed(42) # set random seed for reproducible random shuffling
  d_shuffled <- d[sample(1:nrow(d)), ] # randomly shuffle dataframe rows
  write_csv(d_shuffled, here('data','raw','stage2Articles','forCoding',paste0(thisJournal,'.csv')))
}
