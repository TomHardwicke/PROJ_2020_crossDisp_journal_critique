# this script performs various pre-processing steps on the primary data

# load the primary data
policyData <- read_csv(here('data','primary','data_policy.csv'))
journalData <- read_csv(here('data','primary','data_journal.csv'))

# munging

policyData <- policyData %>%
  rename(journal = `Enter the name of the journal:`)

journalData <- journalData %>%
  rename(journal = `Full Journal Title`)

# merge data frames
policyData <- left_join(policyData, journalData, by = 'journal')

# change variable names to make them easier to work with
policyData <- policyData %>%
  rename(coders = `Coder's initials`,
         anyPPPR = `Does the journal offer any type of PPPR?`,
         rank = Rank,
         jif = `Journal Impact Factor`,
         field = ESI_field,
         cope = `COPE (T = member; F = not a member)`
         )

# filter data
policyData <- policyData %>%
  filter(coders %in% c( # select only the double coded entries
    'LT/RTT','LT/TEH','LT/JEK',
    'SAH/RTT','SAH/TEH','SAH/JEK',
    'TB/RTT','TB/TEH','TB/JEK'))

# tidy data

# switch from wide to long format (i.e., switch to one row per PPPR)
policyData <- policyData %>%
  rename_all( ~ str_replace(.,'[.]', '_')) %>% # change dots to underscores to work with subsequent code
  pivot_longer( # pivot to long format
    cols = c(-coders,-journal,-anyPPPR,-rank,-jif,-field,-cope,-journalHomepage,-originalLink_articleTypes,-permaLink_articleTypes),
    names_to = c("PPPR_type", ".value"), 
    names_sep = "_ "
  ) 

# rename columns
policyData <- policyData %>%
  rename(
    PPPR_name = `Enter the name of the PPPR type offered by the journal:`,
    PPPR_description = `Enter the verbatim description of this type of PPPR provided by the journal`,
    wordLimits = `Are there any word/comment limits for this type of PPPR?`,
    timeLimits = `Are there any time limits for submission of this type of PPPR?`,
    referenceLimits = `Are there any reference limits for this type of PPPR?`,
    peerReviewed = `Is this type of PPPR peer reviewed?`) %>%
  select(-`Are there any other types of PPPR in this journal?`) %>%
  mutate(field = fct_recode(field, # adjust field names for presentation purposes
                            "PSYCHIATRY/PSYCHOLOGY" = "PSYCHIATRY_PSYCHOLOGY",
                            "SOCIAL SCIENCES" = "SOCIAL SCIENCES, GENERAL",
                            "ENVIRONMENT/ECOLOGY" = "ENVIRONMENT_ECOLOGY"),
         field = str_to_title(field),
         field = factor(field))
  
# save the processed data 
save(policyData, file = here('data', 'processed', 'data_policy.RData'))
