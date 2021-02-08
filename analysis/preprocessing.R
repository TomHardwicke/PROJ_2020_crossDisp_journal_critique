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
  
# we have a few cases where an article type was initially classified as PPPR by the primary and secondary coder, but TEH has decided it does not meet our operational definition of PPPR
## we excluded these cases below
## we exclude two cases of "Responses" as they involve "a response to a Letter to the Editor" and our operational definition does not count responses to PPPR as PPPR themselves
## we exclude a case of "Opinionated Articles" as their purpose appears to be to "summarize a research finding" rather than provide critique
## we exclude a case of "Catalysis" as they don't seem to be a way for anyone to comment on published articles. Instead it seems to be a forum for discussion where a target article is written and then others can provide reactions - and those reactions are commissioned.
## we exclude a case of Clinical Implications of Basic Research because it does not appear to allow for critique of articles, only discussion of their clinical implications


## apply exclusions
toExclude <- c("Responses", "Opinionated Articles", "Catalysis", "Clinical Implications of Basic Research")
policyData <- policyData %>% 
  mutate(PPPR_description = ifelse(PPPR_name %in% toExclude, NA, PPPR_description),
         wordLimits = ifelse(PPPR_name %in% toExclude, NA, wordLimits),
         timeLimits = ifelse(PPPR_name %in% toExclude, NA, timeLimits),
         referenceLimits = ifelse(PPPR_name %in% toExclude, NA, referenceLimits),
         peerReviewed = ifelse(PPPR_name %in% toExclude, NA, peerReviewed),
         PPPR_name = ifelse(PPPR_name %in% toExclude, NA, PPPR_name))



# harmonize PPPR names
## current the PPPR names are copied verbatim from the journal websites
## but many of them are basically the same name with small grammatical differences (e.g., letters, letter to the editor etc.)
## firstly, we will standardize names that grammatically similar
## we will then examine conceptual similarity and see if we can summarise the types into a few high level categories
## a guiding assumption here is that we can capture most types with three categories (but we will deviate from this if necessary): 
## letters to the editor (or 'correspondence') - very short PPPR articles
## commentary articles - longer PPPR articles
## below the line comments - informal (non-article) on journal websites

tmp <- policyData %>%
  filter(anyPPPR == "YES",
         !is.na(PPPR_name)) %>%
  select(PPPR_name, PPPR_description)
write_csv(tmp, 'tmp.csv')

## grammatical harmonization
policyData <- policyData %>%
  mutate(PPPR_category = case_when(
    PPPR_name %in% c(
      'Below the line comments ("Comments")',
      '(Below the line comments) Rapid Responses',
      'comments [below the line]',
      'Below the Line Comment',
      'Comments [below the line]',
      'below the line comments',
      'Below the Line Comments',
      '(Below the line) Comments',
      'Comment [below the line]',
      'Responses (below the line comments)',
      '(below the line) Comments',
      'Comments (below the lines)',
      '(Below the line) Comments',
      'Reader Comments [below the line]',
      '(Below the line) Reader comments',
      'Below the line comments ("Comments")',
      'eLetters [below the line]',
      'eLetters [below the line style commenting]',
      'Comments (Below the line comments)',
      '	(Below the line) comments',
      'Below the line comment',
      '	(Below the line) comments	',
      'comments (below the line)',
      '(Below the line) eLetters',
      '(Below the line) comments'
    ) ~ "Below the line comments",
    PPPR_name %in% c(
      'Correspondence/Rebuttal',
      'Letter to the Editor',
      'Letters to the Editor',
      'Letters to the editor',
      'Letter to the editor',
      'Letters to the Editors',
      'Letters to Editors',
      'Correspondence',
      'correspondence',
      'Correspondences',
      'Letters (Correspondence)',
      'Letters',
      'Correspondence or Comments',
      'Letters: Comments',
      'Opinion letters / editorials',
      'Opinion letters / editorials',
      'Letters to the Editor(s):',
      'Correspondence and eLetters',
      'Correspondence: Letters to the Editor',
      'Scientific Correspondence',
      'Correspondence (peer-reviewed)',
      'Correspondence (not peer-reviewed)',      
      'Letter / Letters to the editor',
      'Correspondence/Comment',
      'letters to the editor',
      'Correspondence (Letters)',
      'Communications and Letters to the Editor',
      'Comment Correspondences',
      'Letters to the Editor ("Letter to the Editor")',
      'Correspondence and Replies',
      'Letters to the Editor (Comment Letter)',
      'Letters to the Editor ("Correspondence")',
      'Letters to the Editor ("Correspondences")',
      'Letters to the Editor (correspondence)',
      'Letters & Commentaries',
      'Correpondence',
      'Reader Comments',
      'Letter'
    ) ~ "Letters to the editor",
    PPPR_name %in% c(
      'Commentary ("Responses or Commentaries")',
      'Commentary',
      'Comments',
      'Comment',
      'comment',
      'Comment/Reply',
      'Comment and Reply',
      'Comment and Reply.',
      'Comments and Replies',
      'Commentaries and rejoinders',
      'Commentaries',
      'Commentaries and Views',
      'Peer-reviewed comments',
      'Comment and Reply Exchange',
      'Commentary ("Commentaries")',
      'Technical Comments',
      'Comments paper',
      'Commentary ("Comment")',
      'Commentary ("Comments")',
      'Commentary ("Comments on Published Papers")',
      'Comments Papers and Communications',
      'Transactions Comments or Corrections',
      'Comments/Replies',
      'commentaries'
    ) ~ "Commentary",
    PPPR_name %in% c(
      'Commentary ("Matters Arising")',
      'Editorial',
      'Discussion',
      'Discussion Forum',
      'News and Views',
      'Previews',
      'Essay',
      'Matters Arising',
      'Forum papers',
      'Research Advance',
      'Replications and Corrigenda',
      'Research note',
      'Disputes & Debates',
      'Update articles',
      'Perspectives'
    ) ~ "Other",
    TRUE ~ "Not categorised"
  ))


policyData %>% filter(!is.na(PPPR_name)) %>% count(PPPR_category)


# save the processed data 
save(policyData, file = here('data', 'processed', 'data_policy.RData'))
