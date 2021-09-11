# this script performs various pre-processing steps on the primary data
# we'll do the Stage 1 (PPPR Policy) data first and then the Stage 2 (PPPR in Practice) data.

# load the stage 1 primary data
policyData <- read_csv(here('data','primary','dataPolicy.csv'))
journalData <- read_csv(here('data','primary','dataJournal.csv'))

# prepare to merge datasets by homogenizing column names for journal
policyData <- policyData %>%
  rename(journal = `Enter the name of the journal:`)

journalData <- journalData %>%
  rename(journal = `Full Journal Title`)

# check journal names are the same in both data frames
stopifnot(policyData$journal %in% journalData$journal)

# merge data frames
policyData <- left_join(policyData, journalData, by = 'journal')
rm(journalData) # remove the journalData dataframe

# change variable names to make them easier to work with
policyData <- policyData %>%
  rename(coders = `Coder's initials`,
         anyPPPR = `Does the journal offer any type of PPPR?`,
         rank = Rank,
         jif = `Journal Impact Factor`,
         field = ESI_field,
         cope = `COPE (T = member; F = not a member)`)
         
# select only the dual coding
policyData <- policyData %>%
  filter(coders %in% c( # select only the double coded entries
    'LT/RTT','LT/TEH','LT/JEK',
    'SAH/RTT','SAH/TEH','SAH/JEK',
    'TB/RTT','TB/TEH','TB/JEK'))

stopifnot(nrow(policyData) == 330) # test there are 330 rows (journals)

## Not run:
# visual diagnostics
# skim(policyData)
## End(**Not run**)

# tidy data

# switch from wide to long format (i.e., switch to one row per PPPR instead of one row per journal)
policyData <- policyData %>%
  rename_all( ~ str_replace(.,'[.]', '_')) %>% # change dots to underscores to work with subsequent code
  pivot_longer( # pivot to long format
    cols = c(-coders,-journal,-anyPPPR,-rank,-jif,-field,-cope,-journalHomepage,-originalLink_articleTypes,-permaLink_articleTypes),
    names_to = c("PPPR_type", ".value"), 
    names_sep = "_ "
  )

# rename columns to make them easier to work with
policyData <- policyData %>%
  rename(
    PPPR_name = `Enter the name of the PPPR type offered by the journal:`,
    PPPR_description = `Enter the verbatim description of this type of PPPR provided by the journal`,
    wordLimits = `Are there any word/comment limits for this type of PPPR?`,
    timeLimits = `Are there any time limits for submission of this type of PPPR?`,
    referenceLimits = `Are there any reference limits for this type of PPPR?`,
    peerReviewed = `Is this type of PPPR peer reviewed?`) %>%
  select(-`Are there any other types of PPPR in this journal?`) # drop column we don't need

# adjust field names for presentation purposes
policyData <- policyData %>%
mutate(field = fct_recode(field, 
  "PSYCHIATRY/PSYCHOLOGY" = "PSYCHIATRY_PSYCHOLOGY",
  "ENVIRONMENT/ECOLOGY" = "ENVIRONMENT_ECOLOGY",
  "MULTIDISCIPLINARY" = "Multidisciplinary"))

# tests
stopifnot(policyData$anyPPPR %in% c("YES", "NO")) # anyPPPR should be YES or NO

# change column types
policyData <- policyData %>%
  mutate(anyPPPR = recode(anyPPPR, "NO" = F, "YES" = T),
         anyPPPR = as.logical(anyPPPR),
         coders = as.factor(coders),
         journal = as.factor(journal))

# exclusions
## we have a few cases where an article type was initially classified as PPPR by the primary and secondary coder, but following team discussion we have decided that they do not meet our operational definition of PPPR
## we excluded these cases below
## we exclude two cases of "Responses" as they involve "a response to a Letter to the Editor" and our operational definition does not count responses to PPPR as PPPR themselves
## we exclude a case of "Opinionated Articles" as their purpose appears to be to "summarize a research finding" rather than provide critique
## we exclude a case of "Catalysis" as they don't seem to be a way for anyone to comment on published articles. Instead it seems to be a forum for discussion where a target article is written and then others can provide reactions - and those reactions are commissioned.
## we exclude a case of "Clinical Implications of Basic Research" because it does not appear to allow for critique of articles, only discussion of their clinical implications
## we exclude two cases of "Previews" because they are primarily for providing 'context' (not critique) about papers reported in the same issue (and thus, not *post* publication). They are also primarily commissioned. 
## we exclude two cases of "Editorials" because they are primarily commissioned

## apply exclusions
toExclude <- c("Responses", "Opinionated Articles", "Catalysis", "Essay", "Research note", "Clinical Implications of Basic Research", "Previews", "Editorial")
policyData <- policyData %>% 
  mutate(PPPR_description = ifelse(PPPR_name %in% toExclude, NA, PPPR_description),
         wordLimits = ifelse(PPPR_name %in% toExclude, NA, wordLimits),
         timeLimits = ifelse(PPPR_name %in% toExclude, NA, timeLimits),
         referenceLimits = ifelse(PPPR_name %in% toExclude, NA, referenceLimits),
         peerReviewed = ifelse(PPPR_name %in% toExclude, NA, peerReviewed),
         PPPR_name = ifelse(PPPR_name %in% toExclude, NA, PPPR_name))

## also exclude one case of "News and Views" with "may always be invited/commissioned by the editor" in the description because it appears to be a venue for writing commissioned articles about articles appearing in the same journal issue, and so does not meet our definition of PPPR
policyData <- policyData %>% 
  mutate(PPPR_name = ifelse(str_detect(PPPR_description, 'commissioned by the editor'), NA, PPPR_name),
       wordLimits = ifelse(str_detect(PPPR_description, 'commissioned by the editor'), NA, wordLimits),
       timeLimits = ifelse(str_detect(PPPR_description, 'commissioned by the editor'), NA, timeLimits),
       referenceLimits = ifelse(str_detect(PPPR_description, 'commissioned by the editor'), NA, referenceLimits),
       peerReviewed = ifelse(str_detect(PPPR_description, 'commissioned by the editor'), NA, peerReviewed),
       PPPR_description = ifelse(str_detect(PPPR_description, 'commissioned by the editor'), NA, PPPR_description))

## also exclude the "comment" article type recorded for "Particle and Fibre Toxicology" - format cannot be identified on website
policyData <- policyData %>% 
  mutate(PPPR_description = ifelse(PPPR_name == 'Comment' & journal == 'Particle and Fibre Toxicology', NA, PPPR_description),
         wordLimits = ifelse(PPPR_name == 'Comment' & journal == 'Particle and Fibre Toxicology', NA, wordLimits),
         timeLimits = ifelse(PPPR_name == 'Comment' & journal == 'Particle and Fibre Toxicology', NA, timeLimits),
         referenceLimits = ifelse(PPPR_name == 'Comment' & journal == 'Particle and Fibre Toxicology', NA, referenceLimits),
         peerReviewed = ifelse(PPPR_name == 'Comment' & journal == 'Particle and Fibre Toxicology', NA, peerReviewed),
         PPPR_name = ifelse(PPPR_name == 'Comment' & journal == 'Particle and Fibre Toxicology', NA, PPPR_name))

## also exclude the "commentary" article type for Journal of the Academy of Nutrition and Dietetics
policyData <- policyData %>% 
  mutate(PPPR_description = ifelse(PPPR_name == 'Commentary' & journal == 'Journal of the Academy of Nutrition and Dietetics', NA, PPPR_description),
         wordLimits = ifelse(PPPR_name == 'Commentary' & journal == 'Journal of the Academy of Nutrition and Dietetics', NA, wordLimits),
         timeLimits = ifelse(PPPR_name == 'Commentary' & journal == 'Journal of the Academy of Nutrition and Dietetics', NA, timeLimits),
         referenceLimits = ifelse(PPPR_name == 'Commentary' & journal == 'Journal of the Academy of Nutrition and Dietetics', NA, referenceLimits),
         peerReviewed = ifelse(PPPR_name == 'Commentary' & journal == 'Journal of the Academy of Nutrition and Dietetics', NA, peerReviewed),
         PPPR_name = ifelse(PPPR_name == 'Commentary' & journal == 'Journal of the Academy of Nutrition and Dietetics', NA, PPPR_name))

# after exclusions, make sure that journals with no PPPR have FALSE in the anyPPPR column
policyData <- policyData %>%
  mutate(anyPPPR = ifelse(journal %in% c(
  "Cellular & Molecular Immunology",
  "JOURNAL OF CHILD PSYCHOLOGY AND PSYCHIATRY",
  "JOURNAL OF INTERNATIONAL BUSINESS STUDIES",
  "Nanophotonics"), F, anyPPPR))

# harmonize PPPR names
## currently the PPPR names are copied verbatim from the journal websites
## but many of them are basically the same name with small grammatical differences (e.g., letters, letter to the editor etc.)
## firstly, we will standardize names that grammatically similar
## we will then examine conceptual similarity and see if we can summarise the types into a few high level categories
## a guiding assumption here is that we can capture most types with three categories (but we will deviate from this if necessary): 
## letters to the editor (or 'correspondence') - very short PPPR articles
## commentary articles - longer PPPR articles
## below the line comments - informal (non-article) on journal websites

## grammatical harmonization
policyData <- policyData %>%
  mutate(PPPR_name_harmonized = case_when(
    PPPR_name %in% c(
      'Below the line comments ("Comments")',
      '(Below the line comments) Rapid Responses',
      'comments [below the line]',
      'Below the Line Comment',
      'Below the line comments',
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
      'Discussion Forum', # NB - the description describes these as 'letters'
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
      'Scientific Commentaries',
      'commentaries'
    ) ~ "Commentary article",
    PPPR_name %in% c(
      'Annotations',
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

# harmonize words limits
## the way that word limits were stated in journals and the way that data was recorded by our coders was not standardized. For example, some journals provided limits in terms of pages rather than words and some codes entered "500" meaning "500 words" whereas others entered "500 words".
## the code below will attempt to harmonzise data entry in this column to align entries with the same core meaning

policyData %>% filter(!is.na(PPPR_name)) %>% count(wordLimits)

# WE WILL ASSUME 1 PAGE IS 500 WORDS. SO PAGE COUNTS ARE CONVERTED TO WORD COUNTS BY MULTIPLYING BY 500
# WE WILL ALSO ASSUME THAT THERE ARE, ON AVERAGE, 6 CHARACTERS IN A WORD (roughly based on our last paper). SO CHARACTER COUNTS ARE CONVERTED TO WORD COUNTS BY DIVIDING BY 6

pageToWord <- 500
charToWord <- 6

policyData <- policyData %>%
  mutate(wordLimits = str_remove_all(wordLimits, pattern = '"'), #  remove any quotation marks
         wordLimits = str_remove(wordLimits, pattern = fixed('YES (provide number of words/comments below), ')), # remove option text
         wordLimits = case_when(
           wordLimits == 'not more than 1000 words and 2 pages' ~ as.character(2*pageToWord),
           wordLimits == '3 journal pages' ~ as.character(3*pageToWord),
           wordLimits == '10 pages' ~ as.character(10*pageToWord),
           wordLimits == '45000 characters (from Research Articles information)' ~ as.character(45000/charToWord),
           wordLimits == 'max 500 words' ~ '500',
           wordLimits == '600 words' ~ '600',
           wordLimits == 'NOT STATED, Note: Text is submitted via a text box of unknown size.' ~ 'NOT STATED',
           wordLimits == 'A max of 3000 words, inc references and figures etc' ~ '3000',
           wordLimits == '1 page' ~ as.character(1*pageToWord),
           wordLimits == "UNCLEAR (provide detail below), says articles must be 'short' but does not provide a specific word limit" ~ 'Limit not clearly specified',
           wordLimits == '1200 words' ~ '1200',
           wordLimits == '500 words' ~ '500',
           wordLimits == 'no more than 900' ~ '900',
           wordLimits == '500 words of text' ~ '500',
           wordLimits == 'Matters Arising articles follow the same format and length as Research articles... The total character count of an article must be under 45,000* (including spaces and main figure legends but excluding STAR Methods text, supplemental item legends, and References section)' ~ as.character(45000/charToWord),
           wordLimits == '1000 words' ~ '1000',
           wordLimits == 'no more than 1000' ~ '1000',
           wordLimits == '2000 to 3000' ~ '3000',
           wordLimits == '1,000 words max' ~ '1000',
           wordLimits == '500 words' ~ '500',
           wordLimits == 'up to 750 words' ~ '750',
           wordLimits == 'One page' ~ as.character(1*pageToWord),
           wordLimits == 'NOT STATED, Note: Text is submitted via a text box of unknown size.' ~ 'NOT STATED',
           wordLimits == 'UNCLEAR: 10,000 characters' ~ as.character(round(10000/charToWord,0)),
           wordLimits == '8 double-spaced typed pages with 12 point font, including figures. ' ~ as.character(8*pageToWord),
           wordLimits == '3 printed pages' ~ as.character(3*pageToWord),
           wordLimits == 'no more than 1,500 words' ~ '1500',
           wordLimits == '1500 words (excluding references)' ~ '1500',
           wordLimits == '500 words max' ~ '500',
           wordLimits == 'says average length is 600 words' ~ '600',
           wordLimits == '1500 words' ~ '1500',
           wordLimits == '800 Exceptions to these requirements will be made only under special circumstances.' ~ '800',
           wordLimits == '800 words' ~ '800',
           wordLimits == 'These are each limited to one PDF page, including references, figures, and tables (about 7500 characters of text = one printed page).' ~ as.character(7500/charToWord),
           wordLimits == 'UNCLEAR (provide detail below), 2 double column pages (including references)' ~ as.character(2*pageToWord),
           wordLimits == 'A CoP should be submitted through the ScholarOne Manuscript system as a letter...  Letters are normally 6 pages or less.' ~ as.character(2*pageToWord),
           wordLimits == '3 pages, but no specified word limit' ~ as.character(3*pageToWord),
           wordLimits == '2 pages' ~ as.character(2*pageToWord),
           wordLimits == '3 pages formatted in the IEEE two-column style.' ~ as.character(3*pageToWord),
           wordLimits == '2.5 pages' ~ as.character(2.5*pageToWord),
           wordLimits == '3 pages' ~ as.character(3*pageToWord),
           wordLimits == 'Transactions Comments or Corrections are usually very short (often less than a printed page)' ~ as.character(1*pageToWord),
           wordLimits == '600 words' ~ '600',
           wordLimits == '400 words' ~ '400',
           wordLimits == '3600 characters or approximately 600 words' ~ '600',
           wordLimits == 'UNCLEAR (provide detail below), Based of the perma.cc link, it is NOT STATED. But this site: https://www.ametsoc.org/ams/index.cfm/publications/authors/journal-and-bams-authors/choosing-a-journal-and-submission-type/    suggests that 1500 words is the limit' ~ '1500',
           wordLimits == 'Correspondence submissions must be no longer than 750 words.' ~ '750',
           wordLimits == '20 pages' ~ as.character(20*pageToWord),
           wordLimits == '4000 words (including everything in the manuscript)' ~ '4000',
           wordLimits == 'no more than 500' ~ '500',
           wordLimits == '250 words' ~ '250',
           wordLimits == 'Comments should be no more than 750 words.' ~ '750',
           wordLimits == 'Letters should be no longer than 400 words.' ~ '400',
           wordLimits == 'Comments are limited to a maximum of 1200 words' ~ '1200',
           wordLimits == '(per Research Articles format) 45000 characters' ~ as.character(45000/charToWord),
           wordLimits == 'UNCLEAR (provide detail below), Such manuscripts should be as brief as possible' ~ 'Limit not clearly specified',
           wordLimits == 'OTHER (provide details), As brief as possible' ~ 'Limit not clearly specified',
           wordLimits == '700 words, 900 if no image' ~ '700',
           wordLimits == 'up to 500' ~ '500',
           wordLimits == '250-500 words' ~ '500',
           wordLimits == '300 words' ~ '300',
           wordLimits == '1200 words' ~ '1200',
           wordLimits == '250-500 words' ~ '500',
           wordLimits == 'not more than 1200' ~ '1200',
           wordLimits == 'UNCLEAR: not exceeding 2 formatted pages, but no word limits' ~ as.character(2*pageToWord),
           wordLimits == '600 words' ~ '600',
           wordLimits == 'max 400 words' ~ '400',
           wordLimits == '175 words' ~ '175',
           wordLimits == 'The main text should be as concise as possible, and ideally not exceed 1,200 words.' ~ '1200',
           wordLimits == 'UNCLEAR (provide detail below), A Comment should occupy no more than three journal pages (no more than one page for Optics Letters and no more than two for Express Journals) including title, author list, tables, figures, and references.' ~ as.character(3*pageToWord),
           wordLimits == "around 800 words, including references. ...in excess of 1000 words won't be considered and will be sent back for revision." ~ '1000',
           wordLimits == '3500 words' ~ '3500',
           wordLimits == 'NOT STATED, Note: Text is submitted via a text box of unknown size.' ~ 'NOT STATED',
           wordLimits == '1,000 words (includes main text, notes, acknowledgments, and appendices; does not include cover page, Author Contributions, or reference list)' ~ '1500',
           wordLimits == '1500 words' ~ '1500',
           wordLimits == 'UNCLEAR (provide detail below), they say eLetters are brief online comments' ~ 'Limit not clearly specified',
           wordLimits == 'Commentaries are usually limited to 2 pages' ~ as.character(2*pageToWord),
           wordLimits == 'Correspondences are generally within 1 page' ~ as.character(1*pageToWord),
           wordLimits == '20 pages' ~ as.character(20*pageToWord),
           wordLimits == 'Word limit: 1500 * excluding the abstract, title page, tables, figure legends and references.' ~ '1500',
           wordLimits == 'NOT STATED' ~ 'NOT STATED',
           TRUE ~ wordLimits # if none of the above applies then keep current column content
         ))

# harmonize time limits
## we will convert everything to weeks

monthToWeek <- 4.35 # number to multiply months by to get weeks
yearToWeek <- 52 # number to multiply years by to get weeks 

policyData <- policyData %>%
  mutate(timeLimits = str_remove_all(timeLimits, pattern = '"'), #  remove any quotation marks
         timeLimits = str_remove(timeLimits, pattern = fixed('YES (provide detail of time limits below), ')), # remove option text
         timeLimits = case_when(
           timeLimits == '6 months' ~ as.character(6*monthToWeek),
           timeLimits == 'UNCLEAR (provide detail below), preferably in the last 18-24 months' ~ as.character(24*monthToWeek),
           timeLimits == "Anyone can submit a comment any time after publication, but only those submitted within 4 weeks of an article’s publication will be considered for print publication... One month after publication, editors review all posted comments and select some for publication in the Letters section of the print version of Annals." ~ '4',
           timeLimits == '6 weeks' ~ '6',
           timeLimits == "UNCLEAR (provide detail below), says must pertain to articles 'recently published' in the journal" ~ 'Limit not clearly specified',
           timeLimits == 'UNCLEAR (provide detail below),  recently published' ~ 'Limit not clearly specified',
           timeLimits == 'UNCLEAR (provide detail below),  relating to recent articles' ~ 'Limit not clearly specified',
           timeLimits == "UNCLEAR (provide detail below), Letter must pertain to a 'recent' BJP paper" ~ 'Limit not clearly specified',
           timeLimits == ' 2 years of the publication date of the original article (although the editor can waive this limit in extenuating circumstances)' ~ as.character(2*yearToWeek),
           timeLimits == 'about articles published within the last 2 months' ~ as.character(2*monthToWeek),
           timeLimits == 'they should refer to an article published in the past 3 months' ~ as.character(3*monthToWeek),
           timeLimits == "UNCLEAR (provide detail below), recent" ~ 'Limit not clearly specified',
           timeLimits == "within 6 weeks of the article's publication" ~ '6',
           timeLimits == "within 6 weeks of publication of the original item" ~ '6',
           timeLimits == '1 year' ~ as.character(1*yearToWeek),
           timeLimits == 'within a year of the original publication' ~ as.character(1*yearToWeek),
           timeLimits == "UNCLEAR (provide detail below), are more likely to be published if submitted within 4 weeks of the original article's publication" ~ '4',
           timeLimits == "12 weeks" ~ '12',
           timeLimits == "UNCLEAR (provide detail below), recently published articles" ~ 'Limit not clearly specified',
           timeLimits == "UNCLEAR (provide detail below), To ensure your response to a published article is timely, please submit your letter as soon as possible after publication of the original paper." ~ 'Limit not clearly specified',
           timeLimits == "by then last day of the month in which the original article was published; authors of the original article have 2 weeks to reply." ~ as.character(1*monthToWeek),
           timeLimits == 'The paper to which the Comment is directed should have been published in GCA within the previous 12 months' ~ as.character(12*monthToWeek),
           timeLimits == 'Comments: 6 months; Replies: 1 month' ~ as.character(6*monthToWeek),
           timeLimits == "UNCLEAR (provide detail below), recently published" ~ 'Limit not clearly specified',
           timeLimits == '2 months' ~ as.character(2*monthToWeek),
           timeLimits == "4 weeks" ~ '4',
           timeLimits == 'OTHER (provide detail), We may reject comments because they are submitted a long time after article publication.' ~ 'Limit not clearly specified',
           timeLimits == "within 4 weeks of the article's publication" ~ '4',
           timeLimits == "within 4 weeks of the article's publication in print" ~ '4',
           timeLimits == "OTHER (provide detail), States that the correspondence must concern recent publications in the journal" ~ 'Limit not clearly specified',
           timeLimits == "UNCLEAR (provide detail below), concerning articles recently published in JACI: In Practice" ~ 'Limit not clearly specified',
           timeLimits == '2 years although the editor can waive this limit in extenuating circumstances' ~ as.character(2*yearToWeek),
           timeLimits == 'If the Correspondence is written in response to a JCO article, it must be submitted within 6 weeks of online publication (i.e., the date that the article appears online, ahead of print) of that article in order to ensure timeliness of content. Under no circumstances will exceptions be made.' ~ '6',
           timeLimits == 'within 6 months' ~ as.character(6*monthToWeek),
           timeLimits == 'within 2 weeks of publication' ~ '2',
           timeLimits == 'within the past month or so' ~ as.character(1*monthToWeek),
           timeLimits == 'Letters linked to items published in the journal must reach us within 8 weeks of publication of the original item (for items published Online First, this means within 8 weeks of its online publication).' ~ '8',
           timeLimits == '8 weeks' ~ '8',
           timeLimits == 'OTHER (provide detail), We may reject comments because they… Are submitted a long time after article publication.' ~ 'Limit not clearly specified',
           timeLimits == 'UNCLEAR (provide detail below), It says Appropriate topics include comments on recently published manuscripts' ~ 'Limit not clearly specified',
           timeLimits == 'within 8 weeks after original paper was published' ~ '8',
           timeLimits == "UNCLEAR (provide detail below), concerning recently published information in Mucosal Immunology." ~ 'Limit not clearly specified',
           timeLimits == "UNCLEAR (provide detail below), The editors will decide how to proceed on the basis of the potential interest to readers, importance and timeliness of the contribution." ~ 'Limit not clearly specified',
           timeLimits == "OTHER (provide detail), states that timeliness is a factor: The editors will decide how to proceed on the basis of the potential interest to readers, importance and timeliness of the contribution." ~ 'Limit not clearly specified',
           timeLimits == "These comments should ideally be based on knowledge contemporaneous with the original paper, rather than subsequent scientific developments." ~ 'Limit not clearly specified',
           timeLimits == "OTHER (provide detail), Timeliness appears to be a factor - The editors will decide how to proceed on the basis of the potential interest to readers, importance and timeliness of the contribution." ~ 'Limit not clearly specified',
           timeLimits == 'OTHER (provide detail): Timeliness is a factor: The editors will decide how to proceed on the basis of the potential interest to readers, importance and timeliness of the contribution. ' ~ 'Limit not clearly specified',
           timeLimits == "OTHER (provide detail below), Timeliness seems to be a factor: The editors will decide how to proceed on the basis of the potential interest to readers, importance and timeliness of the contribution." ~ 'Limit not clearly specified',
           timeLimits == 'published within the journal in the last 6 months' ~ as.character(6*monthToWeek),
           timeLimits == 'It says Disputes & Debates is restricted to comments about studies published in Neurology within the past eight weeks, with the exception of submissions identifying possible errors in data or data analysis, or by appeal to the Editor.' ~ 'Limit not clearly specified',
           timeLimits == '3 weeks' ~ '3',
           timeLimits == '3 months' ~ as.character(3*monthToWeek),
           timeLimits == 'no more than 6 months after publishing of original article' ~ as.character(6*monthToWeek),
           timeLimits == '4 months' ~ as.character(4*monthToWeek),
           TRUE ~ timeLimits # if none of the above applies then keep current column content
         ))

# harmonize reference limits

policyData <- policyData %>%
  mutate(referenceLimits = str_remove_all(referenceLimits, pattern = '"'), #  remove any quotation marks
         referenceLimits = str_remove(referenceLimits, pattern = fixed('YES (provide number of references below), ')), # remove option text
         referenceLimits = case_when(
           referenceLimits %in% c('word/page limits include references','Reference limit included in length limit', 'Article must be a maximum of 500 words, including references','Included in overall length limit','references are included in the 1500 word limit, but no specific number of references','NOT STATED, Not explicitly stated, however word limit restrictions will impact reference limits: These are each limited to one PDF page, including references, figures, and tables (about 7500 characters of text = one printed page).','Included in word limit', 'UNCLEAR (provide detail below), No more than three journal pages including title, author list, tables, figures, and references.') ~ 'Limit not clearly specified',
           referenceLimits %in% c('max 10 references','up to 10 essential references','up to 10 references','max of 10','maximum of 10','10 or less','Less than 10 references.','10 references','No more than 10 references.','A strict maximum of ten references.','up to 10','up to 10 references','maximum of 10') ~ '10',
           referenceLimits %in% c('maximum of 5','no more than 5','Less than 5 references.','up to 5','5 references, the first of which cites the original article', '5 references, including the cited article', '5 references, including reference of the discussed paper','5 references, one of which should be the target article','5 or fewer, one of which should be the discussed article','maximum of 5 references','Letters should have a no more than five references.') ~ '5',
           referenceLimits %in% c('20 references','max 20 references') ~ '20',
           referenceLimits %in% c('15 references (labelled Further reading)','up to 15','As a guideline, contributions may have up to 15 references') ~ '15',
           TRUE ~ referenceLimits # if none of the above applies then keep current column content
         ))

# harmonize peer review

policyData <- policyData %>%
  mutate(peerReviewed = str_remove_all(peerReviewed, pattern = '"'), #  remove any quotation marks
         peerReviewed = str_remove_all(peerReviewed,pattern = '‚Äôs'), # remove unusual symbol that appears in some strings
         peerReviewed = case_when(
           peerReviewed %in% c('YES, Letters go through a shorter, one time (typically) review process.',
                               'YES, Every Comments on a Paper (CoP) is processed in the following manner: 1. CoPs are reviewed by one of the authors of the paper commented on plus two independent reviewers selected by the Editor-in-Chief. 2. If the review outcome is a clear acceptance or rejection the process ends.',
                               'YES, Moreover, these submissions will also be peer reviewed.',
                               'YES, The action editor typically solicits feedback on a submitted Commentary from the lead author of the target article, in addition to reviews by two independent experts.',
                               'YES, Update Articles undergo the same rigorous editorial and peer review process as other PLOS Biology articles.',
                               'YES, The Comment and Reply will both be subject to rigorous peer review in consultation with the journal Editorial Board where appropriate.',
                               'YES, Both comments and replies are subject to review.',
                               'OTHER (provide detail), Based of the perma.cc link, it is NOT STATED. But this site: https://www.ametsoc.org/ams/index.cfm/publications/authors/journal-and-bams-authors/choosing-a-journal-and-submission-type/    suggests that YES they are peer reviewed',
                               'YES, All Technical Comments are first reviewed by a member of our Editorial Board, and if deemed novel and of general importance, peer reviewed.') ~ 'YES',
           peerReviewed %in% c(' OTHER (provide detail), not usually subject to peer review','NO, Comments are moderated [by the Editors]',
                               'NO, All submissions (with the general exception of Editorials, Commentaries, and Correspondence) will be subject to peer review',
                               'NO, Comments will not be peer-reviewed.',
                               'NO Acceptance of Letters to the Editor will be the prerogative of the Editor',
                               'OTHER (provide detail), Correspondence letters are not usually peer reviewed, but we might invite replies from the authors of the original publication, or pass on letters to these authors',
                               'OTHER (provide detail), Letters are not usually peer reviewed, but they will be subject to editorial scrutiny before any decision to publish is made; we might consult authors of the original publication for advice; and occasionally invite formal replies from the authors of the original publication for inclusion in the journal alongside a letter',
                               'NO, Letters are not usually peer reviewed, but we might invite replies from the authors of the original publication, or pass on letters to these authors.',
                               'OTHER (provide detail), It says not usually peer reviewed',
                               'OTHER (provide detail), not usually subject to peer review',
                               'OTHER (provide detail), All letters submitted to Expert Series journals are subject to review by members of the Editorial Board.') ~ 'NO',
           peerReviewed %in% c('NOT STATED, seems like no, but not explicitly stated',
                               'NO Acceptance of Letters to the Editor will be the prerogative of the Editor',
                               'UNCLEAR, The Correspondence section is not considered to be an appropriate venue for publishing new data without peer review.',
                               'OTHER (provide detail), The description says Any such response would be vetted similarly and published in the same issue of the journal as the Letter.') ~ 'NOT STATED',
           peerReviewed %in% c('OTHER (provide detail), Matters Arising articles will either be accepted or rejected based on editorial evaluation and/or reviewers comments',
                               'OTHER (provide detail), peer review of submitted letters will occur solely at the Editors discretion.',
                               'it may be, the editors decide',
                               'OTHER (provide detail), Correspondence may undergo peer review under the direction of the assigned editor.',
                               'OTHER (provide detail), The editors reserve the right to edit and/or peer review Correspondence at their discretion.',
                               'OTHER (provide detail), Correspondence may be peer-reviewed at the editors‚Äô discretion',
                               'OTHER (provide detail), It says letters may be peer-reviewed',
                               'OTHER (provide detail), The Letter may be sent for peer review.',
                               'OTHER (provide detail), Correspondence may be peer-reviewed at the editors discretion.',
                               'OTHER (provide detail), . Correspondence may be peer-reviewed at the editors discretion.',
                               'OTHER (provide detail), Correspondence may be peer-reviewed at the editors discretion.',
                               'UNCLEAR (provide details): says will be considered by the editors and published after consultation or peer review',
                               'OTHER (provide detail), ...will be considered by the editors and published after consultation or peer review.',
                               'OTHER (provide detail), Correspondence may be peer-reviewed at the editors’ discretion',
                               'OTHER (provide detail), Correspondence may be peer-reviewed at the editors’ discretion.',
                               'OTHER (provide detail), Matters Arising articles will either be accepted or rejected based on editorial evaluation and/or reviewers comments',
                               'OTHER (provide detail), may or may not be reviewed',
                               'OTHER (provide detail), may be peer-reviewed at the editors discretion',
                               'may be at the editors discretion',
                               'UNCLEAR, Comments may be peer reviewed.',
                               'OTHER (provide detail), All letters will be subjected to editorial reviewand decision before acceptance and may be sent for peer review at the discretion of the editor.',
                               'OTHER (provide detail), . Correspondence may be peer-reviewed at the editors’ discretion.',
                               'OTHER (provide detail), Letters may be reviewed.',
                               'OTHER (provide detail), All letters will be subjected to editorial review and decision before acceptance and may be sent for peer review at the discretion of the editor',
                               'OTHER (provide detail), may be subject to peer review',
                               'OTHER (provide detail), may be peer reviewed',
                               'OTHER (provide detail), may be peer-reviewed at the editors discretion',
                               'UNCLEAR, The editors will decide how to proceed on the basis of the potential interest to readers, importance and timeliness of the contribution.  Matters Arising submissions that meet npj Quantum Information initial selection criteria are sent to the authors of the original paper for a formal response. The comments and formal response may then be sent to independent referees.',
                               'OTHER (provide detail), may be subject to peer review at the editors discretion. Short reports of research work will be peer reviewed.',
                               'OTHER (provide details), All letters will be subjected to editorial reviewand decision before acceptance and may be sent for peer review at the discretion of the editor. ',
                               'UNCLEAR (provide detail), It states articles will be sent to the authors of the original article and published at the discretion of the editors.') ~ 'Editor discretion',
           TRUE ~ peerReviewed # if none of the above applies then keep current column content
         ),
         peerReviewed = factor(peerReviewed, levels = c('NOT STATED', 'NO', "Editor discretion", "YES")))

# save the processed data 
save(policyData, file = here('data', 'processed', 'dataPolicy.RData'))

# practice data
practiceData <- read_csv(here('data','primary','dataPractice.csv'))

# munging
practiceData <- practiceData %>% 
  rename(article_id = id, isPPPR = `Is article itself PPPR`, linkedPPPR = `Is article linked to PPPR`) %>% # rename columns
  filter(exclusion != "EXTRA") %>% # remove any extra coding we did
  mutate(across(c(exclusion, isPPPR, linkedPPPR), as.logical)) %>%# make columns 'logical' type
  mutate(journal = str_replace_all(article_id, "[:digit:]", ""), # extract journal name by removing numbers from article id
         journal = str_replace_all(journal, "_", " "), # and removing underscores
         journal = str_trim(journal)) %>%# and removing whitespace from end of string
  select(journal, article_id, everything()) # reorder columns

# quality checks
practiceData %>%
  check_that(
  is.logical(exclusion), # column should be logical type
  is.logical(isPPPR), # column should be logical type
  is.logical(linkedPPPR), # column should be logical type
  if (exclusion == T) !is.na(`exclusion explanation`), # if article excluded, should be an explanation
  if (exclusion == T) is.na(isPPPR) & is.na(linkedPPPR), # if article excluded, should be nothing in isPPPR and linkedPPPR
  if (exclusion == F) !is.na(isPPPR), # if article not excluded, should be coding in isPPPR
  if (isPPPR == T) is.na(linkedPPPR), # if isPPPR is TRUE then should be nothing in linkedPPPR
  if (isPPPR == F) !is.na(linkedPPPR)) %>%
  summary()

# check data against validation rules
out <- confront(practiceData, rules)
summary(out) # view details about any validation errors

# for every journal we should have exactly ten articles that have T/F in the linked PPPR column
journal_n <- practiceData %>% 
  filter(exclusion == F, isPPPR == F) %>% # remove the excluded articles and articles identified as themselves being PPPR
  filter(journal == "WILDLIFE MONOGRAPHS") %>% # do not check the journal that only published 6 articles in 2018
  count(journal)

rule <- validator(n == 10)
out  <- confront(journal_n , rule)
summary(out)

# save the processed data 
save(practiceData, file = here('data', 'processed', 'dataPractice.RData'))
