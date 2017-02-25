setwd("~/Documents/MSDS/Data Mining Final/Script")
suppressMessages({
  library(caret); library(DAAG); library(car); library(PerformanceAnalytics); library(dplyr); library(readr); 
  library(glmnet); library(htmlTable); library(ggthemes); library(boot); library(lubridate); library(randomForest); 
  library(party); library(ggplot2); library(scales); library(ROCR); library(lubridate); library(reshape2);
})

set.seed(12345)


##############################################################
##
## LOAD DATA
## 
##############################################################

case     <- read_csv('../Data/caseCenteredVote.csv')
justice  <- read_csv('../Data/justiceCenteredVote.csv')

case %>% str
justice %>% str

case %>%
  filter(docket == '00-1853') %>%
  select(caseDisposition)

##############################################################
##
## EXAMINE MISSINGNESS
## 
##############################################################


# count number of NAs in each column
case %>% summarize_each(funs(sum(is.na(.)) > 3000)) 
justice %>% summarize_each(funs(sum(is.na(.)) > 3000))

# remove columns with excessive missing NAs, except for columns we are interested in (e.g. direction, opinion)
# also remove outcome variables
case %>%
  select(-petitionerState, -respondentState, -adminAction, -adminActionState, -caseOriginState, 
         -caseSourceState, -authorityDecision2, threeJudgeFdc, partyWinning,
         -lawType, -lawMinor, -lawSupp, -sctCite, -dateRearg, -threeJudgeFdc, 
         -caseOrigin, -majOpinWriter, -majOpinAssigner, -ledCite, -usCite, 
         -decisionType, -partyWinning, -declarationUncon, -caseDispositionUnusual,
         -petitioner, -respondent, -issue, -caseSource, -caseId, -docketId, -caseIssuesId, 
         -lexisCite, -docket, -caseName, -voteId, -decisionDirection, -decisionDirectionDissent) %>%
  filter(caseDisposition == 2 | caseDisposition == 3) -> case2

justice %>%
  select(-petitionerState, -respondentState, -adminAction, -adminActionState, -caseOriginState, 
         -caseSourceState, -authorityDecision2, -respondentState, 
         -lawSupp, -lawMinor, -firstAgreement, -secondAgreement, 
         -usCite, -sctCite, -ledCite, -dateRearg, -caseOrigin,
         -threeJudgeFdc, -partyWinning, -lawType, -majOpinWriter, -majOpinAssigner,
         -decisionType, -partyWinning, -declarationUncon, -caseDispositionUnusual,
         -petitioner, -respondent, -issue, -caseSource, -caseId, -docketId, -caseIssuesId, 
         -lexisCite, -docket, -caseName, -voteId, -decisionDirection, -decisionDirectionDissent) %>%
  filter(caseDisposition == 2 | caseDisposition == 3) -> justice2

# function for graph of missing data
ggplot_missing <- function(x, title){ 
  # from http://www.njtierney.com/r/missing%20data/rbloggers/2015/12/01/ggplot-missing-data/
  x %>% is.na %>% melt %>%
    ggplot(data = ., aes(x = Var2, y = Var1)) +
    geom_raster(aes(fill = value)) +
    scale_fill_grey(name = "", labels = c("Present","Missing")) +
    theme_fivethirtyeight() + theme(axis.text.x  = element_text(angle=45, vjust=0.5), 
                                    axis.title  = element_text(), 
                                    panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
    xlab("Variables in Dataset") + ylab( "Rows / observations") + ggtitle(title)
}

ggplot_missing(case2    , "Missingness Analysis - Cases")
ggplot_missing(justice2 , "Missingness Analysis - Justices")

##############################################################
##
## REMOVE MISSINGNESS AND ADD COLUMN VARIABLES
## 
##############################################################


## case data #################################################

# remove NAs
case2 %>%
  na.omit() -> case3

# only select cases that have been affirmed or reversed by the justices
case3 %>% 
  filter(caseDisposition == 2 | caseDisposition == 3) %>%
  mutate(caseDisposition = ifelse(caseDisposition == 2, 1, 0)) -> case_data

# factorize
case_data$jurisdiction             <- as.factor(case_data$jurisdiction)
case_data$lcDisagreement           <- as.factor(case_data$lcDisagreement)
case_data$certReason               <- as.factor(case_data$certReason)
case_data$lcDisposition            <- as.factor(case_data$lcDisposition)
case_data$lcDispositionDirection   <- as.factor(case_data$lcDispositionDirection)
case_data$precedentAlteration      <- as.factor(case_data$precedentAlteration)
case_data$voteUnclear              <- as.factor(case_data$voteUnclear)
case_data$issueArea                <- as.factor(case_data$issueArea)
case_data$authorityDecision1       <- as.factor(case_data$authorityDecision1)
case_data$splitVote                <- as.factor(case_data$splitVote)
case_data$vote                     <- as.factor(case_data$splitVote)
case_data$opinion                  <- as.factor(case_data$splitVote)
case_data$direction                <- as.factor(case_data$splitVote)
case_data$majority                 <- as.factor(case_data$splitVote)
case_data$chief                    <- as.factor(case_data$chief)
case_data$caseDisposition          <- as.factor(case_data$caseDisposition) # 0 for Reverse; 1 for Affirm

# sanity check
case_data %>% str
levels(case_data$caseDisposition)


## justice data ##############################################

justice2 %>%
  filter(caseDisposition == 2 | caseDisposition == 3) %>%
  mutate(caseDisposition = ifelse(caseDisposition == 2, 1, 0)) %>%
  na.omit -> justice_data

# factorize
justice_data$decisionType             <- as.factor(justice_data$decisionType)
justice_data$petitioner               <- as.factor(justice_data$petitioner)
justice_data$respondent               <- as.factor(justice_data$respondent)
justice_data$jurisdiction             <- as.factor(justice_data$jurisdiction)
justice_data$caseSource               <- as.factor(justice_data$caseSource)
justice_data$lcDisagreement           <- as.factor(justice_data$lcDisagreement)
justice_data$certReason               <- as.factor(justice_data$certReason)
justice_data$lcDisposition            <- as.factor(justice_data$lcDisposition)
justice_data$lcDispositionDirection   <- as.factor(justice_data$lcDispositionDirection)
justice_data$declarationUncon         <- as.factor(justice_data$declarationUncon)
justice_data$caseDispositionUnusual   <- as.factor(justice_data$caseDispositionUnusual)
justice_data$precedentAlteration      <- as.factor(justice_data$precedentAlteration)
justice_data$voteUnclear              <- as.factor(justice_data$voteUnclear)
justice_data$issue                    <- as.factor(justice_data$issue)
justice_data$issueArea                <- as.factor(justice_data$issueArea)
justice_data$decisionDirection        <- as.factor(justice_data$decisionDirection)
justice_data$decisionDirectionDissent <- as.factor(justice_data$decisionDirectionDissent)
justice_data$authorityDecision1       <- as.factor(justice_data$authorityDecision1)
justice_data$splitVote                <- as.factor(justice_data$splitVote)
justice_data$caseDisposition          <- as.factor(justice_data$caseDisposition) # 0 for Reverse; 1 for Affirm

# sanity check
justice_data %>% str
levels(justice_data$caseDisposition)

# recheck NAs
case_data    %>% summarize_each(funs(sum(is.na(.)))) 
justice_data %>% summarize_each(funs(sum(is.na(.))))

##############################################################
##
## COLUMN INFORMATION FOR CASE AND JUSTICE DATA
## 
##############################################################

### case_data columns ########################################
# caseID - first four digits are the term year. the next four digits are the case within the term (starting at 001 and counting up)
# docketId - first four digits are term year. next four are case within term. last two are number of dockets consolidated under U.S. reports citation
# caseIssuesId - includes number of issues and legal provisions within the case
# voteId - includes indication of split vote (01 for 1 vote; 02 if split vote) and vote in case
# dateDecision - year, month, and day that the Court announced its decision in the case.
# decisionType - decision of court and whether an oral argument was heard
# lexisCite - unique citation to case from  LEXIS
# term - year
# naturalCourt - period during which no personnel change occurs; values divide the Courts into strong natural courts, each of which begins when the Reports first specify that the new justice is present but not necessarily participating in the reported case. Similarly, a natural court ends on the date when the Reports state that an incumbent justice has died, retired, or resigned.
# chief - Chief Justice of Supreme Court
# docket - docket number Supreme Court has assigned to case. Prior to 1971, dockets could be duplicates
# caseName - name of Supreme Court case
# dateArgument - day, month, and year that the case was orally argued before the Court
# petitioner - refers to the party who petitioned the Supreme Court to review the case
# respondent - see above
# jurisdiction - The Court uses a variety of means whereby it undertakes to consider cases that it has been petitioned to review. The most important ones are the writ of certiorari, the writ of appeal, and for legacy cases the writ of error, appeal, and certification. 
# caseSource - identifies the court whose decision the Supreme Court reviewed
# lcDisagreement - indicates that the Supreme Court's majority opinion mentioned that one or more of the members of the court whose decision the Supreme Court reviewed dissented 
# certReason - provides the reason, if any, that the Court gives for granting the petition for certiorari
# lcDisposition - whether the court below the Supreme Court---typically a federal court of appeals or a state supreme court---affirmed, reversed, remanded, etc. the decision of the court it reviewed---typically a trial court
# lcDispositionDirection - specifies whether the decision of the court whose decision the Supreme Court reviewed was itself liberal or conservative as these terms are defined in the direction of decision variable (decisionDirection)
# declarationUncon - entry in this variable indicates that the Court either declared unconstitutional an act of Congress; a state or territorial statute, regulation, or constitutional provision; or a municipal or other local ordinance.
# caseDisposition - treatment the Supreme Court accorded the court whose decision it reviewed is contained in this variable
# caseDispositionUnusual - entry (1) will appear in this variable to signify that the Court made an unusual disposition of the cited case which does not match the coding scheme of the preceding variable
# partyWinning -  indicates whether the petitioning party (i.e., the plaintiff or the appellant) emerged victorious
# precedentAlteration - A "1" will appear in this variable if the majority opinion effectively says that the decision in this case "overruled" one or more of the Court's own precedents
# voteUnclear - votes in a case are those specified in the opinions
# issue - identifies the issue for each decision
# issueArea - separates the issues identified in the preceding variable (issue) into the following larger categories (14 categories)
# decisionDirection - codes the ideological "direction" of the decision (1: Conservative; 2: Liberal; 3: Other)
# decisionDirectionDissent -codes the ideological "direction" of the dissenting decision
# authorityDecision1 - specify the bases on which the Supreme Court rested its decision with regard to each legal provision that the Court considered in the case (see variable lawType)
# splitVote - indicates whether the vote variables (e.g., majVotes, minVotes) pertain to the vote on the first or second issue (or legal provision). Because split votes are so rare over 99 percent of the votes are on the first issue
# majVotes - specifies the number of justices voting in the majority
# minVotes - indicates the number of justices voting in dissent
 
### justice_data columns ######################################
# caseID - first four digits are the term year. the next four digits are the case within the term (starting at 001 and counting up)
# docketId - first four digits are term year. next four are case within term. last two are number of dockets consolidated under U.S. reports citation
# caseIssuesId - includes number of issues and legal provisions within the case
# voteId - includes indication of split vote (01 for 1 vote; 02 if split vote) and vote in case
# dateDecision - year, month, and day that the Court announced its decision in the case.
# decisionType - decision of court and whether an oral argument was heard
# lexisCite - unique citation to case from  LEXIS
# term - year
# naturalCourt - period during which no personnel change occurs; values divide the Courts into strong natural courts, each of which begins when the Reports first specify that the new justice is present but not necessarily participating in the reported case. Similarly, a natural court ends on the date when the Reports state that an incumbent justice has died, retired, or resigned.
# chief - Chief Justice of Supreme Court
# docket - docket number Supreme Court has assigned to case. Prior to 1971, dockets could be duplicates
# caseName - name of Supreme Court case
# dateArgument - day, month, and year that the case was orally argued before the Court
# petitioner - refers to the party who petitioned the Supreme Court to review the case
# respondent - see above
# jurisdiction - The Court uses a variety of means whereby it undertakes to consider cases that it has been petitioned to review. The most important ones are the writ of certiorari, the writ of appeal, and for legacy cases the writ of error, appeal, and certification. 
# caseSource - identifies the court whose decision the Supreme Court reviewed
# lcDisagreement - indicates that the Supreme Court's majority opinion mentioned that one or more of the members of the court whose decision the Supreme Court reviewed dissented 
# certReason - provides the reason, if any, that the Court gives for granting the petition for certiorari
# lcDisposition - whether the court below the Supreme Court---typically a federal court of appeals or a state supreme court---affirmed, reversed, remanded, etc. the decision of the court it reviewed---typically a trial court
# lcDispositionDirection - specifies whether the decision of the court whose decision the Supreme Court reviewed was itself liberal or conservative as these terms are defined in the direction of decision variable (decisionDirection)
# declarationUncon - entry in this variable indicates that the Court either declared unconstitutional an act of Congress; a state or territorial statute, regulation, or constitutional provision; or a municipal or other local ordinance.
# caseDisposition - treatment the Supreme Court accorded the court whose decision it reviewed is contained in this variable
# caseDispositionUnusual - entry (1) will appear in this variable to signify that the Court made an unusual disposition of the cited case which does not match the coding scheme of the preceding variable
# precedentAlteration - A "1" will appear in this variable if the majority opinion effectively says that the decision in this case "overruled" one or more of the Court's own precedents
# voteUnclear - votes in a case are those specified in the opinions
# issue - identifies the issue for each decision
# issueArea - separates the issues identified in the preceding variable (issue) into the following larger categories (14 categories)
# decisionDirection - codes the ideological "direction" of the decision
# decisionDirectionDissent -codes the ideological "direction" of the dissenting decision
# authorityDecision1 - specify the bases on which the Supreme Court rested its decision with regard to each legal provision that the Court considered in the case (see variable lawType)
# splitVote - indicates whether the vote variables (e.g., majVotes, minVotes) pertain to the vote on the first or second issue (or legal provision). Because split votes are so rare over 99 percent of the votes are on the first issue
# majVotes - specifies the number of justices voting in the majority
# minVotes - indicates the number of justices voting in dissent
# justice - unique identification number for each of the justices
# justiceName - string variable indicating the first initial for the five justices with a common surname (Harlan, Johnson, Marshall, Roberts, and White) and last name of each justice
# vote - provides information about each justice's vote in the case
# opinion -  indicates the opinion, if any, that the justice wrote
# direction -  indicates whether the justice cast a liberal (2) or conservative vote (1)
# majority - if vote is in dissent (1) or majority (2)


##############################################################
##
## FEATURE ENGINEERING
##
## congress, unemployment rates, dates, gender
## 
##############################################################

### add congress information #################################
# http://www.infoplease.com/ipa/A0774721.html

case_data %>%
  mutate(senate_dems = 
           ifelse(term==1945 | term==1946, 57,
                  ifelse(term==1947 | term==1948, 45,
                         ifelse(term==1949 | term==1950, 54,
                                ifelse(term==1951 | term==1952, 48,
                                       ifelse(term==1953 | term==1954, 46,
                                              ifelse(term==1955 | term==1956, 48,
                                                     ifelse(term==1957 | term==1958, 49,
                                                            ifelse(term==1959 | term==1960, 64,
                                                                   ifelse(term==1961 | term==1962, 64,
                                                                          ifelse(term==1963 | term==1964, 67,
                                                                                 ifelse(term==1965 | term==1966, 68,
                                                                                        ifelse(term==1967 | term==1968, 64,
                                                                                               ifelse(term==1969 | term==1970, 58,
                                                                                                      ifelse(term==1971 | term==1972, 54,
                                                                                                             ifelse(term==1973 | term==1974, 56,
                                                                                                                    ifelse(term==1975 | term==1976, 61,
                                                                                                                           ifelse(term==1977 | term==1978, 61,
                                                                                                                                  ifelse(term==1979 | term==1980, 58,
                                                                                                                                         ifelse(term==1981 | term==1982, 46,
                                                                                                                                                ifelse(term==1983 | term==1984, 46,
                                                                                                                                                       ifelse(term==1985 | term==1986, 47,
                                                                                                                                                              ifelse(term==1987 | term==1988, 55,
                                                                                                                                                                     ifelse(term==1989 | term==1990, 55,
                                                                                                                                                                            ifelse(term==1991 | term==1992, 56,
                                                                                                                                                                                   ifelse(term==1993 | term==1994, 57,
                                                                                                                                                                                          ifelse(term==1995 | term==1996, 48,
                                                                                                                                                                                                 ifelse(term==1997 | term==1998, 45,
                                                                                                                                                                                                        ifelse(term==1999 | term==2000, 45,
                                                                                                                                                                                                               ifelse(term==2001 | term==2002, 50,
                                                                                                                                                                                                                      ifelse(term==2003 | term==2004, 48,
                                                                                                                                                                                                                             ifelse(term==2005 | term==2006, 44,
                                                                                                                                                                                                                                    ifelse(term==2007 | term==2008, 49,
                                                                                                                                                                                                                                           ifelse(term==2009 | term==2010, 57,
                                                                                                                                                                                                                                                  ifelse(term==2011 | term==2012, 51,
                                                                                                                                                                                                                                                         ifelse(term==2013 | term==2014, 54, 44)))))))))))))))))))))))))))))))))))) %>%
  mutate(senate_reps = 
           ifelse(term==1945 | term==1946, 38,
                  ifelse(term==1947 | term==1948, 51,
                         ifelse(term==1949 | term==1950, 42,
                                ifelse(term==1951 | term==1952, 47,
                                       ifelse(term==1953 | term==1954, 48,
                                              ifelse(term==1955 | term==1956, 47,
                                                     ifelse(term==1957 | term==1958, 47,
                                                            ifelse(term==1959 | term==1960, 34,
                                                                   ifelse(term==1961 | term==1962, 36,
                                                                          ifelse(term==1963 | term==1964, 33,
                                                                                 ifelse(term==1965 | term==1966, 32,
                                                                                        ifelse(term==1967 | term==1968, 36,
                                                                                               ifelse(term==1969 | term==1970, 42,
                                                                                                      ifelse(term==1971 | term==1972, 44,
                                                                                                             ifelse(term==1973 | term==1974, 42,
                                                                                                                    ifelse(term==1975 | term==1976, 37,
                                                                                                                           ifelse(term==1977 | term==1978, 38,
                                                                                                                                  ifelse(term==1979 | term==1980, 41,
                                                                                                                                         ifelse(term==1981 | term==1982, 53,
                                                                                                                                                ifelse(term==1983 | term==1984, 54,
                                                                                                                                                       ifelse(term==1985 | term==1986, 53,
                                                                                                                                                              ifelse(term==1987 | term==1988, 45,
                                                                                                                                                                     ifelse(term==1989 | term==1990, 45,
                                                                                                                                                                            ifelse(term==1991 | term==1992, 44,
                                                                                                                                                                                   ifelse(term==1993 | term==1994, 43,
                                                                                                                                                                                          ifelse(term==1995 | term==1996, 52,
                                                                                                                                                                                                 ifelse(term==1997 | term==1998, 55,
                                                                                                                                                                                                        ifelse(term==1999 | term==2000, 55,
                                                                                                                                                                                                               ifelse(term==2001 | term==2002, 50,
                                                                                                                                                                                                                      ifelse(term==2003 | term==2004, 51,
                                                                                                                                                                                                                             ifelse(term==2005 | term==2006, 55,
                                                                                                                                                                                                                                    ifelse(term==2007 | term==2008, 49,
                                                                                                                                                                                                                                           ifelse(term==2009 | term==2010, 41,
                                                                                                                                                                                                                                                  ifelse(term==2011 | term==2012, 47,
                                                                                                                                                                                                                                                         ifelse(term==2013 | term==2014, 45, 54)))))))))))))))))))))))))))))))))))) %>%
  mutate(house_dems = 
           ifelse(term==1945 | term==1946, 243,
                  ifelse(term==1947 | term==1948, 188,
                         ifelse(term==1949 | term==1950, 263,
                                ifelse(term==1951 | term==1952, 234,
                                       ifelse(term==1953 | term==1954, 213,
                                              ifelse(term==1955 | term==1956, 232,
                                                     ifelse(term==1957 | term==1958, 234,
                                                            ifelse(term==1959 | term==1960, 283,
                                                                   ifelse(term==1961 | term==1962, 262,
                                                                          ifelse(term==1963 | term==1964, 258,
                                                                                 ifelse(term==1965 | term==1966, 295,
                                                                                        ifelse(term==1967 | term==1968, 248,
                                                                                               ifelse(term==1969 | term==1970, 243,
                                                                                                      ifelse(term==1971 | term==1972, 255,
                                                                                                             ifelse(term==1973 | term==1974, 242,
                                                                                                                    ifelse(term==1975 | term==1976, 291,
                                                                                                                            ifelse(term==1977 | term==1978, 292,
                                                                                                                                    ifelse(term==1979 | term==1980, 277,
                                                                                                                                         ifelse(term==1981 | term==1982, 242,
                                                                                                                                                ifelse(term==1983 | term==1984, 269,
                                                                                                                                                       ifelse(term==1985 | term==1986, 253,
                                                                                                                                                              ifelse(term==1987 | term==1988, 258,
                                                                                                                                                                     ifelse(term==1989 | term==1990, 260,
                                                                                                                                                                            ifelse(term==1991 | term==1992, 267,
                                                                                                                                                                                   ifelse(term==1993 | term==1994, 258,
                                                                                                                                                                                          ifelse(term==1995 | term==1996, 204,
                                                                                                                                                                                                 ifelse(term==1997 | term==1998, 207,
                                                                                                                                                                                                        ifelse(term==1999 | term==2000, 211,
                                                                                                                                                                                                               ifelse(term==2001 | term==2002, 212,
                                                                                                                                                                                                                      ifelse(term==2003 | term==2004, 205,
                                                                                                                                                                                                                             ifelse(term==2005 | term==2006, 202,
                                                                                                                                                                                                                                    ifelse(term==2007 | term==2008, 233,
                                                                                                                                                                                                                                           ifelse(term==2009 | term==2010, 256,
                                                                                                                                                                                                                                                  ifelse(term==2011 | term==2012, 193,
                                                                                                                                                                                                                                                         ifelse(term==2013 | term==2014, 201, 188)))))))))))))))))))))))))))))))))))) %>%
  mutate(house_reps = 
           ifelse(term==1945 | term==1946, 190,
                  ifelse(term==1947 | term==1948, 246,
                         ifelse(term==1949 | term==1950, 171,
                                ifelse(term==1951 | term==1952, 199,
                                       ifelse(term==1953 | term==1954, 221,
                                              ifelse(term==1955 | term==1956, 203,
                                                     ifelse(term==1957 | term==1958, 201,
                                                            ifelse(term==1959 | term==1960, 153,
                                                                   ifelse(term==1961 | term==1962, 175,
                                                                          ifelse(term==1963 | term==1964, 176,
                                                                                 ifelse(term==1965 | term==1966, 140,
                                                                                        ifelse(term==1967 | term==1968, 187,
                                                                                               ifelse(term==1969 | term==1970, 192,
                                                                                                      ifelse(term==1971 | term==1972, 180,
                                                                                                             ifelse(term==1973 | term==1974, 192,
                                                                                                                    ifelse(term==1975 | term==1976, 144,
                                                                                                                           ifelse(term==1977 | term==1978, 143,
                                                                                                                                  ifelse(term==1979 | term==1980, 158,
                                                                                                                                         ifelse(term==1981 | term==1982, 192,
                                                                                                                                                ifelse(term==1983 | term==1984, 166,
                                                                                                                                                       ifelse(term==1985 | term==1986, 182,
                                                                                                                                                              ifelse(term==1987 | term==1988, 177,
                                                                                                                                                                     ifelse(term==1989 | term==1990, 175,
                                                                                                                                                                            ifelse(term==1991 | term==1992, 167,
                                                                                                                                                                                   ifelse(term==1993 | term==1994, 176,
                                                                                                                                                                                          ifelse(term==1995 | term==1996, 230,
                                                                                                                                                                                                 ifelse(term==1997 | term==1998, 226,
                                                                                                                                                                                                        ifelse(term==1999 | term==2000, 223,
                                                                                                                                                                                                               ifelse(term==2001 | term==2002, 221,
                                                                                                                                                                                                                      ifelse(term==2003 | term==2004, 229,
                                                                                                                                                                                                                             ifelse(term==2005 | term==2006, 231,
                                                                                                                                                                                                                                    ifelse(term==2007 | term==2008, 198,
                                                                                                                                                                                                                                           ifelse(term==2009 | term==2010, 178,
                                                                                                                                                                                                                                                  ifelse(term==2011 | term==2012, 242,
                                                                                                                                                                                                                                                         ifelse(term==2013 | term==2014, 234, 246)))))))))))))))))))))))))))))))))))) -> case_data2


### add economic indicator information #####################

# unemployment rate
                                                                                                                                                                                                                                                                              
index <- c(3.9, 3.9, 3.8, 5.9, 5.3, 3.3, 3.0, 2.9, 5.5, 
           4.4, 4.1, 4.3, 6.8, 5.5, 5.5, 6.7, 5.5, 5.7, 5.2,     
           4.5, 3.8, 3.8, 3.6, 3.5, 4.9, 5.9, 5.6, 4.9, 5.6,                                                                                                                                                                                                                                                               
           8.5, 7.7, 7.1, 6.1, 5.8, 7.1, 7.6, 9.7, 9.6, 7.5,                                                                                                                                                                                                                                                               
           7.2, 7.0, 6.2, 5.5, 5.3, 5.6, 6.8, 7.5, 6.9, 6.1,                                                                                                                                                                                                                                                               
           5.6, 5.4, 4.9, 4.5, 4.2, 4.0, 4.7, 5.8, 6.0, 5.5,                                                                                                                                                                                                                                                               
           5.1, 4.6, 4.6, 5.8, 9.3, 9.6, 8.9, 8.1, 7.4, 6.2,                                                                                                                                                                                                                                                               
           5.3, 4.6)

values <- c(1946, 1947, 1948, 1949, 1950, 1951, 1952, 1953, 1954, 1955, 1956, 1957, 1958, 1959, 1960, 1961, 1962, 1963, 1964, 1965, 1966, 
            1967, 1968, 1969, 1970, 1971, 1972, 1973, 1974, 1975, 1976, 1977, 1978, 1979, 1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988,
            1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010,
            2011, 2012, 2013, 2014, 2015, 2016)

case_data2$unemployment_rate <- index[match(case_data2$term, values)]



### add date features #######################################

# format oral argument dates
case_data2$dateArgument <- as.Date(case_data2$dateArgument, "%m/%d/%Y")
# format decision dates
case_data2$dateDecision <- as.Date(case_data2$dateDecision, "%m/%d/%Y")

# add month of argument to data
case_data2 %>%
  mutate(monthArgument = as.numeric(format(case_data2$dateArgument, "%m"))) -> case_data2

# add month of decision to data
case_data2 %>%
  mutate(monthDecision = as.numeric(format(case_data2$dateDecision, "%m"))) -> case_data2

# add span of days between oral argument and the decision
case_data2 %>%
  mutate(dateSpan = abs(difftime(case_data2$dateArgument ,case_data2$dateDecision , units = c("days")))) -> case_data2

# graphically depict span across terms in various ways
case_data2 %>%
  group_by(term) %>%
  summarise(mean = mean(dateSpan)) -> span_mean

ggplot(span_mean, aes(x=term, y=mean, fill=factor(term))) + 
  geom_bar(stat = "identity") + ggtitle("Average Days between Argument and Decision", subtitle="Per Term: 1946 - 2015") +
  guides(fill=F) +
  theme_fivethirtyeight() 

ggplot(case_data2, aes(x=term, y=dateSpan, fill=factor(term))) + 
  geom_boxplot() + ggtitle("Average Days between Argument and Decision", subtitle="Per Term: 1946 - 2015") +
  guides(fill=F) +
  theme_fivethirtyeight() 

ggplot(case_data2, aes(x=term, y=dateSpan, fill=factor(term))) + 
  geom_bin2d()+ ggtitle("Average Days between Argument and Decision", subtitle="Per Term: 1946 - 2015") +
  guides(fill=F) +
  theme_fivethirtyeight() 

ggplot(case_data2, aes(x=term, y=dateSpan, fill=factor(term))) + 
  geom_tile() + ggtitle("Days between Argument and Decision", subtitle="Per Term: 1946 - 2015") +
  guides(fill=F) +
  theme_fivethirtyeight() 


# cases per term

case_data %>%
  group_by(term) %>%
  summarise(count=n()) -> term_count

ggplot(term_count, aes(x=term, y=count, fill=factor(term))) + 
  geom_bar(stat = "identity") + ggtitle("Cases Per Term", subtitle="1946 - 2015") +
  guides(fill=F) +
  theme_fivethirtyeight() 


case_data2 %>% str



### add gender information to justice data #################

justice_data %>%
  mutate(gender = ifelse(justiceName == "SDOConnor" | 
                           justiceName == "SSotomayor" | 
                           justiceName == "RBGinsburg" | 
                           justiceName == "EKagan", "F", "M")) -> justice_data2


#######################################################################
#
# CUSTOM FUNCTIONS
#
#######################################################################

# check CV accuracy function
cv.glm.accuracy <- function(model, df, N=10) {
  stopifnot(nrow(df) %% N == 0)
  df    <- df[order(runif(nrow(df))), ]
  bins  <- rep(1:N, nrow(df) / N)
  folds <- split(df, bins)
  all.accuracy <- vector()
  all_fold = list(1:N)[[1]]
  for (i in 1:length(folds)) {
    current.fold = all_fold[-which(all_fold==7)]
    train <- dplyr::bind_rows(folds[current.fold])
    test  <- folds[-current.fold][[1]]
    current.model <- glm(model, family = binomial(link = "logit"), data = train)
    curr.predict  <-ifelse(predict(current.model, newdata = test, type="response") >.5,1,0)
    curr.accuracy <- sum(curr.predict == test$caseDisposition) / nrow(test)
    all.accuracy <- c(all.accuracy, curr.accuracy[[1]]) 
  }
  return(mean(all.accuracy))
}

# ROC curve function
roc_curve <- function(model, test.set, test.set.response) {
  # output predictions and FPR/TPR rate
  predictions        = predict(model, newdata = test.set, type="response")
  pred.fpr.tpr       = prediction(predictions, test.set.response)
  compare.perf       = performance(pred.fpr.tpr, "tpr", "fpr")
  auc.perf           = performance(pred.fpr.tpr, measure="auc")
  roc.vals           = data.frame(cbind(compare.perf@x.values[[1]], compare.perf@y.values[[1]]))
  colnames(roc.vals) = c("fp", "tp")
  
  # plot curve
  ggplot(roc.vals, aes(x=fp, y=tp, colour="#2980b9")) + 
    labs(x=compare.perf@x.name, y=compare.perf@y.name) +
    scale_x_continuous(labels = percent, limits = c(0,1)) + scale_y_continuous(labels = percent, limits = c(0,1)) + 
    geom_abline(aes(intercept=0, slope=1, colour="#34495e")) + guides(colour = FALSE) + coord_equal() +
    ggtitle(bquote(atop(.("Logistic ROC Curve"), atop(italic(.(paste("AUC: ", round(auc.perf@y.values[[1]],digits=2)))), "")))) + geom_line(size=1.2) +
    theme_fivethirtyeight()
}

auc_values <- function(model, test.set, test.set.response) {
  # output predictions and FPR/TPR rate
  predictions        = predict(model, newdata = test.set, type="response")
  pred.fpr.tpr       = prediction(predictions, test.set.response)
  auc.values         = round(performance(pred.fpr.tpr, "auc")@y.values[[1]], digits=3)
  return(auc.values)
}

roc_values <- function(model, test.set, test.set.response) {
  # output predictions and FPR/TPR rate
  predictions        = predict(model, newdata = test.set, type="response")
  pred.fpr.tpr       = prediction(predictions, test.set.response)
  compare.perf       = performance(pred.fpr.tpr, "tpr", "fpr")
  auc.perf           = performance(pred.fpr.tpr, measure="auc")
  roc.vals           = data.frame(cbind(compare.perf@x.values[[1]], compare.perf@y.values[[1]]))
  colnames(roc.vals) = c("fp", "tp")
  return(roc.vals)
}



#######################################################################
#
# COURT CASE ANALYSIS: LOGISTIC REGRESSION
#
#######################################################################


# Model Selection #####################################################

### Create data for fold validation (want even number of rows) ########
training_indices = sample(1:nrow(case_data), 2)
case_fold = case_data2[-training_indices,]

case_fold %>%
  select(-caseDisposition) -> case_test_log

case_fold$caseDisposition
# use hybrid stepwise selection
c.null <- glm(caseDisposition~1,  data=case_fold, family = binomial(link = "logit"))
c.full <- glm(caseDisposition~.,  data=case_fold, family = binomial(link = "logit"))

both <- step(c.null, scope=list(lower=c.null, upper=c.full), direction="both")
summary(both)

# remove insignificant variables lcDispositionDirection and dateSpan to prevent overfitting

both <- glm(caseDisposition ~ lcDisposition + chief + lcDisagreement + 
              minVotes + certReason + issueArea + precedentAlteration + 
              monthArgument + voteUnclear + authorityDecision1 + monthDecision + 
              naturalCourt, family = binomial(link = "logit"), 
              data = case_fold)
vif(both)
summary(both)
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)           42.631779  22.323937   1.910 0.056173 .  
# lcDisposition2         0.775711   0.269167   2.882 0.003953 ** 
#   lcDisposition3         0.074452   0.271039   0.275 0.783554    
# lcDisposition4         0.465351   0.295923   1.573 0.115825    
# lcDisposition5         0.631076   0.342094   1.845 0.065075 .  
# lcDisposition6         0.994443   0.337531   2.946 0.003217 ** 
#   lcDisposition7         0.046111   0.459950   0.100 0.920143    
# lcDisposition8        -0.107545   0.338705  -0.318 0.750850    
# lcDisposition9         0.834301   0.295845   2.820 0.004801 ** 
#   lcDisposition10        0.880417   0.597911   1.472 0.140889    
# lcDisposition11        1.165867   0.447744   2.604 0.009218 ** 
#   lcDisposition12       -0.204753   0.417152  -0.491 0.623544    
# chiefRehnquist         3.322985   1.474652   2.253 0.024234 *  
#   chiefRoberts           6.182794   2.934523   2.107 0.035125 *  
#   chiefVinson           -5.570519   3.017331  -1.846 0.064867 .  
# chiefWarren           -3.096200   1.471295  -2.104 0.035343 *  
#   lcDisagreement1       -0.591035   0.067642  -8.738  < 2e-16 ***
#   minVotes               0.140478   0.018898   7.434 1.06e-13 ***
#   certReason2            0.199287   0.118135   1.687 0.091613 .  
# certReason3            0.026183   0.179116   0.146 0.883779    
# certReason4           -0.103868   0.218868  -0.475 0.635094    
# certReason5            0.384871   0.310406   1.240 0.215014    
# certReason6           -0.474781   0.387914  -1.224 0.220977    
# certReason7           -0.319445   0.412767  -0.774 0.438983    
# certReason8           11.074177 160.433304   0.069 0.944968    
# certReason9            0.420417   0.743731   0.565 0.571882    
# certReason10          -0.295002   0.114282  -2.581 0.009841 ** 
#   certReason11          -0.309976   0.107752  -2.877 0.004018 ** 
#   certReason12          -0.384076   0.095671  -4.015 5.96e-05 ***
#   certReason13          -0.116829   0.297498  -0.393 0.694537    
# issueArea2            -0.231109   0.099437  -2.324 0.020116 *  
#   issueArea3            -0.435732   0.113420  -3.842 0.000122 ***
#   issueArea4            -0.180246   0.145225  -1.241 0.214551    
# issueArea5            -0.374647   0.276008  -1.357 0.174661    
# issueArea6             0.074638   0.289710   0.258 0.796692    
# issueArea7             0.241649   0.146839   1.646 0.099830 .  
# issueArea8             0.131002   0.095237   1.376 0.168967    
# issueArea9            -0.283901   0.113502  -2.501 0.012374 *  
#   issueArea10           -0.019404   0.137312  -0.141 0.887622    
# issueArea11           -0.773421   0.529842  -1.460 0.144367    
# issueArea12           -0.027212   0.153407  -0.177 0.859206    
# issueArea13            1.244890   0.502265   2.479 0.013192 *  
#   issueArea14           11.315878 324.743762   0.035 0.972203    
# precedentAlteration1  -0.756925   0.196403  -3.854 0.000116 ***
#   monthArgument          0.019944   0.006740   2.959 0.003086 ** 
#   voteUnclear1           0.873990   0.349874   2.498 0.012489 *  
#   authorityDecision12   -0.131450   0.098433  -1.335 0.181736    
# authorityDecision13   -0.249450   0.167429  -1.490 0.136255    
# authorityDecision14   -0.230879   0.096601  -2.390 0.016848 *  
#   authorityDecision15   -0.598456   0.150757  -3.970 7.20e-05 ***
#   authorityDecision16    0.206956   0.684558   0.302 0.762408    
# authorityDecision17   -0.370974   0.142518  -2.603 0.009241 ** 
#   monthDecision          0.023120   0.009736   2.375 0.017565 *  
#   naturalCourt          -0.028384   0.014832  -1.914 0.055665 .  

# Testing ###############################################################

cv.glm.accuracy(formula(both), case_fold, N=20)
# 0.6849315 CV Accuracy Score using Training Set

roc_curve(both, case_fold, case_fold$caseDisposition)

log_auc <- auc_values(both, case_fold, case_fold$caseDisposition)
log_roc <- roc_values(both, case_fold, case_fold$caseDisposition)

#######################################################################
#
# COURT CASE ANALYSIS: RANDOM FORESTS
#
#######################################################################

# update case_data with engineered features
case_data <- case_data2

### training and testing data sets ####################################


### Create randomized training/testing sets (75%/25%) #################
training_indices = sample(1:nrow(case_data), as.integer(nrow(case_data) * 0.75))
case_train = case_data[training_indices,]
case_test = case_data[-training_indices,]

case_test %>%
  select(-caseDisposition) -> case_test1
  
# Fit random forest (bagging can be achieved by setting mytry=p
rf.fit <- randomForest(caseDisposition ~ ., data = case_train)

# variable importance graph
varImpPlot(rf.fit)

# Predict testing data.
predictions = predict(rf.fit, newdata = case_test1)

# confusion matrix for predictions
confusionMatrix(predictions, case_test$caseDisposition)

# Output raw accuracy.
sum(predictions == unlist(case_test$caseDisposition)) / nrow(case_test)
# 0.798768

# We can also get prediction probabilities.
# These are computed from the random forest (the percentage of the number of trees in our ensemble that
# voted for the particular observation (x/500))
predict(rf.fit, newdata = case_test1, type = "prob")
# ntree corresponds to B
# mtry is m


### Create training/testing sets based on time (75%/25%) ################
case_data %>% 
  filter(term < 1990) -> case_train2

case_data %>% 
  filter(term > 1990) %>% 
  select(-caseDisposition) -> case_test2

case_data %>% 
  filter(term > 1990) -> case_test2y

# Fit random forest (bagging can be achieved by setting mytry=p
rf.fit2 <- randomForest(caseDisposition ~ ., data = case_train2)

# variable importance graph
varImp(rf.fit2)

# Predict testing data.
predictions2 = predict(rf.fit2, newdata = case_test2)

# confusion matrix for predictions
confusionMatrix(predictions2, case_test2y$caseDisposition)

# Output raw accuracy.
sum(predictions2 == unlist(case_test2y$caseDisposition)) / nrow(case_test)
# 0.5393566

# We can also get prediction probabilities.
# These are computed from the random forest (the percentage of the number of trees in our ensemble that
# voted for the particular observation (x/500))
predict(rf.fit2, newdata = case_test2, type = "prob")
# ntree corresponds to B
# mtry is m


#prepare model for ROC Curve https://www.r-bloggers.com/part-3-random-forests-and-model-selection-considerations/
test.forest1 = predict(rf.fit, newdata = case_test1, type = "prob")
forestpred1  = prediction(test.forest1[,2], case_test$caseDisposition)
forestperf1  = performance(forestpred1, "tpr", "fpr")
auc.perf1    = performance(forestpred1, measure="auc")

roc.vals1 = data.frame(cbind(forestperf1@x.values[[1]], forestperf1@y.values[[1]]))
colnames(roc.vals1) <- c("fp", "tp")

ggplot(roc.vals1, aes(x=fp, y=tp, colour="#2980b9")) + 
  labs(x=forestperf1@x.name, y=forestperf1@y.name) +
  scale_x_continuous(labels = percent, limits = c(0,1)) + scale_y_continuous(labels = percent, limits = c(0,1)) + 
  geom_abline(aes(intercept=0, slope=1, colour="#34495e")) + guides(colour = FALSE) + coord_equal() +
  ggtitle(bquote(atop(.("ROC Curve"), atop(italic(.(paste("AUC: ", round(auc.perf1@y.values[[1]],digits=2)))), "")))) + geom_line(size=1.2) +
  theme_fivethirtyeight()

#prepare model for ROC Curve https://www.r-bloggers.com/part-3-random-forests-and-model-selection-considerations/
test.forest2 = predict(rf.fit2, newdata = case_test2, type = "prob")
forestpred2  = prediction(test.forest2[,2], case_test2y$caseDisposition)
forestperf2  = performance(forestpred2, "tpr", "fpr")
auc.perf2    = performance(forestpred2, measure="auc")
 
roc.vals2 = data.frame(cbind(forestperf2@x.values[[1]], forestperf2@y.values[[1]]))
colnames(roc.vals2) <- c("fp", "tp")

# Compare Random Forests with two different training sets

# round auc values
auc.values1 = round(performance(forestpred1, "auc")@y.values[[1]], digits=3)
auc.values2 = round(performance(forestpred2, "auc")@y.values[[1]], digits=3)

both_rf.roc <- rbind(        
  cbind(ord = 1, mod = 'logi', type = paste("1st Random Forest AUC (Random Train/Test) :: ",     auc.values1),  roc.vals1), 
  cbind(ord = 2, mod = 'logi', type = paste("2nd Random Forest AUC (Time Based Train/Test) :: ", auc.values2),  roc.vals2) %>% 
    arrange(ord))

both.rf <- ggplot(both_rf.roc %>% filter(mod=="logi"), aes(x=fp, y=tp, group=as.factor(ord), colour=type)) + 
  labs(x="False Positive Rate", y="True Positive Rate") +
  scale_x_continuous(labels = percent, limits = c(0,1)) +
  scale_y_continuous(labels = percent, limits = c(0,1)) + 
  geom_abline(aes(intercept=0, slope=1)) +
  ggtitle("Random Forest ROC Curves", subtitle = "Train/Test Set Comparison") + 
  coord_equal() + geom_line(size=1.2) + theme(legend.position="bottom") +
  theme_fivethirtyeight()

# ROC curve comparing both random forests
both.rf



# Compare Random Forest with Logistic Regression


log_auc <- auc_values(both, case_fold, case_fold$caseDisposition)
log_roc <- roc_values(both, case_fold, case_fold$caseDisposition)

# round auc values
auc.values1 = round(performance(forestpred1, "auc")@y.values[[1]], digits=3)


log_rf.roc <- rbind(        
  cbind(ord = 1, mod = 'logi', type = paste("Random Forest AUC (Random Train/Test) :: ",     auc.values1),  roc.vals1), 
  cbind(ord = 2, mod = 'logi', type = paste("Logistic Regression Model AUC :: ",             log_auc),  log_roc) %>% 
    arrange(ord))

log.rf <- ggplot(log_rf.roc %>% filter(mod=="logi"), aes(x=fp, y=tp, group=as.factor(ord), colour=type)) + 
  labs(x="False Positive Rate", y="True Positive Rate") +
  scale_x_continuous(labels = percent, limits = c(0,1)) +
  scale_y_continuous(labels = percent, limits = c(0,1)) + 
  geom_abline(aes(intercept=0, slope=1)) +
  ggtitle("Random Forest + Logistic ROC Curves", subtitle = "Model Comparison") + 
  coord_equal() + geom_line(size=1.2) + theme(legend.position="bottom") +
  theme_fivethirtyeight()

# ROC curve comparing both random forests
log.rf

#######################################################################
#
# COURT CASE ANALYSIS: SVM
#
#######################################################################


library(kernlab)

### Create randomized training/testing sets (75%/25%) #################
training_indices = sample(1:nrow(case_data), as.integer(nrow(case_data) * 0.75))
case_train = case_data[training_indices,]
case_test = case_data[-training_indices,]

svm.fit <- ksvm(caseDisposition ~ ., data = case_train, type="C-svc", kernel="vanilladot", C=100)
# This 'C' is not the same as one in the notes (the cap on sum of epsilons and forces them to be smaller than C. how much violation do you allow?)
# This 'C' is referring to the actual cost of a violation (a large C makes it expensive to do a violation)
# Predict testing data.
predictions = predict(svm.fit, newdata = case_test)

# Output raw accuracy.
sum(predictions == unlist(case_test[,"caseDisposition"])) / nrow(case_test)
# 0.6475017

# We can also get probabilities.
svm.fit = ksvm(caseDisposition ~ ., data = case_train, type="C-svc", kernel="vanilladot", C=100, prob.model = TRUE)
predict(svm.fit, newdata = case_test, type = "probabilities")

#prepare model for ROC Curve https://www.r-bloggers.com/part-3-random-forests-and-model-selection-considerations/
test.svm = predict(svm.fit, newdata = case_test, type = "probabilities")
svm_pred  = prediction(test.svm[,2], unlist(case_test[,"caseDisposition"]))
svmperf  = performance(svm_pred, "tpr", "fpr")
auc.perf.svm    = performance(svm_pred, measure="auc")

roc.vals.svm = data.frame(cbind(svmperf@x.values[[1]], svmperf@y.values[[1]]))
colnames(roc.vals.svm) <- c("fp", "tp")

# Compare Random Forest, Logistic Regression, and SVM

# round auc values
auc.values.svm = round(performance(svm_pred, "auc")@y.values[[1]], digits=3)


log_rf_svm.roc <- rbind(        
  cbind(ord = 1, mod = 'logi', type = paste("Random Forest AUC (Random Train/Test) :: ",     auc.values1),  roc.vals1), 
  cbind(ord = 2, mod = 'logi', type = paste("Logistic Regression Model AUC :: ",             log_auc),  log_roc),
  cbind(ord = 3, mod = 'logi', type = paste("SVM AUC :: ",                                   auc.values.svm),  roc.vals.svm) %>% 
    arrange(ord))

log.rf.svm <- ggplot(log_rf_svm.roc %>% filter(mod=="logi"), aes(x=fp, y=tp, group=as.factor(ord), colour=type)) + 
  labs(x="False Positive Rate", y="True Positive Rate") +
  scale_x_continuous(labels = percent, limits = c(0,1)) +
  scale_y_continuous(labels = percent, limits = c(0,1)) + 
  geom_abline(aes(intercept=0, slope=1)) +
  ggtitle("Random Forest + Logistic + SVM ROC Curves", subtitle = "Model Comparison") + 
  coord_equal() + geom_line(size=1.2) + theme(legend.position="bottom") +
  theme_fivethirtyeight()

# ROC curve comparing both random forests
log.rf.svm


#######################################################################
#
# COURT CASE ANALYSIS: ANALYSIS OF TEXT
#
#######################################################################
library(tm)
caseCorpus <- VCorpus(DirSource("../Data/Arguments/", encoding = "UTF-8"), readerControl = list(language = "lat"))

inspect(caseCorpus[1:2])
caseCorpus.clean[[1]]$content

caseCorpus.clean = tm_map(caseCorpus, stripWhitespace)                          # remove extra whitespace
caseCorpus.clean = tm_map(caseCorpus.clean, removeNumbers)                      # remove numbers
caseCorpus.clean = tm_map(caseCorpus.clean, removePunctuation)                  # remove punctuation
caseCorpus.clean = tm_map(caseCorpus.clean, content_transformer(tolower))       # ignore case
caseCorpus.clean = tm_map(caseCorpus.clean, removeWords, stopwords("english"))  # remove stop words

caseCorpus.tfidf = DocumentTermMatrix(caseCorpus.clean, control = list(weighting = weightTfIdf, removePunctuation = T, removeNumbers = T))

caseCorpus.tfidf2 <- removeSparseTerms(caseCorpus.tfidf, .99)

freq=colSums(as.matrix(caseCorpus.tfidf2))
head(freq,10)

plot(sort(freq, decreasing = T),col="blue",main="Word TF-IDF frequencies", xlab="TF-IDF-based rank", ylab = "TF-IDF")


tail(sort(freq),n=10)

high.freq=tail(sort(freq),n=20)
hfp.df=as.data.frame(sort(high.freq))
hfp.df$names <- rownames(hfp.df) 

ggplot(hfp.df, aes(reorder(names,high.freq), high.freq), fill=) +
  geom_bar(stat="identity") + coord_flip() + 
  xlab("Terms") + ylab("Frequency") +
  ggtitle("Term frequencies") +
  theme_fivethirtyeight()

caseCorpus.m <- as.matrix(caseCorpus.tfidf2)
caseCorpus.m2 <- as.data.frame(caseCorpus.m)
response10 <- c(1,0,1,0,0,0,1,1,0,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,0,0,0,1,1,1,1,1,0,0,0,1,0,0,0,0,0,
              1,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,1,1,0,
              1,1,0,0,1,0,0,0,0,0,1,0,0,1,0,0,0,0,0,1,1,1,0,0,1,1,0,1)
caseCorpus_df <- cbind(response10, caseCorpus.m2)

caseCorpus_df$response10 <- factor(caseCorpus_df$response10)


