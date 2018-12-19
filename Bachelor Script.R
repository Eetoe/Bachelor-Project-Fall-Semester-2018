#Set of the script
set.seed(314159)

#########################
####### Libraries #######
#########################
library(plyr) #Data wrangling
library(dplyr) #Data wrangling
library(tidyr) #Data wrangling
library(tidytext) #Text editing
library(udpipe) #NLP
library(stringr) #Editing strings, i.e. text
library(topicmodels) #For topic models
library(ggplot2) #For plotting
library(textstem) #For stemming and lemmatizing
library(tm) #Text mining, but also for dealing with the corpus and making dtm
library(caret) #For splitting into folds for CV
library(brms)
library(lme4)
library(lmerTest)
library(gridExtra)
library(grid)
# library(pacman)
# library(brmstools)
# p_load(brms)



#remove.packages("tidytext")

#install.packages("gridExtra")


#------------------------

#########################
####### Load Data #######
#########################
#------ Make a list of file paths ------
file_list = list.files(path = "C:/Users/Martin/Documents/UNI/5th semester/Bachelor/Topic Modelling", pattern = 'chat', full.names = TRUE, recursive=T)
if (exists('FullData')){rm(FullData)}

file_participants = read.csv("C:\\Users\\Martin\\Documents\\UNI\\5th semester\\Bachelor\\Topic Modelling\\Data\\2017.02.20\\B\\nps_20170220-141247_participants.csv", sep = ",")
file_score = read.csv("C:\\Users\\Martin\\Documents\\UNI\\5th semester\\Bachelor\\Topic Modelling\\Data\\2017.02.20\\B\\nps_20170220-141247_solution.csv", sep = ",")
#file_list

#------ Make empty data frame in which to append the data ------
d_load = data.frame()

d_participants = data.frame()

d_score = data.frame()

a

#------ Make function for loop ------
#Make function to make pair variable
pair_func = function(m){
  l_sort = sort(m)
  pair = paste(l_sort[1], l_sort[2], sep = "&")
  
  return(pair)
}

#------ Loop to load files ------
#For testing one runthrough of the loop
file = file_list[1]

#Loop through files: Load files, make variables, save in data frame
for (file in file_list){
  #------ Read files ------
  d=read.csv(file)
  file2=gsub('chat','metadata',file)
  d_meta=read.csv(file2)
  
  file3=gsub('chat','participants',file)
  d_part = read.csv(file3)
  
  file4 = gsub('chat','solution',file)
  d_s = read.csv(file4)
  
  
  #------ make condition ------
  #Make network variable
  d$network = d_meta$topology
  
  #Make information variable
  d$information = d_meta$condition
  
  #Make condition variable
  d$condition = paste(d$information, d$network, sep = "_")

  
  #------ Make pair ------
  #Make sender shorter
  d$sender = as.character(d$sender)
  d$sender = str_replace(d$sender, "nps_*", "p_")

  #Make receiver shorter
  d$receiver = as.character(d$receiver)
  d$receiver = str_replace(d$receiver, "nps_*", "p_")
  
  #Make pair variable
  d_pair = as.matrix(d[,4:5])
  
  d$pair = apply(d_pair, MARGIN = 1, function(x) pair_func(x))

  d$sender = as.factor(d$sender)
  d$receiver = as.factor(d$receiver)
  
  
  #------ Make group ------
  #Cut file path to make the string used for group
  cut_path =gsub("C:/Users/Martin/Documents/UNI/5th semester/Bachelor/Topic Modelling/Data/",
                 "", file, fixed = TRUE)
  
  cut_path = str_replace(cut_path, "/nps_[0-9]*?[0-9]*", "") %>%
    str_replace("_[a-z]*[.]?[a-z]*", "") %>%
    str_replace_all("[:punct:]", "-")
  
  d$group = cut_path
  
  
  #------ Save data into a data frame ------
  d_load = rbind(d_load, d)
  
  #------ Save participants data ------
  d_part$group = cut_path
  d_part$condition = as.character(unique(d$condition))
  d_part$participant_ID = paste(d_part$player, d_part$group)
  
  
  d_participants = rbind(d_participants, d_part)
  
  #------ Save score data ------
  #Make pair variable
  d_pair2 = as.matrix(d_s[,4:5])
  
  d_s$pair = apply(d_pair2, MARGIN = 1, function(x) pair_func(x))
  
  d$sender = as.factor(d$sender)
  d$receiver = as.factor(d$receiver)
  
  #group and condition
  d_s$group = cut_path
  d_s$condition = as.character(unique(d$condition))
  
  #Standardize rounds
  d_s$round_standard = d_s$round/max(d_s$round)
  
  #Find first round with optimal solution
  d_s = na.omit(d_s)
  d_s$max_score = max(d_s$score)
  d_s$found_max = min(d_s[d_s$score == d_s$max_score,]$round)
  d_s$found_max_s = min(d_s[d_s$score == d_s$max_score,]$round_standard)
  
  
  d_score = rbind(d_score, d_s)
  
}


#Remove unnecessary things from loop
rm(d, d_meta, file, file2, cut_path, d_pair)


#------ Edit variables ------
#Create full data frame with variables as they will be used later on
d_full = d_load

#Message
d_full$message = as.character(d_full$message)
d_full = d_full[d_full$message != "",] #Remove empty messages
d_full = d_full[-(grep("Waiting for connection to partner...", d_full$message)), ]

#Round
#First investigate round 0
d_round0 = d_full[d_full$round == 0, ]
unique(d_round0$group)
#Check solutions - There were no solutions in round 0, meaning that it only contains chat information
#This means that round standardized by number max(round) can be implemented into the loading loop
#Programming is iterative
#file_round0_1 = read.csv("C:\\Users\\Martin\\Documents\\UNI\\5th semester\\Bachelor\\Topic Modelling\\Data\\2017.10.12\\A\\nps_20171012-120224_solution.csv", sep = ",")
#file_round0_2 = read.csv("C:\\Users\\Martin\\Documents\\UNI\\5th semester\\Bachelor\\Topic Modelling\\Data\\2018.02.07\\A\\nps_20180207-075707_solution.csv", sep = ",")
#file_round0_3 = read.csv("C:\\Users\\Martin\\Documents\\UNI\\5th semester\\Bachelor\\Topic Modelling\\Data\\2018.03.21\\A\\nps_20180321-151250_solution.csv", sep = ",")
#file_round0_4 = read.csv("C:\\Users\\Martin\\Documents\\UNI\\5th semester\\Bachelor\\Topic Modelling\\Data\\2018.04.11\\A\\nps_20180411-141443_solution.csv", sep = ",")

#group
d_full = subset(d_full, !(group %in% c("2016-12-09-B1-125327",
                                      "2016-12-09-B2-132333",
                                      "2016-12-14-C-134715",
                                      "2017-04-20-A-154213",
                                      "2017-09-07-A1-113148")))
d_full$group = as.factor(d_full$group)

#Other variables
d_full$condition = as.factor(d_full$condition)
d_full$pair = as.factor(d_full$pair)


#Remove solution files, as they only clog up the environment
rm(file_round0_1, file_round0_2, file_round0_3, file_round0_4, d_round0)

#------ Create tags for documents ------
###Condition
#make dataset and shorten condition names
d_con = data.frame(condition = unique(d_full$condition), condition_short =unique(d_full$condition))
d_con$condition_short = mapvalues(d_con$condition_short, from = c("Known max_Full",
                                          "Known max_Pairs",
                                          "Known max_Ring",
                                          "Unknown max_Full",
                                          "Unknown max_Pairs",
                                          "Unknown max_Ring" ),
          to = c("kmf", "kmp", "kmr", "umf", "ump", "umr"))

#Merge this dataset to d_full
d_full = left_join(d_full, d_con, by = "condition")

#tag for document = one round across all within a condition, cr = condition and round
d_full$tag_cr = paste("C:", d_full$condition_short, "_R:", d_full$round, sep = "")
#tag for docoument = one round across all, gcr = group, round and condition
d_full$tag_gcr = paste("G:", d_full$group, "_C:", d_full$condition_short, "_R:", d_full$round, sep = "")
#tag for documet = conversation, gcrp = group, round, condition and pair
d_full$tag_gcrp = paste("G:", d_full$group, "_C:", d_full$condition_short, "_R:", d_full$round, "_P:", d_full$pair, sep = "")

d_full$tag_cr = as.factor(d_full$tag_cr)
d_full$tag_gcr = as.factor(d_full$tag_gcr)
d_full$tag_gcrp = as.factor(d_full$tag_gcrp)

#Remove variables for tags again (Not necessary)
#rm(d_full$group_n)
rm(d_con)





#------ Get information on participants ------
unique(d_participants$group)

#Crashed sessions
#"2016-12-09-B1-125327"
#"2016-12-09-B2-132333"
#"2016-12-14-C-134715"
#"2017-04-20-A-154213"
#"2017-09-07-A1-113148"

#Sessions where the same participants went through two conditions
#"2017-09-08-A1-084622" "2017-09-08-A2-094414"

#Remove redundant/crashed information
d_participants2 = subset(d_participants, !(group %in% c("2016-12-09-B1-125327",
                                                        "2016-12-09-B2-132333",
                                                        "2016-12-14-C-134715",
                                                        "2017-04-20-A-154213",
                                                        "2017-09-07-A1-113148",
                                                        "2017-09-08-A2-094414")))
#fix gender variable
d_participants2$gender = as.character(d_participants2$gender)
d_participants2$gender = str_replace(d_participants2$gender, "m", "M")
d_participants2$gender = str_replace(d_participants2$gender, "f", "F")
d_participants2$gender = as.factor(d_participants2$gender)

length(unique(d_participants2$participant_ID))

#some participants registered twice
d_part_not_unique = d_participants2 %>% group_by(participant_ID) %>% filter(n()>1)
#ID of those participants
unique(d_part_not_unique$participant_ID) 
#Remove those particpants
d_participants2 = d_participants2[ ! d_participants2$participant_ID %in% unique(d_part_not_unique$participant_ID), ]
#make empty data frame to get one instance of duplicate participants and add them to df again
d_part_single = data.frame()
for (i in unique(d_part_not_unique$participant_ID)){
  tmp = subset(d_part_not_unique, d_part_not_unique$participant_ID == i)[1,]
  d_part_single = bind_rows(d_part_single, tmp)
}

d_participants2 = rbind(d_participants2, d_part_single)

length(unique(d_participants2[d_participants2$condition == "Unknown max_Full", 8]))
#Groups known max_full 13 groups, 1 group also did unknown max ring, 184 participants
#Groups known max_ring 6 groups 94 participants
#Groups known max_pairs 1 groups 6 participants
#Groups unknown max_full 7 groups 102 participants
#Groups unknown max_ring 6 groups + 1 for the group removed, 88 participants + 16
#Groups unknown max_ring 2 groups 14 participants

length(unique(d_participants[d_participants$group == "2017-09-08-A2-094414",]$participant_ID))

length(unique(d_participants2[d_participants2$gender == "F", ]$participant_ID))
length(unique(d_participants2[d_participants2$gender == "M", ]$participant_ID))

max(d_participants2[d_participants2$gender == "F", ]$age)
mean(d_participants2[d_participants2$gender == "F", ]$age)
min(d_participants2[d_participants2$gender == "F", ]$age)

max(d_participants2[d_participants2$gender == "M", ]$age)
mean(d_participants2[d_participants2$gender == "M", ]$age)
min(d_participants2[d_participants2$gender == "M", ]$age)


#------ Inspecting score ------
#Remove crashed groups
d_score = subset(d_score, !(group %in% c("2016-12-09-B1-125327",
                                                        "2016-12-09-B2-132333",
                                                        "2016-12-14-C-134715",
                                                        "2017-04-20-A-154213",
                                                        "2017-09-07-A1-113148")))
#Density plot of score
ggplot(d_score, aes(score))+
  geom_density()



  


#------------------------

#############################
####### Preprocessing #######
#############################
#------ Lemmatization and Convertion to Lower Case ------
#Define language for lemmatization
udmodel = udpipe_download_model(language = "english")

#Prepare the data for the udpipe function
d_lemma = select(d_full, tag_gcr, message) %>%
  rename(doc_id = tag_gcr, text = message)


#Run udpipe for lemmatization and more
#d_lemmatized = udpipe(x = d_lemma, object = udmodel) #Commented out to not accidentally run

#Select the relevant variables and make rename
d_pp = select(d_lemmatized, doc_id, sentence, lemma, upos) %>%
  rename(tag_gcr = doc_id, word = lemma) %>%
  filter(upos != 'PUNCT')

#Convert to lower case
d_pp$word = tolower(d_pp$word)


#------ Standardize Words with High Variation ------
#Transform words made of digits to "number"
d_pp$word = str_replace(d_pp$word, "[0-9]*[,.]?[0-9]+(st|nd|rd|th)?", "numeral")

#Standardize "ahh", "uhh" etc.
d_pp$word = str_replace(d_pp$word, "^([a]?[h])*$", "ahh")
d_pp$word = str_replace(d_pp$word, "^([u]?[h])*$", "uhh")
d_pp$word = str_replace(d_pp$word, "^([o]?[h])*$", "oh")
d_pp$word = str_replace(d_pp$word, "^([h]?[m])*$", "hmm")
d_pp$word = str_replace(d_pp$word, "^([n]?[o])*$", "no")

#Standardize laughing onomatopeia
d_pp$word = str_replace(d_pp$word, "^([h]+[a]*)*$", "laughter")
d_pp$word = str_replace(d_pp$word, "^([a]+[h]+[a]+)+(ha)*$", "laughter")



#------ Make lists of unique words and frequencies ------
#Make df of unique words and frequency to
#1) Help look through for spelling mistakes
#2) Help inform about which words are most (in)frequent to remove them from the analysis
unique_words = d_pp %>% group_by(word) %>% summarize(n()) %>%
  rename(freq = "n()")


#Find total word count
#sum(unique_words$freq) #157,926 unique words

#Get the least frequent words
infreq_words = unique_words[unique_words$freq < 11,]
sum(infreq_words$freq)# Find word count of the infrequent words
sum(infreq_words$freq)/sum(unique_words$freq) #The infrequent words are less than 1 % of the total word count
#The infrequent words are cut out, as it lessens the workload, but also because the model has a hard time dealing with words occuring only a few times
d_pp = anti_join(d_pp, infreq_words, by = "word")

#Unique words with least frequent words removed.
freqwords = unique_words[unique_words$freq > 10,]


#Histogram of word frequencies
ggplot(freqwords, aes(freq))+
  geom_histogram()

#------ Reset Preprocessing ------
#d_pp_save = d_pp   #As the data was after the previous section
d_pp = d_pp_save

unique_words = d_pp %>% group_by(word) %>%
  summarize(n()) %>%
  rename(freq = "n()")
  
freqwords = unique_words[unique_words$freq > 10,]

#View(freqwords)

#------ Correction I: Punctuation and Numbers ------

#Punctuation errors
d_pp$word = str_replace(d_pp$word, "^(¨)$", "NA")
d_pp$word = str_replace(d_pp$word, "^(´)$", "NA")

#For + and +s i define their utf8 code. They're removed under the Remove Words section
utf8forplus = "\u002B"
utf8forpluss = "\u002Bs"

#Smiley
d_pp$word = gsub(":d", ":D", d_pp$word)
#d_pp$word = gsub(":-)", ":)", d_pp$word) #Kept in as it might be seen as a social marker
#d_pp$word = gsub("=)", ":)", d_pp$word) #As above
d_pp$word = gsub(";d", ";D", d_pp$word)


#Numeral
for (row in grep("numeral", d_pp$word)){
  d_pp[row,]$word = "numeral"
  
}
d_pp$word = str_replace(d_pp$word, "^(nr)$", "number")



#'s from let's to us to we
d_pp$word = str_replace(d_pp$word, "'s", "us")
d_pp$word = str_replace(d_pp$word, "^us$", "we")

#´ll
d_pp$word = str_replace(d_pp$word, "´ll", "will")


#remove -, as when it is used in a negative number it should just be number and when used as hyphen it isn't needed
d_pp$word = str_replace(d_pp$word, "^[-]$", "NA")


#------ Correction II: Standardization of Spelling and Expressions ------
#Standardize the last laughing onomatopeia
d_pp$word = str_replace(d_pp$word, "ahaha", "laughter" )
d_pp$word = str_replace(d_pp$word, "hehe", "laughter" )

#Achieve
d_pp$word = str_replace(d_pp$word, "achiev", "achieve" )


#Greetings
d_pp$word = str_replace(d_pp$word, "^(heyy)$", "hey")
d_pp$word = str_replace(d_pp$word, "^(hii)$", "hi")
d_pp$word = str_replace(d_pp$word, "^(hi!)$", "hi")
d_pp$word = str_replace(d_pp$word, "^(hi.)$", "hi")


#Combination - because of lemmatization
d_pp$word = str_replace(d_pp$word, "combus", "combination" )

#Dunno back to one word
d_pp$word = str_replace(d_pp$word, "dun", "dunno" )
d_pp$word = str_replace(d_pp$word, "dunnono", "dunno" )

#Gonna back to one word
d_pp$word = str_replace(d_pp$word, "gon", "gonna" )
d_pp$word = str_replace(d_pp$word, "^(na)$", "NA" )


#Remove J, j was introducing him/herself
d_pp$word = str_replace(d_pp$word, "^(j)$", "NA" )


#Mozart
d_pp$word = str_replace(d_pp$word, "mozzart", "mozart" )

#Remove n't, as it is unclear if it should be the first or last part
d_pp$word = str_replace(d_pp$word, "^(n´t)$", "NA" )


#Okay
d_pp$word = str_replace(d_pp$word, "okey", "okay")
#d_pp$word = str_replace(d_pp$word, "^(ok)$", "okay")
#d_pp$word = str_replace(d_pp$word, "^(oki)$", "okay")
#d_pp$word = str_replace(d_pp$word, "^(k)$", "okay")

#Point
d_pp$word = str_replace(d_pp$word, "^(p)$", "point")
d_pp$word = str_replace(d_pp$word, "^(pt)$", "point")
d_pp$word = str_replace(d_pp$word, "^(pts.)$", "point")

#Pattern
d_pp$word = str_replace(d_pp$word, "patern", "pattern")

#Press
d_pp$word = str_replace(d_pp$word, "^(pre)$", "press")

#Probably
#d_pp$word = str_replace(d_pp$word, "^(prob)$", "probably")

#score
d_pp$word = str_replace(d_pp$word, "^(scor)$", "score")

#See
d_pp$word = str_replace(d_pp$word, "^(se)$", "see")

#Something
d_pp$word = str_replace(d_pp$word, "^(smth)$", "something")
d_pp$word = str_replace(d_pp$word, "^(somthing)$", "something")
d_pp$word = str_replace(d_pp$word, "^(sth)$", "something")

#so
#d_pp$word = str_replace(d_pp$word, "^(sooo)$", "so")

#Remove t as it is multiple mistakes and so cannot be assigned to one word
d_pp$word = str_replace(d_pp$word, "^(t)$", "NA")
d_pp$word = str_replace(d_pp$word, "^(w)$", "NA")

#Thanks
#d_pp$word = str_replace(d_pp$word, "^(thank)$", "thanks")
#d_pp$word = str_replace(d_pp$word, "^(thx)$", "thanks")

#Though
#d_pp$word = str_replace(d_pp$word, "^(tho)$", "though")

#Try
d_pp$word = str_replace(d_pp$word, "^(trye)$", "try")

#You from u and yu and your from ur
#d_pp$word = str_replace(d_pp$word, "^(u)$", "you")
d_pp$word = str_replace(d_pp$word, "^(yu)$", "you")
d_pp$word = str_replace(d_pp$word, "^(ur)$", "your")

#oops
d_pp$word = str_replace(d_pp$word, "^(ups)$", "oops")

#Wanna
d_pp$word = str_replace(d_pp$word, "^(wan)$", "wanna")

#Yeah
d_pp$word = str_replace(d_pp$word, "^(ye)$", "yeah")
d_pp$word = str_replace(d_pp$word, "^(yea)$", "yeah")
d_pp$word = str_replace(d_pp$word, "^(yeh)$", "yeah")

#Yep
d_pp$word = str_replace(d_pp$word, "^(yeap)$", "yep")
#d_pp$word = str_replace(d_pp$word, "^(yup)$", "yep")

#Yes
d_pp$word = str_replace(d_pp$word, "^(yess)$", "yes")



#[] = interval, also works for letters
#* = 0 or more times
#+ = 1 or more
#? = zero or one time
#^ = start of string
#$ = end of string

#------ Split Words in Two ------
d_pp$word = str_replace(d_pp$word, "^(aswell)$", "as well")
d_pp$word = str_replace(d_pp$word, "^(cant)$", "can not")
d_pp$word = str_replace(d_pp$word, "^(havent)$", "have not")
d_pp$word = str_replace(d_pp$word, "^(ill)$", "i will")
d_pp$word = str_replace(d_pp$word, "^(im)$", "i be")
d_pp$word = str_replace(d_pp$word, "^(its)$", "it be")
d_pp$word = str_replace(d_pp$word, "^(ive)$", "i have")
d_pp$word = str_replace(d_pp$word, "^(lets)$", "let we")
d_pp$word = str_replace(d_pp$word, "^(thats)$", "that be")

#Unnest the words where there is a " ".
d_pp_split = d_pp[grep(" ", d_pp$word), ] %>% unnest_tokens(word, word)

#Make a a list of the words that needed splitting
split_words = d_pp[grep(" ", d_pp$word), 3] %>%
  unique() %>%
  as.data.frame() %>%
  rename(word = ".")
#Make into character
split_words$word = as.character(split_words$word)

#Remove those words from original d_pp
d_pp = anti_join(d_pp, split_words, by = "word")


#Join d_pp with the words that were d_pp_split.
#It is done this way because unnest destroys smileys and when not removing punct it cannot deal with ":D".
d_pp = rbind(d_pp, d_pp_split)


#------ Remove words ------

#Remove stop words
#stop_words = lemmatize_words(stopwords("en"))

#Make own stop words, also contains the Danish and Spanish words
stop_words = data.frame(word = c("a", "the",
                                 "de", "det", "er", "et", "ikke", "ja", "jeg", "kan", "skal", "tak", "vi", #Didn't remove hej
                                 "si", "hola",
                                 "NA",
                                 utf8forplus, utf8forpluss)) 
stop_words$word = as.character(stop_words$word)

d_pp = anti_join(d_pp, stop_words, by = "word")



#------ Word count ------
#Add word frequency to d_pp, so I can remove words based on frequency in it
d_pp = left_join(d_pp, d_pp %>% group_by(word) %>% summarize(n()), by = "word") %>%
  rename(freq = "n()")

#Getting the word count by tag
d_count = d_pp[d_pp$freq > 10, ] %>% #Exclude words which occur less than 10 times
  count(tag_gcr, word, sort = TRUE)#Counts occurence of each unique word in each tag (condition, round etc.)


#------ Convert Documents to Matrix for final LDA ------
#Create full matrix
dtm <- d_count %>%
  cast_dtm(tag_gcr, word, n) #Cast data frame to document term matrix
#Make a dtm for the loop. LDA can't deal with rows with a rowsum of 0
dtm = dtm[apply(dtm , 1, sum) > 0, ]


#------------------------

#############################
####### LDA Analysis ########
#############################
###---- Model specification and Selection 
#------ Model Parameters ------
#List of parameters to use
lda_param = list(
  seed = c(56326, 423643, 3242245,25987, 54820), #List of seeds, one for each nstart
  nstart = 5, #Number of starts, i.e. runs
  best = TRUE, #Method best returns the highest posterior probability amongst all runs
  initialize = "random" #Each topic is randomly initialized
)

#lda_tinkering = LDA(dtm, 10, method = "VEM", control = lda_param)

#------ Create folds for cross validation ------
#Create tags for folds in word count
d_count$Folds = mapvalues(d_count$tag_gcr,
                          from = unique(d_count$tag_gcr),
                          to = c(1:length(unique(d_count$tag_gcr)))) %>%
  as.character() %>% #needed to convert to numeric properly
  as.integer()

#Create the specific folds
folds = createFolds(unique(d_count$Folds), k = 10 )

#------ Cross validation ------
#Empty list/df for loop to store the data
#l_loop_cv = list() #Commented out to no accidentally delete

for (n_topic in c(2:10, 20, 30)){ #Loop through number of topics. To save time 2:5, 10, 20, 30... 100, 100
  for (f in 1:10){#Cross-validate for each number of topics 
    #------ Training Data ------
    #Data
    d_train = filter(d_count,!(Folds %in% unlist(folds[f]))) #training data is without f
    
    #Dtm
    dtm_train <- d_train %>%
      cast_dtm(Folds, word, n) #Cast data frame to document term matrix. Uses Folds because it is numeric
    
    #Remove null rows
    dtm_train = dtm_train[apply(dtm_train , 1, sum) > 0, ]
    
    #------ Test Data ------
    #data
    d_test = filter(d_count,(Folds %in% unlist(folds[f])))
    
    #Dtm
    dtm_test <- d_test %>%
      cast_dtm(Folds, word, n) #Cast data frame to document term matrix
    
    #Remove null rows
    dtm_test = dtm_test[apply(dtm_test , 1, sum) > 0, ]  
    
    #------ Model fitting ------
    lda_train = LDA(dtm_train,
                    k = n_topic,
                    method = "VEM",
                    control = lda_param)#Control list from previous section

    
    #------ Perplexity ------
    #Calculate perplexity on training data
    prplx = perplexity(lda_train, dtm_test)
    
    
    #------ Save data ------
    #Save data as lists
    #one_row = data.frame(n_topic = n_topic, test_fold = f, perplexity = prplx)
    l_loop_cv[[length(l_loop_cv)+1]] = list(n_topic = n_topic, test_fold = f, perplexity = prplx)
    
    #Save data as data frame, very slow
    #one_row = data.frame(n_topic = n_topic, test_fold = f, perplexity = prplx)
    #df_loop = rbind(df_loop, one_row)
    
    #Print for sanity
    #print(f)
  }
  #Print for sanity
  print(n_topic)
}


#Bind to data frame
df_loop <- do.call(rbind, lapply(l_loop_cv, data.frame)) 

#Save loop from data frame as it was, so the loop doesn't need to be run again
#df_save = df_loop
df_loop = df_save #Revert df_loop back to its original form

#------ Remove unnecessary things from loop ------
rm(d_train, d_test, dtm_train, dtm_test, lda_train, prplx, f, n_topic)

#------ Model Selection ------
#Add median and sd to df_loop
df_loop = df_loop %>%
  left_join(ddply(df_loop,~n_topic,summarise,
                  median=median(perplexity),
                  mean = mean(perplexity),
                  sd=sd(perplexity),
                  lower = min(perplexity),
                  upper = max(perplexity)),
            by = "n_topic")

#Find lowest value of perplexity
df_loop[which.min(df_loop$perplexity),]
#Find lowest value of median
df_loop[which.min(df_loop$median),]
#Find lowest value of sd
df_loop[which.min(df_loop$sd),]
#Find lowest value of mean
df_loop[which.min(df_loop$mean),]

#Plot perplexity per model, to visualize to sanity check
ggplot(df_loop, aes(n_topic, perplexity))+
  geom_point(alpha = 0.7)+
  geom_smooth(fill = "lightgrey")+
  labs(title ="Perplexity across folds for different number of topics", x = "Number of topics", y = "Perplexity")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_minimal()

ggplot(df_loop, aes(n_topic, mean))+
  geom_point()+
  geom_point(aes(n_topic, upper))+
  geom_point(aes(n_topic, lower))+
  geom_ribbon(aes(ymin=lower, ymax=upper, alpha = 0.2))+
  geom_vline(aes(xintercept = n_topic))

ggplot(df_loop, aes(n_topic, perplexity, group = n_topic))+
  geom_boxplot()+
  labs(title ="Perplexity across folds for different number of topics", x = "Number of topics", y = "Perplexity")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(breaks = seq(110, 124, by = 2))+
  scale_x_continuous(breaks = seq(0, 30, by = 5))


  


###---- Model Fitting and Inspection
#------ Fit best model ------
#Use best performing k from cross-validation
lda <- LDA(dtm,
           k = 4,
           method = "VEM",
           control = lda_param)


#------ Investigate posterior ------
lda_inf = posterior(lda, dtm)


#Performance measure
perplexity(lda, dtm)

lda_inf$terms


#------ Check Top Words of Each Topic ------
top_terms <- tidy(lda)%>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup()

#Paste " " in front to differentiate between the overlapping words 
top_terms[top_terms$topic == 1,]$term = paste("", top_terms[top_terms$topic == 1,]$term, sep = "")
top_terms[top_terms$topic == 2,]$term = paste(" ", top_terms[top_terms$topic == 2,]$term, sep = "")
top_terms[top_terms$topic == 3,]$term = paste("  ", top_terms[top_terms$topic == 3,]$term, sep = "")
top_terms[top_terms$topic == 4,]$term = paste("   ", top_terms[top_terms$topic == 3,]$term, sep = "")

#top_terms$term = paste(top_terms$term, " ", top_terms$topic, sep = "")




#------ Check Topic Distributions of the Documents ------
lda_gamma = tidy(lda, matrix = "gamma") %>% #Make tidy one-document-per-topic-per-row
  separate(document, c("group", "condition", "round"), sep = "_", convert = TRUE, remove = FALSE) #Split title and chapter

#make topic a factor
lda_gamma$topic = as.factor(lda_gamma$topic)


#Give condition full names for plot
lda_gamma$condition = mapvalues(lda_gamma$condition, from = c("C:kmf", "C:kmp", "C:kmr", "C:umf", "C:ump", "C:umr"),
                                  to = c("Known max, fully connected",
                                         "Known max, pairs",
                                         "Known max, partially connected",
                                         "Unknown max, fully connected",
                                         "Unknown max, pairs",
                                         "Unknown max, partially connected"))
#make condition a factor
lda_gamma$condition = as.factor(lda_gamma$condition)

#Make group a factor
lda_gamma$group = as.factor(lda_gamma$group)

#Make round numeric
lda_gamma$round = lda_gamma$round %>% str_replace("R:", "") %>% as.numeric()

#Make standardized round
lda_gamma = lda_gamma %>% group_by(group) %>%
  mutate(round_standard = round/max(round))


#------ Check and Assign Primary Topic to the Documents ------
#For document
classifications_document = lda_gamma %>%
group_by(document) %>% #document
  top_n(1, gamma) %>% #select top 1 ordered by gamma
  ungroup() %>%
  arrange(gamma)

#Across groups and condtions, which topic was dominant in the round?
classifications_rc = classifications_document %>% group_by(condition, round) %>%
  count(topic) %>% top_n(1,n)

#Across groups and condtions, which topic was dominant in the standardized round?
classifications_rsc = classifications_document %>% group_by(condition, round_standard) %>%
  count(topic) %>% top_n(1,n)



#------ Plots ------
#Set Theme
theme_set(theme_bw())


###Use this
#Top Words of Each Topic and Their beta Values
top_terms %>%
  mutate(term = reorder(term, beta)) %>% 
  ggplot(aes(term, beta)) + #x=word, y=beta
  geom_bar(stat = "identity") + #Bar plot
  facet_wrap(~ topic, scales = "free") + #Wrap by topic
  theme(axis.text.x = element_text(size = 15, angle = 90, hjust = 1))+
  labs(title =" Top 15 terms of each topic", x = "Term", y = "Beta as approximated by lambda")+
  theme(plot.title = element_text(hjust = 0.5))


###Use this  
#Change of Value of Each Topic Over Time (Spaghetti plot, (Documents in order, Proportion of Topic))
ggplot(lda_gamma, aes(round, gamma, color = topic))+
  geom_point(alpha = 0.7)+
  geom_smooth(fill = "lightgrey")+
  facet_wrap(~ condition, nrow = 2)+
  geom_point(data = classifications_rc, aes(y = -0.15, color = topic), shape = 18, alpha = 0.5)+
  scale_alpha_continuous(guide = FALSE)+
  scale_colour_manual(name = "Dominant Topic",
                      values = c("gold2", "red2", "green3", "blue"),
                      breaks = c("1", "2", "3", "4"))+
  guides(color=guide_legend(override.aes=list(fill=c("white"), linetype = "solid", shape = NA), order = 0))+
  labs(title =" Topic weight across rounds in each condition", x = "Round", y = "Gamma") +
  theme(plot.title = element_text(hjust = 0.5))

#round standard
ggplot(lda_gamma,
       aes(round_standard, gamma, color = topic))+
  geom_point(alpha = 0.5)+
  geom_smooth(fill = "lightgrey")+
  facet_wrap(~ condition, nrow = 2)+
  geom_point(data = classifications_rsc, aes(y = -0.15, color = topic), shape = 18, alpha = 0.5)+
  scale_alpha_continuous(guide = FALSE)+
  scale_colour_manual(name = "Dominant Topic",
                      values = c("gold2", "red2", "green3", "blue"),
                      breaks = c("1", "2", "3", "4"))+
  guides(color=guide_legend(override.aes=list(fill=c("white"), linetype = "solid", shape = NA), order = 0))+
  labs(title =" Topic weight across rounds in each condition", x = "Round", y = "Gamma") +
  theme(plot.title = element_text(hjust = 0.5))
  
[lda_gamma$group != c("2016-12-09-B1-125327", "2016-12-09-B2-132333", "2017-09-07-A1-113148"),]

#------------------------

###############################################################
####### KL Divergence Between Document Topics over Time #######
###############################################################
#------ Create KL Function ------
#Define function
kl_divergence = function(from, to){
  
  kl_div = sum(to*log(to/from))
  # i from 1 to number of topics
  # P = to, Q = from
  # sum across each value of i, P(i)*log(P(i)/Q(i))
  
  return(kl_div)
}



#------ Create df for loop -------
d_kl = data.frame()
d_kl2 = data.frame()

#------ Loop for KL Divergence ------
#For each condition get KL between rounds
for (g in as.character(levels(lda_gamma$group))){ #Make a subset for each condition
  #------ Load data ------
  #load data for the group
  d = lda_gamma[lda_gamma$group == g, ]
  
  round_max = max(d$round)
  
  #------ Fix rounds Variable ------
  #If there is a round 0 in this condition, add 1 to round, so the loop can run on all conditions
  if (match(0, d$round, nomatch = 0)>0) {r_add = 1} else {r_add = 0} #make tag to subtract 1 from round later
  if (match(0, d$round, nomatch = 0)>0) d$round = d$round+1
  
  #Get number of rounds. -1 is because the last round has no next round to compare to
  nround = as.numeric(length(unique(d$round)))
  

  
  #------ Loop through all rounds in this condition and measure KL ------
  for (r in 1:nround){#For each round, get KL dist to next round
    #------ Get matrix of gamma ------
    #Current round
    kl_now = d[d$round == r, 4:6]
    kl_now = tapply(kl_now$gamma, kl_now[c("topic", "round")], mean)
    
    #Next round
    kl_next = d[d$round == r+1, 4:6]
    kl_next = tapply(kl_next$gamma, kl_next[c("topic", "round")], mean)
    
    #Past rounds
    kl_past = d[d$round < r, 4:6]
    kl_past = tapply(kl_past$gamma, kl_past[c("topic", "round")], mean)
    
    #Future rounds
    kl_future = d[d$round > r, 4:6]
    kl_future = tapply(kl_future$gamma, kl_future[c("topic", "round")], mean)
    
    
    #------ Taking other rounds into account ------
    #Chronological
    novelty = if (r == 1) {NA} else {apply(kl_past, 2, function(x) kl_divergence(x, kl_now)) %>% #Apply the kl function defined above to each column (2)
        mean()}
    
    
    #Reverse
    transcience = if (r == nround) {NA} else {apply(kl_future, 2, function(x) kl_divergence(x, kl_now)) %>% mean()}
    
    
    #------ Look only at current round ------
    #Chronological KL div.
    kl = if (r == nround) {NA} else {kl_divergence(kl_now, kl_next)}
    
    #Time reversed KL div.
    rkl = if (r == nround) {NA} else {kl_divergence(kl_next, kl_now)}
    
    #compute kl dist
    kl_sum = if (r == nround) {NA} else {kl_divergence(kl_now, kl_next)+kl_divergence(kl_next, kl_now)}
    

    
    #------ Save Data ------
    #Save in long format for plotting
    row_chronological = data.frame("group" = g,
                         "condition" = unique(d$condition),
                         "round" = (if (r_add == 0) {r} else {r-1}),
                         "round_standard" = (if (r_add == 0) {r} else {r-1})/round_max,
                         "split_variable" = "chronological", #Split data for plot
                         "kl" = kl, #KL for plot
                         "plot_variable" = novelty, #Y variable for plot, chronological -> novelty
                         "kl_dist" = kl_sum)

    row_reversed = data.frame("group" = g,
                         "condition" = unique(d$condition),
                         "round" = (if (r_add == 0) {r} else {r-1}),
                         "round_standard" = (if (r_add == 0) {r} else {r-1})/round_max,
                         "split_variable" = "reversed", #Split data for plot
                         "kl" = rkl, #KL for plot
                         "plot_variable" = transcience,#Y variable for plot, reversed -> transcience
                         "kl_dist" = kl_sum)
    
    row_resonance = data.frame("group" = g,
                              "condition" = unique(d$condition),
                              "round" = (if (r_add == 0) {r} else {r-1}),
                              "round_standard" = (if (r_add == 0) {r} else {r-1})/round_max,
                              "split_variable" = "resonance", #Split data for plot
                              "kl" = NA,
                              "plot_variable" = novelty-transcience, #Y variable for plot, resonance -> resonance
                              "kl_dist" = kl_sum)

    row_iteration = rbind(row_chronological, row_reversed, row_resonance)
    
    
    d_kl = rbind(d_kl, row_iteration)
    
    #For long format
    row_long = data.frame("group" = g,
                "condition" = unique(d$condition),
                "round" = (if (r_add == 0) {r} else {r-1}),
                "round_standard" = (if (r_add == 0) {r} else {r-1})/round_max,
                "kl" = kl,
                "rkl" = rkl,
                "novelty" = novelty,
                "transience" = transcience,
                "resonance" = novelty-transcience, #Y variable for plot, resonance -> resonance
                "kl_dist" = kl_sum)
    d_kl2 = rbind(d_kl2, row_long)
  }
}


  
#------ Remove unused things from loop ------
rm(g, con, r, doc, d, round_max, nround, kl_now, kl_next, kl_past, kl_future, novelty, transcience,
   kl, rkl, kl_sum, r_add, row_chronological, row_reversed, row_resonance, row_iteration)



#------ Plots used ------
#Make plot of condition resonance against each other
#Split condition
d_kl = separate(d_kl, condition, c("information", "network"), sep = ", ", convert = TRUE, remove = FALSE)

p_res_rs = ggplot(d_kl[d_kl$split_variable == "resonance",], aes(round_standard, plot_variable, color = network))+
  geom_point(alpha = 0.3)+
  geom_smooth(fill = "lightgrey")+
  facet_wrap(~information, ncol = 2)+
  guides(color = guide_legend(override.aes=list(fill = "white")))+
  labs(title ="C: Resonance across standardized rounds", x = "Standardized round", y = "Resonance") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()
p_res_r = ggplot(d_kl[d_kl$split_variable == "resonance",], aes(round, plot_variable, color = network))+
  geom_point(alpha = 0.3)+
  geom_smooth(fill = "lightgrey")+
  facet_wrap(~information, ncol = 2)+
  guides(color = guide_legend(override.aes=list(fill = "white")))+
  labs(title ="D: Resonance across rounds", x = "Round", y = "Resonance") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")+
  theme_bw()
  

p_nov = ggplot(d_kl[d_kl$split_variable == "chronological",], aes(round_standard, plot_variable, color = network))+
  geom_point(alpha = 0.3)+
  geom_smooth(fill = "lightgrey")+
  facet_wrap(~information, ncol = 2)+
  guides(color = guide_legend(override.aes=list(fill = "white")))+
  labs(title ="A: Novelty across standardized rounds", x = "Standardized round", y = "Novelty") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()

p_tra = ggplot(d_kl[d_kl$split_variable == "reversed",], aes(round_standard, plot_variable, color = network))+
  geom_point(alpha = 0.3)+
  geom_smooth(fill = "lightgrey")+
  facet_wrap(~information, ncol = 2)+
  guides(color = guide_legend(override.aes=list(fill = "white")))+
  labs(title ="B: Transience across standardized rounds", x = "Standardized round", y = "Transience") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()

#Function for merging plots from https://github.com/tidyverse/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
  
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position="none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  
  grid.newpage()
  grid.draw(combined)
  
  # return gtable invisibly
  invisible(combined)
  
}

grid_arrange_shared_legend(p_nov, p_tra, p_res_rs, p_res_r, ncol = 2, nrow = 2, position = "bottom")

#------ Plots not used ------
#plot KL over time in each condition
### Normal rounds, novelty, transcience and resonance
ggplot(d_kl, aes(round, plot_variable))+
  geom_point(aes(size = split_variable, color = split_variable), alpha = 0.3)+
  geom_point(data = classifications_rc, aes(y = -0.15, color = topic), shape = 18, alpha = 0.5)+
  geom_smooth(aes(size = split_variable, color = split_variable), fill = "lightgrey")+
  facet_wrap(~ condition, nrow = 2)+
  scale_alpha_continuous(guide = FALSE)+
  scale_size_manual(name = "Measure",
                    labels = c("Novelty", "Transience", "resonance"),
                    values = c(1, 0.99, 1.01))+
  scale_colour_manual(name = "Dominant Topic",
                      values = c("gold2", "red2", "green3", "blue", "orange", "darkgreen", "steelblue3" ),
                      breaks = c("1", "2", "3", "4"))+
  guides(size=guide_legend(override.aes=list(colour=c("orange", "steelblue", "darkgreen"),
                                             linetype = "solid", fill = "white", size = 1, shape = NA), order = 1),
         color=guide_legend(override.aes=list(fill=c("white"), linetype = "blank"), order = 0))+
  labs(title =" Resonance across rounds in each condition", x = "Round", y = "Resonance") +
  theme(plot.title = element_text(hjust = 0.5))

#For standardized rounds. Remove rounds where the experiment froze early
ggplot(d_kl, aes(round_standard, plot_variable))+
  geom_point(aes(size = split_variable, color = split_variable), alpha = 0.3)+
  geom_point(data = classifications_rsc, aes(y = -0.15, color = topic), shape = 18, alpha = 0.5)+
  geom_smooth(aes(size = split_variable, color = split_variable), fill = "lightgrey")+
  facet_wrap(~ condition, nrow = 2)+
  scale_alpha_continuous(guide = FALSE)+
  scale_size_manual(name = "Measure",
                    labels = c("Novelty", "Transience", "resonance"),
                    values = c(1, 0.99, 1.01))+
  scale_colour_manual(name = "Dominant Topic",
                      values = c("gold2", "red2", "green3", "blue", "orange", "darkgreen", "steelblue3" ),
                      breaks = c("1", "2", "3", "4"))+
  guides(size=guide_legend(override.aes=list(colour=c("orange", "steelblue", "darkgreen"),
                                             linetype = "solid", fill = "white", size = 1, shape = NA), order = 1),
         color=guide_legend(override.aes=list(fill=c("white"), linetype = "blank"), order = 0))+
  labs(title ="Resonance across rounds in each condition", x = "Round", y = "Resonance") +
  theme(plot.title = element_text(hjust = 0.5))



ggplot(d_kl2, aes(novelty, transience))+
  geom_point(alpha = 0.5)+
  facet_wrap(~ condition, nrow = 2)+
  theme_bw()

ggplot(d_kl2, aes(novelty, resonance))+
  geom_point(alpha = 0.5)+
  geom_smooth()+
  theme_bw()
#------------------------

###############################################################
####### Predicting score from resonance #######################
###############################################################
#------ Create data frame ------
#create data
#Make condition for score
d_score$condition = as.factor(d_score$condition)
levels(d_score$condition)
d_score$condition = mapvalues(d_score$condition, from = c("Known max_Full",
                                                              "Known max_Pairs",
                                                              "Known max_Ring",
                                                              "Unknown max_Full",
                                                              "Unknown max_Pairs",
                                                              "Unknown max_Ring"),
                                to = c("Known max, fully connected",
                                       "Known max, pairs",
                                       "Known max, partially connected",
                                       "Unknown max, fully connected",
                                       "Unknown max, pairs",
                                       "Unknown max, partially connected"))


levels(d_score$group)
levels(dmodel_add$group)
#Group as factor
d_score$group = as.factor(d_score$group)

#Add resonance to data frame
dmodel_add = select(d_kl2, group, round, resonance) %>% na.omit()
dmodel_add$group = dmodel_add$group%>% str_replace("G:", "") %>% as.factor()
dmodel = left_join(d_score, dmodel_add, by = c("group", "round")) %>% na.omit()

#split condition into parts
dmodel = separate(dmodel, condition, c("information", "network"), sep = ", ", convert = TRUE, remove = FALSE)
dmodel$information = as.factor(dmodel$information)
dmodel$network = as.factor(dmodel$network)
dmodel$score_z = scale(dmodel$score, center=TRUE, scale=TRUE)
dmodel$score_s = dmodel$score/100

mean(dmodel$score_z)

ggplot(dmodel, aes(resonance))+
  geom_histogram()

#------ Models -----
#Without random effects
m1_norandom = brm(score_s ~ resonance+round_standard+information+network,
                  family = skew_normal(),
                  data = dmodel,
                  chains = 2, cores = 2)
m2_norandom = brm(score_s ~ resonance+round_standard+information*network,
                  family = skew_normal(),
                  data = dmodel,
                  chains = 2, cores = 2)
m3_norandom = brm(score_s ~ resonance*round_standard+information+network,
                  family = skew_normal(),
                  data = dmodel,
                  chains = 2, cores = 2)
m3.5_norandom = brm(score_s ~ resonance*round_standard+information*network,
                   family = skew_normal(),
                   data = dmodel,
                   chains = 2, cores = 2)
m4_norandom = brm(score_s ~ resonance*round_standard*information*network,
                  family = skew_normal(),
                  data = dmodel,
                  chains = 2, cores = 2)

m1_int = brm(score_s ~ resonance+round_standard+information+network+(1|group),
             family = skew_normal(),
             data = dmodel,
             chains = 2, cores = 2)
m2_int = brm(score_s ~ resonance+round_standard+information*network+(1|group),
             family = skew_normal(),
             data = dmodel,
             chains = 2, cores = 2)
m3_int = brm(score_s ~ resonance*round_standard+information+network+(1|group),
             family = skew_normal(),
             data = dmodel,
             chains = 2, cores = 2)
m3.5_int = brm(score_s ~ resonance*round_standard+information*network+(1|group),
             family = skew_normal(),
             data = dmodel,
             chains = 2, cores = 2)
m4_int = brm(score_s ~ resonance*round_standard*information*network+(1|group),
             family = skew_normal(),
             data = dmodel,
             chains = 2, cores = 2)
m_nores = brm(score_s ~ round_standard*information*network+(1|group),
              family = skew_normal(),
              data = dmodel,
              chains = 2, cores = 2)

mw_norandom = model_weights(m1_norandom, m2_norandom, m3_norandom, m3.5_norandom, m4_norandom, weights = "waic")

mw_intvsno = model_weights(m1_norandom, m2_norandom, m3_norandom, m4_norandom, m1_int, m2_int, m3_int, m3.5_int, m4_int, m_nores, weights = "waic")

pp_check(m4_int)
 
hypothesis(m4_int, "resonance:round_standard:informationUnknownmax:networkpairs < 0")

m2_int

#------ Make plot that finds max resonance, mean resonance and resonance of round where solution is found ------
df = d_kl2 %>% na.omit() %>%
  left_join(ddply(d_kl2,~group,summarise,
                  median=median(resonance),
                  mean = mean(resonance),
                  sd=sd(resonance),
                  lower = min(resonance),
                  upper = max(resonance)),
            by = "group")

df = d_kl2 %>% na.omit()

df = df %>% group_by(group) %>%
  mutate(mean_resonance = mean(resonance),
         max_resonance = max(resonance))
df$group = df$group %>% str_replace("G:", "") %>% as.factor()
#Add maximum score to df
add_score = select(d_score, group, found_max) %>% unique()
df = left_join(df, add_score, by = "group")
df = df %>% group_by(group) %>% 
  mutate()

df_test = filter(df, round == found_max)

ggplot(df, aes(x = reorder(group, as.character(condition)), max_resonance))+
  geom_point(shape = 24, size = 5)+ #triangle hollow
  geom_point(data = df, aes(group, mean_resonance), shape = 23, size = 5)+ #rhombus hollow
  geom_point(data = filter(df, round == found_max), aes(group, resonance), shape = 77, size = 4)+ #dot solid 19
  geom_point(data = filter(df, round == found_max+1), aes(group, resonance), shape = 49, size = 4)+ #triangle hollow 17
  geom_point(data = filter(df, round == found_max+2), aes(group, resonance), shape = 50, size = 4)+ #square solid 15
  theme(axis.text.x  = element_blank())+
  labs(title ="Max and mean resonance vs. resonance of first round with optimal solution and the two following rounds", x = "Group", y = "Resonance") +
  theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap(~ condition, nrow = 2, scales = "free")


#------------------------

##############################################
####### Choosing Conversations to Code #######
##############################################
#------ For coding scheme ------
#Subset variables needed for picking conversations
coding_data = d_full[,c(2:6, 9:11)]

#Subset condition
levels(coding_data$condition) #"Known max_Full", "Known max_Pairs", "Known max_Ring", "Unknown max_Full", "Unknown max_Pairs", "Unknown max_Ring"
coding_condition = subset(coding_data, condition == "Known max_Full")
coding_condition$group = as.character(coding_condition$group) #Make it to character, so sample doesn't sample full range of groups

#Subset groups
set.seed(314159)
coding_group = unique(coding_condition$group) %>% sample(3,
                                                         replace = FALSE) #Change to true for Pairs
coding_group

unique(coding_condition$group)

#Subsetting rounds
coding_first_round = filter(coding_condition, group == coding_group[1]) %>%
  filter(round == min(round))

coding_mid_round = filter(coding_condition, group == coding_group[2]) %>%
  filter(round == round(max(round/2)))

coding_last_round = filter(coding_condition, group == coding_group[3]) %>%
  filter(round == max(round))

print(coding_last_round[coding_last_round$sender == "p_1_3", 5])

#------ For coding specific group ------
#Choose condtion
coding_condition = subset(coding_data, condition == "Unknown max_Ring")

#Find group with only a few rounds, so each round counts for more
coding_condition[, c(1,8)] %>% group_by(group) %>% summarise(max(round))

#Check solutions to see if somethings obviously off
file_solutions = read.csv("C:\\Users\\Martin\\Documents\\UNI\\5th semester\\Bachelor\\Topic Modelling\\Data\\2017.02.20\\B\\nps_20170220-141247_solution.csv", sep = ",")

#Group to code: 2018-02-07-A-075707 known max
#Really short experiments were because of technical problems
#middle long experiments were the restarted sessions of the crashed groups giving them a head start

#Data frames for viewing messages
coding_onegroup = subset(coding_data, group == "2018-02-07-A-075707")

#Data for plotting
d_coding_plot = d_kl[d_kl$group == "G:2018-02-07-A-075707", ]


#------ Plots for single group ------
#Resonance
p_group_res = ggplot(d_coding_plot, aes(round, plot_variable))+
  geom_smooth(aes(color = split_variable), fill = "lightgrey")+
  geom_point(aes(color = split_variable), alpha = 0.7)+
  scale_color_manual(name = "Measure",
                     labels = c("Novelty", "Transcience", "Resonance"),
                     values = c("orange", "steelblue", "darkgreen"))+
  theme_bw()+
  labs(title = "B: Resonance, novelty and transcience across rounds", x = "Round", y = "Size of measure")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(minor_breaks = seq(0 , 33, 1), breaks = seq(0, 33, 5))+
  theme(panel.grid.minor = element_line(size = 0.5), panel.grid.major = element_line(size = 1.2))+
  guides(color=guide_legend(override.aes=list(fill=c("white"))))
  
#topics
p_group_topic = ggplot(lda_gamma[lda_gamma$group == "G:2018-02-07-A-075707", ], aes(round, gamma, color = topic))+
  geom_point(alpha = 0.5)+
  geom_smooth(fill = "lightgrey")+
  geom_point(data = classifications_document[classifications_document$group == "G:2018-02-07-A-075707", ], aes(y = -0.15, color = topic), shape = 18)+
  scale_colour_manual(name = "Dominant Topic",
                      values = c("goldenrod2", "red2", "green3", "blue"),
                      breaks = c("1", "2", "3", "4"))+
  guides(color=guide_legend(override.aes=list(fill=c("white"), linetype = "solid", shape = NA), order = 0))+
  labs(title ="A: Topic weight across rounds in each condition", x = "Round", y = "Theta as approximated by gamma") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()+
  scale_x_continuous(minor_breaks = seq(0 , 33, 1), breaks = seq(0, 33, 5))+
  theme(panel.grid.minor = element_line(size = 0.5), panel.grid.major = element_line(size = 1.2))

grid.arrange(p_group_topic, p_group_res, nrow= 1)
#------------------------


#####################
####### Links #######
#####################
#------ Preprocessing ------
#https://eight2late.wordpress.com/2015/05/27/a-gentle-introduction-to-text-mining-using-r/


#------ Using LDA ------
#https://eight2late.wordpress.com/2015/09/29/a-gentle-introduction-to-topic-modeling-using-r/

#https://cran.r-project.org/web/packages/tidytext/vignettes/topic_modeling.html

#------ RegEx ------
#https://stat.ethz.ch/R-manual/R-devel/library/base/html/regex.html




#------------------------

##########################
####### Scrap COde #######
##########################
#------ score vs kl: Plot mean score of round against resonance of the round ------
#get mean score
add = d_score %>%
  group_by(group, round) %>% summarise(meanscore = mean(score))
test$group = as.factor(test$group)

#Get KL measures
d_kl_score = na.omit(d_kl2)
#Edit group
d_kl_score$group = as.character(d_kl_score$group)
d_kl_score$group = str_replace(d_kl_score$group, "G:", "")
d_kl_score$group = as.factor(d_kl_score$group)
#add mean score
d_kl_score = left_join(d_kl_score, add, by = c("group", "round")) %>% na.omit()
#Get round where max was found
add2 = select(d_score, group, found_max) %>% unique()
#Add round where max was found
d_kl_score = left_join(d_kl_score, test2, by = "group")

#Shift round of resonance by one, so the results of around affects the following round
add3 = select(d_kl_score, group, condition, resonance, round)
add3$round = d_kl_score$round-1
add4 = select(d_kl_score, group, condition, meanscore, round)
test = left_join(add, add3, by = c("group", "round")) %>% na.omit()

#d_kl_score %>% group_by(group, condition) %>% summarise(tt = (score-mean(score))/sd(score))


ggplot(d_kl_score, aes(resonance, meanscore, alpha = 0.5))+
  geom_point()+
  facet_wrap(~ condition, nrow = 2)

ggplot(test, aes(resonance, meanscore, alpha = 0.5))+
  geom_point()+
  facet_wrap(~ condition, nrow = 2)


ggplot(d_kl_score, aes(transience, meanscore))+
  geom_point()+
  facet_wrap(~ condition, nrow = 2)

ggplot(d_kl_score, aes(novelty, meanscore))+
  geom_point()+
  facet_wrap(~ condition, nrow = 2)

ggplot(d_kl_score, aes(found_max, resonance))+
  geom_point()




?summarise

#------ Exploratory models ------
#Score ~ Resonance * Round * Condition + (1 + Round | Pair / Group) 
m1 = brm(score_s ~ resonance+round_standard+information+network+(1+round|group),
         data = dmodel,
         family = skew_normal(),
         chains = 2, cores = 2) #no int

m12 = brm(score ~ resonance+round+information+network,
          family = skew_normal(),
          data = dmodel,
          chains = 2, cores = 2)

m122 = brm(score_s ~ resonance+round_standard+information+network,
           family = skew_normal(),
           data = dmodel,
           chains = 2, cores = 2)

pp_check(m122)

m123 = brm(score_s ~ resonance+round_standard+information*network+(1+round|group),
           family = skew_normal(),
           data = dmodel,
           chains = 2, cores = 2)

m124 = brm(score_s ~ resonance+round_standard+information*network+(1+round|group),
           family = skew_normal(),
           prior = c(prior(normal(0,2), class = Intercept), #loose prior for intercept
                     prior(normal(0,0.01), class = b, coef = resonance),
                     prior(normal(0,0.01), class = b, coef = round_standard),
                     prior(normal(0,0.01), class = b, coef = networkpartiallyconnected),
                     prior(normal(0,0.01), class = b, coef = networkpairs),
                     prior(normal(0,0.01), class = b, coef = informationUnknownmax:networkpartiallyconnected),
                     prior(normal(0,0.01), class = b, coef = informationUnknownmax:networkpairs),
                     prior(normal(0,0.01), class = b, coef = informationUnknownmax),
                     prior(cauchy(0,2), class = sigma),
                     prior(normal(-5,3), class = alpha)),
           data = dmodel,
           chains = 2, cores = 2)
m125 = brm(score_s ~ resonance+round_standard+information*network+(1+round|group),
           family = skew_normal(),
           prior = c(prior(normal(0,3), class = Intercept), #loose prior for intercept
                     prior(normal(0,0.1), class = b, coef = resonance),
                     prior(normal(0,0.1), class = b, coef = round_standard),
                     prior(normal(0,0.1), class = b, coef = networkpartiallyconnected),
                     prior(normal(0,0.1), class = b, coef = networkpairs),
                     prior(normal(0,0.1), class = b, coef = informationUnknownmax:networkpartiallyconnected),
                     prior(normal(0,0.1), class = b, coef = informationUnknownmax:networkpairs),
                     prior(normal(0,0.1), class = b, coef = informationUnknownmax),
                     prior(cauchy(0,2), class = sigma),
                     prior(normal(-5,3), class = alpha)),
           data = dmodel,
           chains = 2, cores = 2)
m1252 = lmer(score_s ~ resonance+round_standard+information*network+(1+round|group),
             data = dmodel)
m1252

get_prior(score_s ~ resonance+round_standard+information*network+(1+round|group), dmodel)

pp_check(m123)

m123
m124
m125
m13 = brm(score_s ~ round_standard+information+network,
          data = dmodel,
          family = skew_normal(),
          chains = 2, cores = 2)

m13

plot(m13)

pp_check(m13)
add_ic(m13, ic = c("waic"))

prior_summary(m13)

get_prior(score ~ round, family = skew_normal(), data = dmodel)
prior_summary(m12)

pp_check(m12)

get_prior(score ~ resonance+round+information+network+(1+round|group), data = dmodel)

plot(m1)
pp_check(m1)

add_ic(m1, ic = c("waic"))
#waic 44908.07

m2 = brm(score ~ resonance+round+information*network+(1+round|group), 
         data = dmodel,
         chains = 2, cores = 2) #condition int
add_ic(m2, ic = c("waic"))

m3 = brm(score ~ resonance*information*network+round+(1+round|group), 
         data = dmodel,
         chains = 2, cores = 2) #round not int
add_ic(m3, ic = c("waic"))
#WAIC = 44912.24

m4 = brm(score ~ resonance*round*information*network, 
         data = dmodel,
         chains = 2, cores = 2) #4int no random
m4
plot(m4)
add_ic(m4, ic = c("waic"))
#45871.25

hypothesis(m4, "resonance:round:informationUnknownmax:networkpairs < 0")

m42 = brm(score ~ resonance*round*information*network+(1|group), 
          data = dmodel,
          chains = 2, cores = 2) %>%
  add_ic(ic = c("waic")) #four int, no random slope
m42

add_ic(m42, ic = c("waic"))


m5 = brm(score ~ resonance+round+information+network+(1+round+resonance|group), data = dmodel,
         chains = 2, cores = 2)

plot(m5)
add_ic(m5, ic = c("waic"))
m5
#waic 44878.88

m6 = brm(score ~ resonance*round+information+network+(1+round*resonance|group), data = dmodel,
         chains = 2, cores = 2)
add_ic(m6, ic = c("waic"))
#waic 44802.9
m6

m7 = brm(score ~ resonance*round+information*network+(1+round*resonance|group), data = dmodel,
         chains = 2, cores = 2)
m7 = add_ic(m7, ic = c("waic"))
m7 #WAIC = 44804.28

m8 = brm(score ~ resonance*round+(1+round*resonance|group), data = dmodel,
         chains = 2, cores = 2) %>% add_ic(ic = c("waic"))
pp_check(m8)
m8

m8 #WAIC = 44800.65
m9 = brm(score ~ resonance+round+(1+round+resonance|group), data = dmodel,
         chains = 2, cores = 2) %>% add_ic(ic = c("waic"))
m9 #WAIC = 44880.65

m10 = brm(resonance~round+information*network+(1+round|group), data = dmodel,
          chains = 2, cores = 2) %>% add_ic(ic = c("waic"))
m10
pp_check(m10)



model_weights(m1, m2, m3, m4, m42, m5, m6, m7, m8, m9, weights = "waic")

brms::model_weights(m1, m2, weights = "waic")
#------ With random slopes ------
#No interaction
m1 = brm(score_s ~ resonance+round_standard+information+network+(1+round|group),
         family = skew_normal(),
         data = dmodel,
         chains = 2, cores = 2)

#Condition
m2 = brm(score_s ~ resonance+round_standard+information*network+(1+round|group),
         family = skew_normal(),
         data = dmodel,
         chains = 2, cores = 2)
m2_norandom = brm(score_s ~ resonance+round_standard+information*network,
                  family = skew_normal(),
                  data = dmodel,
                  chains = 2, cores = 2)

#Round resonance
m3 = brm(score_s ~ resonance*round_standard+information+network+(1+round|group),
         family = skew_normal(),
         data = dmodel,
         chains = 2, cores = 2)
m3_norandom = brm(score_s ~ resonance*round_standard+information+network,
                  family = skew_normal(),
                  data = dmodel,
                  chains = 2, cores = 2)

#all four, random effect simplified for convergence 
m4 = brm(score_s ~ resonance*round_standard*information*network+(1|group),
         family = skew_normal(),
         data = dmodel,
         chains = 2, cores = 2)
m4_norandom = brm(score_s ~ resonance*round_standard*information*network,
                  family = skew_normal(),
                  data = dmodel,
                  chains = 2, cores = 2)

#complex randome effect,
m5 = brm(score_s ~ resonance*round_standard+information+network+(1+round*resonance|group),
         family = skew_normal(),
         data = dmodel,
         chains = 2, cores = 2)
#Complex random effect
m6 = brm(score_s ~ resonance*round_standard+information*network+(1+round*resonance|group),
         family = skew_normal(),
         data = dmodel,
         chains = 2, cores = 2)
#Complex random effect, fewer fixed effects no interaction
m7 = brm(score_s ~ resonance+round_standard+(1+round+resonance|group),
         family = skew_normal(),
         data = dmodel,
         chains = 2, cores = 2)
#Complex random effect, fewer fixed effects, interaction
m8 = brm(score_s ~ resonance*round_standard+(1+round*resonance|group),
         family = skew_normal(),
         data = dmodel,
         chains = 2, cores = 2)

m_res = brm(score_s ~ resonance,
            family = skew_normal(),
            data = dmodel,
            chains = 2, cores = 2)
m_res

#Models that had predictors that didn't converge: 1, 2, 4, 5, 6, 7,
#Models that had divergent transitions: 1, 3, 5
#Model weights
mw = model_weights(m1, m2, m3, m4, m5, m6, m7, m8, weights = "waic")

#------ Normally distributed posterior ------


#Normal distributions
#No interaction
m12 = brm(score_s ~ resonance+round_standard+information+network+(1+round|group),
          data = dmodel,
          chains = 2, cores = 2)

#Condition
m22 = brm(score_s ~ resonance+round_standard+information*network+(1+round|group),
          data = dmodel,
          chains = 2, cores = 2)
#Round resonance
m32 = brm(score_s ~ resonance*round_standard+information+network+(1+round|group),
          data = dmodel,
          chains = 2, cores = 2)
#all four, random effect simplified for convergence 
m42 = brm(score_s ~ resonance*round_standard*information*network+(1|group),
          data = dmodel,
          chains = 2, cores = 2)
#complex randome effect,
m52 = brm(score_s ~ resonance*round_standard+information+network+(1+round*resonance|group),
          data = dmodel,
          chains = 2, cores = 2)
#Complex random effect
m62 = brm(score_s ~ resonance*round_standard+information*network+(1+round*resonance|group),
          data = dmodel,
          chains = 2, cores = 2)
#Complex random effect, fewer fixed effects no interaction
m72 = brm(score_s ~ resonance+round_standard+(1+round+resonance|group),
          data = dmodel,
          chains = 2, cores = 2)
#Complex random effect, fewer fixed effects, interaction
m82 = brm(score_s ~ resonance*round_standard+(1+round*resonance|group),
          data = dmodel,
          chains = 2, cores = 2)

mw2 = model_weights(m12, m22, m32, m42, m52, m62, m72, m82, weights = "waic")
#Models that had predictors that didn't converge: 1, 2, 3, 4, 5, 6, 7
#Models that had divergent transitions: 7
mw2


