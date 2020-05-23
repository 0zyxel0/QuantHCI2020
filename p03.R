###########################Load Libraries#######################################
library(purrr)
library(tidyverse)
library(jsonlite)
library(data.table)
library(dplyr)
source('formula.R')
######################Load Data from required files#############################


#####################Load Data from Excel Survey Questions#########################
qq_response <- as_tibble(read.csv("Questionnaire_responses.csv",check.names = FALSE))

  colnames(qq_response)[1] <- "participant_no"
  
  transform(qq_response, participant_no = as.character(participant_no))
  
  
  

###################Load all file paths class-log files################################
path <- "./class-logs"
files <- dir(path, pattern = "*.json")

############Find and make path from paper files##############

mix_files <- list.files(path = "./paper-logs", recursive = TRUE,
                            pattern = "log_Mix", 
                            full.names = TRUE)
ran_files <- list.files(path = "./paper-logs", recursive = TRUE,
                        pattern = "log_Random", 
                        full.names = TRUE)
sen_files <- list.files(path = "./paper-logs", recursive = TRUE,
                        pattern = "log_Sentences", 
                        full.names = TRUE)


#############Load the paper files to a dataframe###############
mix_data <- rbindlist(sapply(mix_files, fread, simplify = FALSE),
                use.names = TRUE, idcol = "FileName")

ran_data <- rbindlist(sapply(ran_files, fread, simplify = FALSE),
                      use.names = TRUE, idcol = "FileName")

sen_data <- rbindlist(sapply(sen_files, fread, simplify = FALSE),
                      use.names = TRUE, idcol = "FileName")



#################Load English Phrases data############################

en_phrase <- read_lines("./phrases/phrases_en.txt") %>%
  as_tibble()

# split tables into different tibbles based on their type
en_sentence<-en_phrase[1:50,]
en_random<-en_phrase[51:100,]
en_mix<-en_phrase[101:150,]


#Load German Phrases data
de_phrase <- read_lines("./phrases/phrases_de.txt") %>%
  as_tibble()

# split tables into different tibbles based on their type
de_sentence<-de_phrase[1:50,]
de_random<-de_phrase[51:100,]
de_mix<-de_phrase[101:150,]






############Create a tibble from the data and label the source file################
class_data <- files %>%
  set_names()%>%
  map_dfr(~fromJSON(file.path(path, .), flatten = TRUE),.id = "file_source") %>%
  mutate(participant_no = as.numeric((str_replace(file_source,".json",""))))


###############Unnest the Transcribe column########################################
withTranscribe <- unnest(class_data,"Transcribe")

#################Place the list column of Action column inside a tibble###############
Action_col <- bind_rows(map(class_data[["Action"]][],as.tibble),.id = "file_source")%>%
  set_names(c("Entry_source","Action", "Letter_no","pressed"))




###################### Combine the survey response to the class data ##########################

combined_class <- left_join(qq_response, class_data, by = c("participant_no"="participant_no"))



##################### Filter the data based on the type and distribute to a new tibble ########

#English Source

fil_c_en_sentences <- combined_class %>% 
  filter(Present %in% en_sentence$value)%>%
  select(,1,13,39:50)
#Add WPM Formula
fil_c_en_sentences<-  mutate(fil_c_en_sentences,wpm = (((nchar(fil_c_en_sentences$Transcribed)-1)/(fil_c_en_sentences$Time/1000))*(60/5)))


fil_c_en_mix <- combined_class %>% 
  filter(Present %in% en_mix$value) %>%
  select(,1,13,39:50)
#Add WPM Formula
fil_c_en_mix<-  mutate(fil_c_en_mix,wpm = (((nchar(fil_c_en_mix$Transcribed)-1)/(fil_c_en_mix$Time/1000))*(60/5)))


fil_c_en_ran <- combined_class %>% 
  filter(Present %in% en_random$value) %>%
  select(,1,13,39:50)
#Add WPM Formula
fil_c_en_ran<-  mutate(fil_c_en_ran,wpm = (((nchar(fil_c_en_ran$Transcribed)-1)/(fil_c_en_ran$Time/1000))*(60/5)))

#German Source

fil_c_de_sentences <- combined_class %>% 
  filter(Present %in% de_sentence$value) %>%
  select(,1,13,39:50)
#Add WPM Formula
  fil_c_de_sentences<-  mutate(fil_c_de_sentences,wpm = (((nchar(fil_c_de_sentences$Transcribed)-1)/(fil_c_de_sentences$Time/1000))*(60/5)))

fil_c_de_mix <- combined_class %>% 
  filter(Present %in% de_mix$value) %>%
  select(,1,13,39:50)

#Add WPM Formula
fil_c_de_mix<-  mutate(fil_c_de_mix,wpm = (((nchar(fil_c_de_mix$Transcribed)-1)/(fil_c_de_mix$Time/1000))*(60/5)))


fil_c_de_ran <- combined_class %>% 
  filter(Present %in% de_random$value) %>%
  select(,1,13,39:50)

#Add WPM Formula
fil_c_de_ran<-  mutate(fil_c_de_ran,
                      wpm =caculateWPM(fil_c_de_ran$Transcribed,fil_c_de_ran$Time)
                       )






inlook <-class_data 
inlook_action <- bind_rows(map(inlook[["Action"]][],as.tibble),.id = "file_source")%>%
  set_names(c("Entry_source","Action", "Letter_no","pressed")) %>%
  tally()



Action_length <- Action_col %>%
  mutate(,participant_no = as.numeric(as.character(Action_length$Entry_source)))
  group_by(Entry_source) %>%
  tally()


  
  
  calculateKE<- function(transcribeCol, actionLength){
    result <- ((nchar(transcribeCol))/(actionLength))
    return(result)
  }

  
  class_iki <- withTranscribe %>%
    group_by(participant_no, Trial) %>%
    mutate(
      IKI = calculateIKI(TimeStamp)
    )






