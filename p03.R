###########################Load Libraries#######################################
library(purrr)
library(tidyverse)
library(jsonlite)
library(data.table)
library(dplyr)
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







