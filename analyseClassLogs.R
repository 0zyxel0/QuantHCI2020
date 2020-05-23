library(tidyverse)
library(dplyr)
library(plotly)
source('formula.R')
import::from(plotly, ggplotly)
import::from(jsonlite, fromJSON)



#Load Questionnaire
df <- read.csv("Questionnaire_responses.csv",header = TRUE)
#Select column and label user to touch typist or not ,and which language user type in.
type_typist <- df %>%
  select(PID, Do.you.use.the.touch.typing.systems.with.all.10.fingers., In.this.study..which.language.did.you.type.in.) %>%
  mutate(Typist = case_when(Do.you.use.the.touch.typing.systems.with.all.10.fingers. == 'Yes' ~ 'touch_typist',
                            Do.you.use.the.touch.typing.systems.with.all.10.fingers. == 'No' ~ 'non_touch_typist')) %>%
  select(PID,Typist, In.this.study..which.language.did.you.type.in.)  

#Separate phrase into 3 categories: random, sentence, mix sentence
#load Phrases in TextTest++ to label the phrases in class logs.
#load Phrases in TextTest++ and label them into 3 categories
phrase_de <- read.delim("Phrases/phrases_de.txt", header = FALSE, col.names = c("Present")) %>%
  mutate(Stimulus_Type = case_when( row_number() <= 50 ~  'sentence',
                                    row_number() > 50 & row_number() <= 100 ~ 'random',
                                    row_number() > 100 ~ 'mix'))
phrase_en <- read.delim("Phrases/phrases_en.txt", header = FALSE, col.names = c("Present")) %>%
  mutate(Stimulus_Type = case_when( row_number() <= 50 ~  'sentence',
                                    row_number() > 50 & row_number() <= 100 ~ 'random',
                                    row_number() > 100 ~ 'mix'))

#Get file list from class-logs folder
file_list <- list.files(path="class-logs")
file_list <- file_list[1:length(file_list)-1]

##results tibble
result_random <- tibble(PID=integer(), Typist=character(), avg_UER=numeric(), WPM=numeric(), Avg.IKI=numeric(), KE=numeric())
result_sentence <- tibble(PID=integer(),Typist=character(), avg_UER=numeric(), WPM=numeric(), Avg.IKI=numeric(), KE=numeric())
result_mix <- tibble(PID=integer(), Typist=character(), avg_UER=numeric(), WPM=numeric(), Avg.IKI=numeric(), KE=numeric())

for (i in 1:length(file_list)){
  #join each json file with stimulus type and order by stimulus type#
  user_info <- filter(type_typist, PID == (i - 1))
  if (user_info$In.this.study..which.language.did.you.type.in. == "English") {
    user_log <- fromJSON(read_file(paste("class-logs/", file_list[i], sep = ""))) %>%
      inner_join(phrase_en)
  } else{
    user_log <- fromJSON(read_file(paste("class-logs/", file_list[i], sep = ""))) %>%
      inner_join(phrase_de)
  }
  
  #Calculate WPM
  wpm <- user_log %>%
    group_by(Stimulus_Type) %>%
    summarise(WPM=mean(caculateWPM(Transcribed, Time)))
  
  mix_wpm <- wpm$WPM[1]
  random_wpm <- wpm$WPM[2]
  sen_wpm <- wpm$WPM[3]
  
  #sum uer
  uer <- user_log %>% 
    group_by(Stimulus_Type) %>% 
    summarise(UER = mean(as.numeric(UER)))
    
  mix_uer <- uer$UER[1] 
  random_uer <- uer$UER[2]
  sen_uer <- uer$UER[3]
  
  #Calculate KE
  ke <- user_log %>%
    group_by(Stimulus_Type) %>%
    summarise(KE=mean(calculateKE(Transcribed, lengths(Action))))
  
  mix_ke <- ke$KE[1] 
  random_ke <- ke$KE[2]
  sen_ke <- ke$KE[3]
  
  #add result
  result_mix <-result_mix %>% 
    add_row(PID=(i- 1), Typist=user_info$Typist, avg_UER=mix_uer, WPM=mix_wpm, KE=mix_ke)
  result_random <- result_random %>% 
    add_row(PID=(i- 1), Typist=user_info$Typist, avg_UER=random_uer, WPM=random_wpm, KE=random_ke)
  result_sentence <-result_sentence %>% 
    add_row(PID=(i- 1), Typist=user_info$Typist, avg_UER=sen_uer, WPM=sen_wpm, KE=sen_ke)
}

#split result to touch-typist and non-touch typist
non_t_mix <- result_mix %>%
  group_by(Typist) %>%
  subset(Typist == "non_touch_typist")
non_t_random <- result_random %>%
  group_by(Typist) %>%
  subset(Typist == "non_touch_typist")
non_t_sentence <- result_sentence %>%
  group_by(Typist) %>%
  subset(Typist == "non_touch_typist")

t_mix <- result_mix %>%
  group_by(Typist) %>%
  subset(Typist == "touch_typist")
t_random <- result_random %>%
  group_by(Typist) %>%
  subset(Typist == "touch_typist")
t_sentence <- result_sentence %>%
  group_by(Typist) %>%
  subset(Typist == "touch_typist")

#draw UER  Bar graph
uer_class <- plot_ly() %>% 
  add_bars(x = ~non_t_mix$PID,
          y = ~non_t_mix$avg_UER,
          name = "Mix: Non-touch Typists",
          marker = list(color = "teal")) %>%
  add_bars(x = ~non_t_random$PID,
           y = ~non_t_random$avg_UER, 
           name = "Random: Non-touch Typists",
           marker = list(color = "#69b3a2")) %>% 
  add_bars(x = ~non_t_sentence$PID,
           y = ~non_t_sentence$avg_UER,
           name = "Sentence: Non-touch Typists",
           marker = list(color = "#006284")) %>%
  add_bars(x = ~t_mix$PID,
           y = ~t_mix$avg_UER, 
           name = "Mix: Touch Typist",
           marker = list(color = "#AB3B3A")) %>% 
  add_bars(x = ~t_random$PID,
           y = ~t_random$avg_UER,
           name = "Random: Touch Typist",
           marker = list(color = "#F05E1C")) %>% 
  add_bars(x = ~t_sentence$PID,
           y = ~t_sentence$avg_UER,
           name = "Sentence: Touch Typist",
           marker = list(color = "#FFC408")) %>% 
  layout(barmode = "stack",
         xaxis = list(title = "Participants",
                      zeroline = FALSE),
         yaxis = list(title = "Uncorrected Error Rate",
                      zeroline = FALSE))

#Draw WPM  Bar graph
wpm_class <- plot_ly() %>%
  add_bars(x = ~non_t_mix$PID,
           y = ~non_t_mix$WPM,
           name = "Mix: Non-touch Typists",
           marker = list(color = "teal")) %>%
  add_bars(x = ~non_t_random$PID,
           y = ~non_t_random$WPM,
           name = "Random: Non-touch Typists",
           marker = list(color = "#69b3a2")) %>% 
  add_bars(x = ~non_t_sentence$PID,
           y = ~non_t_sentence$WPM, 
           name = "Sentence: Non-touch Typists",
           marker = list(color = "#006284")) %>%
  add_bars(x = ~t_mix$PID,
           y = ~t_mix$WPM, 
           name = "Mix: Touch Typist",
           marker = list(color = "#AB3B3A")) %>% 
  add_bars(x = ~t_random$PID,
           y = ~t_random$WPM,
           name = "Random: Touch Typist",
           marker = list(color = "#F05E1C")) %>% 
  add_bars(x = ~t_sentence$PID,
           y = ~t_sentence$WPM,
           name = "Sentence:Touch Typist",
           marker = list(color = "#FFC408")) %>% 
  layout(barmode = "stack",
         xaxis = list(title = "Participants",
                      zeroline = FALSE),
         yaxis = list(title = "Word Per Minute",
                      zeroline = FALSE))
#Draw KE Graph
sub_mpg <- mpg[mpg$class %in% c("mix", "random", "sentence"),]
ggplot(sub_mpg, aes(class, displ, color=factor(cyl))) + geom_quasirandom(dodge.width=1)
