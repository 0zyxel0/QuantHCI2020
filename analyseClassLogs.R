# Libraries
library(tidyverse)
library(dplyr)
library(plotly)
library(data.table)
library(purrr)
source('formula.R')
import::from(plotly, ggplotly)
import::from(jsonlite, fromJSON)

#Load Questionnaire
df <- read.csv("Questionnaire_responses.csv",header = TRUE)
#Select column and label user to touch typist or not ,and which language user type in.
type_typist <- df %>%
  select(PID, Do.you.use.the.touch.typing.systems.with.all.10.fingers., 
         How.many.years.of.experience.do.you.have.in.using.the.touch.typing.systems.with.all.10.fingers.,
         In.this.study..which.language.did.you.type.in.) %>%
  mutate(Typist = case_when((Do.you.use.the.touch.typing.systems.with.all.10.fingers. == 'Yes') &
                            (How.many.years.of.experience.do.you.have.in.using.the.touch.typing.systems.with.all.10.fingers. > 1)  ~ 'touch_typist',
                            TRUE ~ 'non_touch_typist')) %>%
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

#results tibble
result_random <- tibble(PID=integer(), Typist=character(), avg_UER=numeric(), WPM=numeric(), Avg.IKI=numeric(), KE=numeric(), Sti_Type=character())
result_sentence <- tibble(PID=integer(),Typist=character(), avg_UER=numeric(), WPM=numeric(), Avg.IKI=numeric(), KE=numeric(), Sti_Type=character())
result_mix <- tibble(PID=integer(), Typist=character(), avg_UER=numeric(), WPM=numeric(), Avg.IKI=numeric(), KE=numeric(), Sti_Type=character())

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
  
  #Sum UER
  uer <- user_log %>% 
    group_by(Stimulus_Type) %>% 
    summarise(UER = mean(as.numeric(UER)))
  
  #Calculate AVG.IKI
  avg_iki <- user_log %>%
    unnest(Transcribe) %>%
    mutate(IKI = calculateIKI(TimeStamp)) %>%
    group_by(Trial, Stimulus_Type) %>%
    summarise(AVG_iki = mean(IKI)) %>%
    group_by(Stimulus_Type) %>%
    summarise(AVG_IKI = mean(AVG_iki))

  #Calculate KE
  ke <- user_log %>%
    group_by(Stimulus_Type) %>%
    summarise(KE=mean(calculateKE(Transcribed, lengths(Action))))
  
  #add result
  result_mix <-result_mix %>% 
    add_row(PID = (i- 1), Typist = user_info$Typist, avg_UER = uer$UER[1], 
            WPM = wpm$WPM[1], Avg.IKI = avg_iki$AVG_IKI[1], KE = ke$KE[2], Sti_Type='Mix')
  result_random <- result_random %>% 
    add_row(PID = (i- 1), Typist = user_info$Typist, avg_UER = uer$UER[2], 
            WPM = wpm$WPM[2], Avg.IKI = avg_iki$AVG_IKI[2], KE = ke$KE[2], Sti_Type='Random')
  result_sentence <-result_sentence %>% 
    add_row(PID = (i- 1), Typist = user_info$Typist, avg_UER = uer$UER[3], 
            WPM = wpm$WPM[3], Avg.IKI = avg_iki$AVG_IKI[3] , KE = ke$KE[3], Sti_Type="Sentence")
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
         title = "Uncorrected Error Rate of Non-touch Typist and Touch Typist",
         xaxis = list(title = "Participants",
                      zeroline = FALSE),
         yaxis = list(title = "Uncorrected Error Rate (%)",
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
         title = "Word Per Minute of Non-touch Typist and Touch Typist",
         xaxis = list(title = "Participants",
                      zeroline = FALSE),
         yaxis = list(title = "Word Per Minute ()",
                      zeroline = FALSE))

#Prepare KE graph data
ke_graph_data <- rbind(result_mix, result_random) %>%
  rbind(result_sentence) %>%
  select(Sti_Type, KE, Typist)
#Draw KE boxplot
ke_class <- plot_ly(ggplot2::diamonds, x = ~ke_graph_data$Sti_Type, y = ~ke_graph_data$KE, 
                    color = ~ke_graph_data$Typist, type = "box", quartilemethod="inclusive") %>%
  layout(boxmode = "group",
    title = "Keyboard Efficiency of Non-touch Typist and Touch Typist",
    xaxis = list(title='Stimulus Type'), 
    yaxis = list(title='Keyboard Efficiency (%)'))

#Prepare AVG.IKI graph data
avg_iki_graph_data <- rbind(result_mix, result_random) %>%
  rbind(result_sentence) %>%
  select(Sti_Type, Avg.IKI, Typist)
#Draw Avg.IKI boxplot
avg_iki_class <- plot_ly(ggplot2::diamonds, x = ~avg_iki_graph_data$Sti_Type, y = ~avg_iki_graph_data$Avg.IKI, 
                         color = ~avg_iki_graph_data$Typist, type = "box", quartilemethod="inclusive") %>%
  layout(boxmode = "group",
         title = "Average Inter-key interval of Non-touch Typist and Touch Typist",
         xaxis = list(title='Stimulus Type'), 
         yaxis = list(title='Average Inter-key interval (ms)'))


################# Unit of data analysis ################################

# RQ1: different typing performance between touch typists and non-touch typist?
# Approch: For four metrics, explore if the performance average values differs. ANOVA (Analysis of variance) would the first option to
#          test the equavalence of average values. But normarlity test and homogeneity test are prepositive in order to guarantee the 
#          reliance of ANOVA. 

results <- bind_rows(result_mix, result_random, result_sentence)

## For Uncorrected Error Rate (UER) metric

# Normality Test for UER metric, H0 hypothesis is that data is normally distributed. 

 results_t <- results %>%
  filter(Typist == "touch_typist")
 results_non_t <- results %>%
   filter(Typist == "non_touch_typist")


shapiro.test(results_t[["avg_UER"]])
shapiro.test(results_non_t[["avg_UER"]])

##Here two p values will be shown, if p<0.05, then H0 would be rejected, which means data does not comply normality. 

# Homogeneity test of variance, H0 hypothsis is that The two populations have homogeneous variances
bartlett.test(results[["avg_UER"]], results[["Typist"]])

##If p value is smaller than 0.05, two populations' variance are not homogeneous

# ANOVA, usability depending on previous tests. H0 is that two populations' mean value is equavalent 

a.aov <- aov(results[["avg_UER"]] ~ results[["Typist"]])
summary(a.aov)

# Above analysis is ANOVA and its corresponding prepositive tests, if conditions of ANOVA are not met, then non-parameter test would be employed
# Non-parameter test: Mann-Whitney U Test, H0：The two populations are equal versus
wilcox.test(results[["avg_UER"]] ~ results[["Typist"]])


## For Words Per Minute (WPM) metric

# Normality Test for WPM metric, H0 hypothesis is that data is normally distributed. 

results_t <- results %>%
  filter(Typist == "touch_typist")
results_non_t <- results %>%
  filter(Typist == "non_touch_typist")


shapiro.test(results_t[["WPM"]])
shapiro.test(results_non_t[["WPM"]])

##Here two p values will be shown, if p<0.05, then H0 would be rejected, which means data does not comply normality. 

# Homogeneity test of variance, H0 hypothsis is that The two populations have homogeneous variances
bartlett.test(results[["WPM"]], results[["Typist"]])

##If p value is smaller than 0.05, two populations' variance are not homogeneous

# ANOVA, usability depending on previous tests. H0 is that two populations' mean value is equavalent 

a.aov <- aov(results[["WPM"]] ~ results[["Typist"]])
summary(a.aov)

# Above analysis is ANOVA and its corresponding prepositive tests, if conditions of ANOVA are not met, then non-parameter test would be employed
# Non-parameter test: Mann-Whitney U Test, H0：The two populations are equal versus
wilcox.test(results[["WPM"]] ~ results[["Typist"]])


## For Keyboard Efficiency metric

## Normality Test
shapiro.test(results_t[["KE"]])
shapiro.test(results_non_t[["KE"]])

##Here two p values will be shown, if p<0.05, then H0 would be rejected, which means data does not comply normality. 

# Homogeneity test of variance, H0 hypothsis is that The two populations have homogeneous variances
bartlett.test(results[["KE"]], results[["Typist"]])

##If p value is smaller than 0.05, two populations' variance are not homogeneous

# ANOVA, usability depending on previous tests. H0 is that two populations' mean value is equavalent 

a.aov <- aov(results[["KE"]] ~ results[["Typist"]])
summary(a.aov)

# Above analysis is ANOVA and its corresponding prepositive tests, if conditions of ANOVA are not met, then non-parameter test would be employed
# Non-parameter test: Mann-Whitney U Test, H0：The two populations are equal versus
wilcox.test(results[["KE"]] ~ results[["Typist"]])






# RQ2: How does fimilarity of text influence the typing performance?

## For Uncorrected Error Rate metric

# Normality Test for UER metric, H0 hypothesis is that data is normally distributed. 
shapiro.test(result_sentence[["avg_UER"]])
shapiro.test(result_mix[["avg_UER"]])
shapiro.test(result_random[["avg_UER"]])

##Here p values will be shown, if p<0.05, then H0 would be rejected, which means data does not comply normality. 


# Homogeneity test of variance, H0 hypothsis is that The two populations have homogeneous variances
bartlett.test(results[["avg_UER"]], results[["Sti_Type"]])

##If p value is smaller than 0.05, two populations' variance are not homogeneous

# ANOVA, usability depending on previous tests. H0 is that two populations' mean value is equavalent 

a.aov <- aov(result_sentence[["avg_UER"]] ~ result_sentence[["Sti_Type"]])
summary(a.aov)

# Above analysis is ANOVA and its corresponding prepositive tests, if conditions of ANOVA are not met, then non-parameter test would be employed
# Non-parameter test: Kruskal-Wallis Test, H0: N (three in our case) populations are equal versus

kruskal.test(results[["avg_UER"]] ~ results[["Sti_Type"]])


## For Words per minute (WPM) metric

# Normality Test for UER metric, H0 hypothesis is that data is normally distributed. 
shapiro.test(result_sentence[["WPM"]])
shapiro.test(result_mix[["WPM"]])
shapiro.test(result_random[["WPM"]])

##Here p values will be shown, if p<0.05, then H0 would be rejected, which means data does not comply normality. 


# Homogeneity test of variance, H0 hypothsis is that The two populations have homogeneous variances
bartlett.test(results[["WPM"]], results[["Sti_Type"]])

##If p value is smaller than 0.05, two populations' variance are not homogeneous

# ANOVA, usability depending on previous tests. H0 is that two populations' mean value is equavalent 

a.aov <- aov(result_sentence[["WPM"]] ~ result_sentence[["Sti_Type"]])
summary(a.aov)

# Above analysis is ANOVA and its corresponding prepositive tests, if conditions of ANOVA are not met, then non-parameter test would be employed
# Non-parameter test: Kruskal-Wallis Test, H0: N (three in our case) populations are equal versus

kruskal.test(results[["WPM"]] ~ results[["Sti_Type"]])

  
## For Keyboard Efficiency metric

# Normality Test for UER metric, H0 hypothesis is that data is normally distributed. 
shapiro.test(result_sentence[["KE"]])
shapiro.test(result_mix[["KE"]])
shapiro.test(result_random[["KE"]])

##Here p values will be shown, if p<0.05, then H0 would be rejected, which means data does not comply normality. 


# Homogeneity test of variance, H0 hypothsis is that The two populations have homogeneous variances
bartlett.test(results[["KE"]], results[["Sti_Type"]])

##If p value is smaller than 0.05, two populations' variance are not homogeneous

# ANOVA, usability depending on previous tests. H0 is that two populations' mean value is equavalent 

a.aov <- aov(result_sentence[["KE"]] ~ result_sentence[["Sti_Type"]])
summary(a.aov)

# Above analysis is ANOVA and its corresponding prepositive tests, if conditions of ANOVA are not met, then non-parameter test would be employed
# Non-parameter test: Kruskal-Wallis Test, H0: N (three in our case) populations are equal versus

kruskal.test(results[["KE"]] ~ results[["Sti_Type"]])

#






####################### PAPER DATA#####################

all_paper_files <- list.files(path = "paper-logs", recursive = TRUE,
                              pattern = "_log_", 
                              full.names = TRUE)
all_paper <- rbindlist(sapply(all_paper_files, fread, simplify = FALSE),
                       use.names = TRUE, idcol = "FileName")



pp_response <- as_tibble(read.csv("paper_responses.csv",check.names = FALSE))  
colnames(pp_response)[1] <- "user_id"
colnames(pp_response)[13]<- "Touchtyping_years"

paper_data <- left_join(pp_response, all_paper, by=c("user_id"="user_id")) %>%
  select(1,12,13,38:52) 
colnames(paper_data)[2] <- "TouchTypist"



paper_analysis<-paper_data %>% 
  select(user_id, TouchTypist, Touchtyping_years,ke,uer, iki, sd_iki, wpm, input_time_ms, condition) %>%
  mutate(Typist= case_when(Touchtyping_years >=1 ~ "touch_typist",Touchtyping_years<=1 ~ "non_touch_typist")) %>%
  group_by(user_id,Typist,condition) %>%
  summarise(WPM=mean(wpm),avg_UER=mean(uer),avg_IKI=mean(iki),KE=mean(ke))



colnames(paper_analysis)[3] <- "Sti_Type"
colnames(paper_analysis)[1] <- "PID"

paper_analysis<-nest(paper_analysis, WPM=c(WPM),Typist=c(Typist),avg_UER=c(avg_UER),avg_IKI=c(avg_IKI),KE=c(KE),Sti_Type=c(Sti_Type))

wilcox.test(paper_analysis[["KE"]] ~ paper_analysis[["Typist"]])

kruskal.test(paper_analysis[["avg_UER"]] ~ paper_analysis[["Sti_Type"]])
kruskal.test(paper_analysis[["KE"]] ~ paper_analysis[["Sti_Type"]])
kruskal.test(paper_analysis[["WPM"]] ~ paper_analysis[["Sti_Type"]])