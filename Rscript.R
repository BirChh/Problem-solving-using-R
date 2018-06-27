rm(list = ls())
#Packages used#####
library(dplyr)
library(ggplot2)
library(eeptools)
library(tibble)
library(gridExtra)
library(xtable)
##Reading data#######
setwd("C:/Users/Admin/Desktop/DS_Test")
df=read.csv("merged_data.csv")
sapply(df, function(x) length(unique(x))) #checks data lengths
str(df) #dataframe structure

##Date formatting
df$visit_date <- as.Date(df$visit_date,format="%d/%m/%Y")
df$dob <- as.Date(df$dob,format="%d/%m/%Y")
str(df)
df$age <- floor(age_calc(df$dob,units = "years")) #convert dob to age
df$name_dob <- paste(df$name,"-",df$dob)# concatenate name and dob to prevent 
#ambiguity with duplicate names

##pull unique name and their number of visits to the pub
total_visit <-df %>% group_by(name_dob) %>% summarise(count=n())
total_visit <- rename(total_visit,visits=count)

##pull unique dates and number of visitors
visits_by_date <-df %>% group_by(visit_date) %>% summarise(count=n()) #count total visitors daily

##Filter visitors who visted the pub on the day theft occured
df_filtered <- df %>% select(visit_date,name_dob, age) %>% filter(df$Theft_occured == "1")
df_filtered_count <-df_filtered %>% group_by(name_dob) %>% summarise(count=n())
#count number of visitors on days when theft occured
df_theft_count <-df_filtered %>% group_by(visit_date) %>% summarise(count=n())
visit_count <- rename(df_filtered_count,theft_day_visit=count) #rename column
order_count <- arrange(visit_count,desc(theft_day_visit)) # order them in descending order

#########Visualisation##########

## Visitors volume over the year
p1 <- ggplot(visits_by_date,aes(visit_date,count))+
  geom_line(stat="identity",color="#0000e6")+
  ggtitle("Daily visitors Volume")+
  scale_x_date(date_breaks = "10 days")+
  theme(axis.text.x=element_text(angle=45,hjust=1),
        panel.background = element_rect(colour = "grey"),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Daily Visitors' Volume (June 2016-January 2017)",
       y = "Number of Visitors",
       x = "Time on Daily scale")+
  geom_vline(xintercept = df_theft_count$visit_date, linetype=2)
p1 
##Visitors age distribution

p2 <- ggplot (df, aes(x = age))+
  geom_histogram(position="dodge", fill="#ff9900")+
    labs(title = "Frequency distribution of visitor's age",
       x = "Age",
       y = "Frequency")+
  theme(plot.title = element_text(hjust = 0.5))
p2

##Join the dataframes of total visits and theft day visit by unique name
new_df <- merge(x=total_visit, y=visit_count, by="name_dob", all = TRUE)
new_df <- na.omit(new_df)

###Calculation of posterior probabilities
new_df$p_A <- round(new_df$visits/200,2)#probability of visitor Ai visiting the pub
new_df$p_B <- round(new_df$theft_day_visit/32,2)#probability of visitor Bi stealing the mobile
new_df$likelihood <- round(new_df$p_A*new_df$p_B/new_df$p_B,2)##conditional probability
new_df$Likelihood_times_P_B <- round(new_df$likelihood*new_df$p_B,2)##likelihood
new_df$posterior <- round(new_df$Likelihood_times_P_B/sum(new_df$Likelihood_times_P_B),5)
new_df$posterior_percent <- round(new_df$posterior*100,4)

##Selecting the top 20 suspects and ranking them with their relative hances
new_df <- arrange(new_df,desc(posterior_percent))
ranked <- head(new_df,20)# select first 2
ranked$relative_rank <-round(ranked$posterior_percent/sum(ranked$posterior_percent)*100,5)
ranked$name_dob <- factor(ranked$name_dob, levels = ranked$name_dob[order((ranked$relative_rank))])

##Visual plot of suspects ranking 
p3 <- ggplot(ranked, aes(y = name_dob, x =relative_rank ))
p3 + geom_point(colour = "#b30000") +
  geom_segment(aes(x = 4, y = name_dob, xend = relative_rank,yend=name_dob),linetype = 1) +
  labs(title ="Top twenty suspects with their relative chances of committing theft",
       x = "Relative Percent Chance",
       y = "Names ")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_text(aes(label=relative_rank), hjust = -.2,size = 3)+
  scale_x_continuous(limits = c(4,6.5))

##Merge the final data frame to the original and print list of suspects in a table
merge_final <- merge(x=df_filtered, y=ranked, by="name_dob", all = TRUE)
merge_final <- na.omit(merge_final)
merge_final <- select(merge_final,name_dob,age, visits,theft_day_visit,relative_rank)
merge_final <- unique(merge_final, by="name")
merge_final <- arrange(merge_final,desc(relative_rank))##create table in markdown

#################################END of Script##################################################
