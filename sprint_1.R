
#intall packages
install.packages("data.table")
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("stringr")
install.packages("DT")
install.packages("knitr")
install.packages("grid")
install.packages("gridExtra")
install.packages("corrplot")
install.packages("methods")
#install.packages("Matrix")
install.packages("reshape2")

install.packages("Rcampdf")
install.packages("ggthemes")
install.packages("qdap")
install.packages("dplyr")
install.packages("tm")
install.packages("wordcloud")
install.packages("plotrix")
install.packages("dendextend")
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("RWeka")
install.packages("reshape2")
install.packages("caret")
library(qdap)
library(dplyr)
library(tm)
library(wordcloud)
library(plotrix)
library(dendextend)
library(ggplot2)
library(ggthemes)
library(RWeka)
library(reshape2)
library(quanteda)


library(irlba)
library(e1071)
library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)
library(biclust)
library(igraph)
library(fpc)
library(Rcampdf)

# load libraries
library(plyr)
library(dtplyr)
library(data.table)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(stringr)
library(rvest)
library(XML)
library(xml2)
library(tidytext)
library(RColorBrewer)
library(wordcloud)
library(DT)
library(gridExtra)
library(devtools)
library(skimr)
library(tm)
library(qdapTools)
library(ggthemes)
library(plot.matrix)
library(dendextend)
library(reshape2)
library(quanteda)
library(corpus)
library(ngram)

#start by loading some libraries
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(DT)
library(knitr)
library(grid)
library(gridExtra)
library(corrplot)
library(methods)
library(Matrix)
library(reshape2)

#set up working directory - this will set the working directory to the same folder as your R studio RMD file - ensure that the CSVs outlined below are also in this folder
current_path <- "C:/Users/skamto/Documents/GitHub/CSDA-1050F18S1/skamto_11060"
setwd("C:/Users/skamto/Documents/GitHub/CSDA-1050F18S1/skamto_11060")
setwd("data")
print( getwd() )
# Read CSV files along with header and replace empty values with "NA" when read the CSV file.
courses_df <- fread("courses.csv",header = TRUE,na.strings = c("") )
assessments_df <- fread("assessments.csv",header = TRUE,na.strings = c("") )
vle_df <- fread("vle.csv",header = TRUE,na.strings = c("") )
studentInfo_df <- fread("studentInfo.csv",header = TRUE,na.strings = c("") )
studentRegistration_df <- fread("studentRegistration.csv",header = TRUE,na.strings = c("") )
studentAssessment_df <- fread("studentAssessment.csv",header = TRUE,na.strings = c("") )
studentVle_df <- fread("studentVle.csv",header = TRUE,na.strings = c("") )


# The dimension of the data
dim(courses_df) 
glimpse(courses_df)
names(courses_df)

dim(studentInfo_df) 
glimpse(studentInfo_df)
names(studentInfo_df)

#Step 1: Data Summary
cat("The number of observations are", nrow(courses_df))
cat("The number of observations are", nrow(assessments_df))
cat("The number of observations are", nrow(vle_df))
cat("The number of observations are", nrow(studentInfo_df))
cat("The number of observations are", nrow(studentRegistration_df))
cat("The number of observations are", nrow(studentAssessment_df))
cat("The number of observations are", nrow(studentVle_df))


summary(courses_df)
summary(assessments_df)
summary(vle_df)
summary(studentInfo_df)
summary(studentRegistration_df)
summary(studentAssessment_df)
summary(studentVle_df)


fillColor = "#FFA07A"
fillColor2 = "#FFA07A"
#student by gender
studentInfo_df %>%
  group_by(gender) %>%
  filter(!is.na(gender)) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  mutate(gender = reorder(gender,Count)) %>%
  arrange(desc(Count)) %>%
  head(10) %>%
  
  ggplot(aes(x = gender,y = Count)) +
  geom_bar(stat='identity',colour="white", fill = fillColor2) +
  geom_text(aes(x = gender, y = 1, label = paste0("(",Count,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'gender', 
       y = 'Count', 
       title = 'Student by Gender') +
  coord_flip() + 
  theme_bw()


#student by region
studentInfo_df %>%
  group_by(region) %>%
  filter(!is.na(region)) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  mutate(region = reorder(region,Count)) %>%
  arrange(desc(Count)) %>%
  head(10) %>%
  
  ggplot(aes(x = region,y = Count)) +
  geom_bar(stat='identity',colour="white", fill = fillColor2) +
  geom_text(aes(x = region, y = 1, label = paste0("(",Count,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Region', 
       y = 'Count', 
       title = 'Count of Student by region') +
  coord_flip() + 
  theme_bw()

#student by ages
studentInfo_df %>%
  group_by(region) %>%
  filter(!is.na(region)) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  mutate(region = reorder(region,Count)) %>%
  arrange(desc(Count)) %>%
  head(10) %>%
  
  ggplot(aes(x = region,y = Count)) +
  geom_bar(stat='identity',colour="white", fill = fillColor2) +
  geom_text(aes(x = region, y = 1, label = paste0("(",Count,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Region', 
       y = 'Count', 
       title = 'Count of Student by region') +
  coord_flip() + 
  theme_bw()

#Count of Student by ages
studentInfo_df %>%
  group_by(age_band) %>%
  filter(!is.na(age_band)) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  mutate(age_band = reorder(age_band,Count)) %>%
  arrange(desc(Count)) %>%
  head(10) %>%
  
  ggplot(aes(x = age_band,y = Count)) +
  geom_bar(stat='identity',colour="white", fill = fillColor2) +
  geom_text(aes(x = age_band, y = 1, label = paste0("(",Count,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Ages', 
       y = 'Count', 
       title = 'Count of Student by age_band') +
  coord_flip() + 
  theme_bw()

#Count of Student by final_result
studentInfo_df %>%
  group_by(final_result) %>%
  filter(!is.na(final_result)) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  mutate(final_result = reorder(final_result,Count)) %>%
  arrange(desc(Count)) %>%
  head(10) %>%
  
  ggplot(aes(x = final_result,y = Count)) +
  geom_bar(stat='identity',colour="white", fill = fillColor2) +
  geom_text(aes(x = final_result, y = 1, label = paste0("(",Count,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'final_result', 
       y = 'Count', 
       title = 'Summary of Student final_result') +
  coord_flip() + 
  theme_bw()



p2 <- ggplot(studentInfo_df, aes(x = final_result)) + geom_bar(aes(fill = final_result)) + 
  theme(axis.text.x = element_blank()) + scale_fill_brewer(palette="Accent")

p3 <- ggplot(studentInfo_df, aes(x = age_band)) + geom_bar(aes(fill = age_band)) + 
  theme(axis.text.x = element_blank()) + scale_fill_brewer(palette="Dark2")


p4 <- ggplot(studentInfo_df, aes(x = final_result)) + geom_bar(aes(fill = region)) + 
  theme(axis.text.x = element_blank()) + scale_fill_brewer(palette="Accent")

p5 <- ggplot(studentInfo_df, aes(x = age_band)) + geom_bar(aes(fill = highest_education)) + 
  theme(axis.text.x = element_blank()) + scale_fill_brewer(palette="Dark2")

grid.arrange(p2, p3, p4, p5, nrow=2, ncol=2)
# These grahphs are having probles loading when zoomed

# Create a function that shows overview of each product about submission method, company response to consumer, timely response, and consumer disputed.

EDA.Sub.product <- as.data.frame(table(consumer$Product))


EDA.Sub.product <- function(dataframe, prod){
  EDAdf <- consumer[consumer$Product == prod,]
  
  colcount.subproduct = length(unique(consumer$Sub.product))
  getPalette = colorRampPalette(brewer.pal(8, "Accent"))
  
  p2.1 <- ggplot(EDAdf, aes(x = Submitted.via)) + geom_bar(aes(fill = Submitted.via)) + 
    theme(axis.text.x = element_blank()) + scale_fill_brewer(palette="Accent") + 
    labs(title = paste("Submission Method for ", prod))
  
  p3.1 <- ggplot(EDAdf, aes(x = Company.response.to.consumer)) + geom_bar(aes(fill = Company.response.to.consumer)) +
    theme(axis.text.x = element_blank()) + scale_fill_brewer(palette="Dark2") +
    labs(title = paste("Company Response to Complaints regarding ", prod))
  
  p4.1 <- ggplot(EDAdf[EDAdf$Timely.response. %in% "No",], aes(x = factor(1), fill = Sub.product)) + geom_bar(width = 1) + 
    coord_polar(theta = "y") + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), 
                                     axis.title.y = element_blank(), axis.title.x = element_blank()) + 
    scale_fill_brewer(palette = "Set3") + 
    labs(title = paste(prod, " failed to responde timely",sep = ""))
  
  p5.1 <- ggplot(EDAdf[EDAdf$Consumer.disputed %in% "Yes",], aes(x = factor(1), fill = Sub.product)) + geom_bar(width = 1) +
    coord_polar(theta = "y") + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), 
                                     axis.title.y = element_blank(), axis.title.x = element_blank()) + 
    scale_fill_brewer(palette = "Set3") + 
    labs(title = paste(prod, " Complaints that Consumer Disputed", sep=""))
  
  if(EDAdf$Sub.product == ""){
    grid.arrange(p2.1, p3.1, nrow=1, ncol=2)
  }
  else{
    grid.arrange(p2.1, p3.1, p4.1, p5.1, nrow=2, ncol=2)
  }
  
}

