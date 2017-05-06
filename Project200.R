####Project200####

#Sifan Liu
#2017.5.4

####SET UP####
library('dplyr')
library('reshape2')

#setwd("M:/Work In Progress/Project200")
setwd("/Users/Fancy/Google Drive/双百计划/Data")

####READ DATA####
#Read all ONET files with score (abilities, knowledge, skills, work activities)
allfiles <- list.files(path="ONET",full.names = TRUE, all.files = FALSE)
ONET <- lapply (allfiles, read.table, sep='\t', header=TRUE)
names(ONET) <- list.files(path="ONET",full.names = TRUE)
#read education
Edu = read.table("db_21_2_text/Education, Training, and Experience.txt", sep='\t', header=TRUE)

#Keep the education level with highest frequency (mode)
Edu=Edu%>%
  group_by(O.NET.SOC.Code, Element.ID)%>%
  filter(Scale.ID=="RL")%>%
  filter(Data.Value==max(Data.Value))%>%
  ungroup()%>%
  select(O.NET.SOC.Code, Category)

#read technology and tools
Tools = read.table("db_21_2_text/Tools and Technology.txt", sep='\t', header=TRUE, fill = TRUE)

#read xwalk
xwalk=read.csv('CIP2ONET.csv')

####Clean ONET scores####
ONET.score=ONET%>%
#only look at 'level', discard 'IM' importance
    lapply(filter,Scale.ID=="LV")%>%
    lapply(group_by,...=O.NET.SOC.Code)%>%
#select 10 highest elements, ranked by data.value
    lapply(top_n,n=10,wt=Data.Value)


