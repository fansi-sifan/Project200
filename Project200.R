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
names(ONET) <- allfiles
#read education
Edu = read.table("db_21_2_text/Education, Training, and Experience.txt", sep='\t', header=TRUE)
Edu.Cat=read.table("db_21_2_text/Education, Training, and Experience Categories.txt", 
                   sep = '\t', header=TRUE, fill=TRUE, quote=NULL)

#read technology and tools
Tools = read.table("db_21_2_text/Tools and Technology.txt", sep='\t', header=TRUE, fill = TRUE)
#read xwalk
xwalk.CIP=read.csv('cip2soc.csv')
xwalk.ONET=read.csv('onet2soc.csv')

####Clean ONET scores####

#Keep the education level with highest frequency (mode)
ONET.Edu=Edu%>%
  group_by(O.NET.SOC.Code, Element.ID)%>%
  filter(Data.Value==max(Data.Value))%>%
  ungroup()%>%
  select(O.NET.SOC.Code, Scale.ID, Category)%>%
  dcast(O.NET.SOC.Code ~ Scale.ID, value.var='Category', mean)

ONET.Edu[2:5]=lapply(ONET.Edu[2:5],as.integer)

#Count number of tools and technology
ONET.Tool=Tools%>%group_by(O.NET.SOC.Code, T2.Type)%>%
  summarise(freq=n())%>%
  ungroup()%>%
  dcast(O.NET.SOC.Code ~ T2.Type, value.var='freq', mean)

#Keep the 10 highest ONET score
ONET.score=ONET%>%
#only look at 'level', discard 'IM' importance
    lapply(filter,Scale.ID=="LV")%>%
    lapply(group_by,...=O.NET.SOC.Code)%>%
#select 10 highest elements, ranked by data.value
    lapply(top_n,n=10,wt=Data.Value)%>%
    lapply(ungroup)

#rbind 4 dataframes into one
Measures <-list ("Abilities","Knowledge", "Skills", "WorkActivities")
ONET.score=Map(cbind, ONET.score, Measure=Measures)
ONET.score=do.call("rbind",ONET.score)
ONET.score=ONET.score%>%
  arrange(O.NET.SOC.Code, Measure, Data.Value)%>%
  select(O.NET.SOC.Code, Measure, Element.Name, Data.Value, Date)


####xwalk from CIP to ONET####
xwalk=left_join(xwalk.CIP, xwalk.ONET, by=c("SOC2010Code"="SOC.2010.Code"))
xwalk=select(xwalk, -contains("SOC2010"))

####match to master database####
ONET.master=left_join(xwalk,ONET.score, by=c("O.NET.SOC.2010.Code"="O.NET.SOC.Code"))
ONET.master$CIP2010.Code=as.character(ONET.master$CIP2010.Code)

#merge education data
ONET.master=left_join(ONET.master, ONET.Edu, by=c("O.NET.SOC.2010.Code"="O.NET.SOC.Code"))
#merge tools data
ONET.master=left_join(ONET.master, ONET.Tool, by=c("O.NET.SOC.2010.Code"="O.NET.SOC.Code"))

####Qinzhou Data####
Qinzhou=list('14.4201','15.0405','15.0406','15.0613')
Qinzhou.ONET=ONET.master%>%
  filter(CIP2010.Code %in% Qinzhou)%>%
  unique()

####Write results####
write.csv(ONET.master, file='results/master.csv')
write.csv(Qinzhou.ONET, file='results/Qinzhou.csv')
