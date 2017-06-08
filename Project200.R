####Project200####

#Sifan Liu
#2017.5.4

####SET UP####

library('dplyr')
library('reshape2')
library('readxl')

#setwd("M:/Work In Progress/Project200")
#setwd("/Users/Yuqi/Google Drive/双百计划/Data")
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
#read OES 2016 wage and employment
OES=read_xlsx("national_M2016_dl.xlsx")
OES=select(OES, OCC_CODE, TOT_EMP, A_MEAN,A_MEDIAN)

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
  summarize(freq=n())%>%
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

####match to master database####
ONET.master=left_join(xwalk,ONET.score, by=c("O.NET.SOC.2010.Code"="O.NET.SOC.Code"))
ONET.master$CIP2010.Code=as.character(ONET.master$CIP2010.Code)
ONET.master=ONET.master[!duplicated(ONET.master[,c("CIP2010.Code", "O.NET.SOC.2010.Code","Element.Name")]),]

#merge education data
ONET.master=left_join(ONET.master, ONET.Edu, by=c("O.NET.SOC.2010.Code"="O.NET.SOC.Code"))
#merge tools data
ONET.master=left_join(ONET.master, ONET.Tool, by=c("O.NET.SOC.2010.Code"="O.NET.SOC.Code"))
#merge employment and wage
ONET.master=left_join(ONET.master, OES,by=c("SOC2010Code"="OCC_CODE") )

#### 13 Majors Data####
majors=read.csv("ch2cip.csv", colClasses = "character")
Major=apply(majors[3:16],1,as.list)
names(Major)=majors$Code

TEST=list('14.4201','15.0405','15.0406','15.0613')

for (i in 1:13){
  Major[[i]]=ONET.master%>%
    filter(CIP2010.Code %in% Major[[i]])%>%
    unique()%>%
#filter
#RW<=6 working experience fewer than 2 years
#RL<=6 required level of eduation sub-BA
    filter(RL<=6 & RW <=6)
  write.csv(Major[[i]],file=paste0('results/all majors/', names(Major)[i],'.csv'))
}

#### Create Appendix #####
#(note: currently the appendix table does provide the most typical technology and tools, 
### as I think we would need to read through the list of all technology and tools, and pick a few to write)
head(Qinzhou.ONET)
#create combined "elements", and reshape it into wide format
df.elements <- ddply(Qinzhou.ONET, .(O.NET.SOC.2010.Code, Measure), summarize, Elements = paste(Element.Name, collapse = ", "))
df.elements <- dcast(df.elements,O.NET.SOC.2010.Code ~ Measure, value.var='Elements')
#reshape other variabels into wide format
df.everythingelse <- dcast(Qinzhou.ONET, O.NET.SOC.2010.Code + O.NET.SOC.2010.Title+
                                          TOT_EMP + A_MEAN + A_MEDIAN +
                                          OJ + PT + RL + RW +
                                          Technology + Tools ~ Measure, value.var='Element.Name', length)
df.everythingelse <- df.everythingelse[c("O.NET.SOC.2010.Code","O.NET.SOC.2010.Title",
                                          "TOT_EMP", "A_MEAN", "A_MEDIAN",
                                         "OJ", "PT", "RL", "RW")]
#merge
df.appendix <- full_join(df.everythingelse, df.elements, by = "O.NET.SOC.2010.Code")

#transpose
#df.appendix <- t(df.appendix)



####Write results####
write.csv(df.appendix, file='results/Qinzhou_Appendix.csv')

