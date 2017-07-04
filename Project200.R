####Project200####

#Sifan Liu
#2017.5.4

#### SET UP####

library('dplyr')
library('reshape2')
library('readxl')
library('xlsx')

#setwd("M:/Work In Progress/Project200")
#setwd("/Users/Yuqi/Google Drive/双百计划/Data")
setwd("/Users/Fancy/Google Drive/双百计划/Data")

#### READ DATA####
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

#### Clean ONET scores####

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
  arrange(O.NET.SOC.Code, Measure, desc(Data.Value))%>%
  select(O.NET.SOC.Code, Measure, Element.Name, Data.Value, Date)


#### xwalk from CIP to ONET####
xwalk=left_join(xwalk.CIP, xwalk.ONET, by=c("SOC2010Code"="SOC.2010.Code"))

#### match to master database####
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
All_major=Major
for (i in 1:13){
  All_major[[i]]=ONET.master%>%
    filter(CIP2010.Code %in% Major[[i]])%>%
    unique()%>%
#filter
#RW<=6 working experience fewer than 2 years
#RL<=6 required level of eduation sub-BA
    filter(RL<=6 & RW <=6)
  write.xlsx(All_major[[i]], file='results/all majors/all.xlsx', names(Major)[i], append = TRUE)
  #write to separate csv file
  #write.csv(Major[[i]],file=paste0('results/all majors/', names(Major)[i],'.csv'))
}


#### write to text, summarize different majors in text ####

Text_major=ONET.master%>%
  filter(CIP2010.Code %in% unlist(Major))%>%
  filter(RL<=6 & RW <=6)%>%
  unique()%>%
  select(CIP2010.Code, O.NET.SOC.2010.Title, Measure, Element.Name, Data.Value, OJ, PT)

##YL: export text_major in a csv file, use index/match to find the translation, read in Text_translated
write.csv(Text_major, file='results/Text_major.csv')

##YL note to SL: line 92-100生成的spreadsheet “all.xlsx”里的“PHREGN”tab里只有一个专业51.0805，对应的职位是Pharmacy Technicians
##但line 92-100生成的spreadsheet“PHREGN.csv”里只有51.2006专业，对应的职位是Pharmacists。我之前用google sheet翻译的时候只翻译了51.2006，并没有翻译51.0805。
##所以我在翻译line108-112生成的spreadsheet是发现了这个问题。翻译来讲问题不大，我补翻了几个词就行【已做】。只是想make a note，你用空看看line 92-100有没有小bug？
##我感觉“all.xlsx”和line108-112生成的spreadsheet应该是一样的，只是不知道为什么“all.xlsx”和“PHREGN.csv”会有出入。

Text_major_translated=read_xlsx("results/Text_major_with_translation.xlsx")
Text_major_translated=Text_major_translated[c(-1)]

Text=Text_major_translated%>%group_by(CIP2010.Code, O.NET.SOC.2010.Title.Translate, Measure)%>%
  summarise(Elements=paste(Element.Name.Translate, collapse=","), 
            OJ=mean(OJ),
            PT=mean(PT))%>%
  filter(!is.na(Measure))

Text_wide=dcast(Text, CIP2010.Code+O.NET.SOC.2010.Title.Translate+OJ+PT ~ Measure, value.var="Elements")

cat(paste(Text_wide$CIP2010.Code, Text_wide$O.NET.SOC.2010.Title.Translate, "的工作任务包括",Text_wide$WorkActivities,
            "。按重要性排列，要求掌握的技能有",Text_wide$Skills,
            "；知识包括",Text_wide$Knowledge,"。","\n"),file="output.txt", sep="\n", append=FALSE)

#### Write results####
write.csv(df.appendix, file='results/Qinzhou_Appendix.csv')

