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

#中文编码
Sys.setlocale("LC_ALL", 'en_US.UTF-8')

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
CIP_ONET.master=left_join(xwalk,ONET.score, by=c("O.NET.SOC.2010.Code"="O.NET.SOC.Code"))
CIP_ONET.master$CIP2010.Code=as.character(CIP_ONET.master$CIP2010.Code)
CIP_ONET.master=CIP_ONET.master[!duplicated(CIP_ONET.master[,c("CIP2010.Code", "O.NET.SOC.2010.Code","Element.Name")]),]

#merge education data
CIP_ONET.master=left_join(CIP_ONET.master, ONET.Edu, by=c("O.NET.SOC.2010.Code"="O.NET.SOC.Code"))
#merge tools data
CIP_ONET.master=left_join(CIP_ONET.master, ONET.Tool, by=c("O.NET.SOC.2010.Code"="O.NET.SOC.Code"))
#merge employment and wage
CIP_ONET.master=left_join(CIP_ONET.master, OES,by=c("SOC2010Code"="OCC_CODE") )

#### 13 Majors Data####
majors=read.csv("ch2cip.csv", colClasses = "character")
Major=apply(majors[2:15],1,as.list)
names(Major)=majors$Code
All_major=Major
for (i in 1:13){
  All_major[[i]]=CIP_ONET.master%>%
    filter(CIP2010.Code %in% Major[[i]])%>%
    unique()%>%
#filter
#RL<=6 required level of eduation sub-BA
    filter(RL<=6)
  write.xlsx(All_major[[i]], file='results/all majors/all.xlsx', names(Major)[i], append = TRUE)
  #write to separate csv file
  #write.csv(Major[[i]],file=paste0('results/all majors/', names(Major)[i],'.csv'))
}


#### write to text, summarize different majors in text ####

#create xwalk, from chinese major to ONET
major2CIP=melt(majors, id="Code")
major2CIP=major2CIP%>%select(Code, value)%>%filter(value!="")%>%group_by(Code, value)
major2ONET=unique(left_join(major2CIP, CIP_ONET.master[1:5],by=c("value"="CIP2010.Code")))
major2ONET=major2ONET%>%
  ungroup()%>%
  select(Code,O.NET.SOC.2010.Code)%>%
  unique()

#CREATE unique ONET data
ONET.master=CIP_ONET.master%>%
  select(-CIP2010.Code, -CIP2010Title, -SOC2010Code, -SOC2010Title)%>%
  unique()
ONET.master_wide=ONET.master%>%
  group_by(O.NET.SOC.2010.Code, O.NET.SOC.2010.Title,Measure, OJ, PT, RW,RL,TOT_EMP, A_MEAN, A_MEDIAN)%>%
  summarise(Elements=paste(Element.Name, collapse=","))%>%
  filter(!is.na(Measure))%>%
  ungroup()

Text_major=ONET.master%>%
  filter(RL<=6)%>%
  filter(O.NET.SOC.2010.Code %in% major2ONET$O.NET.SOC.2010.Code)%>%
  unique()%>%
  select(-Technology, -Tools)

Text_major_wide=Text_major%>%
  group_by(O.NET.SOC.2010.Code, O.NET.SOC.2010.Title,Measure, OJ, PT, RW,RL,TOT_EMP, A_MEAN, A_MEDIAN)%>%
  summarise(Elements=paste(Element.Name, collapse=","))%>%
  filter(!is.na(Measure))%>%
  ungroup()

#merge ONET.master data to Chinese majors  
Text=unique(left_join(major2ONET, Text_major_wide, by="O.NET.SOC.2010.Code"))
Text=filter(Text, RL<=6)

Text_wide=dcast(Text, Code+O.NET.SOC.2010.Title+OJ+PT+RW+TOT_EMP+A_MEAN+A_MEDIAN ~ Measure, value.var="Elements")
Text_wide$A_MEAN=as.numeric(Text_wide$A_MEAN)

#assign categories for wage, working experience requirement, on-job training time
attach(Text_wide)
Text_wide$wagecat[A_MEAN >150000] <- "15万-20万"
Text_wide$wagecat[A_MEAN >100000 & A_MEAN <= 150000] <- "10万-15万"
Text_wide$wagecat[A_MEAN >50000 & A_MEAN <= 100000] <- "5万-10万"
Text_wide$wagecat[A_MEAN <50000] <- "5万以下"

Text_wide$RWcat[RW==11] <- "高级，10年以上"
Text_wide$RWcat[RW>=7 & RW <11] <- "中级，2-8年"
Text_wide$RWcat[RW<7] <- "初级，2年以下"

Text_wide$OJcat[OJ>=6] <- "1年以上"
Text_wide$OJcat[OJ>3 & OJ <6] <- "3-12月"
Text_wide$OJcat[OJ<=3] <- "0-3月"

detach(Text_wide)




#######Yuqi Liao -> creating appendix

##create and export csv files, use google translation, modify translation
Text_wide_uniqueSOC = Text_wide[!duplicated(Text_wide$O.NET.SOC.2010.Title), ] #so i could get the unique columns of SOC titles
Text_major_uniqueElement.Name = Text_major[!duplicated(Text_major$Element.Name),c("Measure","Element.Name")] #so i could get the unique columns of Element.Name
write.csv(Text_wide_uniqueSOC, file = 'results/Text_wide_uniqueSOC.csv')
write.csv(Text_major_uniqueElement.Name, file = 'results/Text_major_uniqueElement.Name.csv')

##create Text_wide_uniqueSOC.translated & Text_major_uniqueElement.Name.translated
Text_wide_uniqueSOC.translated <- read.csv("/Users/Yuqi/Google Drive/双百计划/Yuqi/专业对应的岗位及技能working folder/redo appendix_072617/Text_wide_uniqueSOC.translated.csv")
Text_major_uniqueElement.Name.translated <- read.csv("/Users/Yuqi/Google Drive/双百计划/Yuqi/专业对应的岗位及技能working folder/redo appendix_072617/Text_major_uniqueElement.Name.translated.csv")
Text_wide_uniqueSOC.translated <- Text_wide_uniqueSOC.translated[,c("O.NET.SOC.2010.Title","Final.Translation")]

##creat Text_major.translated
Text_major.translated=left_join(Text_major, Text_major_uniqueElement.Name.translated, by="Element.Name") %>%
  select(-X, -Measure.y, -Google.Translation)
colnames(Text_major.translated)[colnames(Text_major.translated) == 'Final.Translation'] <- 'Element.Name.Translation'

Text_major.translated = left_join(Text_major.translated, Text_wide_uniqueSOC.translated, by = "O.NET.SOC.2010.Title")
colnames(Text_major.translated)[colnames(Text_major.translated) == 'Final.Translation'] <- 'O.NET.SOC.2010.Title.Translation'

colnames(Text_major.translated)[colnames(Text_major.translated) == 'Measure.x'] <- 'Measure'

##create Text_major_wide.translated
Text_major_wide.translated=Text_major.translated%>%
  group_by(O.NET.SOC.2010.Code, O.NET.SOC.2010.Title, O.NET.SOC.2010.Title.Translation, Measure, OJ, PT, RW,RL,TOT_EMP, A_MEAN, A_MEDIAN)%>%
  summarise(Elements=paste(Element.Name.Translation, collapse=","))%>%
  filter(!is.na(Measure))%>%
  ungroup()


##created Text.translated
Text.translated=unique(left_join(major2ONET, Text_major_wide.translated, by="O.NET.SOC.2010.Code"))
Text.translated=filter(Text.translated, RL<=6)

##create text_wide.translate
Text_wide.translated=dcast(Text.translated, Code+O.NET.SOC.2010.Title+O.NET.SOC.2010.Title.Translation+OJ+PT+RW+TOT_EMP+A_MEAN+A_MEDIAN ~ Measure, value.var="Elements")
Text_wide.translated$A_MEAN=as.numeric(Text_wide.translated$A_MEAN)

#assign categories for wage, working experience requirement, on-job training time
attach(Text_wide.translated)
Text_wide.translated$wagecat[A_MEAN >150000] <- "15万-20万"
Text_wide.translated$wagecat[A_MEAN >100000 & A_MEAN <= 150000] <- "10万-15万"
Text_wide.translated$wagecat[A_MEAN >50000 & A_MEAN <= 100000] <- "5万-10万"
Text_wide.translated$wagecat[A_MEAN <50000] <- "5万以下"

Text_wide.translated$RWcat[RW==11] <- "高级，10年以上"
Text_wide.translated$RWcat[RW>=7 & RW <11] <- "中级，2-8年"
Text_wide.translated$RWcat[RW<7] <- "初级，2年以下"

Text_wide.translated$OJcat[OJ>=6] <- "1年以上"
Text_wide.translated$OJcat[OJ>3 & OJ <6] <- "3-12月"
Text_wide.translated$OJcat[OJ<=3] <- "0-3月"

detach(Text_wide.translated)


##create Text_wide.translated$Code.Translation
Code.translation <- read.csv("/Users/Yuqi/Google Drive/双百计划/Yuqi/专业对应的岗位及技能working folder/redo appendix_072617/Code and Translation.csv")

Text_wide.translated = left_join(Text_wide.translated, Code.translation, by = "Code")

##Write to result
#intro paragraph for each chinese major (Code.Translation)
Text_wide.translated.aggregate.SOC <- aggregate(O.NET.SOC.2010.Title.Translation ~ Code.Translation , data = Text_wide.translated, paste, collapse = ", ")
Text_wide.translated.aggregate.wagecat <- aggregate(wagecat ~ Code.Translation , data = Text_wide.translated, paste, collapse = ", ")
Text_wide.translated.aggregate.RWcat <- aggregate(RWcat ~ Code.Translation , data = Text_wide.translated, paste, collapse = ", ")
Text_wide.translated.aggregate.OJcat <- aggregate(OJcat ~ Code.Translation , data = Text_wide.translated, paste, collapse = ", ")
Text_wide.translated.1.para <- left_join(Text_wide.translated.aggregate.SOC, Text_wide.translated.aggregate.wagecat, by = "Code.Translation")
Text_wide.translated.1.para <- left_join(Text_wide.translated.1.para, Text_wide.translated.aggregate.RWcat, by = "Code.Translation")
Text_wide.translated.1.para <- left_join(Text_wide.translated.1.para, Text_wide.translated.aggregate.OJcat, by = "Code.Translation")

cat(paste0("以美国2015年的数据为例", Text_wide.translated.1.para$Code.Translation, "所对应的主要就业岗位包括", Text_wide.translated.1.para$O.NET.SOC.2010.Title.Translation,
           "。根据美国职业信息库的调查，这些岗位的平均所需要的培训时间为", Text_wide.translated.1.para$OJcat, "。这些岗位所要求的相关工作经验为",Text_wide.translated.1.para$RWcat,"。在美国平均工资区间为", Text_wide.translated.1.para$wagcat, "。这些岗位在中国的平均工资区间为。","\n"),file="firstparagraph.txt", sep="\n", append=FALSE)


#paragraph for each SOC code
cat(paste0(Text_wide.translated$O.NET.SOC.2010.Title.Translation, " (", Text_wide.translated$O.NET.SOC.2010.Title ,") 的工作任务包括",Text_wide.translated$WorkActivities,
            "。按重要性排列，要求掌握的技能有",Text_wide.translated$Skills,
            "；知识包括",Text_wide.translated$Knowledge,"。","\n"),file="SOCparagraph.txt", sep="\n", append=FALSE)

#to do: write code to put the above into a df, delete duplicates, export the spreadsheet, edit, import back in, left_join to the existing df.




