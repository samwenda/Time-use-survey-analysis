library(readstata13)  #for reading the stata files
library(dplyr)  #data manipulation
library(haven)##foreign 
library(htmlTable)##html tables
library(magrittr)  #manipulate
library(loose.rock)  #for proper changing of the string cases

#####from yvone mergeed
df1<- read.dta13("C:/Users/user/Desktop/timeuse/datastata/tus_rediac.dta")  ##read the merged data from stata
#View(df1)
#head(df1)

length(unique(df1$b02))  ##the people selected by the KISH

str(df1$r02) #structure of the day of the week
levels(df1$r02) ##levels


#df1 %>% filter(is.na(r02)) %>% as.data.frame()  ##missing day of the week
#df1 %>% filter(a09==3199)%>% View()  ##its a school going day, we randomly allocate TUESDAY
df1$r02[is.na(df1$r02)]<-"TUESDAY"  ##replacing the NA in the dataset with TUESDAY
df1 %>% filter(a09==3199)%>% 
  group_by(a09,a10,r02) %>%
  summarise() %>%
  select(a09,a10,r02) %>% View()  #cluster household number and day of the week for the cluster ensure none is missing

##create new categories for marital status according to suggestions by team
df1$b07_1<-df1$b07
df1$b07_1<-as.character(df1$b07_1)
df1$b07_1[df1$b07_1=="MARRIED MONOGAMOUS" | df1$b07_1=="MARRIED POLYGAMOUS" | df1$b07_1=="LIVING TOGETHER"] <- "married(mono.poly.living_together)"
df1$b07_1[df1$b07_1=="SEPARATED" | df1$b07_1=="DIVORCED" | df1$b07_1=="WIDOW OR WIDOWER" ] <- "not.married(divorced.separated.widow_widower"
df1$b07_1[df1$b07_1=="NEVER MARRIED"]<-"never.married" 
df1$b07_1<-as.factor(df1$b07_1)
table(df1$b07_1)

##proposed age categories <15, 15-24, 25-44, 45-54, 55-64 and 65+
##adding the age categories in the dataset
df1$agecat[df1$b05_years<15] <- "<15"
df1$agecat[15<=df1$b05_years & df1$b05_years<=24 ] <- "15-24"
df1$agecat[25<=df1$b05_years & df1$b05_years<=44 ] <- "25-44"
df1$agecat[45<=df1$b05_years & df1$b05_years<=54 ] <- "45-54"
df1$agecat[55<=df1$b05_years & df1$b05_years<=64 ] <- "55-64"
df1$agecat[df1$b05_years>=65] <- "65+"

#######BEGINING OF THE BASICS

df1 %>% 
  count(Residence, b04) %>%    #line one counts total residence and gender
  rename(gender=b04) %>%      #renaming the columns in the dataset
  mutate(Residence=recode(Residence,'1'="Rural",'2'="Urban")) %>%  ##recoding the variables into respective groups
  as.data.frame() %>%  #convert the data to dataframe
 # addmargins() %>%   #summing the rows and columns
  htmlTable  ## produce an html table that is copied direct to excel. click on unwrap in excel to format


###### END OF THE BASICS



#respodents whose paid activity exceeeded the normal 8hr*60=480 minutes put it at 600 minutes
head(df1)
#View(df1)
#paid work is r09;  time in minutes is r04; gender is b04 ;name is b02 ;
#filtering paid work whose minutes exceed the normal
d1<-df1 %>% 
  group_by(b02,r09,b04)%>% 
  summarise(time_tot=sum(r04))%>% 
  filter(r09=="Productive Paid Work")%>%
  arrange(desc(time_tot))%>% 
  filter(time_tot>600)%>%
  data.frame()%>%
  select(b02,b04)
d1  ##all the names of the people whose paid work exceeded the slated 600 minutes
d2<-df1 %>% filter(b02 %in% d1$b02)  #filtering the people in the large dataset
head(d2)
n_distinct(d2$b02) #confirming the unique number of the selected people
###



#Table 2 starts here activity, gender; residence, and time


###
####how many hours per day do respodents take on the 4 different activities
##method 1
d2<-df1 %>% 
  group_by(r09,b04,Residence)%>% 
  summarise(sum.tot.time=sum(r04),no.of.occurence.in.data=length(r04))%>% as.data.frame() #%>% htmlTable()
d2

###method 2 reolace the length() with n()
d2<-df1 %>% 
  group_by(r09,b04,Residence)%>% 
  summarise(sum.tot.time=sum(r04),no.of.occurence.in.data=n())
d2

##number of respodents in terms of gender and the said activity and the hours they spend on the main activity
d3<-df1 %>%   
  group_by(b02,r09,b04,Residence) %>%  
  summarise(no.of.occurence.in.hrs.per.person=n())# %>%arrange(desc(no.of.occurence.in.hrs)) ##add this if you want to sort in descening order
d3

d3<-df1 %>%   
  group_by(b02,r09,b04,Residence) %>%  
  summarise(no.of.occurence.in.hrs.per.person=n()) %>%arrange(desc(no.of.occurence.in.hrs.per.person)) ##add this if you want to sort in descening order
d3

####   table lost

#method 1
d2<-df1 %>% 
  group_by(r09,b04)%>% 
  summarise(sum.tot.time=sum(r04),no.of.occurence.in.data=n())
d2
d4<-d3 %>% group_by(r09,b04)%>%
  summarise(no.of.people.in.activity=n())
d4

##geat table in HTML and you copy paste in excel
d5<- d4 %>% left_join(d2, by=c("r09", "b04"))
d5$average.min.in.activ<-round((d5$sum.tot.time/d5$no.of.people.in.activity),0) #divide by 60 minutes
d5$average.hour.in.activ<-round((d5$average.min.in.activ/60),1)
d5 %>% as.data.frame() %>% 
  #select(r09,b04,no.of.people.in.activity,average.min.in.activ,average.hour.in.activ) %>% #if you wish to see number of ppl who reported this
  select(r09,b04,average.min.in.activ,average.hour.in.activ) %>%   ##avoid reporting number of people in activity
  rename(Main_Activity=r09,Gender=b04)%>%
 # mutate(Residence=recode(Residence,'1'="Rural",'2'="Urban")) %>%
  htmlTable()

###
#find the number of people involved in each activity
#method 1
d2<-df1 %>% 
  group_by(r09,b04,Residence)%>% 
  summarise(sum.tot.time=sum(r04),no.of.occurence.in.data=n())
d2
d4<-d3 %>% group_by(r09,b04,Residence)%>%
summarise(no.of.people.in.activity=n())
d4

##geat table in HTML and you copy paste in excel
d5<- d4 %>% left_join(d2, by=c("r09", "b04","Residence"))
d5$average.min.in.activ<-round((d5$sum.tot.time/d5$no.of.people.in.activity),0)  ##minutes in activity
d5$average.hours.in.activ<-round((d5$average.min.in.activ/60),1) #divide by 60 minutes for hours
d5 %>% as.data.frame() %>% 
 # select(r09,b04,Residence,no.of.people.in.activity,average.min.in.activ) %>% #see the abobe comment on reporting number of people in activity
  select(r09,b04,Residence,average.min.in.activ,average.hours.in.activ) %>%
  rename(Main_Activity=r09,Gender=b04)%>%
  mutate(Residence=recode(Residence,'1'="Rural",'2'="Urban")) %>%
  htmlTable

###


#Table 3 starts here; activity, gender: day of the week


###

d2<-df1 %>% 
  group_by(r09,b04,r02)%>% 
  summarise(sum.tot.time=sum(r04),no.of.occurence.in.data=length(r04)) 
d2  
d2 %>% filter(is.na(r02)) #there people who never included day of the week
##number of respodents in terms of gender, day of the week, and the said activity and the hours they spend on the main activity
d3<-df1 %>%   
  group_by(b02,r09,b04,r02) %>%  
  summarise(no.of.occurence.in.hrs.per.person=n())# %>%arrange(desc(no.of.occurence.in.hrs)) ##add this if you want to sort in descening order
d3
#find the number of people involved in each activity
d4<-d3 %>% group_by(r09,b04,r02)%>%
  summarise(no.of.people.in.activity=n())
d4
d5<- d4 %>% left_join(d2, by=c("r09", "b04","r02"))
d5$average.min.in.activ<-round((d5$sum.tot.time/d5$no.of.people.in.activity),0) #divide by 60 minutes
d5$average.hours.in.activ<-round(d5$average.min.in.activ/60,0)
d5 %>%
  rename(Main_activity=r09, Gender=b04, Day.of.week=r02)%>%
  as.data.frame() %>% 
  select(Main_activity, Gender, Day.of.week,average.min.in.activ,average.hours.in.activ) %>%
  htmlTable
 
d<-d5
d<-d5 %>% filter(!is.na(r02))  ##check if the NA could be sartudays since its missing from data


###



#Table 4 starts here; activity, sex: marital status


###

d2<-df1 %>% 
  group_by(r09,b04,b07_1)%>% 
  summarise(sum.tot.time=sum(r04),no.of.occurence.in.data=length(r04)) 
d2  #there people who never included day of the week
##number of respodents in terms of gender, day of the week, and the said activity and the hours they spend on the main activity
d3<-df1 %>%   
  group_by(b02,r09,b04,b07_1) %>%  
  summarise(no.of.occurence.in.hrs.per.person=n())# %>%arrange(desc(no.of.occurence.in.hrs)) ##add this if you want to sort in descening order
d3
#find the number of people involved in each activity
d4<-d3 %>% group_by(r09,b04,b07_1)%>%
  summarise(no.of.people.in.activity=n())
d4
d5<- d4 %>% left_join(d2, by=c("r09", "b04","b07_1"))
d5$average.min.in.activ<-round((d5$sum.tot.time/d5$no.of.people.in.activity),0) #divide by 60 minutes
d5$average.hours.in.activ<-round(d5$average.min.in.activ/60,0)
d5 %>% as.data.frame()%>%
  rename(Activity=r09,sex=b04,marital.status=b07_1)%>%
  select(Activity,sex,marital.status,average.min.in.activ,average.hours.in.activ) %>%
  htmlTable

names(d5)
d<-d5 

###
#Table 4b starts here; activity, without _sex: marital status
d2<-df1 %>% 
  group_by(r09,b07_1)%>% 
  summarise(sum.tot.time=sum(r04),no.of.occurence.in.data=length(r04)) 
d2  #there people who never included day of the week
##number of respodents in terms of gender, day of the week, and the said activity and the hours they spend on the main activity
d3<-df1 %>%   
  group_by(b02,r09,b07_1) %>%  
  summarise(no.of.occurence.in.hrs.per.person=n())# %>%arrange(desc(no.of.occurence.in.hrs)) ##add this if you want to sort in descening order
d3
#find the number of people involved in each activity
d4<-d3 %>% group_by(r09,b07_1)%>%
  summarise(no.of.people.in.activity=n())
d4
d5<- d4 %>% left_join(d2, by=c("r09","b07_1"))
d5$average.min.in.activ<-round((d5$sum.tot.time/d5$no.of.people.in.activity),0) #divide by 60 minutes
d5$average.hours.in.activ<-round(d5$average.min.in.activ/60,0)
d5 %>% as.data.frame()%>%
  rename(Activity=r09,marital.status=b07_1)%>% 
  select(Activity,marital.status,average.min.in.activ,average.hours.in.activ) %>%
  htmlTable()

##################
#####
####
#Table 5 starts here; activity, gender: age category 12-17, 18-34, 35 and above
###
d2<-df1 %>% 
  group_by(r09,b04,agecat)%>% 
  summarise(sum.tot.time=sum(r04),no.of.occurence.in.data=length(r04)) 
d2  #there people who never included day of the week
##number of respodents in terms of gender, day of the week, and the said activity and the hours they spend on the main activity
d3<-df1 %>%   
  group_by(b02,r09,b04,agecat) %>%  
  summarise(no.of.occurence.in.hrs.per.person=n())# %>%arrange(desc(no.of.occurence.in.hrs)) ##add this if you want to sort in descening order
d3
#find the number of people involved in each activity
d4<-d3 %>% group_by(r09,b04,agecat)%>%
  summarise(no.of.people.in.activity=n())
d4
d5<- d4 %>% left_join(d2, by=c("r09", "b04","agecat"))
d5$average.min.in.activ<-round((d5$sum.tot.time/d5$no.of.people.in.activity),0) #divide by 60 minutes
d5$average.hours.in.activ<-round(d5$average.min.in.activ/60,0)
d5 
d5 %>% as.data.frame()%>%
  rename(Activity=r09,sex=b04)%>% 
  select(Activity,sex,agecat,average.min.in.activ,average.hours.in.activ) %>%
  htmlTable

###----------------------starts here



#Table 5 starts here; activity, gender: age category 12-17, 18-34, 35 and above


###

d2<-df1 %>% 
  group_by(r09,b04,r07)%>% 
  summarise(sum.tot.time=sum(r04),no.of.occurence.in.data=length(r04)) 
d2  #there people who never included day of the week
##number of respodents in terms of gender, day of the week, and the said activity and the hours they spend on the main activity
d3<-df1 %>%   
  group_by(b02,r09,b04,r07) %>%  
  summarise(no.of.occurence.in.hrs.per.person=n())# %>%arrange(desc(no.of.occurence.in.hrs)) ##add this if you want to sort in descening order
d3
#find the number of people involved in each activity
d4<-d3 %>% group_by(r09,b04,r07)%>%
  summarise(no.of.people.in.activity=n())
d4


d5<- d4 %>% left_join(d2, by=c("r09", "b04","r07"))
d5$average.min.in.activ<-round((d5$sum.tot.time/d5$no.of.people.in.activity),0) #divide by 60 minutes
d5$average.hours.in.activ<-round(d5$average.min.in.activ/60,0)
d5 %>% as.data.frame()%>%
  rename(Activity=r09,sex=b04,who.did.you.do.it.for=r07)%>% 
  select(Activity,sex,who.did.you.do.it.for,average.min.in.activ,average.hours.in.activ) %>%
  htmlTable

######----------------ends here

###----------------------starts here
#Table 6 starts here; activity, gender: age category 12-17, 18-34, 35 and above
###
d2<-df1 %>% 
  group_by(r09,b04,r08)%>% 
  summarise(sum.tot.time=sum(r04),no.of.occurence.in.data=length(r04)) 
d2  #there people who never included day of the week
##number of respodents in terms of gender, day of the week, and the said activity and the hours they spend on the main activity
d3<-df1 %>%   
  group_by(b02,r09,b04,r08) %>%  
  summarise(no.of.occurence.in.hrs.per.person=n())# %>%arrange(desc(no.of.occurence.in.hrs)) ##add this if you want to sort in descening order
d3
#find the number of people involved in each activity
d4<-d3 %>% group_by(r09,b04,r08)%>%
  summarise(no.of.people.in.activity=n())
d4
d5<- d4 %>% left_join(d2, by=c("r09", "b04","r08"))
d5$average.min.in.activ<-round((d5$sum.tot.time/d5$no.of.people.in.activity),0) #divide by 60 minutes
d5$average.hours.in.activ<-round(d5$average.min.in.activ/60,0)
d5 %>% as.data.frame() %>%
rename(Activity=r09,sex=b04,was.it.for.pay=r08)%>% 
  select(Activity,sex,was.it.for.pay,average.min.in.activ,average.hours.in.activ) %>%
 htmlTable

######----------------ends here

###----------------------starts here

#######
#Table 7 starts here; activity, place of occurence and without_sex
###
d2<-df1 %>% 
  group_by(r09,r11)%>% 
  summarise(sum.tot.time=sum(r04),no.of.occurence.in.data=length(r04)) 
d2  #there people who never included day of the week
##number of respodents in terms of gender, day of the week, and the said activity and the hours they spend on the main activity
d3<-df1 %>%   
  group_by(b02,r09,r11) %>%  
  summarise(no.of.occurence.in.hrs.per.person=n())# %>%arrange(desc(no.of.occurence.in.hrs)) ##add this if you want to sort in descening order
d3
#find the number of people involved in each activity
d4<-d3 %>% group_by(r09,r11)%>%
  summarise(no.of.people.in.activity=n())
d4
d5<- d4 %>% left_join(d2, by=c("r09","r11"))
d5$average.min.in.activ<-round((d5$sum.tot.time/d5$no.of.people.in.activity),0) #divide by 60 minutes
d5$average.hours.in.activ<-round(d5$average.min.in.activ/60,0)
d5 %>% rename(Activity=r09,place.activity.occur=r11)%>% 
  select(Activity,place.activity.occur,average.min.in.activ,average.hours.in.activ) %>%
  htmlTable

#Table 7 starts here; activity, place of occurence and sex
###
d2<-df1 %>% 
  group_by(r09,b04,r11)%>% 
  summarise(sum.tot.time=sum(r04),no.of.occurence.in.data=length(r04)) 
d2  #there people who never included day of the week
##number of respodents in terms of gender, day of the week, and the said activity and the hours they spend on the main activity
d3<-df1 %>%   
  group_by(b02,r09,b04,r11) %>%  
  summarise(no.of.occurence.in.hrs.per.person=n())# %>%arrange(desc(no.of.occurence.in.hrs)) ##add this if you want to sort in descening order
d3
#find the number of people involved in each activity
d4<-d3 %>% group_by(r09,b04,r11)%>%
  summarise(no.of.people.in.activity=n())
d4
d5<- d4 %>% left_join(d2, by=c("r09", "b04","r11"))
d5$average.min.in.activ<-round((d5$sum.tot.time/d5$no.of.people.in.activity),0) #divide by 60 minutes
d5$average.hours.in.activ<-round(d5$average.min.in.activ/60,0)
d5 %>% rename(Activity=r09,sex=b04,place.activity.occur=r11)%>% 
  select(Activity,sex,place.activity.occur,average.min.in.activ,average.hours.in.activ) %>%
  htmlTable


######----------------ends here


###----------------------starts here



#Table 8 starts here; r09_1, gender: r10__1 activity done alone by genderMALE
d3<-df1 %>%  
  filter(!is.na(r09_1)) %>% 
  group_by(b02,r09_1,b04,r10__1) %>%  
  summarise(no.of.occurence.in.hrs.per.person=n())# %>%arrange(desc(no.of.occurence.in.hrs)) ##add this if you want to sort in descening order
d3
#find the number of people involved in each activity top 5 activities by gender
d3 %>% group_by(r09_1,b04,r10__1)%>%
  summarise(no.of.people.in.activity=n())%>%
  arrange(desc(no.of.people.in.activity))%>%
  ungroup() %>%
  filter(r10__1==1) %>%
  filter(b04=="Male")%>%
  slice(1,2,3,4,5)%>%
  ungroup() %>%
  rename(FIVE.TOP.specific.activity.R091.DONE.BY.SELF=r09_1, gender=b04)%>%
  select(FIVE.TOP.specific.activity.R091.DONE.BY.SELF,gender,no.of.people.in.activity)%>%
 htmlTable()

#Table 8 starts here; r09_1, gender: r10__1 activity done alone by gender FEMALE
d3<-df1 %>%  
  filter(!is.na(r09_1)) %>% 
  group_by(b02,r09_1,b04,r10__1) %>%  
  summarise(no.of.occurence.in.hrs.per.person=n())# %>%arrange(desc(no.of.occurence.in.hrs)) ##add this if you want to sort in descening order
d3
#find the number of people involved in each activity top 5 activities by gender
d3 %>% group_by(r09_1,b04,r10__1)%>%
  summarise(no.of.people.in.activity=n())%>%
  arrange(desc(no.of.people.in.activity))%>%
  ungroup() %>%
  filter(r10__1==1) %>%
  filter(b04=="Female")%>%
  slice(1,2,3,4,5)%>%
  ungroup() %>%
  rename(FIVE.TOP.specific.activity.R091.DONE.BY.SELF=r09_1, gender=b04)%>%
  select(FIVE.TOP.specific.activity.R091.DONE.BY.SELF,gender,no.of.people.in.activity)%>%
  htmlTable()

 

######----------------ends here

#Table xxx starts here; r09_1, gender: r10__2 ,children 5years and below  MALE

### male

d3<-df1 %>%  
  filter(!is.na(r09_1)) %>% 
  group_by(b02,r09_1,b04,r10__2) %>%  
  summarise(no.of.occurence.in.hrs.per.person=n())# %>%arrange(desc(no.of.occurence.in.hrs)) ##add this if you want to sort in descening order
d3
#find the number of people involved in each activity top 5 activities by gender
d3 %>% group_by(r09_1,b04,r10__2)%>%
  summarise(no.of.people.in.activity=n())%>%
  arrange(desc(no.of.people.in.activity))%>%
  ungroup() %>%
  filter(r10__2==1) %>%
  filter(b04=="Male")%>%
  slice(1,2,3,4,5)%>%
  ungroup() %>%
  rename(FIVE.TOP.specific.activity.R091.DONE.with.CHILDREN.BELOW.FIVE.YEARS=r09_1, gender=b04)%>%
  select(FIVE.TOP.specific.activity.R091.DONE.with.CHILDREN.BELOW.FIVE.YEARS,gender,no.of.people.in.activity)%>%
  htmlTable()

#####for female
#Table xxx starts here; r09_1, gender: r10__2 ,children 5years and below  FEMALE
d3<-df1 %>%  
  filter(!is.na(r09_1)) %>% 
  group_by(b02,r09_1,b04,r10__2) %>%  
  summarise(no.of.occurence.in.hrs.per.person=n())# %>%arrange(desc(no.of.occurence.in.hrs)) ##add this if you want to sort in descening order
d3
#find the number of people involved in each activity top 5 activities by gender
d3 %>% group_by(r09_1,b04,r10__2)%>%
  summarise(no.of.people.in.activity=n())%>%
  arrange(desc(no.of.people.in.activity))%>%
  ungroup() %>%
  filter(r10__2==1) %>%
  filter(b04=="Female")%>%
  slice(1,2,3,4,5)%>%
  ungroup() %>%
  rename(FIVE.TOP.specific.activity.R091.DONE.BY.CHILDREN.BELOW.FIVE.YEARS=r09_1, gender=b04)%>%
  select(FIVE.TOP.specific.activity.R091.DONE.BY.CHILDREN.BELOW.FIVE.YEARS,gender,no.of.people.in.activity)%>%
  htmlTable()

######----------------ends here

#Table xxx starts here; r09_1, gender: r10__3 ,children 5years to 17  MALE

### male

d3<-df1 %>%  
  filter(!is.na(r09_1)) %>% 
  group_by(b02,r09_1,b04,r10__3) %>%  
  summarise(no.of.occurence.in.hrs.per.person=n())# %>%arrange(desc(no.of.occurence.in.hrs)) ##add this if you want to sort in descening order
d3
#find the number of people involved in each activity top 5 activities by gender
d3 %>% group_by(r09_1,b04,r10__3)%>%
  summarise(no.of.people.in.activity=n())%>%
  arrange(desc(no.of.people.in.activity))%>%
  ungroup() %>%
  filter(r10__3==1) %>%
  filter(b04=="Male")%>%
  slice(1,2,3,4,5)%>%
  ungroup() %>%
  rename(FIVE.TOP.specific.activity.R091.DONE.between.FIVE.AND.SEVENTEEN.YEARS=r09_1, gender=b04)%>%
  select(FIVE.TOP.specific.activity.R091.DONE.between.FIVE.AND.SEVENTEEN.YEARS,gender,no.of.people.in.activity)%>%
  htmlTable()

#####for female
#Table xxx starts here; r09_1, gender: r10__3 ,children 5years to 17   FEMALE
d3<-df1 %>%  
  filter(!is.na(r09_1)) %>% 
  group_by(b02,r09_1,b04,r10__3) %>%  
  summarise(no.of.occurence.in.hrs.per.person=n())# %>%arrange(desc(no.of.occurence.in.hrs)) ##add this if you want to sort in descening order
d3
#find the number of people involved in each activity top 5 activities by gender
d3 %>% group_by(r09_1,b04,r10__3)%>%
  summarise(no.of.people.in.activity=n())%>%
  arrange(desc(no.of.people.in.activity))%>%
  ungroup() %>%
  filter(r10__3==1) %>%
  filter(b04=="Female")%>%
  slice(1,2,3,4,5)%>%
  ungroup() %>%
  rename(FIVE.TOP.specific.activity.R091.DONE.between.FIVE.AND.SEVENTEEN.YEARS=r09_1, gender=b04)%>%
  select(FIVE.TOP.specific.activity.R091.DONE.between.FIVE.AND.SEVENTEEN.YEARS,gender,no.of.people.in.activity)%>%
  htmlTable()

######----------------ends here

#Table xxx starts here; r09_1, gender: r10__4 ,with another household member  MALE

### male

d3<-df1 %>%  
  filter(!is.na(r09_1)) %>% 
  group_by(b02,r09_1,b04,r10__4) %>%  
  summarise(no.of.occurence.in.hrs.per.person=n())# %>%arrange(desc(no.of.occurence.in.hrs)) ##add this if you want to sort in descening order
d3
#find the number of people involved in each activity top 5 activities by gender
d3 %>% group_by(r09_1,b04,r10__4)%>%
  summarise(no.of.people.in.activity=n())%>%
  arrange(desc(no.of.people.in.activity))%>%
  ungroup() %>%
  filter(r10__4==1) %>%
  filter(b04=="Male")%>%
  slice(1,2,3,4,5)%>%
  ungroup() %>%
  rename(FIVE.TOP.specific.activity.R091.DONE.WITH.ANOTHER.HH.MEMBER=r09_1, gender=b04)%>%
  select(FIVE.TOP.specific.activity.R091.DONE.WITH.ANOTHER.HH.MEMBER,gender,no.of.people.in.activity)%>%
  htmlTable()
#Table xxx starts here; r09_1, gender: r10__4 ,with another household member  FEMALE

### male

d3<-df1 %>%  
  filter(!is.na(r09_1)) %>% 
  group_by(b02,r09_1,b04,r10__4) %>%  
  summarise(no.of.occurence.in.hrs.per.person=n())# %>%arrange(desc(no.of.occurence.in.hrs)) ##add this if you want to sort in descening order
d3
#find the number of people involved in each activity top 5 activities by gender
d3 %>% group_by(r09_1,b04,r10__4)%>%
  summarise(no.of.people.in.activity=n())%>%
  arrange(desc(no.of.people.in.activity))%>%
  ungroup() %>%
  filter(r10__4==1) %>%
  filter(b04=="Female")%>%
  slice(1,2,3,4,5)%>%
  ungroup() %>%
  rename(FIVE.TOP.specific.activity.R091.DONE.WITH.ANOTHER.HH.MEMBER=r09_1, gender=b04)%>%
  select(FIVE.TOP.specific.activity.R091.DONE.WITH.ANOTHER.HH.MEMBER,gender,no.of.people.in.activity)%>%
  htmlTable()

#Table xxx starts here; r09_1, gender: r10__4 ,with a collegue  MALE

### male

d3<-df1 %>%  
  filter(!is.na(r09_1)) %>% 
  group_by(b02,r09_1,b04,r10__5) %>%  
  summarise(no.of.occurence.in.hrs.per.person=n())# %>%arrange(desc(no.of.occurence.in.hrs)) ##add this if you want to sort in descening order
d3
#find the number of people involved in each activity top 5 activities by gender
d3 %>% group_by(r09_1,b04,r10__5)%>%
  summarise(no.of.people.in.activity=n())%>%
  arrange(desc(no.of.people.in.activity))%>%
  ungroup() %>%
  filter(r10__5==1) %>%
  filter(b04=="Male")%>%
  slice(1,2,3,4,5)%>%
  ungroup() %>%
  rename(FIVE.TOP.specific.activity.R091.DONE.WITH.colleague=r09_1, gender=b04)%>%
  select(FIVE.TOP.specific.activity.R091.DONE.WITH.colleague,gender,no.of.people.in.activity)%>%
  htmlTable()

#Table xxx starts here; r09_1, gender: r10__4 ,with a collegue  FEMALE

### male

d3<-df1 %>%  
  filter(!is.na(r09_1)) %>% 
  group_by(b02,r09_1,b04,r10__5) %>%  
  summarise(no.of.occurence.in.hrs.per.person=n())# %>%arrange(desc(no.of.occurence.in.hrs)) ##add this if you want to sort in descening order
d3
#find the number of people involved in each activity top 5 activities by gender
d3 %>% group_by(r09_1,b04,r10__5)%>%
  summarise(no.of.people.in.activity=n())%>%
  arrange(desc(no.of.people.in.activity))%>%
  ungroup() %>%
  filter(r10__5==1) %>%
  filter(b04=="Female")%>%
  slice(1,2,3,4,5)%>%
  ungroup() %>%
  rename(FIVE.TOP.specific.activity.R091.DONE.WITH.colleague=r09_1, gender=b04)%>%
  select(FIVE.TOP.specific.activity.R091.DONE.WITH.colleague,gender,no.of.people.in.activity)%>%
  htmlTable()

#Table xxx starts here; r09_1, gender: r10__6 ,with a other known to the respodent  MALE

### male

d3<-df1 %>%  
  filter(!is.na(r09_1)) %>% 
  group_by(b02,r09_1,b04,r10__6) %>%  
  summarise(no.of.occurence.in.hrs.per.person=n())# %>%arrange(desc(no.of.occurence.in.hrs)) ##add this if you want to sort in descening order
d3
#find the number of people involved in each activity top 5 activities by gender
d3 %>% group_by(r09_1,b04,r10__6)%>%
  summarise(no.of.people.in.activity=n())%>%
  arrange(desc(no.of.people.in.activity))%>%
  ungroup() %>%
  filter(r10__6==1) %>%
  filter(b04=="Male")%>%
  slice(1,2,3,4,5)%>%
  ungroup() %>%
  rename(FIVE.TOP.specific.activity.R091.DONE.WITH.others=r09_1, gender=b04)%>%
  select(FIVE.TOP.specific.activity.R091.DONE.WITH.others,gender,no.of.people.in.activity)%>%
  htmlTable()

#Table xxx starts here; r09_1, gender: r10__6 ,with a other known to the respodent FEMALE

### female

d3<-df1 %>%  
  filter(!is.na(r09_1)) %>% 
  group_by(b02,r09_1,b04,r10__6) %>%  
  summarise(no.of.occurence.in.hrs.per.person=n())# %>%arrange(desc(no.of.occurence.in.hrs)) ##add this if you want to sort in descening order
d3
#find the number of people involved in each activity top 5 activities by gender
d3 %>% group_by(r09_1,b04,r10__6)%>%
  summarise(no.of.people.in.activity=n())%>%
  arrange(desc(no.of.people.in.activity))%>%
  ungroup() %>%
  filter(r10__6==1) %>%
  filter(b04=="Female")%>%
  slice(1,2,3,4,5)%>%
  ungroup() %>%
  rename(FIVE.TOP.specific.activity.R091.DONE.WITH.others=r09_1, gender=b04)%>%
  select(FIVE.TOP.specific.activity.R091.DONE.WITH.others,gender,no.of.people.in.activity)%>%
  htmlTable()
###ends here


##################### activities with whom ends here



##maximum 3 main activities in the 
 
#####3 main activities in every hour
df1 %>% 
  select(tus_diary__id, r09_1) %>% ##select only the diary and the specific activities
  filter(!is.na(r09_1)) %>%  ##filter any variable whhich is missing
  group_by(tus_diary__id, r09_1) %>%  ##grouping by time in diary and the specific activity 
  summarise(no.of.activities=n()) %>%  ##number of people reporting activities at that time
  ungroup() %>%
  group_by(tus_diary__id) %>%
  arrange(desc(no.of.activities)) %>%  ## arrange the activities in descedning order within the group.
  slice(1,2,3)%>% ##slice(1) for the top most activity; slice(1,2) for the two top most; slice(1,2,3) the third top most activity in that hour
  ungroup()%>% ##ungroup from the main groupings
  as.data.frame() %>% ###incase you want to write or merge twith other dataset
  htmlTable()   #create a html table, copy to excel and unwrap, justify left




##perception tables
dp1 <-  read.dta13("C:/Users/user/Desktop/timeuse/datastata/perception_roster.dta")
head(dp1)
##merge with the main data from yvone

dp4<-df1 %>% left_join(dp1, by=c("tus_respondent__id","interview__id"))
#head(dp4)
dp44<-dp4 %>% select(-contains("r06__"))  ##unselect the numerous r06__which making the data heavier
head(dp44)

dp44<-dp4 %>% filter(contains("r06__"))
dp55<- dp44 %>% select(interview__id,b04,b07_1,agecat,perception_roster__id,r15_32)
#View(dp55)
dpft<-dp55 %>% filter(!is.na(r15_32))  ##remove the NA from the answers
dp56<-dpft %>% group_by(interview__id,b04,b07_1,agecat,perception_roster__id,r15_32) %>% summarise()  ##removing duplivated by grouping
head(dp56)






table(dp56$r15_32)
levels(dp1$perception_roster__id)
[1] "R15:Girls under 18 years may be married"                                                      
[2] "R16:Boys under 18 years may marry"                                                             
[3] "R17:Girls and women should undergo Female Genital Mutilation/Cut (FGM/C) as a rite of passage"
[4] "R18:Girls should spend more time on domestic work than boys"                                  
[5] "R19:Girls and boys should spend the same amount of time on domestic work"                     
[6] "R20:It is the responsibility of women and girls to cook for their families"                   
[7] "R21:Men should help with cooking for their families"                                          
[8] "R22:It is a woman's responsibility to take care of her home, family and the elderly"          
[9] "R23:Childcare is the mother's responsibility"                                                 
[10] "R24:It is shameful for men to be found by friends and neighbours performing household chores" 
[11] "R25:Men and women should equally share household tasks and childcare if both are working"     
[12] "R26:Men's work is more important than women's work"                                           
[13] "R27:Both husband and wife should contribute financially for the wellbeing of the family"      
[14] "R28:Both husband and wife should manage the income/expenses of the household"                 
[15] "R29:Housewives would prefer to do paid work if they could"                                    
[16] "R30:Husbands prefer housewives to working wives"                                              
[17] "R31:It is justified for a man to beat a wife/partner for not completing household chores"     
[18] "R32:It is justified for a woman to beat a husband/partner for neglect of responsibility"  

d<-table(dp56$perception_roster__id,dp56$r15_32,dp56$b04) # %>% htmlTable()
d<-table(dp56$perception_roster__id,dp56$r15_32,dp56$agecat)


results_clipboard <- function(d, sep="\t", dec=".", max.size=(200*1000))   # Copy a data.frame to clipboard
{
  write.table(d, paste0("clipboard-", formatC(max.size, format="f", digits=0)), sep=sep, row.names=TRUE, dec=dec)
}
results_clipboard(d) ###then paste in excel
########tables and percentages
