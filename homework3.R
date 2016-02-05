print("Jingchi YAN")
print(1503639)
print("jyan91@ucsc.edu")
#1
library("foreign", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
df.ex <- read.dta("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta")
class(df.ex)

#2
install.packages("dplyr")
library("dplyr", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
df.ex.2 <- df.ex %>%
dplyr::filter(year == 2013 & month == 12 )
print(nrow(df.ex.2))
df.ex.2 <- df.ex %>%
dplyr::filter(year == 2013 & (month == 7 | month == 8 | month == 9))
print(nrow(df.ex.2))
#3
df.ex.3a<-arrange(df.ex,-desc(year),-desc(month))
#4
df.ex.4a<-select(df.ex,year:age)
df.ex.4b<-select(df.ex,year,month,starts_with("i"))
print(distinct(select(df.ex,state)))
#5
stndz <- function(x){
  (x - mean(x, na.rm = T))  / sd(x, na.rm = T)
}
nrmlz<-function(x){(x-min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T))}
df.ex.5a<-mutate(df.ex,rw.stndz=stndz(rw),rw_nrmlz=nrmlz(rw))
df.ex.5<-group_by(df.ex,year,month)
df.ex.5b<-mutate(df.ex.5,rw.stndz=stndz(rw),rw_nrmlz=nrmlz(rw),count=n())
#6
df.ex.6<-df.ex %>%
dplyr::group_by(year,month,state) %>%
summarise(min_rw=min(rw,na.rm=T),
           max_rw=max(rw,na.rm=T),
           mean_rw= mean(rw,na.rm=T),
          median_rw=median(rw,na.rm=T),
          rw_1stQnt=quantile(rw,0.25,na.rm=T),
          rw_3rdQnt=quantile(rw,0.75,na.rm=T),
         count=n())
df.ex.6b<-filter(df.ex.6, mean_rw==max(df.ex.6$mean_rw))
print(df.ex.6b)
print("December,2013 in Washington DC has the highest mean real wage")

#7
df.ex$state.char<- as.character(df.ex$state)
df.ex.7a<-arrange(df.ex,-desc(year),-desc(month), desc(state.char))




