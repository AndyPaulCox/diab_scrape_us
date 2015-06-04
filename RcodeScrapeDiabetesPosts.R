# 29th May 2015
# Andy Cox
# 
# code to scrape diabetes posts
rm(list = ls())
library(rvest)
library(pipeR)
################################
###########################################
#########List of Level one urls (the sub-forums)
L2_urls<-c(
  "http://www.diabetesdaily.com/forum/introduce-yourself/",
  "http://www.diabetesdaily.com/forum/personal-updates/",
  "http://www.diabetesdaily.com/forum/type-1-diabetes/",
  "http://www.diabetesdaily.com/forum/type-1-5-diabetes/",
  "http://www.diabetesdaily.com/forum/type-2-diabetes/",
  "http://www.diabetesdaily.com/forum/pre-diabetes/",
  "http://www.diabetesdaily.com/forum/pregnancy/",
  "http://www.diabetesdaily.com/forum/friends-family/",
  "http://www.diabetesdaily.com/forum/food-diet/",
  "http://www.diabetesdaily.com/forum/low-carb-diet/",
  "http://www.diabetesdaily.com/forum/recipes/",
  "http://www.diabetesdaily.com/forum/testing-blood-sugar/",
  "http://www.diabetesdaily.com/forum/multiple-daily-injections-mdi/",
  "http://www.diabetesdaily.com/forum/insulin-pumps/",
  "http://www.diabetesdaily.com/forum/continuous-glucose-monitors-cgms/",
  "http://www.diabetesdaily.com/forum/exercise/",
  "http://www.diabetesdaily.com/forum/weight-loss/",
  "http://www.diabetesdaily.com/forum/depression-staying-positive/",
  "http://www.diabetesdaily.com/forum/complications/",
  "http://www.diabetesdaily.com/forum/eyes/",
  "http://www.diabetesdaily.com/forum/heart/",
  "http://www.diabetesdaily.com/forum/kidneys/",
  "http://www.diabetesdaily.com/forum/neuropathy/",
  "http://www.diabetesdaily.com/forum/sex-intimacy/",
  "http://www.diabetesdaily.com/forum/womens-corner/",
  "http://www.diabetesdaily.com/forum/mens-corner/",
  "http://www.diabetesdaily.com/forum/non-traditional-treatments/",
  "http://www.diabetesdaily.com/forum/health-care-insurance/",
  "http://www.diabetesdaily.com/forum/united-states/",
  "http://www.diabetesdaily.com/forum/other-countries/",
  "http://www.diabetesdaily.com/forum/united-kingdom/",
  "http://www.diabetesdaily.com/forum/canada/",
  "http://www.diabetesdaily.com/forum/australia/",
  "http://www.diabetesdaily.com/forum/arts-culture/",
  "http://www.diabetesdaily.com/forum/research-clinical-trials/",
  "http://www.diabetesdaily.com/forum/other-medical-conditions/",
  "http://www.diabetesdaily.com/forum/promotions-surveys-trial-recruitment/",
  "http://www.diabetesdaily.com/forum/theres-more-life-than-diabetes/",
  "http://www.diabetesdaily.com/forum/humor/",
  "http://www.diabetesdaily.com/forum/non-diabetes-news-links/",
  "http://www.diabetesdaily.com/forum/diabetes-daily-meetups/",
  "http://www.diabetesdaily.com/forum/book-club/")
###############################
##Broken Links
"http://www.diabetesdaily.com/forum/announcements/",
"http://www.diabetesdaily.com/forum/diabetes-news/",

  

for(i in 1:length(l1_urls)){

#retrieve the first forum
mst<-html_session(l1_urls[i])
#now extract to the first
L2_urls<-mst%>% html_nodes('.title') %>% html_attr("href") 

#now ascertain the last page for this forum
L3_urls<-mst %>% html_nodes('#threadpagestats') %>% html_text() 
last<-as.numeric(unlist(strsplit(L3_urls," "))[length(unlist(strsplit(L3_urls," ")))])
#now we have the last page number simply add 'index[n]'
for(pg in 2:last){
  url1<-paste0(l1_urls[i],"index",pg)
  #retrieve the first forum
  mst<-html_session(url1)
  #now extract to the first
  L2_temp<-mst%>% html_nodes('.title') %>% html_attr("href") 
  L2_urls<-c(L2_urls,L2_temp) 
  Sys.sleep(0.25)
  print(length(L2_urls))
}
}

write.table(L2_urls, file = "/Users/AndyC/Dropbox/rdata/diab_scrape_us/db_urls.csv", sep = ",", col.names = NA, na="NA", qmethod = "double")
##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################
rm(list = ls())
library(rvest)
library(pipeR)
L2_urls<-as.character(read.delim(file="/Users/AndyC/Dropbox/rdata/diab_scrape_us/db_urls.csv",header=F,sep=",",stringsAsFactors =F)[,1])
#Now visit the collected URLs one at a time and collect the posts

#visit each of the urls in the collected list wiht for loop
#each url represents 1 thread possibly wiht multiple pages
##########for(pg2 in 1:length(L2_urls)){
  pg2=1
  
#retrieve the first forum
mst3<-html_session(L2_urls[pg2])
#now extract to the first
pgs<-mst3%>% html_nodes('#postpagestats_above') %>% html_text()
pgs<-gsub("\n","",gsub("\r","",gsub("\t","",pgs)))
last<-unlist(strsplit(pgs," "))
last<-as.numeric(last)[!is.na(as.numeric(last))]
t_pgs<-last[2]
last1<-last[length(last)]
###Here extract data from first page

posts3<-data.frame(forum=character(),
thread_no=numeric(),
post_no=character(),
username=character(),
type=character(),
location=character(),
join_date=character(),
block_data= character(),
sign_block=character(),
n_posts=character(),
posts=character(),
url=character())
last_post<-1
pst_pg<-1
#loop through the pages of thread testing to see
#when reach the last post number
while(last1>last_post){
  
  url1<-paste0(substr(L2_urls[pg2],1,nchar(L2_urls[pg2])-1),"-",pst_pg)
  pst_pg<-pst_pg+1
  #retrieve the first forum
  mst3<-html_session(url1)
  pgs<-mst3%>% html_nodes('#postpagestats_above') %>% html_text()
  pgs<-gsub("\n","",gsub("\r","",gsub("\t","",pgs)))
  last<-unlist(strsplit(pgs," "))
  suppressWarnings(last<-as.numeric(last)[!is.na(as.numeric(last))])
  t_pgs<-last[2]
  last1<-last[length(last)]
################
#now scrape the data for hte fields available
forum<-mst3%>% html_nodes('.navbit~ .navbit+ .navbit a') %>% html_text(trim=TRUE)
thread_no<-pg2
post_no<-mst3%>% html_nodes('.postcounter') %>% html_text(trim=TRUE)
last_post<-max(as.numeric(gsub("#","",post_no)))
username<-mst3%>% html_nodes('.onlinestatus') %>% html_attr('alt')
username<-gsub(" is offline","",username)
username<-gsub(" is online","",username)
type<-mst3%>% html_nodes('.usertitle') %>% html_text(trim=TRUE)
location<-mst3%>% html_nodes('dd:nth-child(4)') %>% html_text(trim=TRUE)
#Need some code here to account for missing location 
#on the page when the location is missing the no posts moves up and appears in the 
#place where location whould be, need to detect this
suppressWarnings(numvals<-(!is.na(as.numeric(location)) | location=="> 100"))
#store those values
vals1<-c(location[which(numvals==T)])
valpos1<-which(numvals==T)
location[numvals==T]<-"No Location"

join_date<-mst3%>% html_nodes('dd:nth-child(2)') %>% html_text(trim=TRUE)
n_posts<-mst3%>% html_nodes('dd:nth-child(6)') %>% html_text(trim=TRUE)
n_posts1<-character(length(username))
n_posts1[valpos1]<-vals1
ins<-1
for(k in 1:length(username)){
  
  if(n_posts1[k]==""){
    n_posts1[k]<-n_posts[ins]
    ins<-ins+1
}}

block_data<-mst3%>% html_nodes('.username_container') %>% html_text(trim=TRUE)
block_data<-gsub("\n","",gsub("\r","",gsub("\t","",block_data)))
#sign_block<-mst3%>% html_nodes('.signaturecontainer') %>% html_text(trim=TRUE)
post1<-mst3%>% html_nodes('.postbody') %>% html_text(trim=TRUE)
posts<-gsub("\n","",gsub("\r","",gsub("\t","",post1)))
posts2<-data.frame(rep(forum,length(post_no)),rep(thread_no,length(post_no)),
                   post_no,username,type,location,join_date,block_data,n_posts1,posts,url1)
posts3<-rbind(posts3,posts2)
################

Sys.sleep(0.25)
print(paste0(round((last_post/last1)*100,0)," % complete"))
}
##########}

write.table(posts3, file = "/Users/AndyC/Dropbox/rdata/diab_scrape_us/db_posts.csv", sep = ",", col.names = NA, na="NA", qmethod = "double")
