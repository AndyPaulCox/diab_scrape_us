# 29th May 2015
# Andy Cox
# 
# code to scrape diabetes posts
rm(list = ls())
library(rvest)
library(pipeR)
################################
test_link<-function(ssion,lnk){
  t1<-NULL
  try(t1<-follow_link(ssion,lnk))
  if(is.null(t1))rt<-FALSE else rt<-TRUE
  return(rt)
}
###########################################
###############################
##Broken Links
"http://www.diabetesdaily.com/forum/announcements/",
"http://www.diabetesdaily.com/forum/diabetes-news/",
#########List of Level one urls (the sub-forums)
l1_urls<-c(
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


#Now visit the collected URLs one at a time and collect the posts

for(pg2 in 1:length(L2_urls)){
  pg2=1
  
#retrieve the first forum
mst3<-html_session(L2_urls[i])
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
#sign_block=character(),
n_posts=character(),
posts=character())
pst_pg<-1
while(last1>=t_pgs){
  
  url1<-paste0(substr(L2_urls[i],1,nchar(L2_urls[i])-1),"-",pst_pg)
  #retrieve the first forum
  mst3<-html_session(url1)
  pgs<-mst3%>% html_nodes('#postpagestats_above') %>% html_text()
  pgs<-gsub("\n","",gsub("\r","",gsub("\t","",pgs)))
  last<-unlist(strsplit(pgs," "))
  last<-as.numeric(last)[!is.na(as.numeric(last))]
  t_pgs<-last[2]
  last1<-last[length(last)]
################
#now scrape the data
forum<-mst3%>% html_nodes('.navbit~ .navbit+ .navbit a') %>% html_text(trim=TRUE)
thread_no<-pst_pg
post_no<-mst3%>% html_nodes('.postcounter') %>% html_text(trim=TRUE)
username<-mst3%>% html_nodes('.onlinestatus') %>% html_attr('alt')###this is the tricky one
type<-mst3%>% html_nodes('.usertitle') %>% html_text(trim=TRUE)
location<-mst3%>% html_nodes('dd:nth-child(4)') %>% html_text(trim=TRUE)
join_date<-mst3%>% html_nodes('dd:nth-child(2)') %>% html_text(trim=TRUE)
n_posts<-mst3%>% html_nodes('dd:nth-child(6)') %>% html_text(trim=TRUE)
block_data<-mst3%>% html_nodes('.username_container') %>% html_text(trim=TRUE)
block_data1<-gsub("\n","",gsub("\r","",gsub("\t","",block_data)))
sign_block<-mst3%>% html_nodes('.signaturecontainer') %>% html_text(trim=TRUE)
post1<-mst3%>% html_nodes('.postbody') %>% html_text(trim=TRUE)
posts<-gsub("\n","",gsub("\r","",gsub("\t","",post1)))
posts2<-data.frame(rep(forum,length(post_no)),rep(thread_no,length(post_no)),post_no,username,type,location,join_date,block_data1,n_posts,posts)
posts3<-rbind(posts3,posts2)
################
pst_pg<-pst_pg+1
}
}

write.table(posts3, file = "/Users/AndyC/Dropbox/rdata/diab_scrape_us/db_posts.csv", sep = ",", col.names = NA, na="NA", qmethod = "double")
