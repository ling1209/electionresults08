#2008 election result(source:https://www.theguardian.com/news/datablog/2009/mar/02/us-elections-2008)
#First I use "asap utilities"(http://www.asap-utilities.com) to derive 51 spreadsheets from the xlsx file.

#paste the file name to read the csv files
path=paste('~/Documents/133 cicwd/Project 2/election2008/',state.name,'.csv',sep="")
statedata=list()#create a empty list to get the data
#each csv file has been stored as a list in statedata
statedata=lapply(path,function(x){read.csv(x,header=T,stringsAsFactors=FALSE)})
i=1
#create state name variable for the data
statedata=lapply(statedata,function(df){df$X=state.name[i];i<<-(i+1);return(df)})
i=1
#combine data in different states together
result2008=statedata[[1]]
for(i in 2:length(statedata)){
  result2008=rbind(result2008,statedata[[i]])
}
#rename the variable names
names(result2008)=c("County_08","Total_precincts_08","Report_precincts_08","Obama_08","McCain_08","Other_08","State_08")
#there are some comment in the original data so I have to delete them
result2008=result2008[!is.na(result2008$Other_08),]
#delete temporary variable
rm(statedata)
#data of D.C. are only included in "Total results.csv", so I have to fetch that
totalresult=read.csv('~/Documents/133 cicwd/Project 2/election2008/Total results.csv',header=T,stringsAsFactors = FALSE)
dc=totalresult[totalresult$STATE=="D.C.",c("OBAMA","MCCAIN")]
names(dc)=NULL
#put data of D.C at the bottom of the dataframe
result2008=rbind(result2008,data.frame(County_08="District of Columbia",Total_precincts_08=1,Report_precincts_08=1,Obama_08=dc[1],McCain_08=dc[2],Other_08=0,State_08="District of Columbia"))
#eliminate extra space in variable
result2008[[1]]=gsub(" $","",result2008[[1]])
rm(dc)
rm(i)
rm(path)
rm(totalresult)


#2016 election result(source:http://www.stat.berkeley.edu/users/nolan/data/voteProject/2016_US_County_Level_Presidential_Results.csv)
#read the data
result2016=read.csv("~/Downloads/2016result.csv",header=TRUE,stringsAsFactors = FALSE)
#the first column is useless so I have to delete that
result2016=result2016[,-1]
#there are some repetition in Alaska, I only keep the one of them
result2016=result2016[-c(1:28),]
#rename variables
names(result2016)=c("Clinton_16","Trump_16","Total_16","Per_Clinton_16","Per_Trump_16","Diff_16","Per_point_diff_16","State_16","County_16","Fips_16")
#change name of DC
result2016[result2016$State_16=="DC","State_16"]="District of Columbia"


#This function change the format of county name and put couty names and state names together
changeFormat=function(df,countyNum,stateNum,changeVirginiaName,changeStateName)
{
  #Change "Saint" to "St"
  df[[countyNum]]=gsub("^Saint","St",df[[countyNum]],ignore.case = TRUE)
  #There are irregular pattern for county name started by "La"
  #Need to change them in the format of "La Xyyy"
  df[[countyNum]]=gsub("^Larue","La Rue",df[[countyNum]],ignore.case = TRUE)#Change "Larue" to "La Rue"
  df[[countyNum]]=gsub("^Lacle","La Cle",df[[countyNum]],ignore.case = TRUE)#Change "Lacle" to "La Cle"
  df[[countyNum]]=gsub("^Lava","La Va",df[[countyNum]],ignore.case = TRUE)#Change "Lava" to "La Va"
  #Some counties named "Jefferson Davis" is actually the same as "Jeff Davis" counties
  df[[countyNum]]=gsub("^Jefferson Davis","Jeff Davis",df[[countyNum]],ignore.case = TRUE)
  #Change "&" to "and"
  df[[countyNum]]=gsub("&","and",df[[countyNum]])
  #Eliminate "."
  df[[countyNum]]=gsub("\\.","",df[[countyNum]],ignore.case=TRUE)
  #Eliminate punctuation
  df[[countyNum]]=gsub("[[:punct:]]"," ",df[[countyNum]],ignore.case=TRUE)
  #Change "xxYy" to "xx Yy"
  df[[countyNum]]=gsub("([[:lower:]])([[:upper:]])","\\1 \\2",df[[countyNum]])
  #Eliminate "County" or "Parish" in the back
  df[[countyNum]]=gsub(" ((Parish)|(County))+$","",df[[countyNum]],ignore.case = TRUE)
  df[[countyNum]]=gsub(" ((Parish)|(County))+$","",df[[countyNum]],ignore.case = TRUE)
  #Change all of them to lower case
  df[[countyNum]]=tolower(df[[countyNum]])

  #Change name of states to full name
  if(changeStateName){
    for(i in 1:length(state.name)){
      df[[stateNum]]=gsub(paste("^",state.abb[i],sep=""),state.name[i],df[[stateNum]])
    }
  }
  #Change name of couties and cities in Virginia to the format of 2016 dataset
  if(changeVirginiaName){
    virginia=result2016[result2016$State_16=="Virginia",9]
    bedfordIndex=which(virginia=="bedford")
    virginia=c(virginia[1:bedfordIndex],"bedford city",virginia[(bedfordIndex+1):length(virginia)])
    df[df[[stateNum]]=="Virginia",countyNum]=virginia
  }
  #Eliminate space in county names
  df[[countyNum]]=gsub("[[:blank:]]","",df[[countyNum]])
  #paste county name and state name for future merge
  df$CountyState=paste(df[[countyNum]],df[[stateNum]])
  return(df)
}
result2016=changeFormat(result2016,9,8,FALSE,TRUE)
result2008=changeFormat(result2008,1,7,TRUE,FALSE)
#merge the two dataset and I choose to keep all of them
result0816=merge(result2008,result2016,by.x="CountyState",by.y="CountyState",all=TRUE)
#eliminate extra space in variable
rm(result2008)
rm(result2016)
rm(changeFormat)