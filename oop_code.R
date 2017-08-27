#########################################################################
#The implementation is done in S3 class system.
#
#The order of the encapsulation:  
# LD => SubjectLD => VisitLD => RoomLD
#
#Summary is done by 'logical' data member (in each class its own!) to 
#modify the printout of the class instance.
#
#########################################################################

library(tidyr)
library(dplyr)

subject<-function(x,id)UseMethod("subject") #create generic subject function
visit<-function(x,visit_num)UseMethod("visit") #create generic visit function
room<-function(x,room_name)UseMethod("room") #create generic room function

#########################################################################
# LongitudinalData class
#########################################################################
make_LD <- function(x){
  structure(list(dataLD = x), class = "LongitudinalData")
}

print.LongitudinalData <- function(x){
  n <- length(table(x$dataLD$id))
  print(paste("Longitudinal dataset with",n,"subjects"))
}

#########################################################################
# SubjectLD class
#########################################################################
subject <- function(x,n){
  structure(list(dataSD = x,subj_id = n,sum = FALSE), class = "SubjectLD")
}

print.SubjectLD <- function(x){
  nr<-nrow(x$dataSD$dataLD %>% 
             filter(id==x$subj_id))
  if(nr != 0)
    if(x$sum == FALSE)
      print(paste("Subject ID:",x$subj_id))
    else
    {
      print(paste("Subject ID:",x$subj_id))
      
      df<-x$dataSD$dataLD %>% filter(id==x$subj_id) %>% 
        select(-id) %>%
          group_by(visit,room) %>% 
            summarise(m = mean(value)) %>%  
              spread(room,m)
      print(as.data.frame(df))
    }
  else
    print(NULL)
}

summary.SubjectLD <- function(x){
  x$sum <- TRUE
  return(x)
}

#########################################################################
# visitLD class
#########################################################################
visit <- function(x,n){
  structure(list(dataVD = x,visit_id = n, sum = FALSE), class = "VisitLD")
}

print.VisitLD <- function(x){
  nr<-nrow(x$dataVD$dataSD$dataLD %>% 
             filter(id==x$dataVD$subj_id & visit==x$visit_id))
  if(nr != 0)
  {
    if(x$sum == FALSE)
    {
      print(paste("Subject ID:",x$dataVD$subj_id))
      print(paste("Visit:",x$visit_id))
    }
    else
    {
      print(paste("Subject ID:",x$dataVD$subj_id))
      print(paste("Visit:",x$visit_id))
      
      df<-x$dataVD$dataSD$dataLD %>% 
        filter(id==x$dataVD$subj_id & visit==x$visit_id) %>%
          select(-id,-visit) %>%
            group_by(room) %>%
              summarise(m = mean(value)) %>%
                spread(room,m)
      print(as.data.frame(df))
    }
  }
  else
    print(NULL)
}

summary.VisitLD <- function(x){
  x$sum <- TRUE
  return(x)
}

#########################################################################
# roomLD class
#########################################################################
room <- function(x,n){
  structure(list(dataRD = x,room_id = n, sum = FALSE), class = "RoomLD")
}

print.RoomLD <- function(x){
  nr<-nrow(x$dataRD$dataVD$dataSD$dataLD %>% 
             filter(id==x$dataRD$dataVD$subj_id & visit==x$dataRD$visit_id & room==x$room_id))
  if(nr != 0)
  {
    if(x$sum == FALSE)
    {
      print(paste("Subject ID:",x$dataRD$dataVD$subj_id))
      print(paste("Visit:",x$dataRD$visit_id))
      print(paste("Room:",x$room_id))
    }
    else
    {
      print(paste("Subject ID:",x$dataRD$dataVD$subj_id))
      print(paste("Visit:",x$dataRD$visit_id))
      print(paste("Room:",x$room_id))
      
      df<-x$dataRD$dataVD$dataSD$dataLD %>% filter(id==x$dataRD$dataVD$subj_id & 
                                              visit==x$dataRD$visit_id &
                                              room==x$room_id) %>%
        select(-id,-visit,-room,-timepoint)
      print(summary(df$value))
    }
  }
  else
    print(NULL)
}

summary.RoomLD <- function(x){
  x$sum <- TRUE
  return(x)
}
#########################################################################
# END
#########################################################################
