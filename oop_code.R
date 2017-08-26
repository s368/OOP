library(tidyr)
library(dplyr)

#The implementation is done in S3 class system.
#
#The order of the encapsulation:  
# LD => SubjectLD => VisitLD => RoomLD
#
#Summary is done by 'logical' data member (in each class its own!) to 
#modify the printout of the class instance.
#
#########################################################################
# LongitudinalData class
#########################################################################
make_LD <- function(x){
  structure(list(dataLD = x), class = "LongitudinalData")
}

print.LongitudinalData <- function(x){
  n <- length(table(x$dataLD$id))
  paste("Longitudinal dataset with",n,"subjects")
}

#########################################################################
# SubjectLD class
#########################################################################
subject <- function(x,n){
  structure(list(dataSD = x,subj_id = n,sum = FALSE), class = "SubjectLD")
}

print.SubjectLD <- function(x){
  nr<-nrow(x$dataSD$dataLD %>% filter(id==x$subj_id))
  if(nr != 0)
    if(x$sum == FALSE)
      paste("Subject ID:",x$subj_id)
    else
    {
      x$sum<-FALSE # to avoid infinite recursion but re-use 'print.SubjectLD'!
      print(paste(print(x)))
      x$sum<-TRUE

      df<-x$dataSD$dataLD %>% filter(id==x$subj_id) %>% 
        select(-id) %>%
          group_by(visit,room) %>% 
            summarise(m = mean(value)) %>%  
              spread(room,m)
      print(df)
    }
  else
    NULL
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
  nr<-nrow(x$dataVD$dataSD$dataLD %>% filter(id==x$dataVD$subj_id & visit==x$visit_id))
  if(nr != 0)
  {
    if(x$sum == FALSE)
      paste("Subject ID:",x$subj_id)
    else
    {
      x$sum<-FALSE # to avoid infinite recursion but re-use 'print.VisitLD'!
      strPrint <- print(x$dataVD)
      cat(strPrint,"\nVisit:",x$visit_id,"\n")
      x$sum<-TRUE
      
      df<-x$dataVD$dataSD$dataLD %>% filter(id==x$dataVD$subj_id & visit==x$visit_id) %>%
        select(-id,-visit) %>%
        group_by(room) %>%
        summarise(m = mean(value)) %>%
        spread(room,m)
      print(df)
    }
  }
  else
    NULL
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
  nr<-nrow(x$dataRD$dataVD$dataSD$dataLD %>% filter(room==x$room_id))
  if(nr != 0)
  {
    strPrint <- print(x$dataRD)
    strPrint <- paste(strPrint,"\nRoom:",x$room_id)
    return(cat(strPrint))
  }
  else
    return(NULL)
}
