#The implementation is done in S3 class system.
#
#The order of the encapsulation:  
# LD => SubjectLD => VisitLD => RoomLD
#
#Summary is done by 'logical' data member (in each class its own!) to 
#modify the printout of the class instance.
#
make_LD <- function(x){
  structure(list(dataLD = x), class = "LongitudinalData")
}

print.LongitudinalData <- function(x){
  n <- length(table(x$dataLD$id))
  paste("Longitudinal dataset with",n,"subjects")
}

subject <- function(x,n){
  structure(list(dataSD = x,subj_id = n,sum = FALSE), class = "SubjectLD")
}

print.SubjectLD <- function(x){
  nr<-nrow(x$dataSD$dataLD %>% filter(id==x$subj_id))
  if(nr != 0)
    return(paste("Subject ID:",x$subj_id))
  else
    return(NULL)
}

visit <- function(x,n){
  structure(list(dataVD = x,visit_id = n, sum = FALSE), class = "VisitLD")
}

print.VisitLD <- function(x){
  nr<-nrow(x$dataVD$dataSD$dataLD %>% filter(visit==x$visit_id))
  if(nr != 0)
  {
    strPrint <- print(x$dataVD)
    strPrint <- paste(strPrint,"\nVisit:",x$visit_id)
    return(cat(strPrint))
  }
  else
    return(NULL)
}

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
