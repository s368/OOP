make_LD <- function(x){
  structure(list(data = x,sum = FALSE,subj_id = -1, visit_id = -1, room_id = -1), class = "LongitudinalData")
}

subject <- function(x,n) UseMethod("subject")
subject.LongitudinalData <- function(x,n){
  nr<-nrow(x$data %>% filter(id==n))
  if(nr != 0)
    x$subj_id<-n
  else
    x$subj_id<-(-1)
  x
}

visit <- function(x,n) UseMethod("visit")
visit.LongitudinalData <- function(x,n){
  nr<-nrow(x$data %>% filter(visit==n))
  if(nr != 0)
    x$visit_id<-n
  else
    x$visit_id<-(-1)
  x
}

room <- function(x,s) UseMethod("room")
room.LongitudinalData <- function(x,s){
  nr<-nrow(x$data %>% filter(room==s))
  if(nr != 0)
    x$room_id<-s
  else
    x$room_id<-(-1)
  x
}

print.LongitudinalData <- function(x){
  # if(sum == FALSE)
  # {
  #   paste("Longitudinal dataset with",length(table(x$data$id)),"subjects")
  # }
  # else
  {
    if(x$subj_id == -1)
    {
      NULL
    }
    else
    {
      if(x$visit_id == -1)
      {
        paste("Subject ID:",x$subj_id)
      }
      else
      {
        if(x$room_id == -1)
        {
          cat(paste("ID:",x$subj_id,"\nVisit:",x$visit_id))
        }
        else
        {
          cat(paste("ID:",x$subj_id,"\nVisit:",x$visit_id,"\nRoom:",x$room_id))
        }
      }
    }
  }
}
