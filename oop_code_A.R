#############################
## Object Orientation Code ##
#############################

library(readr) #initiate libraries
library(dplyr)
library(tidyr)
library(magrittr)

download.file("https://d18ky98rnyall9.cloudfront.net/_257dbf6be13177cd110e3ef91b34ff67_data.zip?Expires=1503878400&Signature=DD6wzTxqZaooKWCqcT~jdFQTVNjXqidHdrGYHc63KTf1VBoGVqJX0UH3KFE9-RFQaDvBGse2EShcekxZBwTX3KXYT7grBwfrY2o68QlfloV8~jAni78X6L-STkAEQi6FJ9k3-rHChroIVoX7hnYneGf-KvKMt51DKH2wJamh6os_&Key-Pair-Id=APKAJLTNE6QMUY6HBC5A","data.zip",method="curl")
unzip("data.zip") #download and unzip data

data <- read_csv("data/MIE.csv") #read in data

make_LD<-function(x){ #create make_LD function to turn dataframe into LD object
    structure(x,class="LongitudinalData")
}

subject<-function(x,id)UseMethod("subject") #create generic subject function
visit<-function(x,visit_num)UseMethod("visit") #create generic visit function
room<-function(x,room_name)UseMethod("room") #create generic room function

subject.LongitudinalData<-function(x,id){ #create subject class, sub of LD
    index<-which(x$id==id) #identify subject
    if (length(index) == 0) return(NULL) #return null if subject doesn't exist
    
    x<-lapply(x,function(x) x[index]) #index all subject indices
    structure(x,class="subject") #assign subject class to all indices
}

visit.subject<-function(x,visit_num){ #create visit class, sub of subject
    if (!visit_num %in% 0:2) stop("The visit number must be 0, 1 or 2") #error check
    
    index<-which(x$visit==visit_num) #identify visit
    x<-lapply(x,function(x) x[index]) #index all visit indices
    structure(x,class="visit") #assign visit class to all indices
}

room.visit<-function(x,room_name){ #create room class, sub of visit
    if (!room_name %in% x$room) stop("Please provide a valid room name") #error check
    
    index<-which(x$room==room_name) #identify room
    x<-lapply(x,function(x) x[index]) #index all room indices
    structure(x,class="room") #assign room class to all indices
}

summary.subject<-function(x,...){ #create summary functionality for subject ID
    x<-list( #structure data split between id and data
        summary_id=unique(x$id),
        summary_data=data.frame(
            visit=x$visit,
            room=x$room,
            value=x$value
        )
    )
    
    x$summary_data<-x$summary_data%>% #clean and sort data
        group_by(visit,room)%>%
        summarize(value=mean(value))%>%
        spread(room,value)%>%
        as.data.frame
    
    structure(x,class="summary") #assign summary class
}

summary.room<-function(x,...){
    x<-list( #structure data split between id and data
        summary_id=unique(x$id),
        summary_data=data.frame(
            visit=x$visit,
            room=x$room,
            value=x$value
        )
    )
    
    x$summary_data<-summary(x$summary_data$value) #replace data with summary of data
    
    structure(x,class="summary") #assign summary class
}

print.LongitudinalData<-function(x,...){ #create generic print for all data
    cat("Longitudinal data set with ",length(unique(x$id)),"subjects")
}

print.subject<-function(x,...){ #create print for just subject
    cat("Subject ID: ",unique(x$id),"\n")
    invisible(x)
}

print.visit<-function(x,...){ #create print for subject and visit
    cat("Subject ID: ",unique(x$id),"\n")
    cat("Visit: ",unique(x$visit),"\n")
    invisible(x)
}

print.room<-function(x,...){ #create print for subject, visit and room
    cat("Subject ID: ",unique(x$id),"\n")
    cat("Visit: ",unique(x$visit),"\n")
    cat("Room: ",unique(x$room),"\n")
    invisible(x)
}

print.summary<-function(x,...){ #create summary print
    cat("Subject ID: ",x$summary_id,"\n")
    print(x$summary_data)
    invisible(x)
}