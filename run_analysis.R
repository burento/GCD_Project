# Load necessary library
library(data.table)

#Initialize variables 
dir <- "UCI HAR Dataset"
groups <- c("train","test")
signals <- c("body_acc","body_gyro","total_acc")
dims <- c("x","y","z")

# Load the meta data that we will use for our lookups to aggregate the data
# After loading each file, ascertain the order is correct and relabel columns
labels <- fread(paste(dir,"/activity_labels.txt",sep=""))
setnames(labels,c("ACTIVITY_ID","DESC"))
setorderv(labels,cols="ACTIVITY_ID",order=1)
features <- fread(paste(dir,"/features.txt",sep=""))
setnames(features,c("FEATURE_ID","DESC"))
setorderv(features,cols="FEATURE_ID",order=1)

# Both folders have the same strucutre, so iterate the process to collect and merage all data
for( grp in groups ){
  subj <- fread(paste(dir,"/",grp,"/subject_",grp,".txt",sep=""))
  setnames(subj,"SUBJECT_ID")
  data_set <- read.table(paste(dir,"/",grp,"/X_",grp,".txt",sep=""),colClasses="numeric")
  setnames(data_set,features$DESC)
  activity_set <- fread(paste(dir,"/","test","/y_","test",".txt",sep=""))
  setnames(activity_set,"ACTIVITY_ID")
  
  # Create a main table to which we'll bind additional measurements
  dbx <- cbind(subj,activity_set,data_set)
  
  # Read in all the inertial signals, adding each to the main table for this group
  for(sig in signals){
    for(d in dims){
      ds <- read.table(paste(dir,"/",grp,"/Inertial Signals/",sig,"_",d,"_",grp,".txt",sep=""))
      # Update our columns names as before
      sig_cols <- paste(sig,d,c(1:128),sep="_")
      setnames(ds,sig_cols)
      dbx <- cbind(dbx, ds)
    }
  }
  
  # Create a master record and bind all the data together
  if(!exists("db")){
    db <- dbx
  }else{
    db <- rbind(db, dbx, use.names=TRUE)
  }
}

# Clean up environment
rm(activity_set,data_set,dbx,ds,subj,sig_cols,d,dims,groups,grp,sig,signals)

# Determine which columns to keep (i.e. keys, means, and std)
hdr <- colnames(db)
db_tidy <- subset(db, select=hdr[unique(sort(c(1:2,grep("mean\\(\\)",hdr),grep("std\\(\\)",hdr))))])

#Update our activity from an id to the acutal description
for(r in 1:nrow(labels)){ 
    db_tidy$ACTIVITY_ID[db_tidy$ACTIVITY_ID == labels[r,ACTIVITY_ID]] <- labels[r,DESC]
}
setnames(db_tidy,"ACTIVITY_ID","ACTIVITY")

# DB_TIDY is our result from #4 in our assignment.

# Continuing on to step #5
# Create a smaller aggregate based on subj-activity combination calculating the mean of each input
tidy_agg <- aggregate(db_tidy,by=list(db_tidy$SUBJECT_ID,db_tidy$ACTIVITY),FUN=mean) 
tidy_agg <- subset(tidy_agg, select=-c(Group.1,ACTIVITY))
setnames(tidy_agg,"Group.2","ACTIVITY")

# Write out smaller tidy data set
write.table(tidy_agg,file="gcd_project_tidy_set.txt",row.name=FALSE)


