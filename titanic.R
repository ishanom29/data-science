train<-read.csv("train.csv",header = TRUE)
test<-read.csv("test.csv",header=TRUE)
view(train)
test.survived <- data.frame(Survived = rep("None", nrow(test)), test[,])
data.combined<-rbind(train,test.survived)
str(data.combined)
data.combined$Survived<-as.factor(data.combined$Survived)
data.combined$Pclass<-as.factor(data.combined$Pclass)
table(data.combined$Survived)
table(data.combined$Pclass)
library(ggplot2)
ggplot(train, aes(x = Pclass, fill = factor(Survived))) +
  geom_bar() +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived")
  head(as.character(train$Name)) 
  length(as.character(data.combined$Name))
  length(unique(as.character(data.combined$Name)))
  dup.name <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))), "Name"])
  data.combined[which(data.combined$Name %in% dup.name),]
  library(stringr)
  misses<- data.combined[which(str_detect(data.combined$Name,"Miss.")),]
  misses[1:5,]
  mrs<- data.combined[which(str_detect(data.combined$Name,"Mrs.")),]
  mrs[1:5,]
  males<-data.combined[which(train$Sex=="male"),]
  males[1:5,]
  master<-data.combined[which(str_detect(data.combined$Name,"Master.")),]
  master[1:5,]
  master[,]
  name<-data.combined$Name
  extractTitle <- function(name) {
    name <- as.character(name)
    
    if (length(grep("Miss.", name)) > 0) {
      return ("Miss.")
    } else if (length(grep("Master.", name)) > 0) {
      return ("Master.")
    } else if (length(grep("Mrs.", name)) > 0) {
      return ("Mrs.")
    } else if (length(grep("Mr.", name)) > 0) {
      return ("Mr.")
    } else {
      return ("Other")
    }
  }
  
  titles <- NULL
  for (i in 1:nrow(data.combined)) {
    titles <- c(titles, extractTitle(data.combined[i,"Name"]))
  }
  data.combined$title <- as.factor(titles)
  
  
  ggplot(data.combined[1:891,], aes(x = title, fill = Survived)) +
    geom_bar() +
    facet_wrap(~Pclass) + 
    ggtitle("Pclass") +
    xlab("Title") +
    ylab("Total Count") +
    labs(fill = "Survived")
    
    table(data.combined$Sex)
    ggplot(data.combined[1:891,], aes(x = Sex, fill = Survived)) +
      geom_bar() +
      facet_wrap(~Pclass) + 
      ggtitle("Pclass") +
      xlab("Sex") +
      ylab("Total Count") +
      labs(fill = "Survived")
    summary(data.combined$Age)
summary(data.combined[1:891,"Age"])
    ggplot(data.combined[1:891,],aes(x=Age,fill=Survived))+
      facet_wrap(~Sex+Pclass)+
      geom_histogram(binwidth = 10)+
      ylab("total count")+
      xlab("age")
boys <- data.combined[which(data.combined$title == "Master."),]
summary(boys$Age)
misses <- data.combined[which(data.combined$title == "Miss."),]
summary(misses$Age)
ggplot(misses[misses$Survived != "None",],aes(x=Age,fill=Survived))+
  facet_wrap(~Pclass) +
  geom_histogram(binwidth = 5) +
  xlab("Age")+
  ylab("Total Count")
misses.alone <- misses[which(misses$SibSp==0 & misses$Parch==0),]
summary(misses.alone$Age)
length(which(misses.alone$Age<=14.5))    
summary(data.combined$SibSp)
length(unique(data.combined$SibSp))
ggplot(data.combined[1:891,],aes(x=SibSp,fill=Survived))+
  facet_wrap(~Pclass+title)+
  geom_histogram(binwidth = 1)+
  ggtitle("Pclass,title")+
  xlab("SibSp")+
  ylab("TotalCount")+
  ylim(0,300)+
  labs(fill="Survived")
  data.combined$Parch <- as.factor(data.combined$Parch)
  data.combined$parch <- as.factor(data.combined$parch)
  ggplot(data.combined[1:891,], aes(x = Parch, fill = Survived)) +
    geom_bar() +
    facet_wrap(~Pclass + title) + 
    ggtitle("Pclass, Title") +
    xlab("ParCh") +
    ylab("Total Count") +
    ylim(0,300) +
    labs(fill = "Survived")
temp.sibsp<-c(test$SibSp,train$SibSp)
temp.parch<-c(test$SibSp,train$SibSp)
data.combined$familysize<-as.factor(temp.parch+temp.sibsp+1)
ggplot(data.combined[1:891,],aes(x=familysize,fill=Survived))+
  facet_wrap(~Pclass+title)+
  geom_bar()+
  ggtitle("Pclass,title")+
  xlab("familysize")+
  ylab("TotalCount")+
  ylim(0,300)+
  labs(fill="Survived")
str(data.combined$Ticket)
data.combined$Ticket<-as.character(data.combined$Ticket)
data.combined$Ticket[1:20]
Ticket.first.char<-ifelse(data.combined$Ticket=='',' ',substr(data.combined$Ticket,1,1))
unique(Ticket.first.char)
data.combined$Ticket.first.char<- as.factor(Ticket.first.char)
ggplot(data.combined[1:891,],aes(x=Ticket.first.char,fill=Survived))+
  geom_bar()+
  ggtitle("graph related to first character of ticket")+
  xlab("Ticket.first.char")+
  ylab("total count")+
  ylim(0,350)+
  labs(fill="Survived")
ggplot(data.combined[1:891,],aes(x=Ticket.first.char,fill=Survived))+
  geom_bar()+
  facet_wrap(~Pclass)+
  ggtitle("Pclass")+
  xlab("Ticket.first.char")+
  ylab("total count")+
  ylim(0,350)+
  labs(fill="Survived")
ggplot(data.combined[1:891,],aes(x=Ticket.first.char,fill=Survived))+
  geom_bar()+
  facet_wrap(~Pclass+title)+
  ggtitle("graph related to first character of ticket")+
  xlab("Ticket.first.char")+
  ylab("total count")+
  ylim(0,350)+
  labs(fill="Survived")
summary(data.combined$Fare)
length(unique(data.combined$Fare))
ggplot(data.combined,aes(x=Fare))+
  geom_histogram(binwidth = 5)+
  ggtitle("combined fair distribution")+
  xlab("fair")+
  ylab("total count")+
  ylim(0,200)
ggplot(data.combined[1:891,],aes(x=Fare,fill=Survived))+
  geom_histogram(binwidth = 5)+
  facet_wrap(~Pclass+title)+
  ggtitle("pclass,title")+
  xlab("fare")+
  ylab("total count")+
  ylim(0,350)+
  labs(fill="Survived")
str(data.combined$Cabin)
data.combined$Cabin<-as.character(data.combined$Cabin)
data.combined$Cabin[1:100]
data.combined[which(data.combined$Cabin == ""), "Cabin"] <- "U"
data.combined$Cabin[1:100] 
cabin.first.char <- as.factor(substr(data.combined$Cabin, 1, 1))
str(cabin.first.char)
levels(cabin.first.char)
data.combined$cabin.first.char <- cabin.first.char
ggplot(data.combined[1:891,], aes(x = cabin.first.char, fill = Survived)) +
  geom_bar() +
  ggtitle("Survivability by cabin.first.char") +
  xlab("cabin.first.char") +
  ylab("Total Count") +
  ylim(0,750) +
  labs(fill = "Survived")
ggplot(data.combined[1:891,], aes(x = cabin.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) +
  ggtitle("Survivability by cabin.first.char") +
  xlab("Pclass") +
  ylab("Total Count") +
  ylim(0,500) +
  labs(fill = "Survived")
ggplot(data.combined[1:891,], aes(x = cabin.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("cabin.first.char") +
  ylab("Total Count") +
  ylim(0,500) +
  labs(fill = "Survived")
library(randomForest)
rf.train<-data.combined[1:891,c("Pclass","title")]
rf.label=as.factor(train$Survived)
set.seed(1234)
rf.1<-randomForest(x=rf.train,y=rf.label,importance = TRUE,ntree = 1000)
rf.1
varImpPlot(rf.1)
rf.train.2<-data.combined[1:891,c("Pclass","title","SibSp")]
rf.2<-rf.1<-randomForest(x=rf.train.2,y=rf.label,importance = TRUE,ntree = 1000)
rf.2
varImpPlot(rf.2)
rf.train.3<-data.combined[1:891,c("Pclass","title","Parch")]
rf.3<-randomForest(x=rf.train.3,y=rf.label,importance = TRUE,ntree = 1000)
rf.3
varImpPlot(rf.3)
rf.train.4<-data.combined[1:891,c("Pclass","title","SibSp","Parch")]
rf.4<-randomForest(x=rf.train.4,y=rf.label,importance = TRUE,ntree = 1000)
rf.4
varImpPlot(rf.4)
rf.train.5<-data.combined[1:891,c("Pclass","title","familysize")]
rf.5<-randomForest(x=rf.train.5,y=rf.label,importance = TRUE,ntree = 1000)
rf.5
varImpPlot(rf.5)
rf.train.6<-data.combined[1:891,c("Pclass","title","SibSp","familysize")]
rf.6<-randomForest(x=rf.train.6,y=rf.label,importance = TRUE,ntree = 1000)
rf.6
varImpPlot(rf.6)
rf.train.7<-data.combined[1:891,c("Pclass","title","Parch","familysize")]
rf.7<-randomForest(x=rf.train.7,y=rf.label,importance = TRUE,ntree = 1000)
rf.7
varImpPlot(rf.7)
rf.train.8<-data.combined[1:891,c("Pclass","title","SibSp","Parch","familysize")]
rf.8<-randomForest(x=rf.train.8,y=rf.label,importance = TRUE,ntree = 1000)
rf.8
varImpPlot(rf.8)

