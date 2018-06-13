#load the test and train datasets
test<-read.csv("test.csv")
train<-read.csv("train.csv")

#adding Survived variable to test set
test["Survived"]<-"none"

#combine the datasets
fullset<-rbind(train,test)

#converting survived and pclass variables as factors
fullset$Survived<-as.factor(fullset$Survived)
fullset$Pclass<-as.factor(fullset$Pclass)

table(fullset$Survived)
table(fullset$Pclass)

#load ggplot2 package from the library
library(ggplot2)
train$Pclass<-as.factor(train$Pclass)
ggplot(train,aes(x=Pclass,fill=factor(Survived)))+geom_histogram(width=0.5,stat = "count")+xlab("Pclass")+ylab("toal count")+labs(fill="Survived")

#doing with the name variable
head(as.character(fullset$Name))

#finding unique names from the overall dataset
length(unique(as.character(fullset$Name)))

#we got 2duplicates
#we should see it in overall dataset
dupnames<-as.character(fullset[which(duplicated(as.character(fullset$Name))),"Name"])

#take a look at those records
fullset[which(fullset$Name %in% dupnames),]

library(stringr)
#search for miss titles in name vriable
misses<-fullset[which(str_detect(fullset$Name,"Miss.")),]
misses[1:5,]

#search for mrs title in name variable
mrses<-fullset[which(str_detect(fullset$Name,"Mrs.")),]
mrses[1:5,]

#serach for males in overall dataset
males<-fullset[which(str_detect(fullset$Sex,"male")),]
males[1:5,]

#create a function to extract the title
extractTitle<-function(Name){
  Name<-as.character(Name)
   if(length(grep("Miss.",Name))>0){
     return("Miss.")
   }else if(length(grep("Mrs.",Name))>0){
       return("Mrs.")
   }else if(length(grep("Mr.",Name))>0){
     return("Mr.")
   }else if(length(grep("Miss.",Name))>0){
       return("Miss.")
     }else if(length(grep("Master.",Name))>0){
       return("Master.")
     }else{
         return("Other")
       }
}

titles<-NULL

for (i in 1:nrow(fullset)) {
  titles<-c(titles,extractTitle(fullset[i,"Name"]))
  
}

fullset$titles<-as.factor(titles)

#getting plot from first 891 observations

ggplot(fullset[1:891,],aes(x=titles,fill=Survived))+
  geom_histogram(width = 0.5,stat = "count")+facet_wrap(~Pclass)+
  xlab("title")+ylab("Total count")+labs(fill="Survived")

#ploting for the sex variable to predict survival rates of males and females
ggplot(fullset[1:891,],aes(x=Sex,fill=Survived))+geom_histogram(width=0.5,stat = "count")+facet_wrap(~Pclass)+ggtitle("Pclass")+xlab("Sex")+ylab("total count")+labs(fill="Survived")

#look at the age variable
summary(fullset$Age)

#plot for survival rates broken out by sex ,pclass,age

ggplot(fullset[1:891,],aes(x=Age,fill=Survived))+geom_histogram(binwidth=10,stat = "count")+facet_wrap(~Sex+Pclass)+
  xlab("age")+ylab("total count")

#predicteing male boys 
boys<-fullset[which(fullset$titles=="Master."),]
summary(boys$Age)

#predicting misses 
misses<-fullset[which(fullset$titles=="Miss."),]
summary(misses$Age)

ggplot(misses[misses$Survived !="None",],aes(x=Age,fill=Survived))+
  facet_wrap(~Pclass)+ geom_histogram(width=5,stat = "count")+
  ggtitle("age for 'Miss.' by Pclass")+
  xlab("age")+
  ylab("Total count")

#female children may have different survival rates
miss.alone<-misses[which(misses$SibSp == 0 & misses$Parch == 0),]
summary(miss.alone$Age)
length(which(miss.alone$Age<=14.5))

#look at the sibsp variable
summary(fullset$SibSp)
 
#finding unique
length(unique(fullset$SibSp))

fullset$SibSp <- as.factor(fullset$SibSp) 

#visualization of sibsp,Pclass and title
ggplot(fullset[1:891,],aes(x=SibSp,fill=Survived))+geom_histogram(binwidth = 1,stat = "count")+facet_wrap(~Pclass+titles)+
  ggtitle("Pclass,title")+xlab("sibsp")+ylab("total count")+ylim(0,300)
#parch vaiable as factor and visualize
fullset$Parch<-as.factor(fullset$Parch)
ggplot(fullset[1:891,],aes(x=Parch,fill=Survived))+geom_histogram(binwith=1,stat = "count")+ggtitle("Pcalss,title")+facet_wrap(~Pclass+titles)+xlab("Prach")+ylab("total count")+labs(fill="survived")

#feature engineering...creating family size 

temp.sibsp<-c(train$SibSp,test$SibSp)
temp.parch<-c(train$Parch,test$Parch)
fullset$familysize<-as.factor(temp.sibsp+temp.parch+1)
 #graph for this
ggplot(fullset[1:891,],aes(x=familysize,fill=Survived))+
  geom_histogram(binwidth = 1,stat = "count")+
  facet_wrap(~Pclass+titles)+
  xlab("familysize")+
  ylab("total count")+
  ylim(0,300)+
  labs(fill="Survived")
#analysing the ticket variable
str(fullset$Ticket)
fullset$Ticket<-as.character(fullset$Ticket)

#taking first character from every ticket
firstchar<-ifelse(fullset$Ticket=="","",substr(fullset$Ticket,1,1))
firstchar
unique(firstchar)

#make as factor for visualization
fullset$firstchar<-as.factor(firstchar)

#making plot for this first char
ggplot(fullset[1:891,],aes(x=firstchar,fill=Survived))+
  geom_bar()+
  ggtitle("survivability by firstchar")+
  xlab("first char")+ylab("total count")+
  ylim(0,350)+
  labs(fill="Survived")
#making another plot 
ggplot(fullset[1:891,],aes(x=firstchar,fill=Survived))+
  geom_bar()+
  facet_wrap(~Pclass)+
  ggtitle("pclass")+
  xlab("firstchar")+
  ylab("total count")+
  ylim(0,150)+
  labs(fill="Survived")

#making anotherr plot with plcass and titles
ggplot(fullset[1:891,],aes(x=firstchar,fill=Survived))+
  geom_bar()+
  facet_wrap(~Pclass+titles)+
  ggtitle("pclass")+
  xlab("firstchar")+
  ylab("total count")+
  ylim(0,200)+
  labs(fill="Survived")

#next look at the fare variable
summary(fullset$Fare)
length(unique(fullset$Fare))
table(unique(fullset$Fare))

#we cant make fare as a factor, treat as numeric
#and visualize with histogram
ggplot(fullset[1:891,],aes(x=Fare))+
  geom_histogram(binwidth = 5)+
  ggtitle("combined fare distribution")+
  xlab("fare")+
  ylab("total count")+
  ylim(0,200)
#move to cabin variable
str(fullset$Cabin)
#converting as character
fullset$Cabin<-as.character(fullset$Cabin)
fullset$Cabin[1:100]

#replace empty cabins with "u"
fullset[which(fullset$Cabin==""),"Cabin"]<-"u"
fullset$Cabin[1:100]

#take a look at first character of cabin rows
cabin.firstchar<-as.factor(substr(fullset$Cabin,1,1))
str(cabin.firstchar)

#adding cabin.firstchar in to fullset
fullset$cabin.firstchar<-cabin.firstchar
#making a plot for this
ggplot(fullset[1:891,],aes(x=cabin.firstchar,fill=Survived))+
  geom_bar()+
  ggtitle("with first character")+
  xlab("cabin.first character")+
  ylab("total count")+
  ylim(0,800)
#let us see if there is any predictive power in this
ggplot(fullset[1:891,],aes(x=cabin.firstchar,fill=Survived))+
  geom_bar()+
  facet_wrap(~Pclass+titles)+
  ggtitle("survival rate by cabin.first char")+
  xlab("cabin.firstchar")+
  ylab("total count")+
  ylim(fill="Survived")
#lets look at the embarked variable
str(fullset$Embarked)

#lets make a plot for this
ggplot(fullset[1:891,],aes(x=Embarked,fill=Survived))+
  geom_bar()+
  ggtitle("plot by embarked")+
  facet_wrap(~Pclass+titles)+
  xlab("embarked")+
  ylab("total count")+
  ylim(0,300)+labs(fill="Survivied")


#***************Exploratory Modeling*******
rf.train.1<-fullset[1:891,c("Pclass","titles")]
rf.label<-as.factor(train$Survived)
str(rf.lebel) 
install.packages("randomForest")


#model from title and pclass variables
library(randomForest)
set.seed(1234)
rf.1<-randomForest(x=rf.train.1,y=rf.lebel,importance=TRUE,ntree=1000)
rf.1
varImpPlot(rf.1)


#take a look at another model from title,pclass and sibsp

set.seed(1234)
rf.train2<-fullset[1:891,c("Pclass","titles","SibSp")]

#algorithm

rf.2<-randomForest(x=rf.train2,y=rf.label,importance=TRUE,ntree=1000)
rf.2

#model for 3variables(pclass,parch,titles)

rf.train3<-fullset[1:891,c("Pclass","titles","Parch")]

#algorithm
set.seed(1234)
rf.3<-randomForest(x=rf.train3,y=rf.label,importance = TRUE,ntree = 1000)
rf.3
varImpPlot(rf.3)


#model for 4variables(Pclass,titles,SibSp,Prach)

rf.train4<-fullset[1:891,c("Pclass","titles","SibSp","Parch")]

#algorithm
set.seed(1234)
rf.4<-randomForest(x=rf.train4,y=rf.label,importance = TRUE,ntree = 1000) 
rf.4
varImpPlot(rf.4)

#model for again 3 variables(Pclass,titles,familysize)

rf.train5<-fullset[1:891,c("Pclass","titles","familysize")]

#algorithm
set.seed(1234)
rf.5<-randomForest(x=rf.train5,y=rf.label,importance = TRUE,ntree = 1000)
rf.5
varImpPlot(rf.5)

#model by using 4 variable(titles, pclass ,SibSp ,familysize)

rf.train6<-fullset[1:891,c("titles","Pclass","SibSp","familysize")]

#algorit6hm
set.seed(1234)
rf.6<-randomForest(x=rf.train6,y=rf.label,importance = TRUE,ntree = 1000)
rf.6
varImpPlot(rf.6)


#model by using tiltes,pclass,Parch,familysize

rf.train7<-fullset[1:891,c("titles","Pclass","Parch","familysize")]

#algorithm
set.seed(1234)
rf.7<-randomForest(x=rf.train7,y=rf.label,importance = TRUE,ntree = 1000)
rf.7


#submitting
submit.df<-fullset[892:1309,c("Pclass","titles","familysize")]

###make predictions
rf.5.preds<-predict(rf.5,submit.df)
table(rf.5.preds)

#write out a CSV file for submision
submit<-data.frame(PassengerId=rep(892:1309),Survied=rf.5.preds)

write.csv(submit,file = "aru.csv",row.names = FALSE)





