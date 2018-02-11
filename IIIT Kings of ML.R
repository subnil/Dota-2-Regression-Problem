setwd('C:/Users/SUBHRANIL ROY/Desktop/Machine Learning books/ML Data Sets/AV Practice datasets/AV Hackathon Practice/IIIT Hackathon - Kings of Machine Learning')

koml_train1_raw= read.csv('train1.csv')
koml_train9_raw= read.csv('train9.csv')

koml_test1_raw= read.csv('test1.csv')
koml_test9_raw= read.csv('test9.csv')

koml_hero_data_raw= read.csv('hero_data.csv')

#Check for NA values
colSums(is.na(koml_train1_raw))
colSums(is.na(koml_train9_raw))

colSums(is.na(koml_test1_raw))
colSums(is.na(koml_test9_raw))

colSums(is.na(koml_hero_data_raw))

#Investigate the id column for train9 & train1
NROW(setdiff(koml_train9_raw$id, koml_train1_raw$id))
library(sqldf)
imp_id1=sqldf('select koml_train9_raw.id from koml_train9_raw, koml_train1_raw 
where koml_train9_raw.id= koml_train1_raw.id')

imp_id2=sqldf('select koml_test1_raw.id from koml_train9_raw, koml_test1_raw 
where koml_train9_raw.id= koml_test1_raw.id')

imp_id3=sqldf('select koml_test1_raw.id from koml_test9_raw, koml_test1_raw 
where koml_test9_raw.id= koml_test1_raw.id')

imp_id4=sqldf('select koml_test9_raw.id from koml_train1_raw, koml_test9_raw 
where koml_train1_raw.id= koml_test9_raw.id')

temp=rbind(imp_id1,imp_id3)

##COnvert the required columns to factors for various sets
#koml_train9_raw
koml_train9_raw$user_id=as.factor(as.character(koml_train9_raw$user_id))
koml_train9_raw$hero_id=as.factor(as.character(koml_train9_raw$hero_id))

#koml_train1_raw
koml_train1_raw$user_id=as.factor(as.character(koml_train1_raw$user_id))
koml_train1_raw$hero_id=as.factor(as.character(koml_train1_raw$hero_id))

#koml_test9_raw
koml_test9_raw$user_id=as.factor(as.character(koml_test9_raw$user_id))
koml_test9_raw$hero_id=as.factor(as.character(koml_test9_raw$hero_id))

#koml_test1_raw
koml_test1_raw$user_id=as.factor(as.character(koml_test1_raw$user_id))
koml_test1_raw$hero_id=as.factor(as.character(koml_test1_raw$hero_id))

#koml_hero_data_raw
koml_hero_data_raw$hero_id=as.factor(as.character(koml_hero_data_raw$hero_id))

##Combine the train9 and test9 dataset
koml_traintest9= rbind.data.frame(koml_train9_raw,koml_test9_raw)

#Remove the id column
koml_traintest9= koml_traintest9[-3]

#Join the traintest9 set, train1 set & test1 set with hero data set
koml_traintest9_full= sqldf('select * from koml_traintest9, koml_hero_data_raw
where koml_traintest9.hero_id= koml_hero_data_raw.hero_id')
koml_traintest9_full= koml_traintest9_full[-7]

koml_train1_full= sqldf('select * from koml_train1_raw, koml_hero_data_raw
where koml_train1_raw.hero_id= koml_hero_data_raw.hero_id')
koml_train1_full= koml_train1_full[-c(7)]

koml_test1_full= sqldf('select * from koml_test1_raw, koml_hero_data_raw
where koml_test1_raw.hero_id= koml_hero_data_raw.hero_id')
koml_test1_full= koml_test1_full[-c(5)]
koml_test1_full$hero_id=as.factor(koml_test1_full$hero_id)

#Preprocess id column for traintest9 to change it as required
#id_backup= koml_traintest9_full$id
koml_traintest9_full$id=id_backup
koml_traintest9_full$id=as.character(koml_traintest9_full$id)
#koml_traintest9_full$id= as.factor(koml_traintest9_full$id)
all(temp$id %in% koml_traintest9_full$id)

#Retain only the matching levels and convert rest levels to 0 for traintest9 set
z1=rep(NA,26928)
j=1
for(i in koml_traintest9_full$id){
  if(i %in% temp$id){
    z1[j]='1'
  }
  else{
    z1[j]='0'
  }
  j= j+1
}
length(z1)
z1= as.factor(z1)
table(z1)
koml_traintest9_full$id= z1

#Preprocess id column for train1 to change it as required
koml_train1_full$id=koml_train1_raw$id
koml_train1_full$id=as.character(koml_train1_full$id)
all(temp$id %in% koml_train1_full$id)
any(temp$id %in% koml_train1_full$id)

#Retain only the matching levels and convert rest levels to 1 for train1 set
z2=rep(NA,2094)
j=1
for(i in koml_train1_full$id){
  if(i %in% temp$id){
    z2[j]='1'
  }
  else{
    z2[j]='0'
  }
  j= j+1
}
length(z2)
z2= as.factor(z2)
table(z2)
koml_train1_full$id= z2

#Preprocess id column for train1 to change it as required
koml_test1_full$id= koml_test1_raw$id
koml_test1_full$id=as.character(koml_test1_full$id)
all(temp$id %in% koml_test1_full$id)
any(temp$id %in% koml_test1_full$id)

#Retain only the matching levels and convert rest levels to 0 for test1 set
z3=rep(NA,898)
j=1
for(i in koml_test1_full$id){
  if(i %in% temp$id){
    z3[j]='1'
  }
  else{
    z3[j]='0'
  }
  j= j+1
}
length(z3)
z3= as.factor(z3)
table(z3)
koml_test1_full$id= z3

#Create RMSE function
Smy_RMSE= function(x) {
  sum1=0
  n=NROW(x)
  for(i in 1:n){
    sum1= sum1+ (x[i]-koml_train1_full$kda_ratio[i])^2
  }
  rmse=sqrt(sum1/n)
  return(rmse)
}

#Create output xls function
output_xls= function(x){
  kda_ratio=x
  id= koml_test1_raw$id
  av_koml=cbind.data.frame(id,kda_ratio)
  write.csv(av_koml,'C:/Users/SUBHRANIL ROY/Desktop/av_koml.csv',row.names = FALSE)
}

#Create dummy dataset 
library(caret)
koml_traintest9_full1= koml_traintest9_full[-c(1,2,3,9)]
dummy1= dummyVars("~.",data = koml_traintest9_full1)
koml_traintest9_fulld= data.frame(predict(dummy1, newdata=koml_traintest9_full1))
dim(koml_traintest9_fulld)
koml_traintest9_fulld$user_id=koml_traintest9_full$user_id
koml_traintest9_fulld$hero_id=koml_traintest9_full$hero_id
koml_traintest9_fulld$roles=koml_traintest9_full$roles

#koml_train1_full= read.csv('koml_validation.csv')
#koml_train1_full$user_id= as.factor(as.character(koml_train1_full$user_id))
#koml_train1_full$hero_id= as.factor(as.character(koml_train1_full$hero_id))
setdiff(levels(koml_traintest9_full$roles), levels(koml_train1_full$roles))
levels(koml_train1_full$roles)= c(levels(koml_train1_full$roles),"Carry:Pusher:Durable:Disabler:Initiator:Nuker")
levels(koml_train1_full$roles)= c(levels(koml_train1_full$roles),"Nuker:Pusher")
levels(koml_train1_full$roles)= c(levels(koml_train1_full$roles),"Support:Initiator:Disabler")

koml_train1_full1= koml_train1_full[-c(1,2,3,9)]
dummy1= dummyVars("~.",data = koml_train1_full1)
koml_train1_fulld= data.frame(predict(dummy1, newdata=koml_train1_full1))
dim(koml_train1_fulld)
koml_train1_fulld$user_id=koml_train1_full$user_id
koml_train1_fulld$hero_id=koml_train1_full$hero_id
koml_train1_fulld$roles=koml_train1_full$roles

#koml_test1_full= read.csv('koml_test.csv')
#koml_test1_full$user_id= as.factor(as.character(koml_test1_full$user_id))
#koml_test1_full$hero_id= as.factor(as.character(koml_test1_full$hero_id))
setdiff(levels(koml_traintest9_full$roles), levels(koml_test1_full$roles))
levels(koml_test1_full$roles)= c(levels(koml_test1_full$roles),
c("Carry:Disabler:Durable","Carry:Pusher:Durable:Disabler:Initiator:Nuker",
  "Carry:Support:Pusher:Disabler:Initiator:Escape","Durable:Initiator:Disabler:Nuker:Escape",
  "Initiator:Disabler:Nuker:Durable","Support:Durable:Disabler:Nuker","Support:Jungler:Pusher",
  "Support:Nuker:Disabler:Durable:Escape","Support:Nuker:Disabler:Durable:Initiator"))
koml_test1_full1= koml_test1_full[-c(1,2,3,7)]
dummy1= dummyVars("~.",data = koml_test1_full1)
koml_test1_fulld= data.frame(predict(dummy1, newdata=koml_test1_full1))
dim(koml_test1_fulld)
koml_test1_fulld$user_id=koml_test1_full$user_id
koml_test1_fulld$hero_id=koml_test1_full$hero_id
koml_test1_fulld$roles=koml_test1_full$roles

#koml_traintest9_full= read.csv('koml_train.csv')
#koml_train1_full= read.csv('koml_validation.csv')
#koml_test1_full= read.csv('koml_test.csv')
koml_traintest9_full$user_id=as.factor(as.character(koml_traintest9_full$user_id))
koml_traintest9_full$hero_id=as.factor(as.character(koml_traintest9_full$hero_id))

koml_train1_full$user_id=as.factor(as.character(koml_train1_full$user_id))
koml_train1_full$hero_id=as.factor(as.character(koml_train1_full$hero_id))

koml_test1_full$user_id=as.factor(as.character(koml_test1_full$user_id))
koml_test1_full$hero_id=as.factor(as.character(koml_test1_full$hero_id))

#Transform the columns in the datasets
#base_health column
koml_traintest9_fulld$base_health= koml_traintest9_fulld$base_health + (koml_traintest9_fulld$base_strength*19)
koml_train1_fulld$base_health= koml_train1_fulld$base_health + (koml_train1_fulld$base_strength*19)
koml_test1_fulld$base_health= koml_test1_fulld$base_health + (koml_test1_fulld$base_strength*19)

#base_mana column
koml_traintest9_fulld$base_mana= koml_traintest9_fulld$base_mana + (koml_traintest9_fulld$base_intelligence*13)
koml_train1_fulld$base_mana= koml_train1_fulld$base_mana + (koml_train1_fulld$base_intelligence*13)
koml_test1_fulld$base_mana= koml_test1_fulld$base_mana + (koml_test1_fulld$base_intelligence*13)

#base_mana_regen column
koml_traintest9_fulld$base_mana_regen= koml_traintest9_fulld$base_mana_regen * (1+ koml_traintest9_fulld$base_intelligence*0.02)
koml_train1_fulld$base_mana_regen= koml_train1_fulld$base_mana_regen * (1+ koml_train1_fulld$base_intelligence*0.02)
koml_test1_fulld$base_mana_regen= koml_test1_fulld$base_mana_regen * (1+ koml_test1_fulld$base_intelligence*0.02)

##Add interaction terms
#num_win and num_game interaction
koml_traintest9_fulld$ngnw= koml_traintest9_fulld$num_games*koml_traintest9_fulld$num_wins
koml_train1_fulld$ngnw= koml_train1_fulld$num_games*koml_train1_fulld$num_wins
koml_test1_fulld$ngnw= koml_test1_fulld$num_games*koml_test1_fulld$num_wins

#num_win polynomial interaction
koml_traintest9_fulld$nwpol= koml_traintest9_fulld$num_wins^2
koml_train1_fulld$nwpol= koml_train1_fulld$num_wins^2
koml_test1_fulld$nwpol= koml_test1_fulld$num_wins^2

#num_win polynomial2 interaction
koml_traintest9_fulld$nwpol2= koml_traintest9_fulld$num_wins^3
koml_train1_fulld$nwpol2= koml_train1_fulld$num_wins^3
koml_test1_fulld$nwpol2= koml_test1_fulld$num_wins^3

#num_win and primary attr agi interaction
koml_traintest9_fulld$nwagi= koml_traintest9_fulld$primary_attr.agi*koml_traintest9_fulld$num_wins
koml_train1_fulld$nwagi= koml_train1_fulld$primary_attr.agi*koml_train1_fulld$num_wins
koml_test1_fulld$nwagi= koml_test1_fulld$primary_attr.agi*koml_test1_fulld$num_wins

koml_traintest9_fulld=koml_traintest9_full[-c(31,32,33,34)]
koml_train1_fulld=koml_traintest9_full[-c(31,32,33,34)]
koml_test1_fulld=koml_traintest9_full[-c(30,31,32,33)]

#Create a linear model
koml_lm= lm(kda_ratio~., data = koml_traintest9_fulld)
str(summary(koml_lm))
#qqplot(koml_traintest9_full$kda_ratio,koml_lm$residuals)

#Predict on train1 set
koml_lm_pred= predict(koml_lm, newdata = koml_train1_fulld)
Smy_RMSE(koml_lm_pred)

#Predict column num_wins in test1 set
koml_lm1= lm(num_wins~., data = koml_traintest9_fulld[-c(3)])
str(summary(koml_lm1))
koml_lm1_pred= predict(koml_lm1, newdata = koml_train1_fulld[-c(3)])
Smy_RMSE(koml_lm1_pred)

koml_test1_fulld$num_wins= predict(koml_lm1, newdata = koml_test1_fulld)
sd(koml_test1_fulld$num_wins)

#Do the final Prediction on test1 set
koml_lm_pred1= predict(koml_lm, newdata = koml_test1_fulld)
sd(koml_lm_pred1)

output_xls(koml_lm_pred1)

#Write the preprocessed files to csv
write.csv(koml_traintest9_full,'koml_train.csv',row.names = F)
write.csv(koml_train1_full,'koml_validation.csv',row.names = F)
write.csv(koml_test1_full,'koml_test.csv',row.names = F)

write.csv(koml_traintest9_fulld,'koml_traind1.csv',row.names = F)
write.csv(koml_train1_fulld,'koml_validationd1.csv',row.names = F)
write.csv(koml_test1_fulld,'koml_testd1.csv',row.names = F)

#---------------------------------------------------------------------------#
##Move into deep learning
###Load H2o
library(h2o)
## Create an H2O cloud 
h2o.init(nthreads=-1,            ## -1: use all available threads
         max_mem_size = "2G")    ## specify the memory size for the H2O cloud
h2o.removeAll()

#Load a file from disk
koml_train_h2o= h2o.importFile(path = normalizePath("C:/Users/SUBHRANIL ROY/Desktop/Machine Learning books/ML Data Sets/AV Practice datasets/AV Hackathon Practice/IIIT Hackathon - Kings of Machine Learning/koml_traind1.csv"))
koml_validation_h2o= h2o.importFile(path = normalizePath("C:/Users/SUBHRANIL ROY/Desktop/Machine Learning books/ML Data Sets/AV Practice datasets/AV Hackathon Practice/IIIT Hackathon - Kings of Machine Learning/koml_validationd1.csv"))
koml_test_h2o= h2o.importFile(path = normalizePath("C:/Users/SUBHRANIL ROY/Desktop/Machine Learning books/ML Data Sets/AV Practice datasets/AV Hackathon Practice/IIIT Hackathon - Kings of Machine Learning/koml_testd1.csv"))
dim(koml_train_h2o)
dim(koml_validation_h2o)
dim(koml_test_h2o)

#Check within H2o
View(koml_train_h2o)
head(koml_train_h2o)
str(koml_train_h2o)
str(koml_validation_h2o)
str(koml_test_h2o)

##Convert some columns to factors
#Base mana regen
koml_train_h2o$base_mana_regen= as.factor(koml_train_h2o$base_mana_regen)
koml_validation_h2o$base_mana_regen= as.factor(koml_validation_h2o$base_mana_regen)
koml_test_h2o$base_mana_regen= as.factor(koml_test_h2o$base_mana_regen)

#Base_mana
koml_train_h2o$base_mana= as.factor(koml_train_h2o$base_mana)
koml_validation_h2o$base_mana= as.factor(koml_validation_h2o$base_mana)
koml_test_h2o$base_mana= as.factor(koml_test_h2o$base_mana)

#Base_health
koml_train_h2o$base_health= as.factor(koml_train_h2o$base_health)
koml_validation_h2o$base_health= as.factor(koml_validation_h2o$base_health)
koml_test_h2o$base_health= as.factor(koml_test_h2o$base_health)

#Assignment within H2o
koml_train_h2o= koml_train_h2o[-3]
koml_validation_h2o= koml_validation_h2o[-3]
koml_test_h2o= koml_test_h2o[-3]
train1= h2o.assign(koml_train_h2o, "train1.hex")
validation= h2o.assign(koml_validation_h2o,"validation.hex")
test1= h2o.assign(koml_test_h2o, "test1.hex")

nrow.H2OFrame(train1)
nrow.H2OFrame(validation)
nrow.H2OFrame(test1)

#Create Predictor and Response variables
response= 'kda_ratio'
predictors= setdiff(names(koml_train_h2o), response)

response1= 'num_wins'
predictors1= setdiff(names(koml_train_h2o[-5]), response1)

#Try the first model with 1 epoch
m1= h2o.deeplearning(model_id = 'dl_model_first', training_frame = train1,
validation_frame = validation, x= predictors, y= response, activation = 'Rectifier',
hidden = c(20,20), epochs = 1, variable_importances = T)
#summary(m1)
plot(m1)

##Random Search for hyper parameter tuning
hyper_params = list(activation=c('Tanh','Rectifier','Maxout'),
hidden=list(c(25,35,40)),
input_dropout_ratio=seq(0,1,0.01),l1=seq(0,1e-4,1e-6),l2=seq(0,1e-4,1e-6))
#hyper_params

#Describe the search criteria
search_criteria = list(strategy = "RandomDiscrete", max_runtime_secs = 360, 
max_models = 150, seed=1234567, stopping_rounds=3, stopping_tolerance=0.001)

#Define the grid and start the search
dl_random_grid= h2o.grid(grid_id = 'dl_grid_random1', algorithm = 'deeplearning',
training_frame= train1, validation_frame= validation, x=predictors, y=response,
epochs=50, stopping_metric='RMSE', stopping_tolerance=1e-2, stopping_rounds=3,
max_w2=10, hyper_params = hyper_params,search_criteria = search_criteria)

#Display the grid output
grid = h2o.getGrid("dl_grid_random1",sort_by="RMSE",decreasing=FALSE)
grid@summary_table[1,]

#Build a model to predict num_wins for test set
m2= h2o.deeplearning(model_id = 'dl_model_second', training_frame = train1[-5],
validation_frame = validation[-5], x= predictors1, y= response1,
activation = 'Rectifier',hidden = c(25,30), epochs = 100000, 
input_dropout_ratio = 0.0, stopping_rounds = 5, stopping_metric = 'RMSE', 
stopping_tolerance = 0.001,l1=8.8e-5,l2=2.7e-5)
#summary(m2)
plot(m2)
head(as.data.frame(h2o.varimp(m2)))
y5=h2o.performance(m2, newdata = validation)
y5@metrics$RMSE
p1=h2o.predict(m2, newdata = validation)
Smy_RMSE(as.vector(p1$predict))

num_wins1=h2o.predict(m2, newdata = test1)
sd(num_wins1)
test1$num_wins= num_wins1

##Build the model with the best parameters
##This entire model requires multiple iteration execution to obtain the best soln.
set.seed(1234)
m3= h2o.deeplearning(model_id = 'dl_model_third', training_frame = train1,
validation_frame = validation, x= predictors, y= response,activation = 'Rectifier',
hidden = c(50,60,70,80,90), epochs = 100000, input_dropout_ratio = 0.0, 
stopping_rounds = 5, stopping_metric = 'RMSE', stopping_tolerance = 0.001,
l1=8.8e-5,l2=2.7e-5)
#summary(m3)
plot(m3)
#head(as.data.frame(h2o.varimp(m3)))

#Check performance on validation set and random 40% of the data of validation set
for(i in 1){
  index= sort(sample(2094,2094*0.4), decreasing = F)
  validation_40= validation[index,]
  validation_60= validation[-index,]
  y1=h2o.performance(m3, newdata = validation_40)
  y2=h2o.performance(m3, newdata = validation_60)
  y3=h2o.performance(m3, newdata = validation)
  print(paste(round(y1@metrics$RMSE,2),round(y2@metrics$RMSE,2),round(y3@metrics$RMSE,2)))
}

#Predict using the model
for(i in 1){
  avhh_dlm3_pred=h2o.predict(m3, newdata = test1)
  y4=sd(avhh_dlm3_pred)
  y5=sum(abs(avhh_dlm3_pred- mean(avhh_dlm3_pred)))/nrow.H2OFrame(avhh_dlm3_pred)
  print(paste(round(y4,2),round(y5,2)))
}
head(avhh_dlm3_pred)
x=as.data.frame(avhh_dlm3_pred)
output_xls(x$predict)
































































































































































































































































