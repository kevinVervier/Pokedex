#---------# 
# Pokedex #
#---------#

bar <- load('~/STUDIES/pokedex/pokedex.Rda')

# multiclass ?

# binary ? two types comparison ?

##########
# get some columns out
df$color_1 = NULL
df$color_2 = NULL
df$color_f = NULL
df$conquest_order = NULL
df$species_id = NULL
df$pokemon.x = NULL
df$pokemon.y = NULL
df$url_image = NULL
df$evolution_chain_id = NULL
df$evolution = as.numeric(!is.na(df$evolves_from_species_id))
df$evolves_from_species_id = NULL
df$egg_group_2=NULL
df$habitat_id = NULL
df$forms_switchable = NULL

df$order = NULL
df$habitat_name = NULL
df$shape_name = NULL
df$color_name = NULL

df$egg_group_1 = as.numeric(as.factor(df$egg_group_1))

###################
# create duplicate entries for double types
tmp.df = df
tmp.df$type_1 = tmp.df$type_2
tmp.df$type_2 = NULL
tmp.df = tmp.df[-which(is.na(tmp.df$type_1)),]

df$type_2 = NULL

df = rbind(df,tmp.df)

type_id = cbind(levels(factor(df$type_1)),1:nlevels(factor(df$type_1)))


df$type_1 = as.numeric(factor(df$type_1))



########################################################
# get pokemon from Sun and Moon as a test set (Generation 7)

test.df = df[which(df$generation_id == 7),]
test.df$generation_id = NULL

labels.test = test.df$type_1
test.df$type_1 = NULL

#######################################################
# split generations 1-6 in a train and a validation set
# by keeping in the same set duplicate entries for double types

tmp = df[-which(df$generation_id == 7),]
set.seed(42)

idx = tapply(X=tmp$id,INDEX = tmp$type_1,function(x) sample(x,replace = FALSE,size = round(length(x)*0.2)))

train.df = df[-which(df$id %in% unlist(idx)),]

train.df = train.df[-which(train.df$id > 800),]
train.df$generation_id = NULL # 867

val.df = df[which(df$id %in% unlist(idx)),]

val.df = val.df[-which(val.df$id > 800),]
val.df$generation_id = NULL #333 pokemon

save(train.df,val.df,type_id,file='processed_data.Rdata')
################
# accuracy measure in multiclass with double type entries


##  multi-class logarithmic loss calculation function for local validation: 
multi_class_log_loss <- function( testSetId, output, target) {
  # testSet - the list of Ids that are evaluated (length = N)
  # output - resulted data frame having predicted values for each of class (dimensions: N by n_classes)
  # target - the actual class label
  
  log_sum <- 0
  N = length(testSetId)
  
  for( i in 1:N ) {	
    idx = which(testSetId == testSetId[i]) # cases of double types
    
    curPred = max(output[i, target[idx]])
    if(curPred == 0) curPred = 0.00000000001
    log_sum <- log_sum + log( curPred )
  }
  mclass_log_loss = -1 * log_sum/N
  
  #return
  mclass_log_loss
}


## Example Call:
## predict with the model(mymodel) and validation data frame(test) 
# test_out <- as.data.frame(predict(mymodel, test, "probs"))

## Perform local validation/calculating  multi-class logarithmic loss
# multi_class_log_loss( test, test_out, "Category")


###############################################
# first eval on RF
library(randomForest)

X = train.df
X$id = NULL
X$type_1 = NULL

Y=as.factor(train.df$type_1)
set.seed(42)
m = randomForest(x=X,y=Y,ntree = 10000,mtry=ncol(X),replace = TRUE)

testSetId = train.df$id
target=Y
output=m$votes

multi_class_log_loss( testSetId, output, target) # 0.78 on train (OOB)

X.val = val.df
X.val$id = NULL
X.val$type_1 = NULL

Y.val=as.factor(val.df$type_1)
testSetId = val.df$id
target=Y.val
output=predict(m,newdata = X.val,type = 'vote')
multi_class_log_loss( testSetId, output, target) # 1.4 on validation (OOB)


##########################
# LibLineaR
library(LiblineaR)

X = train.df
X$id = NULL
X$type_1 = NULL

Y=as.factor(train.df$type_1)

m.svm = LiblineaR(data=X,target=Y,type=0) # type =0, 6 or 7 tp get probabilities
output = predict(m.svm,newx = X,proba = TRUE)$probabilities
testSetId = train.df$id
target=Y

multi_class_log_loss( testSetId, output, target) # 3.3 on train

output = predict(m.svm,newx = X.val,proba = TRUE)$probabilities
testSetId = val.df$id
target=Y.val

multi_class_log_loss( testSetId, output, target) # 3.1 on train


############################################################################
# Water and Fire pokemon

bar <- load('~/STUDIES/pokedex/pokedex.Rda')

# multiclass ?

# binary ? two types comparison ?

##########
# get some columns out
df$color_1 = NULL
df$color_2 = NULL
df$color_f = NULL
df$conquest_order = NULL
df$species_id = NULL
df$pokemon.x = NULL
df$pokemon.y = NULL
df$url_image = NULL
df$evolution_chain_id = NULL
df$evolution = as.numeric(!is.na(df$evolves_from_species_id))
df$evolves_from_species_id = NULL
df$egg_group_2=NULL
df$habitat_id = NULL
df$forms_switchable = NULL

df$order = NULL
df$habitat_name = NULL
df$shape_name = NULL
df$color_name = NULL

df$egg_group_1 = as.numeric(as.factor(df$egg_group_1))

###################
# create duplicate entries for double types
tmp.df = df
tmp.df$type_1 = tmp.df$type_2
tmp.df$type_2 = NULL
tmp.df = tmp.df[-which(is.na(tmp.df$type_1)),]

df$type_2 = NULL

df = rbind(df,tmp.df)

df = df[which(df$type_1 %in% c('water','fire')),]

df$type_1 = factor(df$type_1)

########################################################
# get pokemon from Sun and Moon as a test set (Generation 7)

test.df = df[which(df$generation_id == 7),]
test.df$generation_id = NULL

labels.test = test.df$type_1
test.df$type_1 = NULL

#######################################################
# split generations 1-6 in a train and a validation set
# by keeping in the same set duplicate entries for double types

tmp = df[-which(df$generation_id == 7),]
set.seed(42)

idx = tapply(X=tmp$id,INDEX = tmp$type_1,function(x) sample(x,replace = FALSE,size = round(length(x)*0.2)))

train.df = df[-which(df$id %in% unlist(idx)),]

train.df = train.df[-which(train.df$id > 800),]
train.df$generation_id = NULL # 867

val.df = df[which(df$id %in% unlist(idx)),]

val.df = val.df[-which(val.df$id > 800),]
val.df$generation_id = NULL #333 pokemon

save(train.df,val.df,type_id,file='processed_data_water_fire.Rdata')
