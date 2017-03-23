source("../code/data.R")

ntrain = nrow(x_train)
train_test = rbind(x_train, x_test)


features = names(x_train)
features
# Group Reduction
# Variables profiles
x_train.whatis <- whatis(x_train)

# Binning
#age : cut into levels Young (0-25), Middle-aged (26-45), Senior (46-65) and Old (66+).
#hours-per-week :cut into levels Part-time (0-25), Full-time (25-40), Over-time (40-60) and Too-much (60+).
#capital-gain and capital-loss : each cut into levels None (0), Low (0 < median of the values greater zero < max) and High (>=max).

#AdultUCI[[ "age"]] <- ordered(cut(AdultUCI[[ "age"]], c(15,25,45,65,100)),labels = c("Young", "Middle-aged", "Senior", "Old"))
#AdultUCI[[ "hours-per-week"]] <- ordered(cut(AdultUCI[[ "hours-per-week"]], c(0,25,40,60,168)),labels = c("Part-time", "Full-time", "Over-time", "Workaholic"))
#AdultUCI[[ "capital-gain"]] <- ordered(cut(AdultUCI[[ "capital-gain"]], c(-Inf,0,median(AdultUCI[[ "capital-gain"]][AdultUCI[[ "capital-gain"]]>0]),Inf)), labels = c("None", "Low", "High"))
#AdultUCI[[ "capital-loss"]] <- ordered(cut(AdultUCI[[ "capital-loss"]],c(-Inf,0, median(AdultUCI[[ "capital-loss"]][AdultUCI[[ "capital-loss"]]>0]),Inf)), labels = c("None", "Low", "High"))


table(x_train$type_employer, useNA = "always")
# Given "Never worked" and "Without-Pay" are both very small groups, and they are
# likely very similar, we can combine them to form a "Not Working" Category.
# In a similar vein, we can combine government employee categories, and self-employed
# categories. This allows us to reduce the number of categories significantly.


train_test$type_employer = gsub("^Federal-gov","Federal-Govt",train_test$type_employer)
train_test$type_employer = gsub("^Local-gov","Other-Govt",train_test$type_employer)
train_test$type_employer = gsub("^State-gov","Other-Govt",train_test$type_employer)
train_test$type_employer = gsub("^Private","Private",train_test$type_employer)
train_test$type_employer = gsub("^Self-emp-inc","Self-Employed",train_test$type_employer)
train_test$type_employer = gsub("^Self-emp-not-inc","Self-Employed",train_test$type_employer)
train_test$type_employer = gsub("^Without-pay","Not-Working",train_test$type_employer)
train_test$type_employer = gsub("^Never-worked","Not-Working",train_test$type_employer)


table(x_train$occupation, useNA = "always")
# On occupation, a simple way to block the categories would include blue collar versus white
# collar. Separate out service industry, and other occupations that are not fitting well with
# the other groups into their own group. It's unfortunate that Armed Forces won't fit well
# with any of the other groups. In order to get it properly represented, we can try up-sampling
# it when we train the model.
train_test$occupation = gsub("^Adm-clerical","Admin",train_test$occupation)
train_test$occupation = gsub("^Armed-Forces","Military",train_test$occupation)
train_test$occupation = gsub("^Craft-repair","Blue-Collar",train_test$occupation)
train_test$occupation = gsub("^Exec-managerial","White-Collar",train_test$occupation)
train_test$occupation = gsub("^Farming-fishing","Blue-Collar",train_test$occupation)
train_test$occupation = gsub("^Handlers-cleaners","Blue-Collar",train_test$occupation)
train_test$occupation = gsub("^Machine-op-inspct","Blue-Collar",train_test$occupation)
train_test$occupation = gsub("^Other-service","Service",train_test$occupation)
train_test$occupation = gsub("^Priv-house-serv","Service",train_test$occupation)
train_test$occupation = gsub("^Prof-specialty","Professional",train_test$occupation)
train_test$occupation = gsub("^Protective-serv","Other-Occupations",train_test$occupation)
train_test$occupation = gsub("^Sales","Sales",train_test$occupation)
train_test$occupation = gsub("^Tech-support","Other-Occupations",train_test$occupation)
train_test$occupation = gsub("^Transport-moving","Blue-Collar",train_test$occupation)

table(x_train$country, useNA = "always")
# The variable country presents a small problem. Obviously the United States
# represents the vast majority of observations, but some of the groups have
# such small numbers that their contributions might not be significant. A way
# around this would be to block the countries.
# Use a combination of geographical location, political organization, and economic
# zones. Euro_1 is countries within the Eurozone that are considered more affluent,
# and therefore people from there are probably going to be more affluent. Euro_2
# includes countries within the Eurozone that are considered less affluent. These
# included countries that are financially troubled like Spain and Portugal, but also
# the Slavic countries and those formerly influenced by the USSR like Poland. Formerly
# British holdings that are still closely economically aligned with Britain are included
# under the British-Commonwealth.
train_test$country[train_test$country=="Cambodia"] = "SE-Asia"
train_test$country[train_test$country=="Canada"] = "British-Commonwealth"  
train_test$country[train_test$country=="China"] = "China"   
train_test$country[train_test$country=="Columbia"] = "South-America"  
train_test$country[train_test$country=="Cuba"] = "Other"    
train_test$country[train_test$country=="Dominican-Republic"] = "Latin-America"
train_test$country[train_test$country=="Ecuador"] = "South-America"  
train_test$country[train_test$country=="El-Salvador"] = "South-America"
train_test$country[train_test$country=="England"] = "British-Commonwealth"
train_test$country[train_test$country=="France"] = "Euro_1"
train_test$country[train_test$country=="Germany"] = "Euro_1"
train_test$country[train_test$country=="Greece"] = "Euro_2"
train_test$country[train_test$country=="Guatemala"] = "Latin-America"
train_test$country[train_test$country=="Haiti"] = "Latin-America"
train_test$country[train_test$country=="Holand-Netherlands"] = "Euro_1"
train_test$country[train_test$country=="Honduras"] = "Latin-America"
train_test$country[train_test$country=="Hong"] = "China"
train_test$country[train_test$country=="Hungary"] = "Euro_2"
train_test$country[train_test$country=="India"] = "British-Commonwealth"
train_test$country[train_test$country=="Iran"] = "Other"
train_test$country[train_test$country=="Ireland"] = "British-Commonwealth"
train_test$country[train_test$country=="Italy"] = "Euro_1"
train_test$country[train_test$country=="Jamaica"] = "Latin-America"
train_test$country[train_test$country=="Japan"] = "Other"
train_test$country[train_test$country=="Laos"] = "SE-Asia"
train_test$country[train_test$country=="Mexico"] = "Latin-America"
train_test$country[train_test$country=="Nicaragua"] = "Latin-America"
train_test$country[train_test$country=="Outlying-US(Guam-USVI-etc)"] = "Latin-America"
train_test$country[train_test$country=="Peru"] = "South-America"
train_test$country[train_test$country=="Philippines"] = "SE-Asia"
train_test$country[train_test$country=="Poland"] = "Euro_2"
train_test$country[train_test$country=="Portugal"] = "Euro_2"
train_test$country[train_test$country=="Puerto-Rico"] = "Latin-America"
train_test$country[train_test$country=="Scotland"] = "British-Commonwealth"
train_test$country[train_test$country=="South"] = "Euro_2"
train_test$country[train_test$country=="Taiwan"] = "China"
train_test$country[train_test$country=="Thailand"] = "SE-Asia"
train_test$country[train_test$country=="Trinadad&Tobago"] = "Latin-America"
train_test$country[train_test$country=="United-States"] = "United-States"
train_test$country[train_test$country=="Vietnam"] = "SE-Asia"
train_test$country[train_test$country=="Yugoslavia"] = "Euro_2"


table(x_train$education, useNA = "always")
# Block all the dropouts together. Block high school graduates and those that
# attended some college without receiving a degree as another group. Those college
# graduates who receive an associates are blocked together regardless of type of
# associates. Those who graduated college with a Bachelors, and those who went on to
# graduate school without receiving a degree are blocked as another group. Most
# everything thereafter is separated into its own group.
train_test$education = gsub("^10th","Dropout",train_test$education)
train_test$education = gsub("^11th","Dropout",train_test$education)
train_test$education = gsub("^12th","Dropout",train_test$education)
train_test$education = gsub("^1st-4th","Dropout",train_test$education)
train_test$education = gsub("^5th-6th","Dropout",train_test$education)
train_test$education = gsub("^7th-8th","Dropout",train_test$education)
train_test$education = gsub("^9th","Dropout",train_test$education)
train_test$education = gsub("^Assoc-acdm","Associates",train_test$education)
train_test$education = gsub("^Assoc-voc","Associates",train_test$education)
train_test$education = gsub("^Bachelors","Bachelors",train_test$education)
train_test$education = gsub("^Doctorate","Doctorate",train_test$education)
train_test$education = gsub("^HS-Grad","HS-Graduate",train_test$education)
train_test$education = gsub("^Masters","Masters",train_test$education)
train_test$education = gsub("^Preschool","Dropout",train_test$education)
train_test$education = gsub("^Prof-school","Prof-School",train_test$education)
train_test$education = gsub("^Some-college","HS-Graduate",train_test$education)

# Similarly
train_test$marital[train_test$marital=="Never-married"] = "Never-Married"
train_test$marital[train_test$marital=="Married-AF-spouse"] = "Married"
train_test$marital[train_test$marital=="Married-civ-spouse"] = "Married"
train_test$marital[train_test$marital=="Married-spouse-absent"] = "Not-Married"
train_test$marital[train_test$marital=="Separated"] = "Not-Married"
train_test$marital[train_test$marital=="Divorced"] = "Not-Married"
train_test$marital[train_test$marital=="Widowed"] = "Widowed"

train_test$race[train_test$race=="White"] = "White"
train_test$race[train_test$race=="Black"] = "Black"
train_test$race[train_test$race=="Amer-Indian-Eskimo"] = "Amer-Indian"
train_test$race[train_test$race=="Asian-Pac-Islander"] = "Asian"
train_test$race[train_test$race=="Other"] = "Other"


######################## Label encoding

for (f in features) {
  if (class(train_test[[f]])=="character") {
    #cat("VARIABLE : ",f,"\n")
    levels <- unique(train_test[[f]])
    train_test[[f]] <- as.numeric(as.integer(factor(train_test[[f]], levels=levels)))
  }
}


######################## Fixing Column names and splitting dataset back

x_train = train_test[1:ntrain, ]
x_test = train_test[(ntrain+1):nrow(train_test), ]


######################## Missing Value Treatment
# is.na(data) = data=='?'
# is.na(data) = data==' ?'
# data = na.omit(data)

# Missing values are identified with '?'
# Can't do it here after factor to numeric
#idx_train <- Matrix::rowSums(is.na(x_train))
#table(idx_train)
#x_train <- x_train[idx_train==0, ]
#y_train <- y_train[idx_train==0]

#idx_test <- Matrix::rowSums(is.na(x_test))
#table(idx_test)
#x_test <- x_test[idx_test==0, ]
#y_test <- y_test[idx_test==0]

y_train <- ifelse(y_train==">50K", 1, 0)
y_test <- ifelse(y_test==">50K", 1, 0)

######################## Preparing for xgboost
dtrain = xgb.DMatrix(as.matrix(x_train), label=y_train)
dtest = xgb.DMatrix(as.matrix(x_test))

rm(train_test, features)
#rm(x_test.whatis, x_train.whatis)
#rm(idx_test, idx_train)

save(x_train, x_test, y_train, y_test, file="../data/modelready_data.rda")
save(dtrain, dtest, file="../data/modelready_data_xgboost.rda")
print("Feature Engineering complete.")

