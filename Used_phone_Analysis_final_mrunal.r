######installing Packages##############
install.packages("ggplot2")
install.packages("forecast")
install.packages("readxl")
install.packages("zoo")
install.packages("VIM")
install.packages("cowplot") ##for cowplot grid plot multiple plots
install.packages("fastDummies")
install.packages("rpart")#for decision tree
install.packages("rpart.plot")
install.packages("randomForest")
install.packages("caret")
install.packages("fclust")
install.packages("cluster")
install.packages("stats")#Kmeans
install.packages("psych")
install.packages("cowplot")
install.packages("C50")



#####loading libraries#############
library(e1071)
library(stats)
library(cluster)
library(fclust)
library(caret)
library(randomForest)
library(rpart.plot)
library(rpart)
library(arules)
library(ggplot2)
library(forecast)
library(corrplot)
library(zoo)
library(cowplot)
library(VIM)
library(fastDummies)# for dummies creation
library(psych)
library(C50)
##############loading dataset############

phone_data <- read.csv("~/used_device_data.csv")
##############checking data type
str(phone_data)
######printing top of data#####
head_phone_data<-head(phone_data)
print(head_phone_data)
###summary statistics
summary(phone_data)

###############checking null values 
sum(is.na(phone_data))
########Visualization of null values######################

missing<- colMeans(is.na(phone_data))


missing_sorted <- names(missing[order(missing, decreasing = TRUE)])

aggr(phone_data[,missing_sorted],prop=FALSE,numbers=TRUE)
par(mfrow = c(1, 1))
########################Removing NA values #######################


phone_data<-kNN(phone_data,imp_var=FALSE)
sum(is.na(phone_data))

 
aggr(phone_data,prop=FALSE,numbers=TRUE)

sum(is.na(phone_data))

##########################check for zeros###########################


zero_counts <- colSums(phone_data == 0, na.rm = TRUE)
zero_counts


######################### Exploratory Data Analysis###############

#########character columns###########

#barplot for 4g phones
barplot(table(phone_data$X4g), main = "Distribution of 4g phones", ylab = "Frequency", col = "lightgreen")
#barplot for 5g phones
barplot(table(phone_data$X5g), main = "Distribution of 5g phones", ylab = "Frequency", col = "lightgreen")

#pie graph for specification OS
pie(table(phone_data$os), main = "OS Distribution", col = c("lightblue", "red", "lightgreen", "orange"))

#############Device brand###########
brand_table <- sort(table(phone_data$device_brand), decreasing = TRUE)

top_brands <- names(brand_table)[1:15]

# Subset of top 15 brands
brand_subset <- phone_data[phone_data$device_brand %in% top_brands, ]

barplot(sort(table(brand_subset$device_brand), decreasing = TRUE), 
        main = "Top 15 Phone Brands by Distribution", 
        ylab = "Frequency", 
        col = "lightgreen")

#barplot(sort(table(phone_data$device_brand),decreasing = TRUE), main = "phones distribution  by brand", ylab = "Frequency", col = "lightgreen")





#######numeric columns#######################

#new price and old price distribiution and year distribution

hist(phone_data$normalized_new_price,main="distribution of normalized_new_price",col="palegreen")
hist(phone_data$normalized_used_price,main="distribution of normalized_used_price",col="palegreen")

hist(phone_data$release_year,main="distribution by year",col="palegreen")
####################################numerical column###########################################

#############################################Numeric data##############################
numeric_vars <- c("screen_size", "rear_camera_mp", "front_camera_mp", "internal_memory", "ram", "battery", "weight", "days_used")

#histograms
#bar plots
par(mfrow = c(3, 3),mar = c(4, 4, 2, 1))

hist(phone_data$screen_size,main="distribution of screen size",col="palegreen")
hist(phone_data$rear_camera_mp,main="distribution of rear_camera_mp",col="palegreen")
hist(phone_data$front_camera_mp,main="distribution of front_camera_mp",col="palegreen")
hist(phone_data$internal_memory,main="distribution of internal_memory",col="palegreen")
hist(phone_data$ram,main="distribution of ram",col="palegreen")
hist(phone_data$battery,main="distribution of battery size",col="palegreen")
hist(phone_data$weight,main="distribution of weight",col="palegreen")
hist(phone_data$days_used,main = "distribution of days used",col="palegreen")
par(mfrow = c(1, 1))


###box plots
par(mfrow = c(3, 3))  

for (feature in numeric_vars) {

  boxplot(phone_data[[feature]], main = feature)
}

par(mfrow = c(1, 1))  


################################################Attribute VS Target##########################
# normalized_used_price

#######Rear_camera VS normalized_used_price
ggplot(phone_data, aes(y = normalized_used_price, x =rear_camera_mp)) +
  geom_point(color = "palegreen") +
  labs(title = "Scatter Plot of Used price vs rear camera'",
       x= "rear camera",
       y = "Used Price")

#######front_camera VS normalized_used_price
ggplot(phone_data, aes(y = normalized_used_price, x =front_camera_mp)) +
  geom_point(color = "palegreen") +
  labs(title = "Scatter Plot of Front camera vs. front_camera_mp",
       y= "Used Price",
       x = "Front camera")

#######ram VS normalized_used_price
ggplot(phone_data, aes(y = normalized_used_price, x =ram)) +
  geom_point(color = "palegreen") +
  labs(title = "Scatter Plot of Used Price vs. ram",
       y= "Used Price",
       x = "ram")

#######release_year VS normalized_used_price
ggplot(phone_data, aes(y = normalized_used_price, x =release_year)) +
  geom_line(color = "palegreen") +
  labs(title = "Scatter Plot of Used Price vs. release year",
       y= "used Price",
       x = "release year")

#######screen_size VS normalized_used_price
ggplot(phone_data, aes(y = normalized_used_price, x =screen_size)) +
  geom_point(color = "palegreen") +
  labs(title = "Scatter Plot of Used Price vs. screen_size",
       y= "used Price",
       x = "Screen size")

#######battery VS normalized_used_price
ggplot(phone_data, aes(y = normalized_used_price, x =battery)) +
  geom_point(color = "palegreen") +
  labs(title = "Scatter Plot of Used Price vs. battery",
       y= "used Price",
       x = "battery")

#######weight VS normalized_used_price
ggplot(phone_data, aes(y = normalized_used_price, x =weight)) +
  geom_point(color = "palegreen") +
  labs(title = "Scatter Plot of Used Price vs. weight",
       y= "used Price",
       x = "weight")

#######internal_memory VS normalized_used_price
ggplot(phone_data, aes(y = normalized_used_price, x =internal_memory)) +
  geom_point(color = "palegreen") +
  labs(title = "Scatter Plot of Used Price vs. internal memory ",
       y= "used Price",
       x = "Internal menory")

#######days_used VS normalized_used_price
ggplot(phone_data, aes(y = normalized_used_price, x =days_used)) +
  geom_point(color = "palegreen") +
  labs(title = "Scatter Plot of Used Price vs. days used",
       y= "used Price",
       x = "days used")


######################Brand avg price 
brand_avg_price <- aggregate(normalized_used_price ~ device_brand, data = phone_data, FUN = mean)

#####################Device brand vs noramlized_used_price
ggplot(brand_avg_price, aes(x = device_brand, y = normalized_used_price)) +
  geom_bar(stat = "identity", fill = "Palegreen") +
  labs(title = "Average Price by Brand", x = "Brand", y = "Average Price")
#####################Days used avg used_price

ggplot(phone_data, aes(x = days_used, y = normalized_used_price)) +
  geom_point(color = "Palegreen") +
  labs(title = "Average Price by Days Used", x = "Days Used", y = "Average Price")
#####################Year with avg used_price 

ggplot(phone_data, aes(x = as.factor(release_year), y = normalized_used_price)) +
  geom_boxplot(fill = "palegreen") +
  labs(title = "Average Price by Year", x = "Year", y = "Average Price")

############################################Transformation############################

############Data Transformations for Prediction###################


phone_data$X4g <- as.factor(phone_data$X4g)
phone_data$X5g <- as.factor(phone_data$X5g)

#################################################correlation matrix#######################
phones_correlation_matrix <- cor(phone_data[, !(names(phone_data) %in% c('X4g', 'X5g', 'os', 'device_brand'))], use = "complete.obs")

corrplot(phones_correlation_matrix, method = "color", type = "upper", addCoef.col = "black",tl.col = "black", tl.srt = 45)


pairs.panels(phones_correlation_matrix[, c("screen_size", "rear_camera_mp", "front_camera_mp", "internal_memory", "ram", "battery", "weight", "release_year", "days_used","normalized_used_price","normalized_new_price")])



################## Data partitoning for Prediction##############
#


set.seed(140)
training_index <- sample(rownames(phone_data), size = nrow(phone_data) * 0.5)

validation_index <- sample(setdiff(rownames(phone_data), training_index), size = nrow(phone_data) * 0.3)




phone_training_data<-phone_data[training_index,]
phone_validation_data<-phone_data[validation_index,]
holdout_data<-phone_data[setdiff(row.names(phone_data),union(training_index,validation_index)),]

dim(phone_training_data)
dim(phone_validation_data)
dim(holdout_data)
summary(phone_training_data$normalized_used_price)

summary(phone_validation_data$normalized_used_price)
summary(holdout_data$normalized_used_price)


phone_training_data
############################Linear Regression################
lm_model <- lm(normalized_used_price ~ .-c(normalized_used_price)-normalized_new_price, data = phone_training_data)
summary(lm_model)

prediction<-predict(lm_model,phone_validation_data)

accuracy(prediction,phone_validation_data$normalized_used_price)
cor(prediction,phone_validation_data$normalized_used_price)

########################decision tree#########################

tree_model <- rpart(normalized_used_price ~ .-c(normalized_used_price)-normalized_new_price, data = phone_training_data)
tree_model
rpart.plot(tree_model)

treepredictions <- predict(tree_model, newdata = phone_validation_data)
accuracy(treepredictions,phone_validation_data$normalized_used_price)
cor(treepredictions,phone_validation_data$normalized_used_price)




###################################### random forest model
rf_model <- randomForest(normalized_used_price ~ .-c(normalized_used_price)-normalized_new_price, data = phone_training_data)
summary(rf_model)
rf_model

#variable importance
varImpPlot(rf_model)

prediction_Rf <- predict(rf_model, newdata = phone_validation_data)
accuracy(prediction_Rf,phone_validation_data$normalized_used_price)
cor(prediction_Rf,phone_validation_data$normalized_used_price)



################################################Validation for holdout dataset#########

prediction_hold_Rf <- predict(rf_model, newdata = holdout_data)

cor(prediction_hold_Rf,holdout_data$normalized_used_price)
accuracy(prediction_hold_Rf,holdout_data$normalized_used_price)


####### prediction Business usecase  ######

#above prediction model can help our company to buy the phones in market below our predicted price then sell it to our customer
#which generate revenue stream in a long term and also develop an app or website for an customer
#who is willing to sell their phone to us or wants to check their price that could fetch if they want to sell it 

################################# Clustering##########

########Second transformation of data #####

cl_phone<-phone_data
print(colSums(is.na(cl_phone)))


# Removing if any NA's
cl_phone<-kNN(cl_phone,imp_var=FALSE)
sum(is.na(cl_phone))



################################creating dummies ###############

dummied_data_cluster <- dummy_cols(cl_phone, select_columns = c("os","X4g","X5g"), remove_first_dummy = TRUE)
dummied_data_cluster <- dummied_data_cluster[, !names(dummied_data_cluster) %in% c("device_brand",'os','X4g','X5g')]

sum(is.na(dummied_data_cluster))
str(dummied_data_cluster)
colMeans(dummied_data_cluster, na.rm = TRUE)

scaled_cl_data<- as.data.frame(lapply(dummied_data_cluster, scale))
head(scaled_cl_data)

#####################KMEANS clustering######

###seed 2,177 0.32#############high

################ K-means##################

#Improving clusters performance 
set.seed(140)
dis = dist(scaled_cl_data)
wss <- (nrow(scaled_cl_data)-1)*mean(apply(scaled_cl_data,2,var))

for (i in 1:10) wss[i] <- mean(kmeans(scaled_cl_data, centers=i)$withinss)
plot(1:10, wss, type="b", xlab="Number of Clusters (k)",
     ylab="Average Within−Cluster Squared Distance")



########MAIN CLUSTERING
#7   5-0.24  676 
set.seed(140)
#model
phone_clusters<-kmeans(scaled_cl_data,4)

phone_clusters$size
phone_clusters$centers
###############################visualization for interpretation clusters################
#cluster properties
myColors <- c("darkblue", "red", "green", "brown")
barplot(phone_clusters$centers, beside = TRUE, xlab="Features", ylab="value", col = myColors)
legend("top", ncol=2, legend = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4"), fill = myColors)


require(cluster)
distance = dist(scaled_cl_data)
silhouete_eval = silhouette(phone_clusters$cluster, distance)
silhouete_eval
plot(silhouete_eval, col=c(1:length(phone_clusters$size)))
factoextra::fviz_silhouette(silhouete_eval,label = TRUE,palette="jco",ggtheme=theme_classic())

clustered_set<-cbind(phone_data,cluster=phone_clusters$cluster)
head(clustered_set)


# this can help company to achieve its target to group phones also by model
#we selected best k but after having discussions with tthe business owner
#we can try group the phone into different number of groups




####################################################classification models#########################



 


################## Data partitoning for Classification##############
#partitioning with 50-30-20 split
set.seed(140)


training_i <- sample(rownames(clustered_set), size = nrow(clustered_set) * 0.5)

validation_i <- sample(setdiff(rownames(clustered_set), training_i), size = nrow(clustered_set) * 0.3)



cl_train<-clustered_set[training_i,]
cl_valid<-clustered_set[validation_i,]
cl_holdout<-clustered_set[setdiff(row.names(clustered_set),union(training_i,validation_i)),]

head(cl_train)

dim(cl_train)
dim(cl_valid)
dim(cl_holdout)

prop.table(table(cl_train$cluster))*100
prop.table(table(cl_valid$cluster))*100
prop.table(table(cl_holdout$cluster))*100


######################target class Cluster
####this model is created incase of company purchasing
# used mobile phones from the customers and classify them to cluster which it belongs to based on same features it was grouped.


cl_train$cluster <- as.factor(cl_train$cluster)
cl_valid$cluster <- as.factor(cl_valid$cluster)
head(cl_train)
######naive Bayes model
nb_model <- naiveBayes(cluster ~ .-cluster, data = cl_train[,-which(names(cl_train) %in% c('device_brand','normalized_used_price','normalized_new_price'))])

print(nb_model)

naive_prediction <- predict(nb_model, newdata = cl_valid[])


conf_matr_naive <- confusionMatrix(naive_prediction,as.factor(cl_valid$cluster) )

print(conf_matr_naive)
##########c5.0

c50model_default<-C5.0(cl_train[,-which(names(cl_train) %in% c('cluster','brand','normalized_used_price','normalized_new_price'))],as.factor(cl_train$cluster))
c50model_default
#validation
c50_pred<-predict(c50model_default, newdata = cl_valid[])
conf_matr_c50 <- confusionMatrix(c50_pred,as.factor(cl_valid$cluster) )

print(conf_matr_c50)

###############holdout classification#########

c50_pred_hold<-predict(c50model_default, newdata = cl_valid)
conf_matr_c50_hold <- confusionMatrix(c50_pred_hold,as.factor(cl_valid$cluster), positive = "1" )

print(conf_matr_c50_hold)
