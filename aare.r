if (!require("tidyverse")) install.packages("tidyverse",repos = "http://cran.us.r-project.org") ;
if (!require("pracma")) install.packages("pracma",repos = "http://cran.us.r-project.org"); 
if (!require("Metrics")) install.packages("Metrics",repos = "http://cran.us.r-project.org");
library(tidyverse);
library(pracma);
library(Metrics);

all_data <- read.csv(file="https://raw.githubusercontent.com/simonguenter/aare/master/aare_data.csv",header=TRUE, sep=",");

sum(is.na(all_data)); # 1544

# remove na values: Take the previous value of the feature
for(i in 3600:1) {
  for(j in 1:46) {
    if (is.na(all_data [i,j])) {
      k  = i-1;
      while (is.na(all_data [k,j])) {
        k  = k-1;
      }
      all_data [i,j] = all_data [k,j];
    }
  }
}

sum(is.na(all_data)); # 0

# function to extract the same time range for every day
extract_subvalues <- function(set,offset_start,offset_end) {
  size = dim(set) [1]; 
  from = offset_start;  
  to = offset_end;  
  features =set [from:to,1:46];
  from = from + 144;
  to = to + 144;
  while(from < size) {
    features = rbind(features,set [from:to,1:46]);
    from = from + 144;
    to = to + 144; 
  }
  return(features)
}  


# function to extract the feature values of every day (time range 800-1600)
extract_features <- function(set) { 
  from = 49; # 800
  to = 96; # 1600
  return(extract_subvalues(set,from,to));
} 

# function to extract the values to be predicted of every day (time range 1200-2000)
extract_goals <- function(set) {
  from = 73; # 1200
  to = 120; # 2000
  subvalues = extract_subvalues(set,from,to);
  size = dim(subvalues) [1]; 
  return( subvalues [1:size,2]);
} 


# function to extract the feature values of every day of the specified hour
extract_hour_subfeature <- function(set, hour) { 
  from = (hour+7)*6 + 1;
  to = (hour+8)*6; 
  return (extract_subvalues(set,from,to));
} 

# funtion to extract the values to be predicted of every day of the specified hour
extract_hour_goals <- function(set, hour) { 
  from = (hour+11)*6 + 1;
  to = (hour+12)*6; 
  sub_values = extract_subvalues(set,from,to);
  size = dim(sub_values) [1]; 
  return(sub_values [1:size,2]);
} 

# funtion to extract the feature values of every day of the specified hour.
# Here also the features of the previous 10 minutes are added
extract_hour_subfeature2 <- function(set, hour) { 
  from = (hour+7)*6 + 1;
  to = (hour+8)*6; 
  return(cbind(extract_subvalues(set,from,to),extract_subvalues(set,from-1,to-1)));
} 


 
# funtion to extract the feature values of every day of the specified hour.
# Here also the last vals values of Berns water temperature are added to the feature vector
extract_hour_subfeature4 <- function(set, hour,vals) { 
  from = (hour+7)*6 + 1;
  to = (hour+8)*6; 
  values <-extract_subvalues(set,from,to);
  size = dim(values) [1];
  for(i in 1:vals) {
    values <-cbind(values,extract_subvalues(set,from-i,to-i)[1:size,2]);
  }
  return(values);
} 

#
# training, validation and test of models
#
rsme_avf_diff_test_total=0;
rmse_lm_bern_features_test_total=0;
rsme_avg_diff_hour_base_test_total=0;
rsme_lm_bern_features_hour_base_test_total1=0;
rsme_lm_bern_features_hour_test_total2=0;
rsme_simple_total=  0;
rsme_avf_diff_total = 0;
rmse_lm_all_features_total = 0;
rmse_lm_bern_features_total =0; 
rmse_lm_bern_hydro_features_total = 0;
rsme_avg_diff_hour_base_total =0;
rsme_lm_bern_features_hour_base_total = 0;
rmse_lm_bern_time_features_total =0;
rsme_lm_bern_features_hour_base_double_total=0;
val_total = 0;
rsme_lm_bern_hydro_features_hour_base_total=0;
rsme_lm_bern_hydro_features_hour_base_test_total=0;
val2_total=0;
rsme_lm_bern_hydro_features_hour_test_total=0;

ncval = 100 # number of crossvalidations
set.seed (999);

for(cval in 1 : ncval) {

#spitting into train (for construction model), valid (for optimizing parameters) and test (measurement of performance)

reindex = sample(1:25); # randomly resort indices
for(i in 1:25) {
  subset = all_data [(144*(reindex[i]-1)+1): (144*reindex[i]),1:46];
  if(mod(i,4)==0) {
    if(i>4) {
     test = rbind(test,subset);
    }
    else{
      test = subset;
    }
  }
  if(mod(i,4)==1 || mod(i,4)==3 ) {
    if(i>1) { 
     train = rbind(train,subset);
    }
    else {
      train = subset;
    }
  }
  if(mod(i,4)==2) {
    if(i>4) {
     valid = rbind(valid,subset);
    }
    else {
      valid = subset;
    }
  }
}

# Build correspondence of feature sets (from 800-1600) and goals (from 1200-2000)
train_features <-extract_features(train);
train_goals <-extract_goals(train);
valid_features <-extract_features(valid);
vaild_goals <-extract_goals(valid);
test_features <-extract_features(test);
test_goals <-extract_goals(test);

# Model: Temperatur now= Temperatur in 4 hous
rsme_simple = rmse(valid_features[1:288,2],vaild_goals);
rsme_simple; # 0.3066084
rsme_simple_total = rsme_simple_total + rsme_simple * rsme_simple; 

# Constant difference model
avg_diff_all = mean(train_goals - train_features[1:624,2]);
avg_diff_all; # 0.175609

rsme_avf_diff = rmse(valid_features[1:288,2] + avg_diff_all ,vaild_goals);
rsme_avf_diff; # 0.2488141
rsme_avf_diff_total =rsme_avf_diff_total + rsme_avf_diff * rsme_avf_diff; 

rsme_avf_diff_test = rmse(test_features[1:288,2] + avg_diff_all ,test_goals);
rsme_avf_diff_test_total =  rsme_avf_diff_test_total +  rsme_avf_diff_test  * rsme_avf_diff_test; 

 
# LM Model using all features
lm_train_all_features= lm(train_goals ~., data = train_features);
lm_train_all_features$coefficients;
predictions <- lm_train_all_features %>% predict(valid_features);
rmse_lm_all_features = rmse(predictions, vaild_goals); 
rmse_lm_all_features_total = rmse_lm_all_features_total + rmse_lm_all_features * rmse_lm_all_features; # 0.302058

# LM Model using only features from Location Bern
lm_train_bern_features= lm(train_goals [1:624]  ~., data = train_features [1:624,1:9]); 
predictions <- lm_train_bern_features %>% predict(valid_features [1:288,1:9]);
rmse_lm_bern_features = rmse(predictions, vaild_goals); 
rmse_lm_bern_features_total = rmse_lm_bern_features_total + rmse_lm_bern_features * rmse_lm_bern_features ; # 0.1846454
lm_train_bern_features$coefficients [3]; # 0.8762307

predictions_test <- lm_train_bern_features %>% predict(test_features [1:288,1:9]);
rmse_lm_bern_features_test = rmse(predictions_test, test_goals);
rmse_lm_bern_features_test_total = rmse_lm_bern_features_test_total + rmse_lm_bern_features_test * rmse_lm_bern_features_test;

# LM Model using only hyrdo features from Location Bern
lm_train_bern_hydro_features= lm(train_goals [1:624]  ~., data = train_features [1:624,1:2]); 
predictions <- lm_train_bern_hydro_features %>% predict(valid_features [1:288,1:2]);
rmse_lm_bern_hydro_features = rmse(predictions, vaild_goals);  
rmse_lm_bern_hydro_features_total = rmse_lm_bern_hydro_features_total  + rmse_lm_bern_hydro_features * rmse_lm_bern_hydro_features;
rmse_lm_bern_hydro_features ; #0.2410436

# Hourly Constant difference model
error = 0;
error_test=0;
for(i in 1:8) { 
  sub_train_features<-extract_hour_subfeature(train,i);
  
  sub_train_goals <- extract_hour_goals(train,i);
  avg_diff = mean(sub_train_goals - sub_train_features[1:length(sub_train_goals),2]); 
  
  
  sub_valid_features<-extract_hour_subfeature(valid,i);
  sub_valid_goals <- extract_hour_goals(valid,i);
  rmse_avf_diff = rmse(sub_valid_features [1:length(sub_valid_goals),2] + avg_diff ,sub_valid_goals); 
  error= error + rmse_avf_diff*rmse_avf_diff;
  
  sub_test_features<-extract_hour_subfeature(test,i);
  sub_test_goals <- extract_hour_goals(test,i);
  rmse_avf_diff_test = rmse(sub_test_features [1:length(sub_test_goals),2] + avg_diff ,sub_test_goals);
  error_test= error_test + rmse_avf_diff_test*rmse_avf_diff_test;
   
} 
rsme_avg_diff_hour_base = sqrt(error / 8);
rsme_avg_diff_hour_base_total = rsme_avg_diff_hour_base_total + rsme_avg_diff_hour_base * rsme_avg_diff_hour_base ; # 0.1280144

rsme_avg_diff_hour_base_test = sqrt(error_test / 8);
rsme_avg_diff_hour_base_test_total = rsme_avg_diff_hour_base_test_total + rsme_avg_diff_hour_base_test * rsme_avg_diff_hour_base_test ; #  0.1642959


# Hourly LM Model using only features from Location Bern
error = 0;
error_test = 0;
for(i in 1:8) {
  sub_train_features<-extract_hour_subfeature(train,i);
  
  sub_train_goals <- extract_hour_goals(train,i); 
  lm_train_bern_features= lm(sub_train_goals [1:length(sub_train_goals)]  ~., data = sub_train_features [1:length(sub_train_goals),1:9]); 
  
  sub_valid_features<-extract_hour_subfeature(valid,i);
  sub_valid_goals <- extract_hour_goals(valid,i);
  predictions <- lm_train_bern_features %>% predict(sub_valid_features [1:length(sub_valid_goals),1:9]);
  
  rsme =  rmse(predictions, sub_valid_goals) ;
  error= error + rsme*rsme;
  
  sub_test_features<-extract_hour_subfeature(test,i);
  sub_test_goals <- extract_hour_goals(test,i);
  predictions <- lm_train_bern_features %>% predict(sub_test_features [1:length(sub_test_goals),1:9]);
  
  rsme =  rmse(predictions, sub_test_goals) ;
  error_test= error_test + rsme*rsme;
} 
rsme_lm_bern_features_hour_base = sqrt(error / 8);
rsme_lm_bern_features_hour_base_total = rsme_lm_bern_features_hour_base_total +  rsme_lm_bern_features_hour_base*rsme_lm_bern_features_hour_base; # 0.1158224
rsme_lm_bern_features_hour_base_test = sqrt(error_test / 8);
rsme_lm_bern_features_hour_base_test_total1 = rsme_lm_bern_features_hour_base_test_total1 + rsme_lm_bern_features_hour_base_test * rsme_lm_bern_features_hour_base_test;



# Hourly LM Model using only hydro featrues from Location Bern
error = 0;
error_test = 0;
for(i in 1:8) {
  sub_train_features<-extract_hour_subfeature(train,i);
  
  sub_train_goals <- extract_hour_goals(train,i); 
  lm_train_bern_features= lm(sub_train_goals [1:length(sub_train_goals)]  ~., data = sub_train_features [1:length(sub_train_goals),1:2]); 
  
  sub_valid_features<-extract_hour_subfeature(valid,i);
  sub_valid_goals <- extract_hour_goals(valid,i);
  predictions <- lm_train_bern_features %>% predict(sub_valid_features [1:length(sub_valid_goals),1:2]);
  
  rsme =  rmse(predictions, sub_valid_goals) ;
  error= error + rsme*rsme;
  
  sub_test_features<-extract_hour_subfeature(test,i);
  sub_test_goals <- extract_hour_goals(test,i);
  predictions <- lm_train_bern_features %>% predict(sub_test_features [1:length(sub_test_goals),1:2]);
  
  rsme =  rmse(predictions, sub_test_goals) ;
  error_test= error_test + rsme*rsme;
} 
rsme_lm_bern_hydro_features_hour_base = sqrt(error / 8);
rsme_lm_bern_hydro_features_hour_base_total = rsme_lm_bern_hydro_features_hour_base_total +  rsme_lm_bern_hydro_features_hour_base*rsme_lm_bern_hydro_features_hour_base; # 0.1158224
rsme_lm_bern_hydro_features_hour_base_test = sqrt(error_test / 8);
rsme_lm_bern_hydro_features_hour_base_test_total = rsme_lm_bern_hydro_features_hour_base_test_total + rsme_lm_bern_hydro_features_hour_base_test * rsme_lm_bern_hydro_features_hour_base_test;
 


# training set including daytime as feature
train_features2 = cbind(time = mod(strtoi(row.names(train_features)),144) , train_features);
valid_features2 = cbind(time = mod(strtoi(row.names(valid_features)),144) , valid_features);

# LM Model using only features from Location Bern and daytime information
lm_train_bern_time_features= lm(train_goals [1:624]  ~., data = train_features2 [1:624,1:10]); 
predictions <- lm_train_bern_time_features %>% predict(valid_features2 [1:288,1:10]);
rmse_lm_bern_time_features = rmse(predictions, vaild_goals); # 0.1320373
rmse_lm_bern_time_features_total = rmse_lm_bern_time_features_total + rmse_lm_bern_time_features * rmse_lm_bern_time_features ;

# Hourly LM Model with history using only features from Location Bern.
error = 0;
for(i in 1:8) {
  
  sub_train_features<-extract_hour_subfeature2(train,i);
  
  sub_train_goals <- extract_hour_goals(train,i); 
  indices =  c(1,2,3,4,5,6,7,8,9,47,48,49,50,51,52,53,54,55);
  lm_train_bern_features= lm(sub_train_goals [1:length(sub_train_goals)]  ~., data = sub_train_features [1:length(sub_train_goals),indices]); 
  
  sub_valid_features<-extract_hour_subfeature2(valid,i);
  sub_valid_goals <- extract_hour_goals(valid,i);
  predictions <- lm_train_bern_features %>% predict(sub_valid_features [1:length(sub_valid_goals),indices]);
  
  rsme =  rmse(predictions, sub_valid_goals) ;
  error= error + rsme*rsme;
} 
rsme_lm_bern_features_hour_base_double = sqrt(error / 8);
rsme_lm_bern_features_hour_base_double_total = rsme_lm_bern_features_hour_base_double_total + rsme_lm_bern_features_hour_base_double * rsme_lm_bern_features_hour_base_double; #0.1165936


# Hourly LM Model with water temperature history using only features from Location Bern.
# The size of the history is determined on the validation set
model_errors = c()
for (val in 1:15) {
  error = 0
  
  for (i in 1:8) {
    sub_train_features <- extract_hour_subfeature4(train, i, val)
    sub_train_goals <- extract_hour_goals(train, i)
    indices =  c(1, 2, 3, 4, 5, 6, 7, 8, 9)
    for (j in 47:(46 + val)) {
      indices  = c(indices, j)
      
    }
    lm_train_bern_features = lm(sub_train_goals [1:length(sub_train_goals)]  ~
                                  ., data = sub_train_features [1:length(sub_train_goals), indices]); 
    sub_valid_features <- extract_hour_subfeature4(valid, i, val); 
    sub_valid_goals <- extract_hour_goals(valid, i);
    
    predictions <-
      lm_train_bern_features %>% predict(sub_valid_features [1:length(sub_valid_goals), indices]);  
    rsme =  rmse(predictions, sub_valid_goals); 
    error = error + rsme * rsme
    
  }
  rsme_lm_bern_features_hour_val = sqrt(error / 8); 
  model_errors = c(model_errors, rsme_lm_bern_features_hour_val); 
}
val_total = val_total + model_errors[which.min(model_errors)] * model_errors[which.min(model_errors)];  


error = 0 
val =which.min(model_errors); # optimal value on the validation set
for (i in 1:8) {
  sub_train_features <- extract_hour_subfeature4(train, i, val)
  sub_train_goals <- extract_hour_goals(train, i)
  indices =  c(1, 2, 3, 4, 5, 6, 7, 8, 9)
  for (j in 47:(46 + val)) {
    indices  = c(indices, j) 
  }
  lm_train_bern_features = lm(sub_train_goals [1:length(sub_train_goals)]  ~
                                ., data = sub_train_features [1:length(sub_train_goals), indices]); 
  sub_test_features <- extract_hour_subfeature4(test, i, val); 
  sub_test_goals <- extract_hour_goals(test, i);
  
  predictions <-
    lm_train_bern_features %>% predict(sub_test_features [1:length(sub_test_goals), indices]);  
  rsme =  rmse(predictions, sub_test_goals); 
  error = error + rsme * rsme
  
}
rsme_lm_bern_features_hour_test = sqrt(error / 8); 
rsme_lm_bern_features_hour_test_total2= rsme_lm_bern_features_hour_test_total2 + rsme_lm_bern_features_hour_test * rsme_lm_bern_features_hour_test;



# Hourly LM Model with water temperature history using only hydro features from Location Bern.
# The size of the history is determined on the validation set
model_errors = c()
for (val in 1:15) {
  error = 0
  
  for (i in 1:8) {
    sub_train_features <- extract_hour_subfeature4(train, i, val)
    sub_train_goals <- extract_hour_goals(train, i)
    indices =  c(1, 2)
    for (j in 47:(46 + val)) {
      indices  = c(indices, j)
      
    }
    lm_train_bern_features = lm(sub_train_goals [1:length(sub_train_goals)]  ~
                                  ., data = sub_train_features [1:length(sub_train_goals), indices]); 
    sub_valid_features <- extract_hour_subfeature4(valid, i, val); 
    sub_valid_goals <- extract_hour_goals(valid, i);
    
    predictions <-
      lm_train_bern_features %>% predict(sub_valid_features [1:length(sub_valid_goals), indices]);  
    rsme =  rmse(predictions, sub_valid_goals); 
    error = error + rsme * rsme
    
  }
  rsme_lm_bern_features_hour_val = sqrt(error / 8); 
  model_errors = c(model_errors, rsme_lm_bern_features_hour_val); 
} 
val2_total = val2_total + model_errors[which.min(model_errors)] * model_errors[which.min(model_errors)];  



error = 0 
val =which.min(model_errors); # optimal value on the validation set
for (i in 1:8) {
  sub_train_features <- extract_hour_subfeature4(train, i, val)
  sub_train_goals <- extract_hour_goals(train, i)
  indices =  c(1, 2)
  for (j in 47:(46 + val)) {
    indices  = c(indices, j) 
  }
  lm_train_bern_features = lm(sub_train_goals [1:length(sub_train_goals)]  ~
                                ., data = sub_train_features [1:length(sub_train_goals), indices]); 
  sub_test_features <- extract_hour_subfeature4(test, i, val); 
  sub_test_goals <- extract_hour_goals(test, i);
  
  predictions <-
    lm_train_bern_features %>% predict(sub_test_features [1:length(sub_test_goals), indices]);  
  rsme =  rmse(predictions, sub_test_goals); 
  error = error + rsme * rsme
  
}
rsme_lm_bern_hydro_features_hour_test = sqrt(error / 8); 
rsme_lm_bern_hydro_features_hour_test_total= rsme_lm_bern_hydro_features_hour_test_total + rsme_lm_bern_hydro_features_hour_test * rsme_lm_bern_hydro_features_hour_test;

}

# Output of the results
sqrt(rsme_avf_diff_test_total/ ncval) ;
sqrt(rmse_lm_bern_features_test_total/ ncval);
sqrt(rsme_avg_diff_hour_base_test_total/ ncval);
sqrt(rsme_lm_bern_features_hour_base_test_total1/ ncval);
sqrt(rsme_lm_bern_features_hour_test_total2/ ncval);
sqrt(rsme_simple_total/ ncval ); 
sqrt(rsme_avf_diff_total/ ncval);
sqrt(rmse_lm_all_features_total/ ncval);
sqrt(rmse_lm_bern_features_total/ ncval);
sqrt(rmse_lm_bern_hydro_features_total/ ncval);
sqrt(rsme_avg_diff_hour_base_total/ ncval);
sqrt(rsme_lm_bern_features_hour_base_total/ ncval);
sqrt(rmse_lm_bern_time_features_total/ ncval);
sqrt(rsme_lm_bern_features_hour_base_double_total / ncval);
sqrt(val_total  / ncval); 
sqrt(val2_total  / ncval);
sqrt( rsme_lm_bern_hydro_features_hour_base_total / ncval);
sqrt( rsme_lm_bern_hydro_features_hour_base_test_total / ncval);
sqrt(rsme_lm_bern_hydro_features_hour_test_total / ncval) 


 




















 
 






