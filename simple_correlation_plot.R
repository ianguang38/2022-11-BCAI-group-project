library(tidyverse)
rawdt <- read.csv("week2/MNS_data_full.csv")
library(corrplot)

# check number of user and article
length(unique(rawdt$user_id))
length(unique(rawdt$article_id))


nona <- rawdt[complete.cases(rawdt),]
normdt <- nona
## normalization
normdt[c(3:length(colnames(normdt)))] <- scale(normdt[c(3:length(colnames(normdt)))])
normdt <- as.data.frame(normdt)
write.csv(normdt,"week2/mns_full_scaled.csv",row.names = F)
colMeans(normdt)
apply(normdt, 2, sd) # 2 means apply to column

length(unique(nona$user_id))
length(unique(nona$article_id))

# correlation 
cor_data = cor(nona)
corrplot(cor_data, method="circle")


corr_simple <- function(data=nona,sig=0.5){
  #convert data to numeric in order to run correlations
  #convert to factor first to keep the integrity of the data - each value will become a number rather than turn into NA
  df_cor <- data %>% mutate_if(is.character, as.factor)
  df_cor <- df_cor %>% mutate_if(is.factor, as.numeric)
  #run a correlation and drop the insignificant ones
  corr <- cor(df_cor)
  #prepare to drop duplicates and correlations of 1     
  corr[lower.tri(corr,diag=TRUE)] <- NA 
  #drop perfect correlations
  corr[corr == 1] <- NA 
  #turn into a 3-column table
  corr <- as.data.frame(as.table(corr))
  #remove the NA values from above 
  corr <- na.omit(corr) 
  #select significant values  
  corr <- subset(corr, abs(Freq) > sig) 
  #sort by highest correlation
  corr <- corr[order(-abs(corr$Freq)),] 
  #print table
  print(corr)
  #turn corr back into matrix in order to plot with corrplot
  mtx_corr <- reshape2::acast(corr, Var1~Var2, value.var="Freq")
  dev.new(width=20, height=10)
  #plot correlations visually
  corrplot(mtx_corr, is.corr=FALSE, tl.col="black", na.label=" ",
           title = "significant correlations (score > 0.5)",
           mar=c(0,0,1,0))
}

corr_simple()
