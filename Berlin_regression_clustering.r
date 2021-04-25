
require(ggplot2)
require(gridExtra)
require(ggthemes)
library(nnet)
library(neuralnet)
library(NeuralNetTools)
library(dplyr)
library(HH)
library(factoextra)
library(ggfortify)

#data load
Berlin <- read.csv("AirbnbBerlin.csv")

#data preparation
Berlin <- subset(Berlin,select=-c(Review.ID,review_date,Reviewer.ID,Reviewer.Name,Comments,Listing.URL,Listing.Name,Host.ID,Host.URL,Host.Name,Neighborhood.Group,Is.Exact.Location,City,Country.Code,Country,Square.Feet,Postal.Code,neighbourhood,Host.Response.Rate,Property.Type))

#group by listing ID. The only thing that changes between rows of the same listing is the reviewer details and comments which have already been removed. Simply group by listing ID and take all of the other details from the first row of each listing.
Berlin <- Berlin %>% group_by(Listing.ID) %>%
  summarise(Host.Since = first(Host.Since),Host.Response.Time=first(Host.Response.Time),Is.Superhost=first(Is.Superhost),Latitude=first(Latitude),Longitude=first(Longitude),Room.Type=first(Room.Type),Accomodates=first(Accomodates),Bathrooms=first(Bathrooms),Bedrooms=first(Bedrooms),Beds=first(Beds),Price=first(Price),Guests.Included=first(Guests.Included),Min.Nights=first(Min.Nights),Reviews=first(Reviews),First.Review=first(First.Review),Last.Review=first(Last.Review),Overall.Rating=first(Overall.Rating),Accuracy.Rating=first(Accuracy.Rating),Cleanliness.Rating=first(Cleanliness.Rating),Checkin.Rating=first(Checkin.Rating),Communication.Rating=first(Communication.Rating),Location.Rating=first(Location.Rating),Value.Rating=first(Value.Rating),Instant.Bookable=first(Instant.Bookable)) %>%arrange(desc(Listing.ID))

#convert factors to numerical
Berlin <- Berlin %>% mutate(Host.Response.Time = case_when(
  (Host.Response.Time == "within an hour") ~ 3,
  (Host.Response.Time == "within a few hours") ~ 2,
  (Host.Response.Time == "within a day") ~ 1,
  (Host.Response.Time == "within a few days") ~ 0)
  )

Berlin <- Berlin %>% mutate(Room.Type = case_when(
  (Room.Type == "Entire home/apt") ~ 2,
  (Room.Type == "Private room") ~ 1,
  (Room.Type == "Shared room") ~ 0)
  )

Berlin <- Berlin %>% mutate(Instant.Bookable = case_when(
  (Instant.Bookable == "t") ~ 1,
  (Instant.Bookable == "f") ~ 0))

Berlin <- Berlin %>% mutate(Is.Superhost = case_when(
  (Is.Superhost == "t") ~ 1,
  (Is.Superhost == "f") ~ 0))

#convert date variables to epoch time
Berlin$Host.Since = as.numeric(as.POSIXct(Berlin$Host.Since, format="%m/%d/%Y"))
Berlin$First.Review = as.numeric(as.POSIXct(Berlin$First.Review, format="%m/%d/%Y"))
Berlin$Last.Review = as.numeric(as.POSIXct(Berlin$Last.Review, format="%m/%d/%Y"))

#convert numeric variables to numeric
Berlin$Price <- as.numeric(Berlin$Price)

#after removing entries without a rating, the remaining NA's are a very small percent of the data, so just remove 
Berlin <- na.omit(Berlin)

Berlin <- Berlin[!Berlin$Min.Nights>365, ] #filter entries with min nights greater than 365 as unrealistic
Berlin <- Berlin[!Berlin$Price<=0, ] #filter entries with price of zero as incorrect
Berlin <- Berlin[!Berlin$Beds<=0,] #filter entries with zero beds as incorrect
Berlin <- Berlin[which(Berlin$Bathrooms <= Berlin$Accomodates),] #several entries have unrealistic bathroom counts
Berlin <- Berlin[!Berlin$Price > 2500,] #several entries have unrealistic prices

summary(Berlin)

#Examine linear fit to assist with variable selection and check multicollinearity
fit=lm(Price~Latitude+Longitude+Room.Type+Accomodates+Bathrooms+Bedrooms+Beds+Guests.Included+Min.Nights+Reviews+Cleanliness.Rating+Communication.Rating+Location.Rating+Value.Rating,data=Berlin)
summary(fit)
vif(fit)

#hist.plot
#function for plotting histograms of predictors,
#takes arguements:
#data- dataframe containing the predictor)
#predictor- a column of the dataframe data (the variable of interest), call with $ notation
#plot.color- string for bin fill color
#width- integer for bin width
#label- string for x plot label
#returns plot item

hist.plot <- function(data,predictor,plot.color="white",width="1",label="predictor",cont=FALSE)
{
  if(cont==TRUE){
  p.item <- ggplot( data,  aes( x = predictor)) + 
  geom_histogram( aes( y = ..density..),
                 binwidth = width,
                 colour = "black",
                 fill = plot.color) +
                 labs(x=label) +
                 stat_function( fun = dnorm,
                     args = list( mean = mean(predictor),
                              sd = sd(predictor))) + 
                 theme_tufte() + theme(text = element_text(size = 16))
  }
  else{p.item <- ggplot( data,  aes( x = predictor)) + 
  geom_histogram( aes( y = ..density..),
                 binwidth = width,
                 colour = "black",
                 fill = plot.color) +
    labs(x=label) + theme_tufte() + theme(text = element_text(size = 16))
  }
  return(p.item)
}

#plot histograms of variables
p1 <- hist.plot(Berlin,Berlin$Price,"#00887d",75,"Price",cont=TRUE)
p2 <- hist.plot(Berlin,Berlin$Latitude,"#adadad",.05,"Latitude",cont=TRUE)
p3 <- hist.plot(Berlin,Berlin$Longitude,"#ee8f71",.1,"Longitude",cont=TRUE)
p4 <- hist.plot(Berlin,Berlin$Room.Type,"#7c260b",1,"Room.Type")
p5 <- hist.plot(Berlin,Berlin$Accomodates,"#a0d6ec",1,"Accomodates")
p6 <- hist.plot(Berlin,Berlin$Bathrooms,"#76c0c1",1,"Bathrooms")
p7 <- hist.plot(Berlin,Berlin$Bedrooms,"#a18376",1,"Bedrooms")
p8 <- hist.plot(Berlin,Berlin$Beds,"#6794a7",1,"Beds")
p9 <- hist.plot(Berlin,Berlin$Guests.Included,"#8fb9c3",1,"Guests.Included")
p10 <- hist.plot(Berlin,Berlin$Min.Nights,"#01a2d9",30,"Min.Nights")
p11 <- hist.plot(Berlin,Berlin$Reviews,"#7ad2f6",50,"Reviews")
p12 <- hist.plot(Berlin,Berlin$Cleanliness.Rating,"#014d64",1,"Cleanliness.Rating")
p13 <- hist.plot(Berlin,Berlin$Communication.Rating,"#D7D29E",1,"Communicaton.Rating")
p14 <- hist.plot(Berlin,Berlin$Value.Rating,"#7B92A8",1,"Value.Rating")
grid.arrange( p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, ncol = 3)

fulldata.in = Berlin
suppressWarnings(set.seed(2, sample.kind = "Rounding"))
#split into 3 sets, 80% train, 10% validation, 10% test
#first do 80/20 split
train = sample(nrow(Berlin)*.8)
Berlin_train = Berlin[train,]
Berlin_rest = Berlin[-train,]
#then split 20% into validiation and test sets
valid = sample(nrow(Berlin_rest)*.5)
Berlin_valid = Berlin_rest[valid,]
Berlin_test = Berlin_rest[-valid,]

#create another set including training and validiation sets
Berlin_trainvalid = rbind(Berlin_train,Berlin_valid)

#independently standardize numeric columns for each set
Berlin_train= Berlin_train %>%
    mutate_if(is.numeric, scale)
Berlin_valid = Berlin_valid %>%
    mutate_if(is.numeric, scale)
Berlin_test = Berlin_test %>%
    mutate_if(is.numeric, scale)
Berlin_trainvalid = Berlin_trainvalid %>%
    mutate_if(is.numeric, scale)

#total model count
decayRate <- seq(.01, .024, by = .001)
sizes = 5:10
nmodels = length(decayRate) * length(sizes)
formula = Price~Latitude+Longitude+Room.Type+Accomodates+Bathrooms+Bedrooms+Beds+Guests.Included+Min.Nights+Reviews+Cleanliness.Rating+Communication.Rating+Location.Rating+Value.Rating

suppressWarnings(set.seed(2, sample.kind = "Rounding"))

allmodelCV.in = rep(NA,nmodels) #place-holder for results
r2.in = rep(NA,nmodels) #place-holder for results
alllmfitCV.in = NA

# set up storage for predicted values across models
n.in = nrow(Berlin_valid)
allpredictedCV.in = matrix(rep(NA,n.in*nmodels),ncol=nmodels)

#choose decay and hidden layer nodes
m = 1
for(j in 1:length(decayRate)){
  for(s in 1:length(sizes)){
    lmfitCV.in = nnet(formula, data=Berlin_train, size = sizes[s], trace = F, linout = T, maxit = 1000, MaxNWts=5000, decay = decayRate[j]) 
    allpredictedCV.in[,m] = predict(lmfitCV.in,newdata=Berlin_valid)
    allmodelCV.in[m] = mean((allpredictedCV.in[,m]-Berlin_valid$Price)^2)
    r2.in[m] = 1-sum((allpredictedCV.in[,m]-Berlin_valid$Price)^2)/sum((Berlin_valid$Price-mean(Berlin_valid$Price))^2)
    print(m)
    if(lmfitCV.in$convergence == 1){
      print("model not converged") #check for convergence
    }
    #print(allmodelCV.in[m])
    #print(r2.in[m])
    m = m+1  #increment for next model
  }#end iteration over s
} # end iteration over j

bestmodel.in = (1:nmodels)[order(allmodelCV.in)[1]]  # actual selection
# state which is best model and minimum MSE value
bestmodel.in; min(allmodelCV.in, na.rm = TRUE)

#determine the decay rate and hidden nodes based on best model
models_decayRate = rep(NA,nmodels)
models_sizes = rep(NA,nmodels)
k=1
for (i in 1:length(decayRate))  {
  for (j in 1:length(sizes)){
    models_decayRate[k] = decayRate[i]; models_sizes[k] = sizes[j]
    k=k+1
    }
  }
best_dR = models_decayRate[bestmodel.in]; best_size = models_sizes[bestmodel.in]
print(best_dR);print(best_size)
print(r2.in[bestmodel.in])

# visualize error values across all models
model_index <- c(1:nmodels)
mse_df <- data.frame(allmodelCV.in,models_decayRate,models_sizes,model_index)
mse_df$models_sizes <- as.factor(mse_df$models_sizes)

myPalette <- c("#00887d","#adadad","#ee8f71","#7c260b","#a0d6ec","#76c0c1","#a18376","#6794a7","#8fb9c3","#01a2d9","#7ad2f6","#014d64","#3E647D","#7B92A8","#82C0E9","#2D6D66","#BFA19C","#008BBC","#97B6B0","#D7D29E")

#plot MSE vs decay rate by hidden layer nodes
mse_plot <- ggplot(mse_df[models_sizes==5,], aes(models_decayRate, allmodelCV.in)) +
  geom_point(aes(color = models_sizes)) + geom_line(aes(color = models_sizes),size=1) +
  geom_point(data=mse_df[models_sizes==6,],aes(color = models_sizes))+ geom_line(data=mse_df[models_sizes==6,],aes(color = models_sizes),size=1) +
  geom_point(data=mse_df[models_sizes==7,],aes(color = models_sizes))+ geom_line(data=mse_df[models_sizes==7,],aes(color = models_sizes),size=1) +
  geom_point(data=mse_df[models_sizes==8,],aes(color = models_sizes))+ geom_line(data=mse_df[models_sizes==8,],aes(color = models_sizes),size=1) +
  geom_point(data=mse_df[models_sizes==9,],aes(color = models_sizes))+ geom_line(data=mse_df[models_sizes==9,],aes(color = models_sizes),size=1) +
  geom_point(data=mse_df[models_sizes==10,],aes(color = models_sizes))+ 
  geom_line(data=mse_df[models_sizes==10,],aes(color = models_sizes),size=1) +
  theme_tufte() + theme(aspect.ratio=1/3, legend.position = "bottom",
        axis.text=element_text(size=13),axis.title=element_text(size=16),plot.title=element_text(size=20),
        legend.text=element_text(size=16),legend.title=element_text(size=16))+ coord_cartesian(ylim=c(0.4, .6)) +
  scale_color_manual(values=myPalette, labels=c(5,6,7,8,9,10)) + guides(colour = guide_legend(nrow = 1)) +
  ggtitle("Validation Error (MSE)- ANN Models") +ylab("MSE") + xlab("Decay Rate (lambda)")
mse_plot

suppressWarnings(set.seed(2, sample.kind = "Rounding"))

#fit best model on training and validation data
bestfit <- nnet(Price~Latitude+Longitude+Room.Type+Accomodates+Bathrooms+Bedrooms+Beds+Guests.Included+Min.Nights+Reviews+Cleanliness.Rating+Communication.Rating+Location.Rating+Value.Rating, data=Berlin_trainvalid, size = best_size, trace = F, linout = T, maxit = 1000, MaxNWts=5000, decay = best_dR) 

#predict new values with test data to evaluate model
allpredictedCV=predict(bestfit,newdata=Berlin_test)
allmodelCV = mean((allpredictedCV-Berlin_test$Price)^2)
bestfit$convergence
print(allmodelCV)
R2 = 1-sum((allpredictedCV-Berlin_test$Price)^2)/sum((Berlin_test$Price-mean(Berlin_test$Price))^2); print(R2)

#############################
## End of modeling process ##
#############################

summary(bestfit)

plotnet(bestfit, cex_val=1, circle_col="#76c0c1", bord_col="#76c0c1",pad_x=.6)

garson(bestfit) +
  scale_fill_gradientn(colours = myPalette) + 
  scale_colour_gradientn(colours = myPalette) +
  theme(panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        axis.text=element_text(angle =   90,size=12),axis.title=element_text(size=16),plot.title=element_text(size=20)) +
  ggtitle("Relative Variable Importance in Best ANN Model")

n=10898
p=24
pc.info=prcomp(Berlin[-c(1)],center=T,scale=T)

#pc.info$rotation  #loadings
#pc.info$sdev

#calculate percentage of variation explained (PVE)
vjs = pc.info$sdev^2
PVE = vjs/sum(vjs)

PCs=1:24
var.df <- data.frame(PCs,PVE)
  ggplot(var.df, aes(x=PCs,y=PVE))+geom_point(size=3,color="#2d6d66")+geom_line(size=2,color="#2d6d66")+
    theme_tufte() +
    theme(axis.text=element_text(size=14),axis.title=element_text(size=16),plot.title=element_text(size=20))+
    ggtitle("Scree Plot of Scaled Berlin Airbnb data")

#determine number of clusters with elbow method

k.values <- 1:10
wss.values=rep(NA,length(k.values))
for(i in k.values){
  wss.values[i] = kmeans(pc.info$x, i)$tot.withinss
}

k.df <- data.frame(k.values, wss.values)
  ggplot(k.df, aes(x=k.values,y=wss.values))+geom_point(size=3,color="#014d64")+geom_line(size=2,color="#014d64")+
    theme_tufte() + theme(axis.text=element_text(size=14),axis.title=element_text(size=16),plot.title=element_text(size=20))+
    ggtitle("Within-cluster Sum of Squares") + scale_x_continuous(name="Number of Clusters", limits=c(1,10),breaks=1:10)

suppressWarnings(set.seed(4, sample.kind = "Rounding"))
myPaletteRand <- sample(myPalette)

#place first three PCs in data frame
Berlin.pcaScores <- data.frame(pc.info$x[,1:3])

#perform k-means with k=3 on reduced data
suppressWarnings(set.seed(2, sample.kind = "Rounding"))
k <- kmeans(pc.info$x,3)
Cluster <- as.factor(k$clust)

#plot first three PCs against each other
pca1 <- ggplot(Berlin.pcaScores, aes(y = PC2, x = PC1)) + geom_point(aes(col=Cluster)) + scale_color_manual(values=myPaletteRand) +theme_tufte() + theme(aspect.ratio=1, legend.position = "none", axis.text=element_text(size=12),axis.title=element_text(size=12))
pca2 <- ggplot(Berlin.pcaScores, aes(y = PC3, x = PC2)) + geom_point(aes(col=Cluster)) + scale_color_manual(values=myPaletteRand) +theme_tufte() + theme(aspect.ratio=1, legend.position = "none", axis.text=element_text(size=12),axis.title=element_text(size=12))
pca3 <- ggplot(Berlin.pcaScores, aes(y = PC3, x = PC1)) + geom_point(aes(col=Cluster)) + scale_color_manual(values=myPaletteRand) +theme_tufte() + theme(aspect.ratio=1.15, legend.position = "bottom", axis.text=element_text(size=12),axis.title=element_text(size=12),legend.text=element_text(size=16),legend.title=element_text(size=16))

grid.arrange(pca1,pca2,pca3, ncol=3, top = textGrob("Principal Components by Cluster",gp=gpar(fontsize=20)))

#look at more detailed plot of just first two PCs
suppressWarnings(set.seed(4, sample.kind = "Rounding"))
myPaletteRand <- sample(myPalette)

autoplot(pc.info, data = Berlin[-c(1)], colour = as.factor(k$clust), loadings.label.colour = 'black',
         loadings = TRUE, loadings.colour = 'black', loadings.label = TRUE, loadings.label.size = 4) +
  geom_point(aes(col=Cluster)) +scale_color_manual(values=myPaletteRand) + theme_tufte(base_size=20) + ggtitle("PC1 vs PC2") 

#create dataframe with column for cluster assignment
Berlin.clusters <- Berlin
Berlin.clusters<-data.frame(Berlin.clusters,k$cluster)

#convert variables to factor
Berlin.clusters$Room.Type <- factor(Berlin.clusters$Room.Type, labels = c("Shared Room", "Private Room", "Entire House"))
Berlin.clusters$Host.Response.Time <- as.factor(Berlin.clusters$Host.Response.Time)
Berlin.clusters$Instant.Bookable <- as.factor(Berlin.clusters$Instant.Bookable)
Berlin.clusters$Is.Superhost <- as.factor(Berlin.clusters$Is.Superhost)
Berlin.clusters$k.cluster <- as.factor(Berlin.clusters$k.cluster)

#examine each cluster
print("Cluster 1 Summary")
summary(Berlin.clusters[which(Berlin.clusters$k.cluster==1),])
print("Cluster 2 Summary")
summary(Berlin.clusters[which(Berlin.clusters$k.cluster==2),])
print("Cluster 3 Summary")
summary(Berlin.clusters[which(Berlin.clusters$k.cluster==3),])

#make_bar_plot
#function for plotting bar plots by cluster
#takes arguements:
#var1, variable of interest
#bar_df- dataframe containing the data)
#y_lab- label for y-axis
#returns plot item
make_bar_plot <- function(var1, bar_df, y_lab){
  p.item<-ggplot(data=bar_df, aes(x=k.cluster, y=var1, fill=k.cluster)) +
  geom_bar(stat="identity")+theme_tufte()+ scale_fill_manual(values=myPaletteRand)+ theme(plot.title=element_text(size=20),legend.title=element_text(size=16),legend.position="none",legend.text=element_text(size=14),axis.text=element_text(size=12),axis.title=element_text(size=14))+labs(y=y_lab,x="Cluster",fill="Cluster")
  return(p.item)
}

#calculate percentages for comparison in bar plots rather than count
bar_df <- Berlin.clusters %>%
  count(k.cluster,Room.Type) %>%
  group_by(k.cluster) %>%
  mutate(Percent = (n/sum(n))*100) %>%
  ungroup()

suppressWarnings(set.seed(88, sample.kind = "Rounding"))
cbar <- ggplot( data = bar_df, aes( x = k.cluster, y=Percent, fill = Room.Type) ) + 
      geom_bar(stat="identity") + scale_fill_manual(values=sample(myPalette), guide=guide_legend(nrow=3)) +
      xlab("Cluster") +theme_tufte()  + theme(plot.title=element_text(size=20),legend.title=element_text(size=12),legend.text=element_text(size=12),axis.text=element_text(size=12),
                                                     axis.title=element_text(size=14), legend.position = "top")

#regular bar graphs
bar_df2 <- Berlin.clusters %>%
  group_by(k.cluster) %>%
  summarize(avgPrice = mean(Price),avgAccom = mean(Accomodates),avgBeds = mean(Beds),avgBedrooms=mean(Bedrooms),avgClean=mean(Cleanliness.Rating),avgLoc=mean(Location.Rating),avgVal=mean(Value.Rating),avgCom=mean(Communication.Rating),avgRating=mean(Overall.Rating)) %>%
  ungroup()

attach(bar_df2)
bar1 <- make_bar_plot(avgPrice,bar_df2,"Average Price")
bar2 <- make_bar_plot(avgAccom,bar_df2,"Average Accomodates")
bar3 <- make_bar_plot(avgBedrooms,bar_df2,"Average Bedrooms")
bar4 <- make_bar_plot(avgBeds,bar_df2,"Average Beds")
bar5 <- make_bar_plot(avgClean,bar_df2,"Average Cleanliness Rating")+ coord_cartesian(ylim=c(7.5, 10))
bar6 <- make_bar_plot(avgLoc,bar_df2,"Average Location Rating")+ coord_cartesian(ylim=c(7.5, 10))
bar7 <- make_bar_plot(avgVal,bar_df2,"Average Value Rating")+ coord_cartesian(ylim=c(7.5, 10))
bar8 <- make_bar_plot(avgRating,bar_df2,"Average Overall Rating")+ coord_cartesian(ylim=c(75, 100))

grid.arrange(cbar,bar1,bar2,bar3,bar4,bar5,bar6,bar7,bar8, ncol=3)
```  
