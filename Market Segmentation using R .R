## Load necessary Packages and Set Seed to student ID
set.seed(40386014)

# Import Restaurant Data in csv format
resdata <- read.csv(file.choose()) ## Choose restaurant data .csv file

#view the first few parts and last few parts of the dataset
head(resdata)
tail(resdata)

#check if scaling is required
apply(resdata, 2, var)

#decided to scale due to variables having different variances
# Run hierarchical clustering with bases variables
resdata_hclust <- hclust(dist(scale(cbind(resdata$Food_Quality, resdata$Beverages, resdata$Location, resdata$innovation, resdata$Quality_of_Service, resdata$Menu_Design,
                                       resdata$Prioritize_Hygiene, resdata$Interior_design, resdata$Reasonable_Pricing, resdata$Restaurant_Technology,
                                       resdata$Brands, resdata$Staff_behavior, resdata$avg_order_size, resdata$avg_order_freq))), method="complete")

# Elbow plot for first 10 segments
x <- c(1:10)
sort_height <- sort(resdata_hclust$height,decreasing=TRUE)
y <- sort_height[1:10]
plot(x,y); lines(x,y,col="green")

# Run k-means with 3 segments
resdata_kmeans <- kmeans(x = data.frame(resdata$Food_Quality, resdata$Beverages, resdata$Location,resdata$innovation, resdata$Quality_of_Service, resdata$Menu_Design,
                                     resdata$Prioritize_Hygiene, resdata$Interior_design, resdata$Reasonable_Pricing, resdata$Restaurant_Technology,
                                     resdata$Brands, resdata$Staff_behavior, resdata$avg_order_size, resdata$avg_order_freq), 3)

# Add segment number back to original data
segment = resdata_kmeans$cluster
segmentation_result <- cbind(resdata, segment)

#view segmentation
resdata_kmeans

resdata_kmeans$withinss #shows within cluster variation

resdata_kmeans$tot.withinss #shows total within cluster variation

# Export data to a CSV file
write.csv(segmentation_result, "/Users/Okwani/segmentation_result.csv",row.names = FALSE)
