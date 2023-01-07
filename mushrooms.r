###### Data Preparation ######

# Loading the data (the expanded version) from storage
data <- read.csv('D:/Studying/Current/Data mining/Project/mushroom/expanded')

# Configuring columns names
columns <- c('class', 'cap-shape', 'cap-surface', 'cap-color', 'bruises', 'odor', 'gill-attachment', 'gill-spacing', 
             'gill-size', 'gill-color', 'stalk-shape', 'stalk-root', 'stalk-surface-above-ring', 'stalk-surface-below-ring',
             'stalk-color-above-ring', 'stalk-color-below-ring', 'veil-type', 'veil-color', 'ring-number', 'ring-type', 
             'spore-print-color', 'population', 'habitat')

# Setting the columns names into the data set
colnames(data) <- columns

# A summary of the current state of the data set
summary(data)
str(data)

# A summary of the class and its plot 
str(data$class)
summary(data$class)

install.packages("plotrix")
library(plotrix)

x<-table(data$class)
pie(x)

# Calculate the percentage of each category
x_percents <- prop.table(x) * 100

# Create the pie chart
pie(x, labels=paste(names(x), "\n", round(x_percents, 1), "%"), main="Category Distribution")


### Removing duplicate data
install.packages("tidyverse")
library(tidyverse)
sum(duplicated(data)) # Num of duplicates
data <- data[!duplicated(data), ]


# Missing values are represented by '?'
sum(data == '?')

# All missing values are in the 'stalk-root' attribute
# and this is their distribution
counts <- table(data$`stalk-root`)
counts # To make sure that 2480 question marks are only in this attribute 
barplot(counts)

# To make sure there are 0 NA
sum(is.na(data)) # prints 0

## Approach 1: Fill the missing values with the mode value
## Approach 2: Fill the missing values randomly
## Approach 3: Delete the column

# Lets see the distribution of the attributes with regard to the class
library(dplyr)

### Plotting each attribute with regard to the class

# cap-shape 
cap_shape_df <- select(data, class, `cap-shape`)
table(cap_shape_df)
barplot(table(cap_shape_df), xlab = 'cap-shape', ylab = 'count', sub = 'Grey for Edible and Silver for Poisonous', main = 'Cap Shape')

# cap-surface
cap_surface_df <- select(data, class, `cap-surface`)
table(cap_surface_df)
barplot(table(cap_surface_df), xlab = 'cap-surface', ylab = 'count', sub = 'Grey for Edible and Silver for Poisonous', main = 'Cap Surface')

# cap-color
cap_color_df <- select(data, class, `cap-color`)
table(cap_color_df)
barplot(table(cap_color_df), xlab = 'cap-color', ylab = 'count', sub = 'Grey for Edible and Silver for Poisonous', main = 'Cap Color')

# bruises
bruises_df <- select(data, class, bruises)
table(bruises_df)
barplot(table(bruises_df), xlab = 'bruises', ylab = 'count', sub = 'Grey for Edible and Silver for Poisonous', main = 'Bruises')

# odor
odor_df <- select(data, class, odor)
table(odor_df)
barplot(table(odor_df), xlab = 'odor', ylab = 'count', sub = 'Grey for Edible and Silver for Poisonous', main = 'Odor')

# gill-attachment
gill_attachment_df <- select(data, class, `gill-attachment`)
table(gill_attachment_df)
barplot(table(gill_attachment_df), xlab = 'gill-attachment', ylab = 'count', sub = 'Grey for Edible and Silver for Poisonous', main = 'Gill Attachment')

# gill-spacing
gill_spacing_df <- select(data, class, `gill-spacing`)
table(gill_spacing_df)
barplot(table(gill_spacing_df), xlab = 'gill-spacing', ylab = 'count', sub = 'Grey for Edible and Silver for Poisonous', main = 'Gill Spacing')

# gill-size 
gill_size_df <- select(data, class, `gill-size`)
table(gill_size_df)
barplot(table(gill_size_df), xlab = 'gill-size', ylab = 'count', sub = 'Grey for Edible and Silver for Poisonous', main = 'Gill Size')

# gill-color
gill_color_df <- select(data, class, `gill-color`)
table(gill_color_df)
barplot(table(gill_color_df), xlab = 'gill-color', ylab = 'count', sub = 'Grey for Edible and Silver for Poisonous', main = 'Gill Color')

# stalk-shape
stalk_shape_df <- select(data, class, `stalk-shape`)
table(stalk_shape_df)
barplot(table(stalk_shape_df), xlab = 'stalk-shape', ylab = 'count', sub = 'Grey for Edible and Silver for Poisonous', main = 'Stalk Shape')

# stalk-root
stalk_root_df <- select(data, class, `stalk-root`)
table(stalk_root_df)
barplot(table(stalk_root_df), xlab = 'stalk-root', ylab = 'count', sub = 'Grey for Edible and Silver for Poisonous', main = 'Stalk Root')

# stalk-surface-above-ring
stalk_surface_above_ring_df <- select(data, class, `stalk-surface-above-ring`)
table(stalk_surface_above_ring_df)
barplot(table(stalk_surface_above_ring_df), xlab = 'stalk-surface-above-ring', 
        ylab = 'count', sub = 'Grey for Edible and Silver for Poisonous', main = 'Stalk Surface Above Ring')

# stalk-surface-below-ring
stalk_surface_below_ring_df <- select(data, class, `stalk-surface-below-ring`)
table(stalk_surface_below_ring_df)
barplot(table(stalk_surface_below_ring_df), xlab = 'stalk-surface-below-ring',
        ylab = 'count', sub = 'Grey for Edible and Silver for Poisonous', main = 'Stalk Surface Below Ring')

# stalk-color-above-ring
stalk_color_above_ring_df <- select(data, class, `stalk-color-above-ring`)
table(stalk_color_above_ring_df)
barplot(table(stalk_color_above_ring_df), xlab = 'stalk-color-above-ring',
        ylab = 'count', sub = 'Grey for Edible and Silver for Poisonous', main = 'Stalk Color Above Ring')

# stalk-color-below-ring
stalk_color_below_ring_df <- select(data, class, `stalk-color-below-ring`)
table(stalk_color_below_ring_df)
barplot(table(stalk_color_below_ring_df), xlab = 'stalk-color-below-ring',
        ylab = 'count', sub = 'Grey for Edible and Silver for Poisonous', main = 'Stalk Color Below Ring')

# veil-type
veil_type_df <- select(data, class, `veil-type`)
table(veil_type_df)
barplot(table(veil_type_df), xlab = 'veil-type', ylab = 'count', sub = 'Grey for Edible and Silver for Poisonous', main = 'Veil Type')
unique(data$`veil-type`) # one value for this attribute -> should be deleted

# veil-color 
veil_color_df <- select(data, class, `veil-color`)
table(veil_color_df)
barplot(table(veil_color_df), xlab = 'veil-color', ylab = 'count', sub = 'Grey for Edible and Silver for Poisonous', main = 'Veil Color')

# ring-number
ring_number_df <- select(data, class, `ring-number`)
table(ring_number_df)
barplot(table(ring_number_df), xlab = 'ring-number', ylab = 'count', sub = 'Grey for Edible and Silver for Poisonous', main = 'Ring Number')

# ring-type 
ring_type_df <- select(data, class, `ring-type`)
table(ring_type_df)
barplot(table(ring_type_df), xlab = 'ring-type', ylab = 'count', sub = 'Grey for Edible and Silver for Poisonous', main = 'Ring Type')

# spore-print-color
spore_print_color_df <- select(data, class, `spore-print-color`)
table(spore_print_color_df)
barplot(table(spore_print_color_df), xlab = 'spore-print-color', 
        ylab = 'count', sub = 'Grey for Edible and Silver for Poisonous', main = 'Spore Print Color')

# population
population_df <- select(data, class, `population`)
table(population_df)
barplot(table(population_df), xlab = 'population', ylab = 'count', sub = 'Grey for Edible and Silver for Poisonous', main = 'Population')

# habitat
habitat_df <- select(data, class, `habitat`)
table(habitat_df)
barplot(table(habitat_df), xlab = 'habitat', ylab = 'count', sub = 'Grey for Edible and Silver for Poisonous', main = 'Habitat')


# Since veil-type attribute has only one values it should be deleted
data <- subset(data, select = -`veil-type`)

# We will convert all '?' to NA
marks <- data == '?'
is.na(data) <- marks

sum(is.na(data)) # prints 2480

# In the stalk-root attribute we will fill the missing values with the mode value which is 'BULBLOUS'
# Function to calculate the mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmode(data$`stalk-root`) # prints "BULBOUS"

data$`stalk-root`[is.na(data$`stalk-root`)] <- getmode(data$`stalk-root`)

sum(is.na(data)) # prints 0


###### Finding relationship between attributes and class 
library(MASS)

# show correlation between odor and class
tbl1<- table(data$class,data$odor)
chisq.test(tbl1)

# show correlation between gill-color and class
tbl3<- table(data$class,data$`gill-color`)
chisq.test(tbl3)

# show correlation between spore-print-color and class
tbl2<- table(data$class,data$`spore-print-color`)
chisq.test(tbl2) 


################################################ Classification using Naive Bayes
data$class <- as.factor(data$class)

install.packages('caret')
library(caret)
trainIndex <- createDataPartition(data$class, p = 0.7, list = FALSE)
train <- data[trainIndex,]
test <- data[-trainIndex,]

str(train)
table(train$class) # to make sure the data is split in a convenient way

install.packages("e1071")
library(e1071)

nv <- naiveBayes(class~., data, laplace = 1)

p <- predict(nv, test)
table(p, test$class)
prediction <- table(p, test$class)
##to calculate the accuracy for test part
accuracy = (sum(diag(prediction)) / sum(prediction)) * 100

accuracy
