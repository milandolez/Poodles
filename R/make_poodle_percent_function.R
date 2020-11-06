#' make poodle percent
#'
#' makes a data frame of each zip code and poodles as a percent of total dogs
#' @param clean_dog_data the cleaned data frame, an output from the clean dog function
#' @param poodle_data the poodle data frame, an output from the make poodle frame function
#' @keywords percent
#' @export
#' @examples 
#' make_poodle_percent (clean_dog_data, poodle_data)


make_poodle_percent <- function(clean_dog_data, poodle_data) {

#Groups together owner zips into factors and then counts the number of poodles within each zip code using 'plyr' package.
poodle_data$OwnerZip <-as.factor(poodle_data$OwnerZip)
library('plyr')
poodle_count <- count(poodle_data, "OwnerZip")
colnames(poodle_count) <- c("OwnerZip", "poodles")
#Groups together owner zips into factors and then counts the number of dogs within each zip code using 'plyr' package.
clean_dog_data$OwnerZip <- as.factor(clean_dog_data$OwnerZip)
dog_count <- count(clean_dog_data, "OwnerZip")
colnames(dog_count) <- c("OwnerZip", "dogs")

#finds zipcodes without poodles and adds those to the poodle count data frame as zipcodes with 0 poodles
Poodle_zips <- as.vector(poodle_count$OwnerZip)
Dog_zips <- as.vector(dog_count$OwnerZip)
no_poodles <-setdiff(Dog_zips, Poodle_zips)
no_poodles_frame <- data.frame(matrix(0,nrow=length(no_poodles), ncol=2))
colnames(no_poodles_frame) <- c("OwnerZip", "poodles")
no_poodles_frame$OwnerZip <- no_poodles
poodle_count <- rbind(no_poodles_frame, poodle_count)

#makes both the poodle count and the dog count data frames organized by decreasing zipcode and removes NA in preparation for percentage calculation
poodle_count <- poodle_count[order(poodle_count$OwnerZip, decreasing=TRUE),]
dog_count <- dog_count[order(dog_count$OwnerZip, decreasing=TRUE),]
na.omit(poodle_count)
na.omit(dog_count)

#constructs a new data frame using values from the poodle count and the dog count to calculate poodle percent
percent_poodles <- data.frame(poodle_count$OwnerZip, (poodle_count$poodles/dog_count$dogs *100))
#changes row/column names and make the percentages in decreasing order
colnames(percent_poodles) <- c("OwnerZip", "Poodle_Percent")
percent_poodles <- percent_poodles[order(percent_poodles$Poodle_Percent, decreasing =TRUE),]
row.names(percent_poodles) <- 1:nrow(percent_poodles)
percent_poodles }
