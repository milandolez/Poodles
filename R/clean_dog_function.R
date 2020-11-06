#' clean dog
#'
#' Cleans county dog data
#' @param dog_data the data frame of dog information
#' @keywords clean
#' @export
#' @examples 
#' clean_dog(dog_data)


clean_dog <- function (dog_data) {

#fixes first column to just sex
dog_data$LicenseType <- tolower(dog_data$LicenseType)
dog_data$LicenseType[grep(pattern="female",dog_data$LicenseType)] <- "female"
dog_data$LicenseType[grep(pattern="neutered male",dog_data$LicenseType)] <- "male"
dog_data$LicenseType[grep(pattern="lifetime",dog_data$LicenseType)] <- "male"
dog_data$Sex <-  dog_data$LicenseType

#cleans valid date and excludes time of registration
dog_data$Date_Registered <- substr(dog_data$ValidDate, 
                                   1,
                                   nchar(dog_data$ValidDate)-9)
dog_data$Date_Registered <-as.Date(dog_data$Date_Registered, format="%Y-%m-%d")
#includes only necessary columns
clean_dog_data <- dog_data[,c("DogName","Sex","Breed","Color", "Date_Registered", "OwnerZip")]
clean_dog_data <- na.omit(clean_dog_data)
clean_dog_data }