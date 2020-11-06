#' make poodle frame
#'
#' Removes all dogs that aren't poodles from the data frame, renames rows and columns to fit this new data frame, adds a poodle type column in place of breed column
#' @param clean_dog_data the cleaned data, an output from the clean dog function
#' @keywords poodle, rename
#' @export
#' @examples 
#' make_poodle_frame(clean_dog_data)

make_poodle_frame <- function (clean_dog_data) {
#finds the rows of poodles
poodle_rows <- grep("poodle", clean_dog_data$Breed, ignore.case = TRUE)
#takes the poodle rows to construct a new data frame
poodle_data <- clean_dog_data[poodle_rows,]
#takes the substring of poodle type and makes it it's own column in the data frame
poodle_data$Poodle_Type <- substr(poodle_data$Breed, 
                                  7,
                                  nchar(poodle_data$Breed))
#removes any entries with "NA"
poodle_data <- na.omit(poodle_data)
#changes column name to reflect the new poodle-only frame
poodle_data$Name <- poodle_data$DogName
#includes only the necessary  columns in the final data frame
poodle_data <- poodle_data[,c("Name","Sex","Poodle_Type","Color", "Date_Registered", "OwnerZip")]
#renames the rows so that the non-poodle data isn't reflected in row names
row.names(poodle_data) <- 1:nrow(poodle_data)
poodle_data
}