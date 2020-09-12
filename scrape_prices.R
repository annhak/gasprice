library(rvest)
library(lubridate)

# Define the url and read the page
url <- 'http://www.tankille.fi/tampere/'
webpage <- read_html(url)

#### Store table data ####

# Create a table of the first tab (95E) ([[1]] takes the df from list). Parse datetimes
t <- html_table(html_nodes(webpage, "table")[1])[[1]]
t$Päivitetty <- gsub("juuri nyt", 0, t$Päivitetty)
t$Päivitetty <- gsub("tunti sitten", 1, t$Päivitetty)
t$time <- ymd_hms(Sys.time() - as.numeric(gsub("[^0-9]", "", t$Päivitetty))*60*60)

# Create the scraped price df
prices <- data.frame(
  time = t$time,
  station = t$Asema,
  price = t$Hinta
)

# Get the maximum unix time from the filenames and get the name of the latest file
filenames <- list.files()
max_time <- max(as.numeric(gsub("[^0-9]", "",  filenames)), na.rm = TRUE)
last_file_name <- filenames[grep(max_time, filenames)]

# Read in the file content to be appended (not mofidying the old to avoid corrupting data)
file_content <- read.csv(last_file_name, stringsAsFactors = FALSE)
file_content$time <- ymd_hms(file_content$time)

# Get the new entries
new_prices <- prices[as.numeric(prices$time) > (max_time + 60*60),]

if (dim(new_prices)[1] > 0) {
  print("Saving new rows:") 
  print(paste(new_prices$station))
  # Bind new rows to existing data
  all_prices <- rbind(file_content, new_prices)
  
  # Create a new file with the current unix time in the file name
  name <- paste("prices", floor(as.numeric(Sys.time())), ".txt", sep = "")
  write.table(all_prices, file = name, sep = ",", row.names = FALSE, quote = FALSE)
}

#### Store also average prices ####

# Get the lines, remove the "6" related to header
average <- html_nodes(webpage, "h6")
average <- gsub("h6>", "", average)

# Get the prices and store them
average <- as.numeric(gsub("[^0-9.]", "",  average))
averages <- data.frame(
  time = Sys.time(),
  E95 = average[1],
  E98 = average[2],
  diesel = average[3]
)

write.table(
  averages, file = "averages.txt", sep = ",", 
  row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE)
