# Load neccesary libraries
library(RSQLite)
library(readr)
library(dplyr)

## File Prefix and Suffix
# read files from 'data' folder
project_files <- list.files("New_Data/")
project_files

prefix <- "ecommerce_"
suffix <- "_dataset.csv"
project_files <- gsub(prefix,"",project_files)
project_files <- gsub(suffix,"",project_files)
project_files

## Read .csv files
address_data <- read.csv("New_Data/ecommerce_address_dataset.csv")
buyer_data <- read.csv("New_Data/ecommerce_buyer_dataset.csv")
dimension_data <- read.csv("New_Data/ecommerce_dimension_dataset.csv")
product_data <- read.csv("New_Data/ecommerce_product_dataset.csv")
promotion_data <- read.csv("New_Data/ecommerce_promotion_dataset.csv")
review_data <- read.csv("New_Data/ecommerce_review_dataset.csv")
shipper_data <- read.csv("New_Data/ecommerce_shipper_dataset.csv")
supplier_data <- read.csv("New_Data/ecommerce_supplier_dataset.csv")
payment_data <- read.csv("New_Data/ecommerce_payment_dataset.csv")
order_details_data <- read.csv("New_Data/ecommerce_order_details_dataset.csv")


## Columns present in each datafile
all_files <- list.files("New_Data/")

for (variable in all_files) {
  print(paste0("The file : ",variable, " has the following columns : "))
  this_filepath <- paste0("New_Data/",variable)
  this_file_contents <- readr::read_csv(this_filepath)
  
  column_names <- colnames(this_file_contents)
  
  print(paste0(column_names))
}

## Number of rows and columns in the dataset
all_files <- list.files("New_Data/")

for (variable in all_files) {
  this_filepath <- paste0("New_Data/",variable)
  this_file_contents <- readr::read_csv(this_filepath)
  
  number_of_rows <- nrow(this_file_contents)
  number_of_columns <- ncol(this_file_contents)
  
  print(paste0("The file: ",variable,
              " has : ",
              format(number_of_rows,big.mark = ","),
              " rows and ",
              number_of_columns," columns"))
}

## Check if the first column of each file is a primary 
for (variable in all_files) {
  this_filepath <- paste0("New_Data/",variable)
  this_file_contents <- readr::read_csv(this_filepath)
  number_of_rows <- nrow(this_file_contents)
  
  print(paste0("Checking for: ",variable))
  
  print(paste0(" is ",nrow(unique(this_file_contents[,1]))==number_of_rows))
}

## Data Quality Assurance
### Data Integrity Checks
promotion_data <- mutate(promotion_data,
                         start_date = as.Date(start_date,format= "%d/%m/%Y"),
                         end_date = as.Date(end_date,format= "%d/%m/%Y"))

order_details_data <- mutate(order_details_data,
                         order_dates = as.Date(order_dates,format= "%Y-%m-%d"))




dataname_list <- list("address_data", "buyer_data", "dimension_data", "order_details_data", "payment_data",
                 "product_data", "promotion_data", "review_data", "shipper_data", "supplier_data")

idname_list <- list("address_id", "buyer_id", "dimension_id","order_id","payment_id","product_id",
                    "promotion_id", "review_id", "shipper_id", "supplier_id")

idformat_list <-  list("^AD","^BU","^DM","^OR","^PD","^PR",
                       "^PM","^RV","^SH","^SP")

# Loop through each data file
for (i in seq_along(dataname_list)) {
  # Load data file
  datatemp <- get(dataname_list[[i]])
  
  # Get the ID column name and format for the current data file
  id_col <- idname_list[[i]]
  id_format <- idformat_list[[i]]
  
  # Check if the ID column exists in the data frame
  if (id_col %in% colnames(datatemp)) {
    # Filter rows where the ID column does not match the specified format
    id_data <- datatemp %>%
      filter(!grepl(paste0("^", id_format), get(id_col)))
    
  } else {
    cat("ID column ", id_col, " not found in ", dataname_list[[i]], "\n")
  }
  
  # Print the filtered data
    print(paste0("Checking for format for ", id_col))
    
    if (nrow(id_data)==0){
    print(paste0("No rows are removed from ",dataname_list[[i]], " because of incorrect id format" ))
    } else{
    print(paste0(nrow(id_data), " rows are removed from ",dataname_list[[i]], " because of incorrect id format" ))
    print(paste0("THe removed data are ", id_data))
    assign(dataname_list[[i]], anti_join(datatemp, id_data, by = id_col))
    }    
}

### Referential Integrity Checks
# PRODUCT ENTITY
# Relationship between Product and Promotion : 1 Promotion applied on N Products
setdiff(unique(product_data$promotion_id), unique(promotion_data$promotion_id))
# Relationship between Product and Supplier : 1 Supplier sells N Products 
setdiff(unique(product_data$supplier_id), unique(supplier_data$supplier_id))
# Relationship between Product and Dimension : 1 Product has 1 Dimension
setdiff(unique(product_data$dimension_id), unique(dimension_data$dimension_id))

# BUYER ENTITY
# Relationship between Buyer and Address : 1 Buyer has 1 Address
setdiff(unique(buyer_data$address_id), unique(address_data$address_id))

# SUPPLIER ENTITY
# Relationship between Supplier and Address : 1 Supplier has 1 Address
setdiff(unique(supplier_data$address_id), unique(address_data$address_id))

# REVIEW ENTITY
# Relationship between Product and Review : 1 Product has n Reviews
setdiff(unique(product_data$review_id), unique(review_data$review_id))
# Relationship between Buyer and Review : 1 Buyer gives n Reviews
setdiff(unique(buyer_data$review_id), unique(review_data$review_id))

# PAYMENT ENTITY
# Relationship between Payment and Order : 1 Order has n Payments
setdiff(unique(payment_data$order_id), unique(order_details_data$order_id))

# ORDER DETAILS ENTITY
# Relationship between Order and Buyer : 1 Buyer has n Orders
setdiff(unique(order_details_data$buyer_id), unique(buyer_data$buyer_id))
# Relationship between Order and Product : 1 Product present in n Orders
setdiff(unique(order_details_data$product_id), unique(product_data$product_id))
# Relationship between Order and Shipper : 1 Shipper satisfies n Orders
setdiff(unique(order_details_data$shipper_id), unique(shipper_data$shipper_id))
# Relationship between Order and Supplier : 1 Supplier has n Orders
setdiff(unique(order_details_data$supplier_id), unique(supplier_data$supplier_id))

## Access Existing Database
connection <- dbConnect(RSQLite::SQLite(), "Database/ecommerce.db")

## Writing data to Existing Database tables
RSQLite::dbWriteTable(connection,"promotion",promotion_data,append=TRUE,rowname=FALSE)
RSQLite::dbWriteTable(connection,"shipper",shipper_data,append=TRUE,rowname=FALSE)
RSQLite::dbWriteTable(connection,"address",address_data,append=TRUE,rowname=FALSE)
RSQLite::dbWriteTable(connection,"dimension",dimension_data,append=TRUE,rowname=FALSE)
RSQLite::dbWriteTable(connection,"buyer",buyer_data,append=TRUE,rowname=FALSE)
RSQLite::dbWriteTable(connection,"supplier",supplier_data,append=TRUE,rowname=FALSE)
RSQLite::dbWriteTable(connection,"product",product_data,append=TRUE,rowname=FALSE)
RSQLite::dbWriteTable(connection,"review",review_data,append=TRUE,rowname=FALSE)
RSQLite::dbWriteTable(connection,"order_details",order_details_data,append=TRUE,rowname=FALSE)
RSQLite::dbWriteTable(connection,"payment",payment_data,append=TRUE,rowname=FALSE)

## Basic Data Analysis



