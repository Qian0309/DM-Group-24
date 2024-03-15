library(readr)
library(RSQLite)


# File Prefix and Suffix

# read files from 'data' folder
project_files <- list.files("data/")
project_files

prefix <- "ecommerce_"
suffix <- "_dataset.csv"
project_files <- gsub(prefix,"",project_files)
project_files <- gsub(suffix,"",project_files)
project_files


address_data <- readr::read.csv("data/ecommerce_address_dataset.csv")
buyer_data <- readr::read.csv("data/ecommerce_buyer_dataset.csv")
dimension_data <- readr::read.csv("data/ecommerce_dimension_dataset.csv")
product_data <- readr::read.csv("data/ecommerce_product_dataset.csv")
promotion_data <- readr::read.csv("data/ecommerce_promotion_dataset.csv")
review_data <- readr::read.csv("data/ecommerce_review_dataset.csv")
shipper_data <- readr::read.csv("data/ecommerce_shipper_dataset.csv")
supplier_data <- readr::read.csv("data/ecommerce_supplier_dataset.csv")
payment_data <- readr::read.csv("data/ecommerce_payment_datatset.csv")
order_details_data <- readr::read.csv("data/ecommerce_order_details_dataset.csv")


all_files <- list.files("data/")

for (variable in all_files) {
  print(paste0("The file : ",variable, " has the following columns : "))
  this_filepath <- paste0("data/",variable)
  this_file_contents <- readr::read_csv(this_filepath)
  
  column_names <- colnames(this_file_contents)
  
  print(paste0(column_names))
}


# Number of rows and columns in the dataset
all_files <- list.files("data/")

for (variable in all_files) {
  this_filepath <- paste0("data/",variable)
  this_file_contents <- readr::read_csv(this_filepath)
  
  number_of_rows <- nrow(this_file_contents)
  number_of_columns <- ncol(this_file_contents)
  
  print(paste0("The file: ",variable,
              " has : ",
              format(number_of_rows,big.mark = ","),
              " rows and ",
              number_of_columns," columns"))
}

# Check if the first column of each file is a primary

for (variable in all_files) {
  this_filepath <- paste0("data/",variable)
  this_file_contents <- readr::read_csv(this_filepath)
  number_of_rows <- nrow(this_file_contents)
  
  print(paste0("Checking for: ",variable))
  
  print(paste0(" is ",nrow(unique(this_file_contents[,1]))==number_of_rows))
}


# Creating Database
# Load the library
library(RSQLite)

#setup the connection
connection <- RSQLite::dbConnect(RSQLite::SQLite(),"ecommerce.db")


#Creating table promotion
dbExecute(connection, 
"CREATE TABLE promotion (
  'promotion_id' VARCHAR(50) PRIMARY KEY, 
  'promotion_type' VARCHAR(50),
  'start_date' DATE,
  'end_date' DATE,
  'status' VARCHAR(50)
) ; " )


#Creating table shipper
dbExecute(connection, 
"CREATE TABLE shipper (
  'shipper_id' VARCHAR(50) PRIMARY KEY, 
  'shipper_name' VARCHAR(50),
  'service' VARCHAR(100),
  'fixed_price' DOUBLE,
  'cost_per_mile' DOUBLE
) ; " )


#Creating table address
dbExecute(connection, 
"CREATE TABLE address (
  'address_id' VARCHAR(50) PRIMARY KEY, 
  'house_number' INT,
  'house_name' VARCHAR(50),
  'street' VARCHAR(50),
  'city' VARCHAR(20) NOT NULL,
  'county' VARCHAR(20),
  'post_code' VARCHAR(10)
) ; " )


#creating table dimension
dbExecute(connection, 
"CREATE TABLE dimension (
  'dimension_id' VARCHAR(50) PRIMARY KEY, 
  'length' DOUBLE,
  'width' DOUBLE,
  'height' DOUBLE
) ; " )

#creating table buyer
dbExecute(connection, 
"CREATE TABLE 'buyer' (
  'buyer_id' VARCHAR(50) PRIMARY KEY, 
  'first_name' VARCHAR(250) NOT NULL,
  'last_name' VARCHAR(250),
  'date_of_birth' DATE,
  'address_id' VARCHAR(50) NOT NULL,
  FOREIGN KEY('address_id') REFERENCES address('address_id')
) ; " )


#creating table supplier
dbExecute(connection, 
"CREATE TABLE 'supplier' (
  'supplier_id' VARCHAR(50) PRIMARY KEY, 
  'supplier_type' VARCHAR(250),
  'contract_type' VARCHAR(250),
  'number_of_year_of_association' INT,
  'supplier_name' VARCHAR(50),
  'address_id' VARCHAR(50) NOT NULL,
  FOREIGN KEY('address_id') REFERENCES address('address_id')
) ; " )



#creating table product
dbExecute(connection, 
"CREATE TABLE 'product' (
  'product_id' VARCHAR(50) PRIMARY KEY, 
  'product_name' VARCHAR(250),
  'product_description' VARCHAR(250),
  'weight' DOUBLE,
  'price' DOUBLE,
  'promotion_id' VARCHAR(50) NOT NULL,
  'supplier_id' VARCHAR(50) NOT NULL,
  'dimension_id' VARCHAR(50) NOT NULL,
  FOREIGN KEY('promotion_id') REFERENCES promotion('promotion_id'),
  FOREIGN KEY('supplier_id') REFERENCES supplier('supplier_id'),
  FOREIGN KEY('dimension_id') REFERENCES dimension('dimension_id')
) ; " )


#creating table review
dbExecute(connection, 
"CREATE TABLE 'review' (
  'review_id' VARCHAR(50) PRIMARY KEY, 
  'buyer_id' VARCHAR(250) NOT NULL,
  'product_id' VARCHAR(250) NOT NULL,
  'review_score' DOUBLE,
  'review_description' VARCHAR(250),
  FOREIGN KEY('buyer_id') REFERENCES buyer('buyer_id'),
  FOREIGN KEY('product_id') REFERENCES product('product_id')

) ; " )


#creating table order_details
dbExecute(connection, 
"CREATE TABLE 'order_details' (
  'order_id' VARCHAR(50) PRIMARY KEY, 
  'product_id' VARCHAR(50) NOT NULL,
  'buyer_id' VARCHAR(50) NOT NULL,
  'supplier_id' VARCHAR(50) NOT NULL,
  'order_dates' DATE,
  'shipper_id' VARCHAR(50) NOT NULL,
  FOREIGN KEY('buyer_id') REFERENCES buyer('buyer_id'),
  FOREIGN KEY('product_id') REFERENCES product('product_id'),
  FOREIGN KEY('shipper_id') REFERENCES shipper('shipper_id'),
  FOREIGN KEY('supplier_id') REFERENCES supplier('supplier_id')
) ; " )


#creating table transaction
dbExecute(connection, 
"CREATE TABLE 'payment' (
  'payment_id' VARCHAR(50) PRIMARY KEY, 
  'payment_type' VARCHAR(250),
  'payment_amount' DOUBLE,
  'installment_number' INT,
  'order_id' VARCHAR(50) NOT NULL,
  FOREIGN KEY('order_id') REFERENCES order_details('order_id')
) ; " )

# Write to existing table - buyer
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