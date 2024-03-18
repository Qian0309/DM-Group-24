# Load neccesary libraries
library(RSQLite)
library(readr)
library(dplyr)
library(anytime)
library(DBI)
library(ggplot2)

## File Prefix and Suffix
# read files from 'data' folder
project_files <- list.files("New_Data/")
prefix <- "ecommerce_"
suffix <- "_dataset.csv"
project_files <- gsub(prefix,"",project_files)
project_files <- gsub(suffix,"",project_files)

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
## Id format checks for all primary keys
dataname_list <- list("address_data", "buyer_data", "dimension_data", "order_details_data", "payment_data",
                 "product_data", "promotion_data", "review_data", "shipper_data", "supplier_data")

idname_list <- list("address_id", "buyer_id", "dimension_id","order_id","payment_id","product_id",
                    "promotion_id", "review_id", "shipper_id", "supplier_id")

idformat_list <-  list("AD","BU","DM","OR","PD","PR",
                       "PM","RV","SH","SP")

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
    print(paste0("ID column ", id_col, " not found in ", dataname_list[[i]], "\n"))
  } 
  
  # Print the filtered data
    print(paste0("Checking for format for ", id_col, " in the dataframe ", dataname_list[[i]]))
    
    if (nrow(id_data)!=0){
    print(paste0(nrow(id_data), " row(s) is/are removed from ",dataname_list[[i]], " because of incorrect id format" ))
    
    print(paste0("The removed data is/are ", id_data[1]))
    assign(dataname_list[[i]], anti_join(datatemp, id_data, by = id_col))
    }   
    id_data <-  id_data[0, ]
}

## Data checks for the address 
# Getting the negative house number data
negative_data <- address_data %>% 
  filter(house_number < 0)


# Print the filtered data
print(paste0("Checking for incorrect data in the address "))
    
if (nrow(negative_data)!=0){
print(paste0(nrow(negative_data), " rows are removed from address because of incorrect data" ))
  print(paste0("The removed data are ", negative_data))
}

# Removing it from the address data
address_data <- anti_join(address_data, negative_data, by = "address_id")

# Removing the same from the other tables having address as a foreign key
buyer_data <- anti_join(buyer_data, negative_data, by = "address_id")
supplier_data <- anti_join(supplier_data, negative_data, by = "address_id")

## Data checks for the buyer 
# Convert character dates to Date data type
buyer_data$date_of_birth <- anydate(buyer_data$date_of_birth)

# Filter for dates later than the current date
current_date <- Sys.Date()
negative_data <- buyer_data[buyer_data$date_of_birth > current_date, ]

# Print the filtered data
print(paste0("Checking for incorrect data in the buyer "))
    
if (nrow(negative_data)!=0){
print(paste0(nrow(negative_data), " rows are removed from buyer because of incorrect data" ))
  print(paste0("The removed data are ", negative_data))
}

# Removing it from the buyer data
buyer_data <- anti_join(buyer_data, negative_data, by = "buyer_id")

# Removing the same from the other tables having address as a foreign key
order_details_data <- anti_join(order_details_data, negative_data, by = "buyer_id")
review_data <- anti_join(review_data, negative_data, by = "buyer_id")

## Data checks for the dimension 
# Getting the negative and over the upper limit dimensions data
negative_data <- dimension_data %>% 
  filter(length < 0 | length > 150 |
           width < 0 | width > 150 |
           height < 0 | height > 150)



# Print the filtered data
print(paste0("Checking for incorrect data in the dimension"))
    
if (nrow(negative_data)!=0){
print(paste0(nrow(negative_data), " rows are removed from dimension because of incorrect data" ))
  print(paste0("The removed data are ", negative_data))
}

# Removing it from all tables containing dimension id
dimension_data <- anti_join(dimension_data, negative_data, by = "dimension_id")
product_data <- anti_join(product_data, negative_data, by = "dimension_id")

## Data checks for the order details 
# Convert character dates to Date data type
order_details_data$order_dates <- anydate(order_details_data$order_dates)

# Filter for dates later than the current date
current_date <- Sys.Date()
negative_data <- order_details_data[order_details_data$order_dates > current_date, ]

# Print the filtered data
print(paste0("Checking for incorrect data in the order details "))
    
if (nrow(negative_data)!=0){
print(paste0(nrow(negative_data), " rows are removed from order because of incorrect data" ))
  print(paste0("The removed data are ", negative_data))
}

# Removing it from the order data
order_details_data <- anti_join(order_details_data, negative_data, by = "order_id")

# Removing the same from the other tables having address as a foreign key
payment_data <- anti_join(payment_data, negative_data, by = "order_id")

## Data checks for the payment data 
# Getting the negative payment data
negative_data <- payment_data %>% 
  filter(payment_amount < 0 | installment_number < 0 )

# Print the filtered data
print(paste0("Checking for incorrect data in the payment details"))
    
if (nrow(negative_data)!=0){
print(paste0(nrow(negative_data), " rows are removed from payment data because of incorrect data" ))
  print(paste0("The removed data are ", negative_data))
}

# Removing it from all tables containing payment id
payment_data <- anti_join(payment_data, negative_data, by = "payment_id")

## Data checks for the product data 
# Getting the negative and out of limits product data
negative_data <- product_data %>% 
  filter(price < 0 | weight < 0 | weight > 100)

# Print the filtered data
print(paste0("Checking for incorrect data in the product data"))
    
if (nrow(negative_data)!=0){
print(paste0(nrow(negative_data), " rows are removed from product data because of incorrect data" ))
  print(paste0("The removed data are ", negative_data))
}

# Removing it from all tables containing product id
product_data <- anti_join(product_data, negative_data, by = "product_id")

## Data checks for the promotion data 
# Convert character dates to Date data type
promotion_data$start_date <- anydate(promotion_data$start_date)
promotion_data$end_date <- anydate(promotion_data$end_date)

# Filter for end dates before start dates
negative_data <- promotion_data[promotion_data$start_date > promotion_data$end_date, ]

# Print the filtered data
print(paste0("Checking for incorrect data in the promotion details "))
    
if (nrow(negative_data)!=0){
print(paste0(nrow(negative_data), " rows are removed from promotion because of incorrect data" ))
  print(paste0("The removed data are ", negative_data))
}

# Removing it from all tables containing product id
promotion_data <- anti_join(promotion_data, negative_data, by = "promotion_id")
product_data <- anti_join(product_data, negative_data, by = "promotion_id")

## Data checks for the review data 
# Getting the negative and out of limits review data
negative_data <- review_data %>% 
  filter(review_score < 0 | review_score >10 )

# Print the filtered data
print(paste0("Checking for incorrect data in the review data"))
    
if (nrow(negative_data)!=0){
print(paste0(nrow(negative_data), " rows are removed from review data because of incorrect data" ))
  print(paste0("The removed data are ", negative_data))
}

# Removing it from all tables containing review id
review_data <- anti_join(review_data, negative_data, by = "review_id")

## Data checks for the shipper data 
# Getting the negative and out of limits shipper data
negative_data <- shipper_data %>% 
  filter(fixed_price < 0 | cost_per_mile < 0 )

# Print the filtered data
print(paste0("Checking for incorrect data in the shipper data"))
    
if (nrow(negative_data)!=0){
print(paste0(nrow(negative_data), " rows are removed from shipper data because of incorrect data" ))
  print(paste0("The removed data are ", negative_data))
}

# Removing it from all tables containing shipper id
shipper_data <- anti_join(shipper_data, negative_data, by = "shipper_id")

## Data checks for the supplier data 
# Getting the negative and out of limits supplier data
negative_data <- supplier_data %>% 
  filter(number_of_year_of_association < 0 )

# Print the filtered data
print(paste0("Checking for incorrect data in the supplier data"))
    
if (nrow(negative_data)!=0){
print(paste0(nrow(negative_data), " rows are removed from supplier data because of incorrect data" ))
  print(paste0("The removed data are ", negative_data))
}

# Removing it from all tables containing shipper id
supplier_data <- anti_join(supplier_data, negative_data, by = "supplier_id")

### Referential Integrity Checks
print("Referential Integrity Checks")

# PRODUCT ENTITY
# Relationship between Product and Promotion : 1 Promotion applied on N Products
if(length(setdiff(unique(product_data$promotion_id),
                  unique(promotion_data$promotion_id)))>0){
  print("Referential integrity check violated between Product and Promotion table")
  to_remove_ids <- setdiff(unique(product_data$promotion_id),
                  unique(promotion_data$promotion_id))
  print(paste0("ID(s) removed : ", to_remove_ids))
  product_data <- product_data %>% filter(!promotion_id %in% to_remove_ids)
  
  
}
# Relationship between Product and Supplier : 1 Supplier sells N Products
if(length(setdiff(unique(product_data$supplier_id),
                  unique(supplier_data$supplier_id)))>0){
  print("Referential integrity check violated between Product and Supplier table")
  to_remove_ids <- setdiff(unique(product_data$supplier_id),
                  unique(supplier_data$supplier_id))
  print(paste0("ID(s) removed : ", to_remove_ids))
  product_data <- product_data %>% filter(!supplier_id %in% to_remove_ids)
  
}
# Relationship between Product and Dimension : 1 Product has 1 Dimension
if(length(setdiff(unique(product_data$dimension_id),
                  unique(dimension_data$dimension_id)))>0){
  print("Referential integrity check violated between Product and Dimension table")
  to_remove_ids <- setdiff(unique(product_data$dimension_id),
                  unique(dimension_data$dimension_id))
  print(paste0("ID(s) removed : ", to_remove_ids))
  product_data <- product_data %>% filter(!dimension_id %in% to_remove_ids)
  
}

# BUYER ENTITY
# Relationship between Buyer and Address : 1 Buyer has 1 Address
if(length(setdiff(unique(buyer_data$address_id),
                  unique(address_data$address_id)))>0){
  print("Referential integrity check violated between Buyer and Address table")
  to_remove_ids <- setdiff(unique(buyer_data$address_id),
                  unique(address_data$address_id))
  print(paste0("ID(s) removed : ", to_remove_ids))
  buyer_data <- buyer_data %>% filter(!address_id %in% to_remove_ids)
  
}

# SUPPLIER ENTITY
# Relationship between Supplier and Address : 1 Supplier has 1 Address
if(length(setdiff(unique(supplier_data$address_id),
                  unique(address_data$address_id)))>0){
  print("Referential integrity check violated between Supplier and Address table")
  to_remove_ids <- setdiff(unique(supplier_data$address_id),
                  unique(address_data$address_id))
  print(paste0("ID(s) removed : ", to_remove_ids))
  supplier_data <- supplier_data %>% filter(!address_id %in% to_remove_ids)
  
}

# REVIEW ENTITY
# Relationship between Product and Review : 1 Product has n Reviews
if(length(setdiff(unique(review_data$product_id),
                  unique(product_data$product_id)))>0){
  print("Referential integrity check violated between Review and Product table")
  to_remove_ids <- setdiff(unique(review_data$product_id),
                  unique(product_data$product_id))
  print(paste0("ID(s) removed : ", to_remove_ids))
  review_data <- review_data %>% filter(!product_id %in% to_remove_ids)
  
}
# Relationship between Buyer and Review : 1 Buyer gives n Reviews
if(length(setdiff(unique(review_data$buyer_id),
                  unique(buyer_data$buyer_id)))>0){
  print("Referential integrity check violated between Review and Buyer table")
  to_remove_ids <- setdiff(unique(review_data$buyer_id),
                  unique(buyer_data$buyer_id))
  print(paste0("ID(s) removed : ", to_remove_ids))
  review_data <- review_data %>% filter(!buyer_id %in% to_remove_ids)
  
}

# PAYMENT ENTITY
# Relationship between Payment and Order : 1 Order has n Payments
if(length(setdiff(unique(payment_data$order_id),
                  unique(order_details_data$order_id)))>0){
  print("Referential integrity check violated between Payment and Order Details table")
  to_remove_ids <- setdiff(unique(payment_data$order_id),
                  unique(order_details_data$order_id))
  print(paste0("ID(s) removed : ", to_remove_ids))
  payment_data <- payment_data %>% filter(!order_id %in% to_remove_ids)
  
}

# ORDER DETAILS ENTITY
# Relationship between Order and Buyer : 1 Buyer has n Orders
if(length(setdiff(unique(order_details_data$buyer_id),
                  unique(buyer_data$buyer_id)))>0){
  print("Referential integrity check violated between Order Details and Buyer table")
  to_remove_ids <- setdiff(unique(order_details_data$buyer_id),
                  unique(buyer_data$buyer_id))
  print(paste0("ID(s) removed : ", to_remove_ids))
  order_details_data <- order_details_data %>% filter(!buyer_id %in% to_remove_ids)
  
}
# Relationship between Order and Product : 1 Product present in n Orders
if(length(setdiff(unique(order_details_data$product_id),
                  unique(product_data$product_id)))>0){
  print("Referential integrity check violated between Order Details and Product table")
  to_remove_ids <- setdiff(unique(order_details_data$product_id),
                  unique(product_data$product_id))
  print(paste0("ID(s) removed : ", to_remove_ids))
  order_details_data <- order_details_data %>% filter(!product_id %in% to_remove_ids)
  
}
# Relationship between Order and Shipper : 1 Shipper satisfies n Orders
if(length(setdiff(unique(order_details_data$shipper_id),
                  unique(shipper_data$shipper_id)))>0){
  print("Referential integrity check violated between Order Details and Shipper table")
  to_remove_ids <- setdiff(unique(order_details_data$shipper_id),
                  unique(shipper_data$shipper_id))
  print(paste0("ID(s) removed : ", to_remove_ids))
  order_details_data <- order_details_data %>% filter(!shipper_id %in% to_remove_ids)
  
}
# Relationship between Order and Supplier : 1 Supplier has n Orders
if(length(setdiff(unique(order_details_data$supplier_id),
                  unique(supplier_data$supplier_id)))>0){
  print("Referential integrity check violated between Order Details and Supplier table")
  to_remove_ids <- setdiff(unique(order_details_data$supplier_id),
                  unique(supplier_data$supplier_id))
  print(paste0("ID(s) removed : ", to_remove_ids))
  order_details_data <- order_details_data %>% filter(!supplier_data %in% to_remove_ids)
  
}

## Access Existing Database
connection <- dbConnect(RSQLite::SQLite(), "Database/ecommerce.db")


## Checking for duplicates
# Get the list of table names in the database
table_names <- dbListTables(connection)

# Create an empty list to store data frames
dataframes <- list()

# Loop through each table and fetch its data into R data frames
for (table_name in table_names) {
  # Construct the SQL query to select all data from the table
  query <- paste("SELECT * FROM", table_name)
  
  # Fetch the data into a data frame
  dataframes[[table_name]] <- dbGetQuery(connection, query)
}

# Removing all the duplicates from the newly uploaded data
for (i in seq_along(dataname_list)) {
  # Load data file
  datatemp <- get(dataname_list[[i]])
  
  # Getting the primary keys
  id_col <- idname_list[[i]]
  
  # Removing the duplicates from the newly updated data
  assign(dataname_list[[i]], anti_join(datatemp, dataframes[[i]], by = id_col))
}

## Writing data to Existing Database tables
# Write to existing table
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

## Access Existing Database
db_conn <- dbConnect(RSQLite::SQLite(), "Database/ecommerce.db")


## Basic Data Analysis
address_table <- dbGetQuery(db_conn,"SELECT * FROM address")
buyer_table <- dbGetQuery(db_conn,"SELECT * FROM buyer")
dimension_table <- dbGetQuery(db_conn,"SELECT * FROM dimension")
payment_table <- dbGetQuery(db_conn,"SELECT * FROM payment")
product_table <- dbGetQuery(db_conn,"SELECT * FROM product")
promotion_table <- dbGetQuery(db_conn,"SELECT * FROM promotion")
review_table <- dbGetQuery(db_conn,"SELECT * FROM review")
shipper_table <- dbGetQuery(db_conn,"SELECT * FROM shipper")
supplier_table <- dbGetQuery(db_conn,"SELECT * FROM supplier")
order_details_table <- dbGetQuery(db_conn,"SELECT * FROM order_details")

dataname_list <- list("address_table", "buyer_table", "dimension_table", "payment_table", "product_table", "promotion_table", "review_table", "shipper_table", "supplier_table", "order_details_table")


# Loop through each data file
for (i in seq_along(dataname_list)) {
  # Load data file
  print(paste0("Summary Statistics of : ",dataname_list[[i]]))
  print(paste0(summary(get(dataname_list[[i]]))))
}

### Distribution of Total Payment Amount across orders
payment_table <- dbGetQuery(db_conn,
                            "SELECT
    p.order_id,
    ROUND(SUM(p.payment_amount),2) AS total_payment_amount
FROM
    payment p
GROUP BY
    p.order_id;
")


# Plot
histogram_plot <- ggplot(payment_table, aes(total_payment_amount)) +
  geom_histogram(aes(y = after_stat(density)), binwidth=50) +
  geom_density() +
  ggtitle("Histogram visualising Distribution of 'Payment Amount' attribute") +
  labs(x="Payment Amount", y="Density") +
  theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5))
histogram_plot

### Product table : Distribution of Weight and Price of products
histogram_plot <- ggplot(product_table, aes(weight)) +
  geom_histogram(aes(y = after_stat(density))) +
  geom_density() +
  ggtitle("Histogram visualising Distribution of 'Weight' of Products") +
  labs(x="Payment Amount", y="Density") +
  theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5))
histogram_plot

histogram_plot <- ggplot(product_table, aes(price)) +
  geom_histogram(aes(y = after_stat(density))) +
  geom_density() +
  ggtitle("Histogram visualising Distribution of 'Price' attribute") +
  labs(x="Price", y="Density") +
  theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5))
histogram_plot

### Distribution of Review Score across products
# Plot
histogram_plot <- ggplot(review_table, aes(review_score)) +
  geom_histogram(aes(y = after_stat(density)), bins=20) +
  geom_density() +
  ggtitle("Histogram visualising Distribution of 'Review Score' attribute") +
  labs(x="Review Score", y="Density") +
  theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5))
histogram_plot

### Distribution of Fixed Cost and Cost per mile of Shippers
histogram_plot <- ggplot(shipper_table, aes(fixed_price)) +
  geom_histogram(aes(y = after_stat(density)), bins=20) +
  geom_density() +
  ggtitle("Histogram visualising Distribution of 'Fixed Cost' attribute") +
  labs(x="Fixed Cost", y="Density") +
  theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5))
histogram_plot

histogram_plot <- ggplot(shipper_table, aes(cost_per_mile)) +
  geom_histogram(aes(y = after_stat(density)), bins=20) +
  geom_density() +
  ggtitle("Histogram visualising Distribution of 'Cost per mile' attribute") +
  labs(x="Cost per mile", y="Density") +
  theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5))
histogram_plot

### Distribution of Number of Years of association with Suppliers
# Plot
histogram_plot <- ggplot(supplier_table, aes(number_of_year_of_association)) +
  geom_histogram(aes(y = after_stat(density)), bins=50) +
  geom_density() +
  ggtitle("Histogram visualising Distribution of 'Number of Years of Association' attribute") +
  labs(x="Number of Years of Association", y="Density") +
  theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5))
histogram_plot