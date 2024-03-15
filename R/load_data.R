library(readr)
library(RSQLite)

dimension_data <- readr::read_csv("/cloud/project/Assignment/data/dimension.csv")
#customer_data <- readr::read_csv("/cloud/project/Assignment/data/buyer.csv")
#transaction_data <- readr::read_csv("/cloud/project/Assignment/data/transaction.csv")
#supplier_data <- readr::read_csv("/cloud/project/Assignment/data/supplier.csv")
#review_data <- readr::read_csv("/cloud/project/Assignment/data/review.csv")
#promotion_data <- readr::read_csv("/cloud/project/Assignment/data/promotion.csv")
#product_data <- readr::read_csv("/cloud/project/Assignment/data/product.csv")
#order_details_data <- readr::read_csv("/cloud/project/Assignment/data/order_details.csv")
#address_data <- readr::read_csv("/cloud/project/Assignment/data/address.csv")

#create database
my_connection <- RSQLite::dbConnect(RSQLite::SQLite(), "/cloud/project/database/database.db")

#
RSQLite::dbWriteTable(my_connection, "Dimension_ID", dimension_data)

