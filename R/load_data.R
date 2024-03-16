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


address_data <- readr::read_csv("./data/ecommerce_address_dataset.csv")
buyer_data <- readr::read_csv("./data/ecommerce_buyer_dataset.csv")
dimension_data <- readr::read_csv("./data/ecommerce_dimension_dataset.csv")
product_data <- readr::read_csv("./data/ecommerce_product_dataset.csv")
promotion_data <- readr::read_csv("./data/ecommerce_promotion_dataset.csv")
review_data <- readr::read_csv("./data/ecommerce_review_dataset.csv")
shipper_data <- readr::read_csv("./data/ecommerce_shipper_dataset.csv")
supplier_data <- readr::read_csv("./data/ecommerce_supplier_dataset.csv")
payment_data <- readr::read_csv("./data/ecommerce_payment_datatset.csv")
order_details_data <- readr::read_csv("./data/ecommerce_order_details_dataset.csv")

#setup the connection
connection <- RSQLite::dbConnect(RSQLite::SQLite(),"./database/ecommerce.db")


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