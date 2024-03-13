library(readr)
library(RSQLite)

dimension_data <- readr::read_csv("/cloud/project/Assignment/data/dimension.csv")
my_connection <- RSQLite::dbConnect(RSQLite::SQLite(), "/cloud/project/database/database.db")
RSQLite::dbWriteTable(my_connection, "Dimension_ID", dimension_data)
