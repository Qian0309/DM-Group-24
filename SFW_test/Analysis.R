

install.packages(c("DBI", "RSQLite", "dplyr","ggplot2"))

library(DBI)
library(RSQLite)
library(dplyr)
library(ggplot2)

db_conn <- dbConnect(RSQLite::SQLite(), "database/ecommerce.db")

# Products With Highest Sales

top_5_products_sales <- dbGetQuery(db_conn, "SELECT
    od.product_id,
    p.product_name,
    SUM(p.price) AS total_sales
FROM
    order_details od
JOIN
    product p ON od.product_id = p.product_id
GROUP BY
    od.product_id,
    p.product_name
ORDER BY
    total_sales DESC
LIMIT
    5;
")

# Viewing the results
print(top_5_products_sales)


ggplot(top_5_products_sales, aes(x = reorder(product_name, total_sales), y = total_sales, fill = product_name)) +
  geom_col() + 
  labs(title = "Top 5 Products by Sales", x = "Product Name", y = "Total Sales") +
  theme_minimal() +
  scale_fill_viridis_d() +
  coord_flip() + 
  scale_fill_discrete(name = "Product Name") + 
  theme(legend.position = "none") 

## save image in folder "SFW_test"

this_filename_date <- as.character(Sys.Date())
# format the Sys.time() to show only hours and minutes 
this_filename_time <- as.character(format(Sys.time(), format = "%H_%M"))
ggsave(paste0("SFW_test/top5products_plot_",
              this_filename_date,"_",
              this_filename_time,".png"))

# Number of days promotions are running (Not Working)

no_of_days_promotions <- dbGetQuery(db_conn, "SELECT
    promotion_id,
    promotion_type,
    start_date,
    end_date,
    julianday(end_date) - julianday(start_date) AS promotion_duration_days
FROM
    promotion
ORDER BY
    promotion_duration_days DESC
LIMIT
    5;;
")

print(no_of_days_promotions)

