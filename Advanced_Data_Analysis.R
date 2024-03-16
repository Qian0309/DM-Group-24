# Load neccesary libraries
library(DBI)
library(RSQLite)
library(dplyr)
library(ggplot2)

## Access Existing Database
db_conn <- dbConnect(RSQLite::SQLite(), "Database/ecommerce.db")

## Visualisations
#1) Products With Highest Sales
top_5_products_sales <- dbGetQuery(db_conn, "SELECT
    od.product_id,
    p.product_name,
    ROUND(SUM(p.price),2) AS total_sales
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

ggplot(top_5_products_sales, aes(x = reorder(product_name, total_sales), y = total_sales)) +
  geom_col(width = 0.8) + 
  geom_text(aes(label = total_sales), vjust = 0.2, hjust=-0.1, position=position_dodge(width=0.9),size=3) +
  labs(x = "Product Name", y = "Total Sales") +
  theme_minimal() +
  scale_fill_viridis_d() +
  coord_flip() + 
  ggtitle("Top 5 Products by Sales") +
  theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5), legend.position = 'none')

## save image in folder "Updated_Visualisations"
this_filename_date <- as.character(Sys.Date())
# format the Sys.time() to show only hours and minutes 
this_filename_time <- as.character(format(Sys.time(), format = "%H_%M"))
ggsave(paste0("Updated_Visualisations/top5_product_sales_plot_",
              this_filename_date,"_",
              this_filename_time,".png"))


#2) Promotion for top products
promotion_data <- dbGetQuery(db_conn, "SELECT * FROM promotion;")
top_5_products_promotion <- dbGetQuery(db_conn, "SELECT
    od.product_id,
    p.product_name,
    p.promotion_id,
    pm.status,
    ROUND(SUM(p.price),2) AS total_sales
FROM
    order_details od
JOIN
    product p ON od.product_id = p.product_id
JOIN 
    promotion pm ON p.promotion_id = pm.promotion_id
GROUP BY
    od.product_id,
    p.product_name
ORDER BY
    total_sales DESC
LIMIT
    5;
")

ggplot(top_5_products_promotion, aes(x = reorder(product_name, total_sales), y = total_sales, fill = status)) +
  geom_col(width = 0.8) + 
  labs(title = "Top 5 Products by Sales", x = "Product Name", y = "Total Sales") +
  theme_minimal() +
  scale_fill_viridis_d() +
  coord_flip() + 
  scale_fill_discrete(name = "Promotion Status") + 
  ggtitle("Promotion Status of Top 5 products with respect to Sales") +
  theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5), legend.position = 'right') 

## save image in folder "Updated_Visualisations"
this_filename_date <- as.character(Sys.Date())
# format the Sys.time() to show only hours and minutes 
this_filename_time <- as.character(format(Sys.time(), format = "%H_%M"))
ggsave(paste0("Updated_Visualisations/top5_product_promos_plot_",
              this_filename_date,"_",
              this_filename_time,".png"))

#3) Number of days promotions are running
no_of_days_promotions <- mutate(promotion_data,
                         start_date = as.Date(start_date,format= "%d/%m/%Y"),
                         end_date = as.Date(end_date,format= "%d/%m/%Y"),
                         num_days_promotion = end_date - start_date) %>%
  filter(status == 'active')
no_of_days_promotions <- no_of_days_promotions %>%
  arrange(desc(num_days_promotion))

no_of_days_promotions <- no_of_days_promotions %>% 
  group_by(num_days_promotion) %>% 
  summarise(num_promotions = n()) %>%
  arrange((num_days_promotion))

ggplot(no_of_days_promotions, aes(x = num_days_promotion, y = num_promotions)) +
  geom_col(colors='') + 
  labs(x = "Number of days promotion is running", y = "Number of promotions") +
  theme_minimal() +
  scale_fill_viridis_d() +
  ggtitle("Number of Promotions running across each different duration") +
  theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5), legend.position = 'none') 

#4) Sales Trends
order_details <- dbGetQuery(db_conn,"SELECT * FROM order_details")
order_details <- mutate(order_details,
                        order_dates = as.Date(order_dates,format= "%d/%m/%Y"))
product <- dbGetQuery(db_conn,"SELECT * FROM product")


# Viewing the results
sales_trend_data <- inner_join(order_details, product, 
           by = c("product_id")) 

sales_trend_data <- sales_trend_data %>%
  group_by(order_dates) %>% 
  summarise(total_sales = sum(price))

ggplot(sales_trend_data, aes(x = order_dates, y = total_sales)) +
  geom_line() + 
  labs(x = "Date", y = "Sales") +
  theme_minimal() +
  scale_fill_viridis_d() +
  ggtitle("Sales Trend") +
  theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5), legend.position = 'none') 

#5) Products With Highest Review
top_5_products_reviews <- dbGetQuery(db_conn, "SELECT
    r.product_id,
    p.product_name,
    ROUND(AVG(r.review_score),2) AS avg_review_score
FROM
    review r
JOIN
    product p ON r.product_id = p.product_id
GROUP BY
    r.product_id,
    p.product_name
ORDER BY
    avg_review_score DESC
LIMIT
    5;
")

ggplot(top_5_products_reviews, aes(x = reorder(product_name, avg_review_score), y = avg_review_score)) +
  geom_col(show.legend = FALSE, width=0.8) + 
  geom_text(aes(label = avg_review_score), vjust = 0.2, hjust=-0., position=position_dodge(width=0.9),size=4) +
  coord_flip() + 
  labs(x = "Product Name", y = "Average Review Score") +
  theme_minimal() +
  scale_fill_viridis_d() + 
  ggtitle("Top 5 Products by Average Review Score") +
  theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5), legend.position = 'none') 

#6) Products With Lowest Review
lowest_5_products_reviews <- dbGetQuery(db_conn, "SELECT
    p.product_id,
    p.product_name,
    ROUND(AVG(r.review_score),2) AS average_review_score
FROM
    review r
JOIN
    product p ON r.product_id = p.product_id
GROUP BY
    p.product_id,
    p.product_name
ORDER BY
    average_review_score ASC
LIMIT
    5;
")

ggplot(lowest_5_products_reviews, aes(x = reorder(product_name, -average_review_score), y = average_review_score)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = average_review_score), vjust = 0.2, hjust=-0.3, position=position_dodge(width=0.9),size=4) +
  labs(x = "Product", y = "Average Review Score") +
  theme_minimal() +
  coord_flip() +
  ggtitle("Products with Lowest Review Scores") +
  theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5), legend.position = 'none')


#7) Cities With Lowest Review
lowest_5_city_reviews <- dbGetQuery(db_conn, "SELECT
    a.city,
    ROUND(AVG(r.review_score),2) AS average_review_score
FROM
    review r
JOIN
    buyer b ON r.buyer_id = b.buyer_id
JOIN
    address a ON b.address_id = a.address_id
GROUP BY
    a.city
ORDER BY
    average_review_score ASC
LIMIT
    5;
")

ggplot(lowest_5_city_reviews, aes(x = reorder(city, average_review_score), y = average_review_score)) +
  geom_bar(stat = "identity", width=0.7) +
  geom_text(aes(label = average_review_score), vjust = 0.2, hjust=-0.3, position=position_dodge(width=0.9),size=4) +
  labs(x = "City", y = "Average Review Score") +
  theme_minimal() +
  coord_flip() +
  ggtitle("Cities with Lowest Review Scores") +
  theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5), legend.position = 'none')

#Cities With Highest Review
highest_5_city_reviews <- dbGetQuery(db_conn, "SELECT
    a.city,
    ROUND(AVG(r.review_score),2) AS average_review_score
FROM
    review r
JOIN
    buyer b ON r.buyer_id = b.buyer_id
JOIN
    address a ON b.address_id = a.address_id
GROUP BY
    a.city
ORDER BY
    average_review_score DESC
LIMIT
    5;
")


ggplot(highest_5_city_reviews, aes(x = reorder(city, -average_review_score), y = average_review_score)) +
  geom_bar(stat = "identity", width=0.7) +
  geom_text(aes(label = average_review_score), vjust = 0.2, hjust=-0.3, position=position_dodge(width=0.9),size=4) +
  labs(x = "City", y = "Average Review Score") +
  theme_minimal() +
  coord_flip() +
  ggtitle("Cities with Highest Review Scores") +
  theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5), legend.position = 'none')


#9) Estimate the CLV based on historical transactions to identify your most valuable customers.
customer_lifetime_value <- dbGetQuery(db_conn, "SELECT
    buyer_id,
    ROUND(SUM(p.price),2) AS total_spent,
    COUNT(distinct order_id) AS number_of_orders,
    ROUND((SUM(p.price) / COUNT(distinct order_id)),2) AS average_order_value
FROM
    order_details od
JOIN
    product p ON od.product_id = p.product_id
GROUP BY
    buyer_id
ORDER BY
    total_spent DESC;
")


top_n_buyers <- head(customer_lifetime_value, 10) 
# Change 10 to the desired number of top buyers to display

ggplot(top_n_buyers, aes(x = reorder(buyer_id, -total_spent), y = total_spent)) +
  geom_bar(stat = "identity", width = 0.7) +
  labs(x = "Buyer ID", y = "Total Spent") +
  theme_minimal() +
  ggtitle("Top Buyers by Customer Lifetime Value") +
  theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5), legend.position = 'none')


#10) Effective of Promotions
effective_promotions <- dbGetQuery(db_conn, "SELECT
    promo.promotion_type,
    COUNT(*) AS number_of_sales,
    ROUND(SUM(prod.price),2) AS total_revenue
FROM
    order_details od
JOIN
    product prod ON od.product_id = prod.product_id
JOIN
    promotion promo ON prod.promotion_id = promo.promotion_id
WHERE
    promo.status = 'active'
GROUP BY
    promo.promotion_type
ORDER BY
    total_revenue DESC;
")
```

ggplot(effective_promotions, aes(x = reorder(promotion_type, -total_revenue), y = total_revenue)) +
  geom_bar(stat = "identity", width=0.7) +
  geom_text(aes(label = total_revenue), vjust = -0.2, hjust=0.5, position=position_dodge(width=0.9),size=4) +
  labs(x = "Promotion Type", y = "Total Revenue") +
  theme_minimal() +
  ggtitle("Effectiveness of Promotions") +
  theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5), legend.position = 'none')


#11) Most Reviewed Products
most_reviewed_products <- dbGetQuery(db_conn,"SELECT
    p.product_name,
    COUNT(*) AS review_count,
    ROUND(AVG(r.review_score),2) AS avg_review_score
FROM
    review r
JOIN
    product p ON r.product_id = p.product_id
GROUP BY
    p.product_name
ORDER BY
    review_count DESC
LIMIT 10;
")


ggplot(most_reviewed_products, aes(x = reorder(product_name, -review_count), y = review_count)) +
  geom_bar(stat = "identity", width=0.7) +
  geom_text(aes(label = review_count), vjust = -0.2, hjust=0.5, position=position_dodge(width=0.9),size=4) +
  labs(x = "Product Name", y = "Number of Reviews") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Most Reviewed Products") +
  theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5), legend.position = 'none')

#The high number of reviews for Touch Desk Lamp and Ultra-Light Smart Speaker indicates 
#strong customer engagement with these products. More customers are actively providing feedback 
#for these products, which can be valuable for understanding product satisfaction and areas for improvement.


#12) Cities With Highest Sales.
cities_highest_sales <- dbGetQuery(db_conn,"SELECT
    a.city,
    ROUND(SUM(p.price),2) AS total_sales
FROM
    order_details od
JOIN
    product p ON od.product_id = p.product_id
JOIN
    buyer b ON od.buyer_id = b.buyer_id
JOIN
    address a ON b.address_id = a.address_id
GROUP BY
    a.city
ORDER BY
    total_sales DESC
LIMIT
    5;
")

ggplot(cities_highest_sales, aes(x = city, y = total_sales)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = total_sales), vjust = -0.3, hjust=0.5, position=position_dodge(width=0.9),size=3) +
  labs(x = "City", y = "Total Sales") +
  theme_minimal() +
  ggtitle("City with Highest Sales") +
  theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5), legend.position = 'none')
