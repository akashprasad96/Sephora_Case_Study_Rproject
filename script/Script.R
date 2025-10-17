#Environment reset (run first)
rm(list = ls())            # clear objects
graphics.off()             # close plots
cat("\014")                # clear console

#Load Libraries
library(tidyverse)
library(janitor)
library(ggplot2)
library(forcats)
library(scales)

#'#########################################
#'# Part I - Exploring
#'#########################################

# Step 1: Read the product file
product <- read_csv("product_info.csv")
colnames(product)
summary(product)
glimpse(select(product, where(is.character)))


# Step 1 – Drop the high-NA numeric columns
product <- select(product, -child_max_price, -child_min_price, -sale_price_usd, -value_price_usd)

# Step 2 - Removing unwanted character fields
product <- select(product,-variation_type, -variation_value, -highlights,
-variation_desc, -ingredients, -secondary_category, -tertiary_category)
nrow(product)


# Step 3 - Standardizing Size column into ounces into new column

#Triming data after ounces
product$size_oz <- tolower(trimws(sub("/.*", "", product$size)))

#Converting Size column to standardize numeric metric
product$size_oz <- case_when(
  # Case 1: contains 'x' → multiply the first two numbers
  str_detect(product$size_oz, "x") ~ {
    nums <- str_extract_all(product$size_oz, "\\d*\\.?\\d+")
    sapply(nums, function(v) {
      if (length(v) >= 2) as.numeric(v[1]) * as.numeric(v[2]) else as.numeric(v[1])
    })},
  # Case 2: contains 'g' → extract first number and convert grams to oz
  str_detect(product$size_oz, "\\bg\\b") ~ {
    val <- as.numeric(str_extract(product$size_oz, "\\d*\\.?\\d+"))
    val / 28.3495},
  # Case 3: contains 'ml' → extract first number and convert ml to fl oz
  str_detect(product$size_oz, "\\bml\\b") ~ {
    val <- as.numeric(str_extract(product$size_oz, "\\d*\\.?\\d+"))
    val / 29.5735},
  # Default: just extract the first numeric (assume already in oz)
  TRUE ~ as.numeric(str_extract(product$size_oz, "\\d*\\.?\\d+"))
)

# Step 4 - Dropping Rows with NA
product <- drop_na(product)
nrow(product)

# Step 5 - Removing duplicates for product name
product <-product[!duplicated(product$product_name), ]

# Step 6 - Visualizations from raw data

# Q1. Which product categories have the highest representation?
#
#Goal: Identify which categories dominate the dataset based on product count.
#This helps reveal the main areas of focus (e.g., makeup, skincare, haircare)
#and sets context for later analyses like pricing and popularity.

ggplot(product, aes(fct_infreq(primary_category))) +
  geom_bar(fill = "mediumpurple", alpha = 0.8) +
  coord_flip() +
  labs(
    title = "Product Count by Category",
    x = "Product Category",
    y = "Number of Products"
  ) +
  theme_minimal()

#The catalog is dominated by skincare and makeup products, followed by fragrance and hair items.
#Categories like men's, mini size, and tools & brushes form a smaller share.
#This indicates that Sephora’s product lineup focuses mainly on everyday beauty and personal-care segments.
#


# Q2. Does a higher product price lead to better ratings?
#
#Goal: Test whether premium-priced products are associated with higher customer satisfaction.
#This helps examine if perceived value aligns with actual review sentiment.

ggplot(filter(product, reviews >= 122), aes(price_usd, rating)) +
  geom_point(alpha = 0.4, color = "tomato") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(
    title = "Relationship Between Price and Rating",
    x = "Price (USD)",
    y = "Rating"
  ) +
  theme_minimal()

#The scatter plot shows a slight upward trend,but ratings remain consistently high across all price ranges.
#This suggests that customer satisfaction is largely independent of price.
#Lower-priced products perform just as well as premium ones.
#


# Q3. Are limited-edition products more loved by customers?
#
#Goal: Compare customer engagement between limited-edition and regular products.
#This helps determine whether exclusivity increases popularity and brand appeal.

ggplot(product, aes(x = as.factor(limited_edition), y = loves_count)) +
  geom_boxplot(fill = "skyblue", alpha = 0.6, outlier.color = "darkblue") +
  scale_y_log10() +
  labs(
    title = "Customer Love for Limited vs Regular Products",
    x = "Limited Edition (0 = No, 1 = Yes)",
    y = "Loves Count (log scale)"
  ) +
  theme_minimal()

#The boxplot shows that regular (non-limited) products have a slightly higher median loves count,
#indicating that popularity is largely independent of exclusivity, with wide variation across both groups.
#


# Q4. Do Sephora-exclusive products receive higher customer ratings?
#
#Goal: Compare customer satisfaction levels between Sephora-exclusive
#and non-exclusive products.
#This helps assess whether exclusive availability on Sephora
#correlates with higher perceived quality or better customer feedback.

ggplot(filter(product, reviews >= 122), aes(x = as.factor(sephora_exclusive), y = rating)) +
  geom_boxplot(fill = "lightseagreen", alpha = 0.6, outlier.color = "darkgreen") +
  labs(
    title = "Ratings Distribution: Sephora-Exclusive vs Non-Exclusive Products",
    x = "Sephora Exclusive (0 = No, 1 = Yes)",
    y = "Customer Rating"
  ) +
  theme_minimal()

# The boxplot shows nearly identical rating distributions for Sephora-exclusive and non-exclusive products.
# This suggests that exclusivity does not significantly affect customer satisfaction, as both groups maintain similar ratings. 
#

#'#########################################
#'# Part II - Expanding
#'#########################################


# 1️⃣Brand Popularity and Satisfaction

#Step 1: Filter data for reliable reviews
reviewed_product <- product %>%
  filter(reviews >= 122)

#Step 2: Summarize by brand and compute average rating + total loves
popular_brands <- reviewed_product %>%
  group_by(brand_name) %>%
  summarise(
    avg_rating = mean(rating, na.rm = TRUE),
    total_loves = sum(loves_count, na.rm = TRUE)
  ) %>%
  arrange(desc(total_loves)) %>%
  slice(1:10)

#Step 3: Plot top 10 brands by love count
ggplot(popular_brands, aes(x = reorder(brand_name, total_loves), y = total_loves, fill = avg_rating)) +
  geom_col() +
  coord_flip() +
  scale_fill_gradient(low = "#ff9aa2", high = "#660033", guide = "colorbar") +  # reversed: high rating = darker
  labs(
    title = "Top 10 Brands by Love Count (Reviews ≥ 122)",
    x = "Brand Name", y = "Total Love Count", fill = "Avg Rating"
  ) +
  theme_minimal()

#Insight: While Sephora Collection, The Ordinary, and Fenty Beauty are the most popular.
#Average ratings show that brand popularity doesn’t necessarily translate to higher customer satisfaction.

# 2️⃣Stock Status Analysis

pal_pink <- "#f9a3b1"   # In Stock
pal_plum <- "#581845"   # Out of Stock
pal_grey <- "#bfbfbf"   # Unknown

## Limited Edition → Out Of Stock %
limited_df <- product %>% filter(limited_edition == 1)
limited_total <- nrow(limited_df)

limited_pie <- limited_df %>%
  mutate(stock_label = case_when(
    out_of_stock == 1 ~ "Out of Stock",
    out_of_stock == 0 ~ "In Stock",
    TRUE ~ "Unknown"
  )) %>%
  count(stock_label, name = "n") %>%
  mutate(prop = n / sum(n))

#compute OOS rate for inline use
le_oos_rate <- limited_pie %>%
  filter(stock_label == "Out of Stock") %>%
  summarise(rate = sum(prop)) %>%
  pull(rate)

ggplot(limited_pie, aes(x = "", y = prop, fill = stock_label)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c(
    "In Stock" = pal_pink,
    "Out of Stock" = pal_plum,
    "Unknown" = pal_grey
  )) +
  labs(
    title = "Limited Edition Products: Stock Status",
    subtitle = paste("Total limited edition items:", comma(limited_total)),
    x = NULL, y = NULL, fill = NULL
  ) +
  geom_text(aes(label = paste0(percent(prop, accuracy = 0.1))),
            position = position_stack(vjust = 0.5),
            size = 5.2,
            color = "white",
            fontface = "bold"
            ) +
  theme_void() +
  theme(plot.title = element_text(face = "bold"))

## Sephora Exclusive → Out Of Stock %

exclusive_df <- product %>% filter(sephora_exclusive == 1)
exclusive_total <- nrow(exclusive_df)

exclusive_pie <- exclusive_df %>%
  mutate(stock_label = case_when(
    out_of_stock == 1 ~ "Out of Stock",
    out_of_stock == 0 ~ "In Stock",
    TRUE ~ "Unknown"
  )) %>%
  count(stock_label, name = "n") %>%
  mutate(prop = n / sum(n))

#compute OOS rate for inline use
ex_oos_rate <- exclusive_pie %>%
  filter(stock_label == "Out of Stock") %>%
  summarise(rate = sum(prop)) %>%
  pull(rate)

ggplot(exclusive_pie, aes(x = "", y = prop, fill = stock_label)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c(
    "In Stock" = pal_pink,
    "Out of Stock" = pal_plum,
    "Unknown" = pal_grey
  )) +
  labs(
    title = "Sephora Exclusive Products: Stock Status",
    subtitle = paste("Total exclusive items:", comma(exclusive_total)),
    x = NULL, y = NULL, fill = NULL
  ) +
  geom_text(aes(label = paste0(percent(prop, accuracy = 0.1))),
            position = position_stack(vjust = 0.25),
            size = 5.2,
            color = "white",
            fontface = "bold" ) +
  theme_void() +
  theme(plot.title = element_text(face = "bold"))

#Insight: Sephora-exclusive products show strong availability, 
#while limited-edition items (≈12% out of stock) could benefit from improved inventory focus to ensure better accessibility.


# 3️⃣E-Commerce Product Sizing

#Compute upper bound to identify outliers
Q1 <- quantile(product$size_oz, 0.25, na.rm = TRUE)
Q3 <- quantile(product$size_oz, 0.75, na.rm = TRUE)
IQR_val <- Q3 - Q1
upper_limit <- Q3 + 1.5 * IQR_val

#Filter to exclude extreme outliers
sephora_trimmed <- product %>%
  filter(size_oz <= upper_limit)

ggplot(sephora_trimmed, aes(x = factor(online_only), y = size_oz, fill = factor(online_only))) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  scale_fill_manual(values = c("0" = "#f9a3b1", "1" = "#581845")) +
  labs(
    title = "Online Availability vs Product Size (Outliers Removed)",
    x = "Online Only (0 = No, 1 = Yes)",
    y = "Product Size (oz)",
    fill = "Online Only"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

#Insight: Customers prefer or access bigger-sized products through Sephora’s online platform compared to in-store options.