# slice cohorts - workspace

library(tidyverse)
library(lubridate)


df <- read.csv('data/orders.csv')

str(df)
head(df)

# 'data.frame':	42198 obs. of  11 variables:
# $ order_id        : int  47433960 47434359 47437194 47442942 47443107 47446248 47446272 47454681 47456028 47456199 ...
# $ source          : chr  "iosapp" "iosapp" "partner_website" "partner_website" ...
# $ date_purchased  : chr  "2019-03-13T08:02:07.000Z" "2019-03-13T08:11:38.000Z" "2019-03-13T09:07:31.000Z" "2019-03-13T11:01:04.000Z" ...
# $ shipping_type   : chr  "Delivery" "Delivery" "Delivery" "Delivery" ...
# $ payment_method  : chr  "credit" "credit" "cash" "credit" ...
# $ promo_value     : int  0 0 0 0 0 0 0 0 0 0 ...
# $ restaurant_total: num  15.5 24.8 43.2 27.9 23.1 ...
# $ customer        : num  1.33e+10 1.56e+10 2.57e+10 2.54e+10 1.85e+10 ...
# $ shops_id        : int  24597 64392 33699 19002 34911 261 24807 13989 44211 43320 ...
# $ state           : chr  "MD" "NY" "NJ" "WI" ...
# $ postal_code     : int  21162 12944 8007 53235 2122 10301 94612 21234 12570 10541 ...

summary(df)

# some observations:
# promo_value from 0.0 to 5.0
# restaurant_total from -3.0 to 1142.85   .. interesting there are negative values
# customer has five NA values .. drop these rows?

###
### MISSING VALUES
###

# apply functions to check for NA, NULL and empty values across all columns, show which are true
df %>% 
  apply(2,function(c) any(is.na(c)|is.null(c)|is.nan(c)|nchar(c)==0)) %>% 
  which()

# our suspects are date_purchased, customer and state

# date_purchased
df_na_date_purchased <- df %>% 
  filter(is.na(date_purchased)|
           is.null(date_purchased)|
           is.nan(date_purchased)|
           nchar(date_purchased)==0)

# 9 rows, no way to impute, drop them
df<- df %>% 
  filter(!(is.na(date_purchased)|
           is.null(date_purchased)|
           is.nan(date_purchased)|
           nchar(date_purchased)==0))

# customer 
df_na_customer <- df %>% 
  filter(is.na(customer)|
           is.null(customer)|
           is.nan(customer)|
           nchar(customer)==0)

# 5 rows, no way to impute, drop them
df <- df %>% 
  filter(!(is.na(customer)|
           is.null(customer)|
           is.nan(customer)|
           nchar(customer)==0))

# state
df_na_state <- df %>% 
  filter(is.na(state)|
           is.null(state)|
           is.nan(state)|
           nchar(state)==0)

# 59 rows, all have postal codes so we'll impute

# keep rows that have state
df_has_state <- df %>% 
  filter(!(is.na(state)|
           is.null(state)|
           is.nan(state)|
           nchar(state)==0))

# get zip code data from simplemaps.com/data/us-zips
zips <- read.csv('data/uszips.csv') %>% 
  select(c('zip','state_id'))

# inner join and discard mismatches, copy values to state column and drop state_id column
df_join_state <- inner_join(df_na_state, zips, by=c('postal_code'='zip')) %>% 
  mutate(state=state_id) %>% 
  select(!state_id)

# append joined to main df
df<- union_all(df_has_state, df_join_state)

# quick check
df %>% 
  apply(2,function(c) any(is.na(c)|is.null(c)|is.nan(c)|nchar(c)==0)) %>% 
  which()

# all good


###
### DATE HANDLING
###

# convert date_purchased into a Date.  use new column and keep original for timestamp info.
df <- mutate(df,order_date=as.Date(date_purchased))

# order_month
df <- mutate(df, order_month=floor_date(order_date,'month'))

# order_week (default begins Sundays.  Do a +1 for Mondays.)
df <- mutate(df, order_week = floor_date(order_date,'week'))


###
### EDA
### 

## DATES

min(df$order_date) # 2019-03-13
max(df$order_date) # 2020-05-11

# trim df to last day of April 2020
df <- filter(df, order_date < '2020-05-01')

## ORDERS

# how many distinct orders?
n_orders <- summarize(df,n_distinct(order_id)) %>% pull() # 42,180 matches row count, all order_id are unique.

# how many distinct customers?
n_customers <- summarize(df, n_distinct(customer, na.rm=TRUE)) %>% pull() # 10,316

# avg orders per customer
avg_orders_customers <- n_orders / n_customers # 4.09 orders per customer

# count of orders per customer, sorted
orders_customers <- select(df, c('customer','order_id')) %>% 
  group_by(customer) %>% 
  summarize(num_orders = n()) %>% 
  arrange(desc(num_orders))

# this would be useful to see as a histogram ..
ggplot(orders_customers, aes(x=num_orders)) + 
  geom_histogram()

# maybe some labels and a log function to see the tail?
ggplot(orders_customers, aes(x=log(num_orders))) +
  geom_histogram() + 
  geom_text(aes(y=0,label=num_orders))  # works, just takes a while to render.

# bucket the num_orders by customer count, and express as % of total customers
order_buckets <- orders_customers %>% 
  group_by(num_orders) %>% 
  summarize(customer_count = n()) %>% 
  arrange(desc(customer_count)) %>% 
  mutate(customer_total=n_customers,
         pct_customers = customer_count / customer_total) %>%
  select(!customer_total)

# ! 30% of customers made just one order .. 55% two or fewer .. 68% three or fewer .. 32% four or more.

# orders by date
orders_date <- df %>% 
  group_by(order_date) %>% 
  summarize(orders=n()) %>% 
  arrange(order_date)

# graph out of curiosity
orders_date_plt <- 
  ggplot(orders_date, aes(order_date,orders)) +
  geom_line() +
  geom_smooth(color='green')

orders_week <- df %>% 
  group_by(order_week) %>% 
  summarize(orders=n()) %>% 
  arrange(order_week)

orders_week_plt <- 
  ggplot(orders_week, aes(order_week, orders, group=1)) +
  geom_line() +
  geom_smooth(color='green')

orders_date_plt

orders_week_plt

# looks like a weekly business cycle, some seasonal dips around winter holidays, overall increasing trend.

# !! look more closely at that dip. (went away with weekstart = Sunday?)
# !! we might have a partial week at the tail of the data.  truncate?


## PROMO_VALUE

sort(unique(df$promo_value)) # integers 0-5

promo_values <- group_by(df, promo_value) %>% 
  summarize(orders=n()) %>% 
  mutate(pct=orders/n_orders)

# 96% of orders had no promo code. 

# let's add a boolean has_promo_value
df <- mutate(df, has_promo_value = ifelse(promo_value == 0, FALSE, TRUE))


## RESTAURANT_TOTAL (Revenue)


restaurant_total_plt <- df %>%
  select(c(shops_id,restaurant_total)) %>%
  group_by(shops_id) %>%
  summarize(rest_revenue = round(sum(restaurant_total))) %>%
  ggplot(aes(x=rest_revenue)) + geom_histogram()

restaurant_total_plt


## SHIPPING_TYPE

# values for shipping_type .. using pull() to get into a vector, nice!
vals_shipping_type <- df %>% 
  distinct(shipping_type) %>% 
  pull() %>% 
  sort(na.last=TRUE)

# orders by shipping_type
orders_shipping_type <- group_by(df,shipping_type) %>% 
  summarize(orders=n()) %>% 
  arrange(desc(orders))


### PAYMENT_METHOD

# values for payment_method
distinct(df, payment_method) %>% 
  pull()  # what is this '/' method?  bad data?

## filter rows for payment_method = '/'
df %>% filter(payment_method=='/')

## convert these to NA
df <- df %>% mutate(payment_method = replace(payment_method, payment_method=='/',NA))

vals_payment_method <- distinct(df, payment_method) %>% 
  pull() %>% 
  sort(na.last=TRUE)

# orders by payment_method
orders_payment_method <- group_by(df,payment_method) %>% 
  summarize(orders=n()) %>% 
  arrange(desc(orders))


### SOURCE

# values for source
vals_source <- distinct(df, source) %>% 
  pull() %>% 
  sort(na.last=TRUE)

# orders by source
orders_source <- group_by(df, source) %>% 
  summarize(orders=n()) %>% 
  arrange(desc(orders))


### STATE
orders_state <- group_by(df, state) %>% 
  summarize(orders=n()) %>%
  mutate(pct=orders/n_orders) %>% 
  arrange(desc(orders))



### Stop and save as transformed dataset
df$source <- as.factor(df$source)
df$shipping_type <- as.factor(df$shipping_type)
df$payment_method <- as.factor(df$payment_method)
saveRDS(df, "data/orders_corrected.rdata")



#
# COHORTS
#

df <- readRDS('data/orders_corrected.rdata')

# get details of each customer's first order 
customer_first_order <- df %>% 
  select(customer,date_purchased) %>% 
  group_by(customer) %>% 
  summarize(date_purchased = min(date_purchased)) %>% 
  left_join(df) %>% 
  arrange(date_purchased, customer)

# assign a cohort_id to each customer based on first order_month, join details of their first order.
# dor now including only source, promo_value
customer_cohort <- df %>%
  select(customer, order_month) %>% 
  group_by(customer) %>% 
  summarize(cohort_id = min(order_month)) %>% 
  left_join(select(customer_first_order, c('customer','source','has_promo_value','promo_value')), by='customer' ) 

# get each customer_id and order_month in which they had at least 1 order .. join to get their cohort_id and first-order details
customer_activity <- df %>% 
  select('customer','order_month') %>% 
  group_by(customer, order_month) %>% 
  summarize() %>% 
  left_join(customer_cohort) %>% 
  arrange(customer, order_month)

# turn this on its side to get count of active customers and their cohort_id by order_month
cohort_activity <- customer_activity %>% 
  group_by(order_month, cohort_id, source, promo_value, has_promo_value) %>% 
  summarize(active_customers = n()) %>% 
  mutate(cohort_period=interval(cohort_id, order_month) %/% months(1) + 1) %>% 
  arrange(order_month, cohort_id)


#
# the fast line plot
#

cohort_activity %>% 
  group_by(cohort_id, order_month, source) %>% 
  summarize(active_customers=sum(active_customers)) %>% 
  ggplot(aes(x=order_month, y=active_customers)) +
  geom_line(aes(group=cohort_id, color=factor(cohort_id)), position=position_stack(reverse=TRUE)) +
  facet_grid(cols=vars(source))


cohort_activity %>% 
  group_by(cohort_id, order_month, has_promo_value) %>% 
  summarize(active_customers=sum(active_customers)) %>% 
  ggplot(aes(x=order_month, y=active_customers)) +
  geom_line(aes(group=cohort_id, color=factor(cohort_id)), position=position_stack(reverse=TRUE)) +
  facet_grid(cols=vars(has_promo_value))

cohort_activity %>% 
  group_by(cohort_id, order_month, promo_value) %>% 
  summarize(active_customers=sum(active_customers)) %>% 
  ggplot(aes(x=order_month, y=active_customers)) +
  geom_line(aes(group=cohort_id, color=factor(cohort_id)), position=position_stack(reverse=TRUE)) +
  facet_grid(cols=vars(factor(promo_value)))




#
# RETENTION/CHURN
#


# everyone

# get cohort counts
cohort_counts <- customer_cohort %>% 
  group_by(across(!customer)) %>% 
  summarize(cohort_count = n()) %>% 
  arrange(cohort_id)

cohort_counts_all <- cohort_counts %>% 
  group_by(cohort_id) %>% 
  summarize(cohort_count=sum(cohort_count))

cohort_activity_all<- cohort_activity %>% 
  group_by(order_month, cohort_id) %>% 
  summarize(active_customers = sum(active_customers)) %>% 
  mutate(cohort_period=interval(cohort_id, order_month) %/% months(1) + 1)

cohort_retention_all<- cohort_activity_all %>% 
  left_join(cohort_counts_all) %>% 
  mutate(retention_rate = active_customers / cohort_count) 

# tile graph
ggplot(cohort_retention_all, aes(x=cohort_period, y=cohort_id, fill=retention_rate)) +
  geom_tile() +
  geom_text(aes(label=sprintf('%.0f%%',retention_rate * 100))) +
  scale_fill_gradient2(low = 'red', high = 'green', midpoint = .50) +
  scale_y_date(date_breaks = '1 month') 


# line graph
ggplot(cohort_retention_all, aes(x=cohort_period, y=retention_rate)) +
  geom_line(aes(group=cohort_id, color=retention_rate))


# customers acquired with a promo code

cohort_counts_has_promo <- cohort_counts %>% 
  group_by(cohort_id, has_promo_value) %>% 
  summarize(cohort_count = sum(cohort_count))

cohort_activity_has_promo <- cohort_activity %>% 
  group_by(order_month, cohort_id, has_promo_value) %>% 
  summarize(active_customers = sum(active_customers)) %>% 
  mutate(cohort_period=interval(cohort_id, order_month) %/% months(1) + 1)
  
cohort_retention_has_promo <- cohort_activity_has_promo %>% 
  left_join(cohort_counts_has_promo) %>% 
  mutate(retention_rate = active_customers / cohort_count) %>% 
  mutate(cohort=as.character(cohort_id))

# tile graph
ggplot(cohort_retention_has_promo, aes(x=cohort_period, y=cohort_id, fill=retention_rate)) +
  geom_tile() +
  geom_text(aes(label=sprintf('%.0f%%',retention_rate * 100))) +
  scale_fill_gradient2(low = 'red', high = 'green', midpoint = .50) +
  scale_y_date(date_breaks = '1 month') +
  facet_grid(cols=vars(has_promo_value))

# line graph
ggplot(cohort_retention_has_promo, aes(x=cohort_period, y=retention_rate)) +
  geom_line(aes(group=cohort_id, color=retention_rate)) +
  facet_grid(rows=vars(has_promo_value))



# promo value for customers acquired with a promo code

cohort_counts_promo_value <- cohort_counts %>% 
  group_by(cohort_id, promo_value) %>% 
  summarize(cohort_count = sum(cohort_count))

cohort_activity_promo_value <- cohort_activity %>% 
  group_by(order_month, cohort_id, promo_value) %>% 
  summarize(active_customers = sum(active_customers)) %>% 
  mutate(cohort_period=interval(cohort_id, order_month) %/% months(1) + 1)

cohort_retention_promo_value <- cohort_activity_promo_value %>% 
  left_join(cohort_counts_promo_value) %>% 
  mutate(retention_rate = active_customers / cohort_count) %>% 
  mutate(cohort=as.character(cohort_id))

# tile graph
ggplot(cohort_retention_promo_value, aes(x=cohort_period, y=cohort_id, fill=retention_rate)) +
  geom_tile() +
  geom_text(aes(label= ifelse(is.na(retention_rate), 0, sprintf('%.0f%%',retention_rate * 100)))) +
  scale_fill_gradient2(low = 'red', high = 'green', midpoint = .5) +
  scale_y_date(date_breaks = '1 month') 
  facet_grid(rows=vars(promo_value))

# line graph
ggplot(cohort_retention_promo_value, aes(x=cohort_period, y=retention_rate)) +
  geom_line(aes(group=cohort_id, color=retention_rate)) +
  facet_grid(rows=vars(promo_value))





#
# the nice area plot 
#

# for plot; needs addl 0 value for prior unique cohort_id month.
for (i in unique(cohort_activity$cohort_id)){
  row <- list(as_date(i) - months(1), as_date(i), NA, NA, 0, NA, 0, 0)
  names(row) <- colnames(cohort_activity)
  cohort_activity <- cohort_activity %>% bind_rows(row)
}

# all
cohort_activity_all <- cohort_activity %>% 
  select(cohort_id, order_month, active_customers) %>% 
  group_by(cohort_id, order_month) %>% 
  summarize(active_customers=sum(active_customers)) %>% 
  arrange(cohort_id, order_month)

ggplot(cohort_activity_all, aes(x=order_month, y=active_customers)) +
  geom_area(aes(group=cohort_id, fill=factor(cohort_id)), position= position_stack(reverse=TRUE))

  
  
