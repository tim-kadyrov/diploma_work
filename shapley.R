folder <- "path_to_folder"
data_feed_tbl <- read.csv2(paste0(folder,'/raw_data.csv'), header = TRUE)


library(GameTheoryAllocation)
library(tidyverse)
library(dplyr)
library(sparklyr)
# install.packages("pracma")
library(pracma)


# add conversion
data_feed_tbl['conversion'] = 0
data_feed_tbl$conversion[data_feed_tbl$is_click == 'conversion'] = 1

# Construct conversions sequences for all visitors
data_feed_tbl1 = data_feed_tbl %>%
  group_by(client) %>%
  arrange(timestamp) %>%
  mutate(order_seq = ifelse(conversion > 0, 1, NA)) %>%
  mutate(order_seq = lag(cumsum(ifelse(is.na(order_seq), 0, order_seq)))) %>%
  mutate(order_seq = ifelse((row_number() == 1) & (conversion > 0), 
                            -1, ifelse(row_number() == 1, 0, order_seq))) %>% 
  ungroup()


channel_stacks = data_feed_tbl1 %>%
  group_by(client, order_seq) %>%
  summarize(
    path = concat_ws(" > ", collect_list(channels)),
    conversion = sum(conversion)
  ) %>% ungroup() %>%
  group_by(path) %>%
  summarize(
    conversion = sum(conversion)
  ) %>%
  filter(path != "") %>%
  collect()



# Summarizing Order Sequences
seq_summaries = data_feed_tbl1 %>%
  group_by(client, order_seq) %>%
  summarize(
    channel_1_touches = max(ifelse(channels == "channel_1",1,0)),
    channel_2_touches = max(ifelse(channels == "channel_2",1,0)),
    channel_3_touches = max(ifelse(channels == "channel_3",1,0)),
    channel_4_touches = max(ifelse(channels == "channel_4",1,0)),
    conversions = sum(conversion)
  ) %>% ungroup()


# Sum up the number of sequences and conversions
# for each combination of marketing channels
conv_rates = seq_summaries %>%
  group_by(vk_touches,
           mail_touches,
           ok_touches,
           okprom_touches) %>%
  summarize(
    conversions = sum(conversions),
    total_sequences = n()
  ) %>% collect()


library(GameTheoryAllocation)

number_of_channels = 4

# The coalitions function is a handy function from the GameTheoryALlocation
# library that creates a binary matrix to which you can fit your
# characteristic function (more on this in a bit) 
touch_combos = as.data.frame(coalitions(number_of_channels)$Binary)
names(touch_combos) = c("channel_1","channel_2","channel_3",
                        "channel_4")

# Join previous summary results with the binary matrix
# the GameTheoryAllocation library built.
touch_combo_conv_rate = left_join(touch_combos, conv_rates, 
                                  by = c(
                                    "channel_1"="channel_1_touches",
                                    "channel_2" = "channel_2_touches",
                                    "channel_3" = "channel_3_touches",
                                    "channel_4" = "channel_4_touches"
                                  )
)

# Fill in any NAs with 0
touch_combo_conv_rate = touch_combo_conv_rate %>%
  mutate_all(funs(ifelse(is.na(.),0,.))) %>%
  mutate(
    conv_rate = ifelse(total_sequences > 0, conversions/total_sequences, 0)
  )

# Building Shapley Values for each channel combination

shap_vals = as.data.frame(coalitions(number_of_channels)$Binary)
names(shap_vals) = c("channel_1","channel_2","channel_3",
                     "channel_4")
coalition_mat = shap_vals
shap_vals[2^number_of_channels,] = Shapley_value(touch_combo_conv_rate$conv_rate, game="profit")

for(i in 2:(2^number_of_channels-1)){
  if(sum(coalition_mat[i,]) == 1){
    shap_vals[i,which(shap_vals[i,]==1)] = touch_combo_conv_rate[i,"conv_rate"]
  }else if(sum(coalition_mat[i,]) > 1){
    if(sum(coalition_mat[i,]) < number_of_channels){
      channels_of_interest = which(coalition_mat[i,] == 1)
      char_func = data.frame(rates = touch_combo_conv_rate[1,"conv_rate"])
      for(j in 2:i){
        if(sum(coalition_mat[j,channels_of_interest])>0 & 
           sum(coalition_mat[j,-channels_of_interest])==0)
          char_func = rbind(char_func,touch_combo_conv_rate[j,"conv_rate"])
      }
      shap_vals[i,channels_of_interest] = 
        Shapley_value(char_func$rates, game="profit")
    }
  }
}

# Apply Shapley Values as attribution weighting
order_distribution = shap_vals * touch_combo_conv_rate$total_sequences
shapley_value_orders = t(t(round(colSums(order_distribution))))
shapley_value_orders = data.frame(channels = row.names(shapley_value_orders), 
                                  orders = as.numeric(shapley_value_orders))




# Optimization (based on Simplex method)

# install.packages("lpSolve")
# install.packages("linprog")

library (lpSolve)
library (linprog)
# ROI = (channel_1, channel_2, channel_3, channel_4)
ROI <- c(0.03836, 0.02362, 0.13, 0.268436) # Calculated ROI = Shapley_val / total_cost
b <- c(100, 0, 0, 0, 0)
A<- rbind (
  c(1,1,1,1), #first constraint, x1+x2+x3+x4 <= 100
  c(-1,0,0,0), #second constraint, x1 >= 0
  c(0,-1,0,0), #third constriant, x2 >= 0
  c(0,0,-1,0), #forth constriant, x3 >= 0
  c(0,0,0,-1) #fifth constriant, x4 >= 0
)
solveLP(ROI, b, A, TRUE)

