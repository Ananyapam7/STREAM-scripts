rm(list = ls())

library(readr)
bubble <- read_csv("bubble.csv")
START_all_task_data_Shared_final_xlsx_Combined_data <- read_csv("START_all_task_data_Shared_final.xlsx - Combined_data.csv")
selected_df <- START_all_task_data_Shared_final_xlsx_Combined_data[, c(1, 2, 15, 16, 17, 18)]

colnames(selected_df) <- selected_df[1,]
selected_df<- selected_df[-1,]

na_rows_count <- sum(apply(selected_df, 1, anyNA))

na_rows_count_bubble <- sum(apply(bubble, 1, anyNA))

write.csv(selected_df, "res.csv", row.names = FALSE)