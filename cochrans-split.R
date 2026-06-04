library(dplyr)
library(readr)

# Warmup Round + 1
target_rounds <- c(
  "Home Interact" = 7 + 1,
  "List Interact" = 6 + 1,
  "List Page"     = 0 + 1,
  "Live Page"     = 9 + 1,
  "Navigate Page" = 8 + 1,
  "Static Page"   = 11 + 1
)

data_list <- list(
  "Home Interact" = read_csv("benchmark-data/home-interact.csv") %>% 
    filter(Round == target_rounds["Home Interact"]),
  
  "List Interact" = read_csv("benchmark-data/list-interact.csv") %>% 
    filter(Round == target_rounds["List Interact"]),
  
  "List Page"     = read_csv("benchmark-data/list.csv") %>% 
    filter(Round == target_rounds["List Page"]),
  
  "Live Page"     = read_csv("benchmark-data/live.csv") %>% 
    filter(Round == target_rounds["Live Page"]),
  
  "Navigate Page" = read_csv("benchmark-data/navigate.csv") %>% 
    filter(Round == target_rounds["Navigate Page"]),
  
  "Static Page"   = read_csv("benchmark-data/static.csv") %>% 
    filter(Round == target_rounds["Static Page"])
)

all_data <- bind_rows(data_list, .id = "Source")

cat("\n--- SERVER ENERGY TEST ---\n")

# Calculate CV for Server
server_data <- all_data %>%
  mutate(
    CV_Server = `Server Energy SD (J)` / `Server Energy Average (J)`
  )

max_cv_server_row <- server_data %>% 
  arrange(desc(CV_Server)) %>%
  head(1)

print(max_cv_server_row %>% select(Source, Framework, Round, `Server Energy Average (J)`, `Server Energy SD (J)`, CV_Server))

max_sd_server <- max_cv_server_row %>% pull(`Server Energy SD (J)`)

# Cochran's for Server (p=0.05)
iterations_server <- ceiling(((1.96 * max_sd_server) / 0.02)^2)
cat("Required Iterations (Server, p=0.05):", iterations_server, "\n")


cat("\n--- CLIENT ENERGY TEST ---\n")

# Calculate CV for Client
client_data <- all_data %>%
  mutate(
    CV_Client = `Client Energy SD (J)` / `Client Energy Average (J)`
  )

max_cv_client_row <- client_data %>% 
  arrange(desc(CV_Client)) %>%
  head(1)

print(max_cv_client_row %>% select(Source, Framework, Round, `Client Energy Average (J)`, `Client Energy SD (J)`, CV_Client))

max_sd_client <- max_cv_client_row %>% pull(`Client Energy SD (J)`)

# Cochran's for Client (p=0.05)
iterations_client <- ceiling(((1.96 * max_sd_client) / 0.02)^2)
cat("Required Iterations (Client, p=0.05):", iterations_client, "\n")

cat("\n--- SUMMARY ---\n")
cat("Run the benchmark for the higher of the two iteration counts: ", max(iterations_server, iterations_client), "iterations.\n")
