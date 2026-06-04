library(tidyverse)

paths <- list.files(path = "combined-results", pattern = ".*_round-.*\\.csv$", full.names = TRUE)

data <- paths %>%
  map_dfr(function(x) {
    file_name <- tools::file_path_sans_ext(basename(x))
    site_name <- str_extract(file_name, "^(.*)(?=_round)")
    
    read_csv(x, show_col_types = FALSE) %>%
      mutate(Site = site_name)
  })

results_list <- data %>%
  group_split(Site) %>%
  set_names(map(., ~ unique(.x$Site))) %>% 
  map(function(df) {
    
    groups <- unique(df$Framework)
    
    pairwise_results <- expand.grid(Group1 = groups, Group2 = groups, stringsAsFactors = FALSE) %>%
      filter(Group1 != Group2) %>%
      as_tibble() %>%
      mutate(
        data = map2(Group1, Group2, function(g1, g2) {
          
          # Client
          group_1_client <- df %>% filter(Framework == g1) %>% pull(`Client Energy (J)`)
          group_2_client <- df %>% filter(Framework == g2) %>% pull(`Client Energy (J)`)
          
          # Server
          group_1_server <- df %>% filter(Framework == g1) %>% pull(`Server Energy (J)`)
          group_2_server <- df %>% filter(Framework == g2) %>% pull(`Server Energy (J)`)
          
          # Client
          wt_client <- wilcox.test(group_1_client, group_2_client, 
                                   alternative = "less",
                                   conf.int = TRUE, 
                                   conf.level = 0.95)
          
          # Server
          wt_server <- wilcox.test(group_1_server, group_2_server, 
                                   alternative = "less",
                                   conf.int = TRUE, 
                                   conf.level = 0.95)
          
          tibble(
            Framework_1 = g1,
            Framework_2 = g2,
            Comparison = paste(g1, "<", g2),
            Client_W_Statistic = wt_client$statistic,
            Client_P_Value = wt_client$p.value,
            Server_W_Statistic = wt_server$statistic,
            Server_P_Value = wt_server$p.value
          )
        })
      ) %>%
      unnest(data) %>%
      select(-Group1, -Group2)
    
    pairwise_results %>%
      mutate(
        Site = unique(df$Site),
        Client_P_Adjusted = p.adjust(Client_P_Value, method = "holm"),
        Server_P_Adjusted = p.adjust(Server_P_Value, method = "holm")
      ) %>%
      relocate(Site, Framework_1, Framework_2, Comparison, 
               Client_W_Statistic, Client_P_Value, Client_P_Adjusted,
               Server_W_Statistic, Server_P_Value, Server_P_Adjusted)
  })

if(!dir.exists("output")) dir.create("output")
iwalk(results_list, ~ write_csv(.x, file.path("output", paste0(.y, ".csv"))))

print(results_list)
