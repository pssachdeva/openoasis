library(tidyverse)
library(ggiraph)
library(maps)
#library(tigris)
library(ggmap)
#library(kableExtra)
library(grid)
library(gridExtra)
library(shinydashboard)
library(rgdal)
library(Cairo);options(shiny.usecairo=TRUE)


#setwd("/home/andrew/projects/openoak_oasis/")

###Alameda standards
ala_low_income_thresh <- c(62750, 71700, 80650, 89600, 96800, 103950, 111150, 118300)
np <- c(1:8)
ala_c <- rep("Alameda", 8)

###Contra Costa standards
cc_low_income_thresh <- c(62750, 71700, 80650, 89600, 96800, 103950, 111150, 118300)
cc_c <- rep("Contra Costa", 8)

###San Francisco standards
sf_low_income_thresh <- c(82200, 93950, 105700, 117400, 126800, 136200, 145600, 155000)
sf_c <- rep("San Francisco", 8)

low_income_thresh_df <- data.frame("NP" = rep(np, 3), county = c(ala_c, cc_c, sf_c), low_income_thresh = c(ala_low_income_thresh, cc_low_income_thresh, sf_low_income_thresh))

ss16hca <- read.table("ss16hca.csv", header = TRUE, stringsAsFactors = FALSE, quote = "", sep = ",")

puma_crosswalk <- read.table("PUMA2000_PUMA2010_crosswalk_NA_fixed.tsv", header = TRUE, stringsAsFactors = FALSE, quote = "", sep = "\t") %>% dplyr::select(PUMA10, PUMA10_Name) %>% dplyr::rename(PUMA = PUMA10) %>% unique

ss16hca <- ss16hca %>% 
  dplyr::left_join(puma_crosswalk, by = "PUMA") %>% 
  dplyr::mutate(county = ifelse(grepl("Alameda County", PUMA10_Name), "Alameda", 
                                ifelse(grepl("Contra Costa County", PUMA10_Name), "Contra Costa", 
                                       ifelse(grepl("San Francisco County", PUMA10_Name), "San Francisco", NA)))) %>% 
  dplyr::filter(!is.na(county)) %>% dplyr::mutate(LAPTOP = as.character(LAPTOP)) %>% 
  dplyr::mutate(FINCP_NP_norm = FINCP/NP) %>%
  dplyr::mutate(FINCP_NP_norm_decile = 10*ntile(FINCP_NP_norm, 10)) %>%
  dplyr::arrange(FINCP_NP_norm_decile) %>%
  dplyr::mutate(FINCP_NP_norm_decile = as.character(FINCP_NP_norm_decile)) %>%
  dplyr::mutate(FINCP_NP_norm_decile = factor(FINCP_NP_norm_decile, levels = unique(FINCP_NP_norm_decile))) %>%
  dplyr::mutate(NP = ifelse(NP > 8, 8, NP)) %>%
  dplyr::left_join(low_income_thresh_df, by = c("NP", "county")) %>%
  dplyr::mutate(ACCESS = as.character(ACCESS))





#ca_pumas <- pumas("CA")

ca_pumas <- readOGR(dsn = ".", layer = "cb_2016_06_puma10_500k")

ca_pumas_df <- broom::tidy(ca_pumas) %>% dplyr::mutate(group = as.character(group))

#ca_pumas_df <- broom::tidy(ca_pumas) %>% dplyr::mutate(group = as.character(group))

pumas_group_df <- data.frame(PUMA = as.numeric(as.character(ca_pumas$PUMACE10)), group = as.character(c(0:264) + 0.1))


pumas_of_interest <- ss16hca %>% dplyr::select(PUMA) %>% unique

pumas_group_df_filtered <- pumas_group_df %>% semi_join(pumas_of_interest)

ca_pumas_df_filtered <- ca_pumas_df %>% semi_join(pumas_group_df_filtered)

map_data <- ss16hca %>% 
            dplyr::select(PUMA, PUMA10_Name, county) %>% 
            unique() %>% 
            left_join(pumas_group_df_filtered) %>% 
            left_join(ca_pumas_df_filtered)
#bay_map <- get_map(location = "Bay Area")

bay_map = get_map(c(-123, 37.4, -121.2, 38.2))

low_income_percent_no_laptop <- ss16hca %>% 
                                filter(!is.na(LAPTOP), !is.na(FINCP)) %>% 
                                dplyr::mutate(LAPTOP = ifelse(LAPTOP == "2", "No laptop", "Laptop"), low_income = ifelse(FINCP <= low_income_thresh, "Low income", "Not low income")) %>% 
                                dplyr::select(PUMA10_Name, county, LAPTOP, FINCP, low_income) %>% 
                                dplyr::filter(low_income == "Low income") %>% 
                                dplyr::count(PUMA10_Name, county, LAPTOP) %>% 
                                dplyr::group_by(PUMA10_Name) %>% 
                                dplyr::mutate(percent_laptop_status = 100*n/sum(n), total_obs = sum(n)) %>% 
                                dplyr::ungroup() %>% dplyr::filter(LAPTOP == "No laptop") %>% 
                                dplyr::rename(percent_no_laptop = percent_laptop_status) %>% 
                                dplyr::select(-n, -LAPTOP)


low_income_percent_no_internet <- ss16hca %>% 
  filter(!is.na(ACCESS), !is.na(FINCP)) %>% 
  dplyr::mutate(ACCESS = ifelse(ACCESS == "3", "No internet", "Internet"), low_income = ifelse(FINCP <= low_income_thresh, "Low income", "Not low income")) %>% 
  dplyr::select(PUMA10_Name, county, ACCESS, FINCP, low_income) %>% 
  dplyr::filter(low_income == "Low income") %>% 
  dplyr::count(PUMA10_Name, county, ACCESS) %>% 
  dplyr::group_by(PUMA10_Name) %>% 
  dplyr::mutate(percent_internet_status = 100*n/sum(n), total_obs = sum(n)) %>% 
  dplyr::ungroup() %>% dplyr::filter(ACCESS == "No internet") %>% 
  dplyr::rename(percent_no_internet = percent_internet_status) %>% 
  dplyr::select(-n, -ACCESS)

laptop_map_data <- map_data %>% 
            left_join(low_income_percent_no_laptop, by = c("PUMA10_Name", "county")) %>% 
            dplyr::mutate(tooltip = paste0(PUMA10_Name, ": ", percent_no_laptop, "% with ", total_obs, " obs"))


internet_map_data <- map_data %>% 
  left_join(low_income_percent_no_internet, by = c("PUMA10_Name", "county")) %>% 
  dplyr::mutate(tooltip = paste0(PUMA10_Name, ": ", percent_no_internet, "% with ", total_obs, " obs"))

laptop_map <- ggmap(bay_map, zoom = 10) + 
  geom_polygon_interactive(data = laptop_map_data, aes(x = long, y = lat, fill = percent_no_laptop, linetype = county, tooltip = PUMA10_Name, data_id = PUMA10_Name), alpha = 0.4, col = "black", size = 0.8) + 
  theme_bw() + 
  coord_fixed() + 
  scale_fill_gradient(low = "transparent", high = "red") + 
  ggtitle("%Low income residents lacking laptop in ALA, SF, CC counties") +
  theme(legend.background = element_rect(fill = NA, colour = NA),
        panel.background = element_rect(fill = NA, colour = NA),
        plot.background = element_rect(fill = NA, colour = NA))

internet_map <- ggmap(bay_map, zoom = 10) + 
  geom_polygon_interactive(data = internet_map_data, aes(x = long, y = lat, fill = percent_no_internet, linetype = county, tooltip = PUMA10_Name, data_id = PUMA10_Name), alpha = 0.4, col = "black", size = 0.8) + 
  theme_bw() + 
  coord_fixed() + 
  scale_fill_gradient(low = "transparent", high = "red") + 
  ggtitle("%Low income residents lacking internet in ALA, SF, CC counties") + 
  theme(legend.background = element_rect(fill = NA, colour = NA),
        panel.background = element_rect(fill = NA, colour = NA),
        plot.background = element_rect(fill = NA, colour = NA))


laptop_map_data_readable <- laptop_map_data %>% dplyr::select(PUMA10_Name, percent_no_laptop, total_obs) %>% unique()

internet_map_data_readable <- internet_map_data %>% dplyr::select(PUMA10_Name, percent_no_internet, total_obs) %>% unique()


input_data_for_plotting <- ss16hca %>% 
  filter(!is.na(ACCESS), !is.na(FINCP), !is.na(LAPTOP)) %>% 
  dplyr::mutate(LAPTOP = ifelse(LAPTOP == "2", "No laptop", "Laptop")) %>%
  dplyr::mutate(ACCESS = ifelse(ACCESS == "1" | ACCESS == "2", "Internet access", "No internet access")) %>%
  dplyr::mutate(low_income = ifelse(FINCP <= low_income_thresh, "Low income", "Not low income"))

laptop_bargraph_list = list()
for (i in unique(laptop_map_data$PUMA10_Name)) {
  laptop_bargraph_list[[i]] <- input_data_for_plotting %>% 
                     filter(PUMA10_Name == i) %>% 
                     ggplot(aes(x = low_income, fill = LAPTOP)) + 
                          geom_bar(position = "fill", col = "black") + 
                          theme_bw() + 
                          scale_y_continuous(labels = scales::percent) + 
                          xlab("Low income status") + 
                          ylab("% laptop access") + 
                          ggtitle(paste0("Laptop access by income category \n", i)) +
                          theme(panel.background = element_rect(fill=NA, color = NA),
                                plot.background = element_rect(fill=NA, color = NA))
}


internet_bargraph_list = list()
for (i in unique(laptop_map_data$PUMA10_Name)) {
  internet_bargraph_list[[i]] <- input_data_for_plotting %>% 
    filter(PUMA10_Name == i) %>% 
    ggplot(aes(x = low_income, fill = ACCESS)) + 
    geom_bar(position = "fill", col = "black") + 
    theme_bw() + 
    scale_y_continuous(labels = scales::percent) + 
    xlab("Low income status") + 
    ylab("% laptop access") + 
    ggtitle(paste0("Internet access by income category \n", i)) + 
    theme(panel.background = element_rect(fill=NA, color = NA),
          plot.background = element_rect(fill=NA, color = NA))
}




fincp_norm_density_plot_list <- list()

density_min <- min(input_data_for_plotting$FINCP - input_data_for_plotting$low_income_thresh)
density_max <- max(input_data_for_plotting$FINCP - input_data_for_plotting$low_income_thresh)

for (i in unique(laptop_map_data$PUMA10_Name)) {
  fincp_norm_density_plot_list[[i]] <- input_data_for_plotting %>% 
    mutate(region = ifelse(PUMA10_Name == i, "Current region", "Everywhere else")) %>%
    ggplot(aes(x = FINCP - low_income_thresh, fill = region, linetype = region)) + 
    geom_density(col = "black", size = 1, alpha = 0.5) + 
    geom_vline(xintercept = 0, linetype = "dashed", col = "red", size = 0.8) + 
    annotation_custom(textGrob("Low income threshold", rot = -90, gp = gpar(fontsize = 14, fontface = "bold")), xmin = 54000, xmax = 54000) + 
    theme_bw() + 
    ggtitle(paste0("Density FINCP relative to low income threshold \n", i)) +  
    xlab("FINCP - low income threshold (for household size corresponding to FINCP observation)") + 
    ylab("Density") + 
    xlim(density_min - 1, density_max - 1) + 
    scale_fill_manual(values = c("#00FFFF", "#D3D3D3")) +
    theme(panel.background = element_rect(fill=NA, color = NA),
          plot.background = element_rect(fill=NA, color = NA))
}

server <- function(input, output, session) {

  laptop_selected_region <- reactive({
    input$laptop_plot_map_selected
  })
  
  internet_selected_region <- reactive({
    input$internet_plot_map_selected
  })

  output$laptop_plot_map <- renderggiraph({
    ggiraph(code = print(laptop_map), selection_type = "single", zoom_max = 10,
            hover_css = "fill:#00FFFF;stroke:black;cursor:pointer;",
            selected_css = "fill:#00FFFF;stroke:black;", flexdashboard = TRUE)
  })
  
  output$internet_plot_map <- renderggiraph({
    ggiraph(code = print(internet_map), selection_type = "single", zoom_max = 10,
            hover_css = "fill:#00FFFF;stroke:black;cursor:pointer;",
            selected_css = "fill:#00FFFF;stroke:black;", flexdashboard = TRUE)
  })

  observeEvent(input$laptop_reset, {
    session$sendCustomMessage(type = 'laptop_plot_map_set', message = character(0))
  })
  
  observeEvent(input$internet_reset, {
    session$sendCustomMessage(type = 'internet_plot_map_set', message = character(0))
  })

  output$laptop_datatab <- renderTable({
    laptop_out <- laptop_map_data_readable[laptop_map_data_readable$PUMA10_Name %in% laptop_selected_region(), ] %>% rename(low_income_percent_no_laptop = percent_no_laptop)
    if( nrow(laptop_out) < 1 ) return(NULL)
    row.names(laptop_out) <- NULL
    laptop_out
  })
  
  output$internet_datatab <- renderTable({
    internet_out <- internet_map_data_readable[internet_map_data_readable$PUMA10_Name %in% internet_selected_region(), ] %>% rename(low_income_percent_no_internet = percent_no_internet)
    if( nrow(internet_out) < 1 ) return(NULL)
    row.names(internet_out) <- NULL
    internet_out
  })
  
  output$laptop_plot_bar <- renderPlot({
                            if(is.null(laptop_selected_region())) return(NULL)
                            laptop_out <- laptop_bargraph_list[[laptop_selected_region()]]
                            laptop_out
  }, bg = "transparent")
  
  output$internet_plot_bar <- renderPlot({
                            if(is.null(internet_selected_region())) return(NULL)
                            internet_out <- internet_bargraph_list[[internet_selected_region()]]
                            internet_out
  }, bg = "transparent")
  
  output$laptop_plot_density <- renderPlot({
                            if(is.null(laptop_selected_region())) return(NULL)
                            out <- fincp_norm_density_plot_list[[laptop_selected_region()]]
                            out
    
  }, bg = "transparent")
  
  output$internet_plot_density <- renderPlot({
                            if(is.null(internet_selected_region())) return(NULL)
                            out <- fincp_norm_density_plot_list[[internet_selected_region()]]
                            out
                            
  }, bg = "transparent")

}


#ui <- fluidPage(
#  tags$h1("Open Oasis putative dashboard"),
  
#  fluidRow(
#    column(width = 8,
#           h4("Select regions: "),
#           actionButton("reset", label = "Reset selection"),
#           ggiraph::ggiraphOutput("plot_map", width = "900px", height = "900px")
#    ),
#    column(width = 4,
#           h4("Selected regions"),
#           tableOutput("datatab"),
#           plotOutput("plot_bar"),
#           plotOutput("plot_density")
#    )
#  )
#)



sidebar <- dashboardSidebar(
  sidebarMenu(
    # Create two `menuItem()`s, "Dashboard" and "Inputs"
    menuItem(text = "Laptop Access",
             tabName = "laptop_access"
    ),
    menuItem(text = "Internet Access",
             tabName = "internet_access"
    )
  )
)

body <- dashboardBody(
  tabItems(
    # Add two tab items, one with tabName "dashboard" and one with tabName "inputs"
    tabItem(tabName = "laptop_access", class = "active",
            fluidRow(
              column(width = 8,
                     h4("Select regions: "),
                     actionButton("laptop_reset", label = "Reset selection"),
                     ggiraph::ggiraphOutput("laptop_plot_map", width = "900px", height = "900px")
              ),
              column(width = 4,
                     h4("Selected regions"),
                     tableOutput("laptop_datatab"),
                     plotOutput("laptop_plot_bar"),
                     plotOutput("laptop_plot_density")
              )
            )),
    tabItem(tabName = "internet_access",
            fluidRow(
              column(width = 8,
                     h4("Select regions: "),
                     actionButton("internet_reset", label = "Reset selection"),
                     ggiraph::ggiraphOutput("internet_plot_map", width = "900px", height = "900px")
              ),
              column(width = 4,
                     h4("Selected regions"),
                     tableOutput("internet_datatab"),
                     plotOutput("internet_plot_bar"),
                     plotOutput("internet_plot_density")
              )
            ))
  )
)


#,
#column(width = 4,
#       h4("Selected regions"),
#       tableOutput("internet_datatab"),
#       plotOutput("internet_plot_bar"),
#       plotOutput("plot_density")
#)

# Use the new body
ui <- dashboardPage(header = dashboardHeader(title = "OpenOasis"),
                    sidebar = sidebar,
                    body = body
)
shinyApp(ui, server)






