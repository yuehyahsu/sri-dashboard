
# Define server logic
function(input, output, session) {
  observeEvent(input$tab0_time,
               {
                 
                 # resetting the tab color whenever the time frame is changed
                 # is important since color options are different when filtered
                 # by all time or by year
                 updateSelectizeInput(session, "tab0_color",
                                      selected = "surveys")
                 
                 time_input <- input$tab0_time
                 
                 if(time_input == "All Time") {
                   color_choices <- var_opts_alltime
                   domain_choices <- ""
                 }
                 else {
                   color_choices <- var_opts_yearly
                   domain_choices <- domain_var_opts
                 }
                 
                 updateSelectizeInput(session, "tab0_color",
                                      choices = color_choices
                 )
                 
                 updateSelectizeInput(session, "tab0_sri_domains",
                                      choices = domain_choices
                 )
               }
  )
  
  observeEvent(input$tab1_agency,
               {
                 updateSelectizeInput(session, "tab1_country",
                                      choices = sort(unique(data[data$agency == input$tab1_agency, ]$country))
                 )
                 tab1_data_brushed <- NULL
               }
  )
  
  observeEvent(input$tab1_country,
               {
                 updateSelectizeInput(session, "tab1_round",
                                      choices = sort(unique(data[data$agency == input$tab1_agency & data$country == input$tab1_country, ]$round))
                 )
                 tab1_data_brushed <- NULL
               }
  )
  
  observeEvent(input$tab2_survey,
               {
                 updateSelectizeInput(session, "tab2_phase",
                                      choices = unique(data[data$survey %in% input$tab2_survey, ]$phase)
                 )
               }
  )
  
  # The Caritas data has some issues to be warned about.
  tab1_caritas_warn <- reactive({
    if(input$tab1_agency == "Caritas") {
      "Warning: there were several issues with the Caritas data, including missing values and
      incorrectly calculated SRI scores. Also, a reduced number of options were given for
      Domains 8-10. Use with caution."
    }
  })
  
  tab2_caritas_warn <- reactive({
    if(any(gsub("/.*", "", input$tab2_survey) %in% "Caritas")) {
      "Warning: there were several issues with the Caritas data, including missing values and
      incorrectly calculated SRI scores. Also, a reduced number of options were given for
      Domains 8-10. Use with caution."
    }
  })
  
  # Creates the data for tab 0 based on selections of time frame, statistic, and domains
  tab0_data <- reactive({
    validate(
      need(input$tab0_time != "", "Please select a time frame.")
    )
    if(input$tab0_time == "All Time") {
      country_shp
    }
    
    else {
      country_yearly_shp %>% 
        filter(year == input$tab0_time) %>% 
        # necessary so tab0_label statistic pull doesn't create unwanted grouping
        ungroup()
    }
  })
  
  tab0_stat_values <- reactive({
    statistic <- input$tab0_color
    
    stat_values <- subset(tab0_data(), select = eval(parse(text = statistic))) %>% 
      unlist(use.names = FALSE)
    
    min <- ifelse(!is.null(stat_values), min(stat_values, na.rm = T), 0)
    max <- ifelse(!is.null(stat_values), max(stat_values, na.rm = T), 0)
    
    stat_extrema <- setNames(c(min, max), c("min", "max"))
    stat_extrema
  })
  
  # Creates the color gradient for tab 0, based on  the selected variable
  tab0_palette <- reactive({
    
    statistic <- input$tab0_color
    
    colorNumeric(palette = "Blues", domain = c(0, 
                                               getElement(tab0_stat_values(), 'max')))
  })
  
  # Creates the color gradient for tab 0, based on  the selected variable
  tab0_label <- reactive({
    validate(
      need(input$tab0_color != "", "Please select a color variable.")
    )
    country_name <- tab0_data()$country
    
    variable <- input$tab0_color
    
    #statistic <- tab0_data()[variable, names(tab0_data())]
    statistic <- tab0_data() %>% 
      select(variable) %>% 
      unlist(use.names = FALSE)
    
    domains <- input$tab0_sri_domains
    domain_string <- domain_string(tab0_data(), input$tab0_sri_domains)
    
    label <- str_c("<b>", country_name, "</b><br/>", variable, ": ", statistic,
                   domain_string)
  })
  
  # Creates the data for tab 1 based on selections of agency, country, round, and domain.
  tab1_data <- reactive({
    validate(
      need(input$tab1_agency != "", "Please select an agency."),
      need(input$tab1_country != "", "Please select a country."),
      need(input$tab1_round != "", "Please select a round."),
      need(input$tab1_domain != "", "Please select a domain.")
    )
    
    data %>%
      filter(agency == input$tab1_agency & country == input$tab1_country & round == input$tab1_round) %>% 
      select(id, hh_age_1, hh_gender_1, SRI, phase, starts_with("domain")) %>% 
      mutate(across(starts_with("domain"), haven::as_factor)) %>% 
      mutate(phase = factor(phase, levels = c("targeting", "followup", "followup #2")))
    
  })
  
  # User has the option to populate the graph by "brushing" the table
  tab1_data_brushed <- reactive({
    user_brush <- input$tab1_brush
    brushedPoints(tab1_data(), user_brush, xvar = "phase", yvar = "SRI")
  })
  
  # The multiple-choice domains (Domain 8 - Domain 10) require some reshaping before they
  # can be plotted.
  tab1_mc_data <- reactive({
    if(input$tab1_domain %in% c("domain8", "domain9", "domain10")) {
      data %>% 
        filter(agency == input$tab1_agency & country == input$tab1_country & round == input$tab1_round) %>% 
        select_at(vars("phase", eval(parse(text = str_c(input$tab1_domain, "_vars"))))) %>% 
        mutate(phase = factor(phase, levels = c("targeting", "followup", "followup #2"))) %>% 
        reshape2::melt(id.vars = "phase") %>% 
        group_by(phase, variable) %>%
        mutate(prop = sum(value, na.rm = TRUE) / n()) %>% 
        ungroup() %>% 
        select(-value) %>% 
        unique()
    }
    
    else {
      TRUE
    }
  })
  
  # Creates the data for tab 2 based on selections of survey and domain.
  tab2_data <- reactive({
    validate(
      need(input$tab2_survey != "", "Please select an agency."),
      need(input$tab2_phase != "", "Please select a survey phase."),
      need(input$tab2_domain != "", "Please select a domain.")
    )
    if(input$tab2_domain %in% c("All", "domain1a", "domain1b", "domain2", "domain3", "domain4",
                                "domain5", "domain6", "domain7", "domain11", "domain12a", "domain12b")) {
      data %>%
        filter(survey %in% input$tab2_survey & phase == input$tab2_phase) %>% 
        select(id, hh_age_1, hh_gender_1, SRI, survey, starts_with("domain")) %>% 
        mutate(across(starts_with("domain"), haven::as_factor))
    }
    
    else if(input$tab2_domain %in% c("domain8", "domain9", "domain10")) {
      data %>% 
        filter(survey %in% input$tab2_survey & phase == input$tab2_phase) %>% 
        select_at(vars("survey", eval(parse(text = str_c(input$tab2_domain, "_vars"))))) %>%
        reshape2::melt(id.vars = "survey") %>% 
        group_by(survey, variable) %>%
        mutate(prop = sum(value, na.rm = TRUE) / n()) %>% 
        ungroup() %>% 
        select(-value) %>% 
        unique()
    }
  })
  
  # Creates the data for tab 2 based on selections of survey and domain.
  tab2_summ_data <- reactive({
    if(input$tab2_domain == "All") {
      summ_data <- data %>%
        filter(survey %in% input$tab2_survey & phase == input$tab2_phase) %>% 
        select(id, survey, SRI, starts_with("domain")) %>% 
        group_by(survey) %>% 
        summarize(mean = mean(SRI),
                  sd = sd(SRI),
                  min = min(SRI),
                  max = max(SRI)) %>% 
        ungroup() %>% 
        mutate(mean = round(mean, 2),
               sd = round(sd, 2))
      names(summ_data) <- c("Survey", "Mean SRI", "Std. Dev", "Minimum", "Maximum")
      summ_data
    }
    
    else if(input$tab2_domain %in% c("domain1a", "domain1b", "domain2", "domain3", "domain4",
                                     "domain5", "domain6", "domain7", "domain11", "domain12a", "domain12b")) {
      data %>% 
        filter(survey %in% input$tab2_survey & phase == input$tab2_phase) %>% 
        select(id, survey, SRI, starts_with("domain")) %>% 
        mutate(across(starts_with("domain"), haven::as_factor)) %>% 
        group_by_at(vars(survey, input$tab2_domain)) %>% 
        summarize(n = n()) %>% 
        mutate(prop = n / sum(n)) %>% 
        ungroup() %>% 
        dcast(paste0("survey ~ ", input$tab2_domain)) %>% 
        tibble() %>% 
        mutate_if(is.numeric, funs(percent(., 1)))
    }
    
    else if(input$tab2_domain %in% c("domain8", "domain9", "domain10")) {
      summ_data <- data %>% 
        filter(survey %in% input$tab2_survey & phase == input$tab2_phase) %>% 
        select_at(vars("survey", eval(parse(text = str_c(input$tab2_domain, "_vars"))))) %>%
        group_by(survey) %>%
        summarize_if(is.numeric, mean, na.rm = TRUE) %>% 
        ungroup() %>% 
        mutate_if(is.numeric, funs(percent(., 1)))
      colnames(summ_data)[2:ncol(summ_data)] <- eval(parse(text = str_c(input$tab2_domain, "_labels")))
      summ_data
    }
  })
  
  ### OUTPUTS
  
  
  # Tab 0: Maps SRI participation globally
  output$tab0_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>% 
      #addTiles() %>%
      setView(lng = 0, lat = 20, zoom = 3)
  })
  
  observe({
    
    palette <- tab0_palette()
    
    map_data <- tab0_data() %>% 
      st_as_sf() %>%  # need this command to "remind" Shiny that this is a SF data
      st_transform('+proj=longlat +datum=WGS84')
    statistic <- input$tab0_color # this is just a string; will need to parse within palette command below
    
    leafletProxy("tab0_map", data = map_data) %>%
      clearShapes() %>%
      clearControls() %>% 
      addPolygons(
        fillColor = ~palette(eval(parse(text = statistic))),
        stroke = TRUE,
        weight = 3,
        fillOpacity = 0.9,
        color = "white",
        label = lapply(tab0_label(), HTML)
      ) %>% 
      addLegend(
        position = "bottomright",
        pal = palette,
        values = c(0, getElement(tab0_stat_values(), 'max')),
        bins = ifelse(getElement(tab0_stat_values(), 'max') - getElement(tab0_stat_values(), 'min') <= 2, 1, 3),
        labFormat = labelFormat(digits = 0)
      )
  })
  
  # Tab 1: Creates the violin plot (for all SRI values) or the bar plot (for single domain)
  output$tab1_domain_plot <- renderPlot({
    if(input$tab1_domain == "All") {
      ggplot(data = tab1_data(), aes(y = SRI, x = phase, color = phase)) +
        geom_violin() +
        geom_jitter(position = position_jitter(width = 0.02), alpha = 0.4, size = 3) +
        theme_bw() +
        labs(x = NULL) +
        scale_color_manual(values = c("targeting" = "#4E84C4", "followup" = "#D16103",
                                      "followup #2" = "#C4961A")) +
        theme(legend.title = element_blank()) +
        theme(legend.position = "none") +
        theme(axis.text.x = element_text(size = 15),
              axis.text.y = element_text(size = 15),
              axis.title.y = element_text(size = 15))
    }
    
    else if(input$tab1_domain %in% c("domain1a", "domain1b", "domain2", "domain3", "domain4",
                                     "domain5", "domain6", "domain7", "domain11", "domain12a", "domain12b")) {
      ggplot(data = tab1_data(), aes(x = phase, fill = as_factor(eval(parse(text = input$tab1_domain))))) +
        geom_bar(aes(y = (..count..)/sum(..count..)), position = "fill", width = 0.25, color = "black") +
        theme_bw() +
        labs(x = NULL, y = "Response Percentage") +
        scale_y_continuous(labels = scales::percent_format()) + 
        scale_fill_manual(values = c("#4E84C4", "#52854C", "#D16103", "#C4961A", "#FFD86D"), 
                          name = "Response") +
        theme(legend.title = element_text(size = 15),
              legend.text = element_text(size = 13),
              axis.text.x = element_text(size = 15),
              axis.text.y = element_text(size = 15),
              axis.title.y = element_text(size = 15))
    }
    
    else if(input$tab1_domain %in% c("domain8", "domain9", "domain10")) {
      ggplot(data = tab1_mc_data(), aes(x = variable, y = prop, fill = phase)) +
        geom_bar(stat = "identity", position = "dodge", width = 0.25, color = "black") +
        theme_bw() +
        labs(x = "Support Type", y = "Response Percentage") +
        scale_x_discrete(labels = eval(parse(text = str_c(input$tab1_domain, "_labels")))) +
        scale_y_continuous(labels = scales::percent_format()) + 
        scale_fill_manual(values = c("#4E84C4", "#D16103"), 
                          name = "Phase") +
        theme(legend.title = element_text(size = 15),
              legend.text = element_text(size = 13),
              axis.text.x = element_text(size = 15),
              axis.text.y = element_text(size = 15),
              axis.title.x = element_text(size = 15),
              axis.title.y = element_text(size = 15))
    }
  })
  
  # Creates the data table with all respondents given the specifications
  output$tab1_domain_tbl <- DT::renderDataTable({
    if(input$tab1_domain == "All") {
      DT::datatable(tab1_data_brushed()[, c("id", "hh_age_1", "hh_gender_1", "phase", "SRI")],
                    colnames = c("ID", "Age of HH Member 1", "Gender of HH Member 1", "Phase", "SRI Score"),
                    rownames = FALSE)
    }
    
    else {
      DT::datatable(tab1_data()[, c("id", "hh_age_1", "hh_gender_1", "phase", input$tab1_domain, "SRI")],
                    colnames = c("ID", "Age, HH Member 1", "Gender, HH Member 1", "Phase", "Domain Response", "SRI Score"),
                    rownames = FALSE)
    }
  })
  
  # Tab 2: Creates the violin plot (for all SRI values) or the bar plot (for single domain)
  output$tab2_comp_plot <- renderPlot({
    
    if(input$tab2_domain == "All") {
      ggplot(data = tab2_data(), aes(y = SRI, x = survey, color = survey)) +
        geom_violin() +
        geom_jitter(position = position_jitter(width = 0.02), alpha = 0.4, size = 3) +
        theme_bw() +
        labs(x = NULL) +
        scale_color_manual(values = c("#4E84C4", "#52854C", "#D16103", "#C4961A")) +
        theme(legend.title = element_blank()) +
        theme(legend.position = "none") +
        theme(axis.text.x = element_text(size = 15),
              axis.text.y = element_text(size = 15),
              axis.title.y = element_text(size = 15))
    }
    
    else if(input$tab2_domain %in% c("domain1a", "domain1b", "domain2", "domain3", "domain4",
                                     "domain5", "domain6", "domain7", "domain11", "domain12a", "domain12b")) {
      ggplot(data = tab2_data(), aes(x = survey, fill = as_factor(eval(parse(text = input$tab2_domain))))) +
        geom_bar(aes(y = (..count..)/sum(..count..)), position = "fill", width = 0.25, color = "black") +
        theme_bw() +
        labs(x = NULL, y = "Response Percentage") +
        scale_y_continuous(labels = scales::percent_format()) + 
        scale_fill_manual(values = c("#4E84C4", "#52854C", "#D16103", "#C4961A", "#FFD86D"), 
                          name = "Response") +
        theme(legend.title = element_text(size = 15),
              legend.text = element_text(size = 13),
              axis.text.x = element_text(size = 15),
              axis.text.y = element_text(size = 15),
              axis.title.y = element_text(size = 15))
    }
    
    else if(input$tab2_domain %in% c("domain8", "domain9", "domain10")) {
      ggplot(data = tab2_data(), aes(x = variable, y = prop, fill = survey)) +
        geom_bar(stat = "identity", position = "dodge", width = 0.25, color = "black") +
        theme_bw() +
        labs(x = "Support Type", y = "Response Percentage") +
        scale_x_discrete(labels = eval(parse(text = str_c(input$tab2_domain, "_labels")))) +
        scale_y_continuous(labels = scales::percent_format()) + 
        scale_fill_manual(values = c("#4E84C4", "#52854C", "#D16103", "#C4961A"), 
                          name = "Survey") +
        theme(legend.title = element_text(size = 15),
              legend.text = element_text(size = 13),
              axis.text.x = element_text(size = 15),
              axis.text.y = element_text(size = 15),
              axis.title.x = element_text(size = 15),
              axis.title.y = element_text(size = 15))
    }
  })
  
  # Creates the summary data table for tab 2 given the specifications
  output$tab2_summ_tbl <- DT::renderDataTable({
    
    DT::datatable(tab2_summ_data(),
                  rownames = FALSE)
  })
  
  output$tab1_caritas_warn <- renderText(tab1_caritas_warn())
  output$tab2_caritas_warn <- renderText(tab2_caritas_warn())
}
