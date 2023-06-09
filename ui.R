# Define UI for application that draws a histogram
shinyUI(
  fluidPage(
    useShinyjs(),
    
    # Application title
    navbarPage("Self-Reliance Index Data", theme = shinytheme("lumen"),
               tabPanel("SRI Usage Map", icon = icon("globe"), fluid = TRUE,
                        
                        sidebarLayout(
                          sidebarPanel(
                            p("This panel describes SRI usage globally."),
                            p(strong("First, select whether you would like to view data for all time,
                                     or by year. Then, select the variable that the map's color scale
                                     should use.")),
                            
                            selectizeInput(
                              inputId = "tab0_time",
                              label = "Time Frame",
                              choices = c("All Time", sort(unique(country_yearly_shp$year))),
                              selected = "All Time"
                            ),
                            
                            selectizeInput(
                              inputId = "tab0_color",
                              label = "Color variable:",
                              choices = "",
                              selected = "surveys"
                            ),
                            
                            selectizeInput(
                              inputId = "tab0_sri_domains",
                              label = "Select SRI domains to include in tooltip. Select up to 4.",
                              choices = "",
                              multiple = TRUE,
                              options = list(maxItems = 4, placeholder = "Only applicable if a yearly time frame is selected.")
                            )
                          ),
                          
                          mainPanel(
                            leafletOutput(
                              outputId = "tab0_map",
                              width = "100%",
                              height = 600
                            )
                          )
                        )
               ),
               
               tabPanel("Domain Information", icon = icon("list-ul"), fluid = TRUE,
                        sidebarLayout(
                          sidebarPanel(
                            p("This panel describes domain distributions for the selected SRI survey 
                              round."),
                            p(strong("Select the agency, country, survey round, and domain of interest.")),
                            
                            # Select Agency
                            selectizeInput(
                              inputId = "tab1_agency",
                              label = "Select Agency",
                              choices = sort((unique(data$agency))),
                              selected = "Sitti"
                            ),
                            
                            # Select Country
                            selectizeInput(
                              inputId = "tab1_country",
                              label = "Select Country",
                              choices = c("")
                            ),
                            
                            # Select Round
                            selectizeInput(
                              inputId = "tab1_round",
                              label = "Select Round",
                              choices = c("")
                            ),
                            
                            # Select Domain
                            selectizeInput(
                              inputId = "tab1_domain",
                              label = "Select Domain",
                              choices = domain_opts,
                              selected = "All"
                            )
                          ),
                          
                          mainPanel(
                            plotOutput(outputId = "tab1_domain_plot",
                                       brush = "tab1_brush"),
                            hr(),
                            helpText("Tip: When viewing the violin plot (Domain selection is 'All'),
                                     you can highlight points to populate the table. When a domain
                                     is selected, all respondents in that round of surveying will
                                     be included in the table."),
                            textOutput(outputId = "tab1_caritas_warn"),
                            br(),
                            dataTableOutput(outputId = "tab1_domain_tbl")
                          )
                          
                        )
               ),
               
               tabPanel("Cross-Survey Comparison", icon = icon("handshake"), fluid = TRUE,
                        sidebarLayout(
                          sidebarPanel(
                            p("This panel allows you to compare SRI scores and domain outcomes across
                              different surveys. The options are listed in the format: 
                              agency/country/round. You are only allowed to make comparisons at the same
                              phase."),
                            
                            # Select Survey
                            selectizeInput(
                              inputId = "tab2_survey",
                              label = "Select Survey (maximum 4)",
                              choices = sort((unique(data$survey))),
                              multiple = TRUE,
                              options = list(maxItems = 4, placeholder = "Search by agency, country, survey round, or phase.")
                            ),
                            
                            # Select Phase
                            selectizeInput(
                              inputId = "tab2_phase",
                              label = "Select Phase (targeting or followup)",
                              choices = c("")
                            ),
                            
                            helpText("Tip: Be aware that not every survey round has a targeting
                                     and followup phase. If you select a new survey, and the plot
                                      and table do not update, it may be that it
                                     does not have a followup phase."),
                            
                            # Select Domain
                            selectizeInput(
                              inputId = "tab2_domain",
                              label = "Select Domain",
                              choices = domain_opts,
                              selected = "All"
                            )
                          ),
                          
                          mainPanel(
                            plotOutput(outputId = "tab2_comp_plot"),
                            br(),
                            textOutput(outputId = "tab2_caritas_warn"),
                            br(),
                            dataTableOutput(outputId = "tab2_summ_tbl")
                          )
                        )
               ),
               
               tabPanel("About", icon = icon("info-circle"), fluid = TRUE,
                        h2("How should the SRI be administered?"),
                        h4("Frequency"),
                        p("It is envisioned that the SRI will be administered
                        with the same clients every 3 or 6 months for the
                        duration of time they receive services or assistance.
                        If they are “graduated” from assistance, it is
                        recommended to continue monitoring the household for
                        some period after assistance ends to ensure sustained
                        self-reliance given the fragility of refugee situations."),
                        h4("Assessment vs. Questionnaire"),
                        p("The SRI is not designed as a questionnaire to read
                        verbatim and simply record clients’responses. Rather, it
                        is a tool to aid an assessor in making an educated
                        evaluation of the household’s status across the domains.
                        The assessor will use a combination of discussion with
                        the clients, direct observation (e.g. during a home
                        visit), knowledge of local conditions, and any prior
                        knowledge of the household’s circumstances, to arrive at
                          an evaluation."),
                        p("It is understood that assessor bias may be a concern
                        with this type of tool. The User Guide elaborates on
                        assessment elements within each domain to minimize bias,
                        as well as training and administration techniques to
                        maximize the validity of the SRI. An understanding of
                        the local context will also help minimize assessor bias."),
                        h4("Respondents"),
                        p("For the sake of data-tracking, a “Chief Respondent” 
                        should be identified for each household, but that person
                        need not be the traditional head of household. When 
                        possible, efforts should be made to include the 
                        perspectives of all household members. An inherent
                        challenge in a household-level measurement is assigning
                        a single score meant to reflect the aggregate experience
                        of all household members. Experiences can be highly
                          individualized, particularly given age, gender, health
                          and other differences. In addition to the SRI, the
                          assessor may wish to use more detailed survey tools to
                          assess differential statuses among household members."),
                        h4("Scoring"),
                        p("The response options of the SRI cannot cover every
                        variation in a household’s situation. The assessor should
                        select the option that most closely resembles the
                        household’s circumstances. The calculations behind the
                        scores are programmed to compute automatically in the
                        digital versions of the tool. A detailed scoring syntax
                        will be provided to those not using the digital version.
                        Scores on individual domains may aid in making referrals
                        for needed services or improving relevant sectoral 
                        responses. The aggregate household score signals the
                        household’s overall level of self-reliance. Agencies may
                        wish to set score thresholds that trigger certain events,
                        such as entry to and exit from services/support."),
                        h4("Non-Exhaustive"),
                        p("The SRI is not designed to be the exhaustive source of
                        information that agencies use for their programming.
                        Service providers will likely want to gather additional
                        information to understand the needs and challenges of
                        each household and its members. The SRI can be used to
                        complement existing monitoring tools."),
                        h4("Simplicity"),
                        p("Lastly, efforts have been made to pare the SRI down
                        to the fewest domains possible for assessing
                        self-reliance. It is designed for simplicity of use and
                        to respect the refugee household’s time. The intent of
                        the SRI is to provide a high-level, reliable indication
                        of refugee households’ experience and change over time."),
                        p(strong("This Shiny application was designed by Sam Hsu.
                          Contact: yuehya.hsu@gmail.com"))
               )
               
    ),
    
  )
)
