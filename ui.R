# ui.R

shinyUI(navbarPage(strong("V4-IDEA"), inverse=TRUE,
                   #footer=list("Author: Szymon Talaga"),
                   tabPanel("Introduction",
                            br(),
                            p("Hello,"),
                            p("welcome to the V4 Lab Project Interactive Data Exploration Application (V4-IDEA). It is aim is to provide you with convenient tools for easy exploration of the data collected for the purpose of the V4 Lab Project (learn more at:",
                              a(href="http://v4lab.eu/", "http://v4lab.eu/"),")."),
                            p("The application consists of four components:"),
                            tags$ol(
                                  tags$li("Interface for exploring distributions of variables that were measured in the study"),
                                  tags$li("Interface for studying relationships between two (selected) variables"),
                                  tags$li("Interface for studying associations betweem the L.I. scale and knowledge scores in groups (country and type of studies"),
                                  tags$li("Interface for analyzing difficulties of items concerning economic and financial knowledge in the joint sample and by country (including basic comparisons)")),
                            br(),
                            p("Full description of the dataset and the variables can be found in the ", a(href="https://github.com/sztal/V4Lab_Analyses/blob/master/Data/OfficialData/Codebook.pdf", "Codebook"), "."),
                            br(),
                            p(strong("Contact:"), "sztal111@gmail.com")),
                   tabPanel("Univariate distrubutions",
                            # Panel layout
                            sidebarLayout(
                                  sidebarPanel(
                                        helpText("Below is the menu for choosing
                                                 variables to explore.",
                                                 "There are two display options:
                                                 numerical summary or a chart.",
                                                 "If a variable is categorical,",
                                                 "then the numerical summary is raw",
                                                 "frequencies and the the chart",
                                                 "is a boxplot.",
                                                 "If a variable is numerical,",
                                                 "then the summary consists of the five",
                                                 "quartiles, the mean, and the standard deviation",
                                                 "and the chart is a histogram (with a vertical",
                                                 "line indicating the mean) with a customizable",
                                                 "number of bins.",
                                                 "Moreover, the output may be grouped by country",
                                                 "and/or type of studies.",
                                                 "Once all options are set, press 'Apply Changes.'"
                                                 ),
                                        helpText("For detailed description of the variables",
                                                 "please refer to the ",
                                                 a(href="https://github.com/sztal/V4Lab_Analyses/blob/master/Data/OfficialData/Codebook.pdf", "Codebook"), "."),
                                        selectInput(inputId="var",
                                                    label="Choose a variable to display",
                                                    choices=list( 
                                                          "L.-I. scale", "E-F knowledge: raw score",
                                                          "E-F knowledge: non-guessed score",
                                                          "F knowledge: non-guessed score",
                                                          "Parents' higher education: jointly",
                                                          "Father education", "Mother education",
                                                          "Study year", "Work experience",
                                                          "Gender", "Age",
                                                          "Hometown size PL", "Hometown size CZ",
                                                          "Country", "Type of education")),
                                        checkboxGroupInput(inputId="cross", label="grouping variables",
                                                           choices=list(
                                                                 "country", "type of studies")),
                                        sliderInput("bins",
                                                    "Number of bins:",
                                                    min = 5,
                                                    max = 50,
                                                    value = 19),
                                        checkboxInput(inputId="count",
                                                      label="raw frequencies (histograms)"),
                                        submitButton()
                                  ),
                                  
                                  # Show a plot of the generated distribution
                                  mainPanel(
                                        plotOutput("univarPlot"),
                                        verbatimTextOutput("univarSummary")
                                  )
                            )),
                   tabPanel("Bivariate associations",
                            # Panel layout
                            sidebarLayout(
                                  sidebarPanel(
                                        helpText("This panel shows bivariate relationships between",
                                                 "selected variables. Any variable from the dataset",
                                                 "can be selected. The output consists of a plot",
                                                 "and appropriate statistical model and/or",
                                                 "significance test. Concretely, pairs of continuous",
                                                 "variables are presented on a scatterplot and",
                                                 "summarized in a regression model.",
                                                 "Pairs of categorical variables are visualized",
                                                 "using levelplot and summarized in a contingency table",
                                                 "with tests of associations.",
                                                 "If one variable is continuous and one categorical,",
                                                 "then t test or ANOVA is conducted depending on the",
                                                 "number of groups and data is visualized via",
                                                 "box-and-whiskers plots (convention is:",
                                                 "box - minimum, median, maximum;",
                                                 "whiskers - +/- 1.5 IQR). Additionaly on box plots",
                                                 "group means are joined with a ragged red line."),
                                        selectInput(inputId="bivar1",
                                                    label="Choose the first variable",
                                                    choices=list( 
                                                          "L.-I. scale", "E-F knowledge: raw score",
                                                          "E-F knowledge: non-guessed score",
                                                          "F knowledge: non-guessed score",
                                                          "Parents' higher education: jointly",
                                                          "Father education", "Mother education",
                                                          "Study year", "Work experience",
                                                          "Gender", "Age",
                                                          "Hometown size PL", "Hometown size CZ",
                                                          "Country", "Type of education")),
                                        selectInput(inputId="bivar2",
                                                    label="Choose the first variable",
                                                    choices=list( 
                                                          "L.-I. scale", "E-F knowledge: raw score",
                                                          "E-F knowledge: non-guessed score",
                                                          "F knowledge: non-guessed score",
                                                          "Parents' higher education: jointly",
                                                          "Father education", "Mother education",
                                                          "Study year", "Work experience",
                                                          "Gender", "Age",
                                                          "Hometown size PL", "Hometown size CZ",
                                                          "Country", "Type of education")),
                                        submitButton()),
                                  mainPanel(
                                        plotOutput("bivarPlot"),
                                        verbatimTextOutput("bivarSummary")
                                        ))),
                   tabPanel("E-F knowledge and the L.-I. scale",
                            # panel layout
                            sidebarLayout(
                                  sidebarPanel(
                                        helpText("This panel is dedicated to the analysis",
                                                 "of relationships between the two most",
                                                 "important variables in the V4 Lab Project:",
                                                 "economic-financial (or strictly financal)",
                                                 "knowledge and",
                                                 "the L.-I. scale that indicates respondents'",
                                                 "preference towards economic liberalism",
                                                 "(negative pole of the scale) or interventionism",
                                                 "(positive pole of the scale)."),
                                        helpText("It is aim is to provide both graphical and",
                                                 "numerical descriptions of the correlation",
                                                 "between knowledge and the L.-I. scale.",
                                                 "Association between the variables is modelled",
                                                 "using linear regression. Plots depict data points",
                                                 "in the plane (x-axis is the L.-I. scale and y-axis",
                                                 "is knowledge). Additionaly numerical assesement of",
                                                 "the model may be printed (anova F test and marginal",
                                                 "tests; F tests use III type of sums of squares).",
                                                 "Analysis may be conducted in the subgroups",
                                                 "based on country and/or type of studies."
                                                 ),
                                        selectInput(inputId="varKL",
                                                    label="Choose a type of knowledge indicator",
                                                    choices=list(
                                                          "E-F knowledge: non-guessed score",
                                                          "F knowledge: non-guessed score")),
                                        checkboxGroupInput(inputId="crossKL",
                                                           label="grouping variables",
                                                           choices=list(
                                                                 "country", "type of studies")),
                                        checkboxInput(inputId="Ftests", label="F tests"),
                                        checkboxInput(inputId="interact",
                                                      label="Interactions in F tests"),
                                        checkboxInput(inputId="Mtests", label="Marginal tests"),
                                        submitButton()
                                        ),

                                  mainPanel(
                                        plotOutput("LIknowPlot"),
                                        verbatimTextOutput("regSummary"))
                                  )
                            ),
                   tabPanel("Items analysis",
                            # panel layout
                            sidebarLayout(
                                  sidebarPanel(
                                        helpText("This panel provides tools for item-wise",
                                                 "analysis of respondents E-F knowledge.",
                                                 "In other words it shows what were the difficulty",
                                                 "levels of specific items (difficulty level is",
                                                 "defined here as a fraction of respondents that",
                                                 "did not give the correct answer to an item).",
                                                 "Moreover the set of all items may be partitioned",
                                                 "into three subsets: 1) tricky items",
                                                 "(significantly harder than 0.5);",
                                                 "2) medium items (about 0.5);",
                                                 "3) easy items (significantly below 0.5).",
                                                 "Rationale for this partition scheme can be",
                                                 "found in the codebook.",
                                                 "The panel also allows grouping by country",
                                                 "and testing significance of differences",
                                                 "between Polish and Czech subsamples."),
                                        helpText(),
                                        helpText("Difficulty levels of items are reported with",
                                                 "95% Agresti-Coul confidence intervals.",
                                                 "Significance of differences between countries",
                                                 "is tested using standard asymptotic",
                                                 "chi-square test."),
                                        checkboxGroupInput(inputId="cntry",
                                                     label="Country/countries",
                                                     choices=list(
                                                           "PL", "CZ")),
                                        checkboxInput(inputId="stats", label="Show statistics"),
                                        checkboxInput(inputId="tests", label="Show comparisons"),
                                        checkboxInput(inputId="sigonly",
                                                      label="Show only statistically significant"),
                                        checkboxInput(inputId="parti", label="Show partition"),
                                        sliderInput("digits",
                                                    "Significant digits on display",
                                                    min = 0,
                                                    max = 6,
                                                    value = 3),
                                        submitButton()
                                  ),
                                  mainPanel(
                                        plotOutput("itemsPlot"),
                                        verbatimTextOutput("itemsSummary"))
                                  )
                            )))