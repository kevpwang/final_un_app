#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(scales)
library(ggthemes)
library(tidyverse)

majs_gdps <- read_rds("majs_gdps.rds")

country_choices <- majs_gdps %>% 
    distinct(country_name) %>% 
    arrange()

ui <- fluidPage(
    navbarPage(
        "Analysis of UN General Assembly Votes",
        tabPanel(
            title = "About",
            h3("Background"),
            p(
                "The United Nations General Assembly is 'the main deliberative, policymaking and representative organ of the UN' (https://www.un.org/en/ga/about/index.shtml). The UNGA meets in annual session in New York and is only body of the UN in which all member countries enjoy equal representation."
            ),
            p(
                "The actual power of the UNGA is subject to numerous limitations. Most notably, resolutions adopted by the UNGA are not considered binding on member countries. In that sense, other institutions like the UN Security Council, comprised of five permanent voting members with unilateral veto power, are far more substantive actors in the course of global affairs."
            ),
            p(
                "Nevertheless, the votes of the UNGA are meaningful. The UNGA offers a unique record, spanning over seventy years, of how the whole international community has viewed what it deems the most pressing global issues of a particular time. More significantly for the United States, the votes of the UNGA provide insight on the reality of our aspirations to global leadership. In a forum of equal representation, how often do we agree with other countries? Where have our interests aligned or diverged? How might our positions on the world stage relate to domestic circumstances or the idiosyncrasies of our politics?"
            ),
            p(
                "Through analysis and visualization of UNGA voting data, this project will evaluate conventional ideas about the UN through an empirical lens, highlight patterns and trends that may be surprising, and ultimately contribute to finding answers to those important questions."
            ),
            h3("Data"),
            p(
                "Erik Voeten of Georgetown University has compiled a dataset containing all roll-call votes in the UN General Assembly from 1946 to 2018. It is available at the Harvard Dataverse at the following link: https://doi.org/10.7910/DVN/LEJUQZ."
            ),
            p(
                "Citation: Erik Voeten 'Data and Analyses of Voting in the UN General Assembly' Routledge Handbook of International Organization, edited by Bob Reinalda (published May 27, 2013). Available at SSRN: http://ssrn.com/abstract=2111149)"
            ),
            p(
                "Each observation in the data represents a single vote by a country on a UNGA resolution. There are four kinds of votes: 'yes', 'no', 'abstain', and 'absent'. Each observation also records the final vote tally on the resolution and a short description of resolution's content. One variable in the data identifies votes that are considered 'important' by the US State Department. The 'issue code' variables classify certain resolutions by subject matter (e.g. 'Palestinian conflict', 'nuclear weapons', 'human rights'). This allows analysis of, for example, how a country tends to vote on a particular issue."
            ),
            p(
                "The GDP data comes from the World Bank and records the annual percentage change in GDP from all countries. Data is not necessarily available for all countries over all period of time."
            ),
            p(
                "Citation: https://data.worldbank.org/indicator/NY.GDP.MKTP.KD.ZG."
            )
        ),
        tabPanel(
            title = "Model",
            sidebarPanel(
                p("Select a country to see the relationship between its economy and UNGA voting power."),
                selectInput(
                    inputId = "country",
                    label = "Country:",
                    choices = country_choices,
                    selected = "United States"
                ),
                br(),
                p(
                    "Regression Summary contains specific statistical information."
                ),
                checkboxInput(
                    inputId = "summary",
                    label = "Show Regression Summary",
                    value = FALSE
                )
            ),
            mainPanel(plotOutput("UNPlot")),
            br(),
            br(),
            br(),
            br(),
            br(),
            verbatimTextOutput("summary")
        ),
        tabPanel(
            title = "Statistical Choices"
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$UNPlot <- renderPlot({
        plot <- majs_gdps %>% 
            filter(str_detect(country_name, input$country)) %>% 
            ggplot(aes(x = growth, y = prop_maj)) +
            geom_point() +
            geom_smooth(method = "lm", se = FALSE, size = 1) +
            scale_x_continuous(labels = percent) +
            scale_y_continuous(labels = percent) +
            labs(
                title = "Economic Growth vs. Frequency in UNGA Majority",
                caption = 'Source: Erik Voeten, "Data and Analyses of Voting in the UN General Assembly"'
            ) + 
            theme_fivethirtyeight() +
            
            # NB: use margin to shift elements up, down, &c. Top margin is first value.  
            
            theme(
                plot.caption = element_text(margin = margin(20,1,1,1))
            )
        plot
    })
    
    output$summary <- renderPrint({
        if (input$summary == TRUE) {
            country <- majs_gdps %>% 
                filter(str_detect(country_name, input$country))
            model <- lm(prop_maj ~ growth, data = country)
            summary(model)
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
