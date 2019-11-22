library(shiny)
library(scales)
library(ggthemes)
library(tidyverse)

# load relevant RDS for use in plot

majs_gdps <- read_rds("majs_gdps.rds")

# list of country choices for the drop-down menu

country_choices <- majs_gdps %>% 
    distinct(country_name) %>% 
    arrange()

####### UI #######

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
                
                # used later in plot section to receive country choice
                # set default value with selected, same with value in checkboxInput()
                
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

            # fix this later: line breaks seem to have no effect here

            br(),
            br(),
            br(),
            br(),
            br(),
            verbatimTextOutput("summary")
        ),
        tabPanel(
            title = "Statistical Choices",
            h3("Initial explorations"),
            p(
                "I began the project by trying to explore and visualize how often different countries voted as part of a majority in the UN General Assembly. Even given the symbolic value of many UNGA resolutions, one can reasonably expect that how often a country 'gets its way'--i.e. sees the desired success or failure of a particular agenda item--relates to its international standing and how it is viewed by the international community. One striking trend I immediately found was how infrequently the US is part of a UNGA majority. In a few years, the US votes with the majority on up to 75 percent of votes. Far more frequently, however, the US votes with the majority in under 20 percent of votes--in other words, the US disagrees with the international consensus up to 80 percent of the time."
            ),
            h3("Data-processing choices"),
            p(
                "I wondered whether a country's economic growth would be associated with more UNGA majority votes, which I am assuming as a reasonable proxy for some dimension of international influence. I used data from the World Bank containing percent change in GDP for all countries since 1961. The data contained many blanks; this is to be expected, since many countries likely did not or were not able to collect accurate annual economic data for the entirety of the past five decades. Therefore, I decided to limit the countries in the model to only those for which there exists annual data from 1961 to 2018."
            ),
            p(
                "Analyzing Erik Voeten's raw data of all UNGA roll call votes, I created a clean dataset of the proportion of all votes for which a country was in the majority ('majorities data'). Because the GDP data had to be restricted to countries that had continuous data going back to 1961, I also restricted my majorities data to 1961 at the earliest. I also considered that 'yes' and 'no' are not the only possible votes. A country can either vote 'abstain' or be absent, though obviously neither ever formed a majority vote. I decided to exclude a country's absentions or absences from the total number of votes in which they participated. This was largely because in certain early UNGA sessions, some countries like Albania only cast 'abstain' or were absent. Therefore, to include those votes would result in an anomalously low majority rate that is attributable to idiosyncratic circumstances, not to the explanatory variable of economic strength."
            ),
            h3("Regression"),
            p(
                "For the purposes of this rough draft, my model uses a linear regression for each country, with GDP change as the explanatory variable and proportion of votes in the majority as the response variable. The user also has an option to view the summary of the model for more detailed statistical information. The results of the model vary significantly across different countries. For the US and China, the model suggests that higher economic growth is associated with a greater proportion of majority votes. Other countries like Australia and the UK show a slight negative association."
            ),
            p(
                "For a country like China, the association makes intuitive sense: China's economy has grown in tandem with other factors like military strength and diplomatic strength that could affect its international standing generally. For other countries, a negative association could suggest economic decline accompanied by loss of importance, but specific investigation is needed to determine exactly how circumstances change and whether they justify the association. For small or internationally inconsiderable countries, the association is more likely to be random chance rather than any sort of deep pattern."
            )
        )
    )
)

server <- function(input, output) {

    output$UNPlot <- renderPlot({
        plot <- majs_gdps %>% 
            
        # filter country_name based on received input from user
            
            filter(str_detect(country_name, input$country)) %>% 
            ggplot(aes(x = growth, y = prop_maj)) +
            geom_point() +
            geom_smooth(method = "lm", se = FALSE, size = 1) +
            scale_x_continuous(labels = percent) +
            scale_y_continuous(labels = percent) +
            
            # NB: concatenate strings with paste(), default sep = " "
            # use user input to indicate that the plot is the right country
            
            labs(
                title = "Economic Growth vs. Frequency in UNGA Majority",
                subtitle = paste(input$country, ", 1961-2018", sep = ""),
                caption = 'Source: Erik Voeten, "Data and Analyses of Voting in the UN General Assembly"'
            ) + 
            
            # NB: fivethirtyeight() automatically hides x- and y-axis labels
            
            theme_fivethirtyeight() +
            
            # NB: use margin to shift elements up, down, &c. Top margin is first value.  
            
            theme(
                plot.caption = element_text(margin = margin(20,1,1,1))
            )
        
        # make sure to display separately
        
        plot
    })
    
    output$summary <- renderPrint({
        if (input$summary == TRUE) {
            
            # NB: cannot pipe into lm()
            
            country <- majs_gdps %>% 
                filter(str_detect(country_name, input$country))
            model <- lm(prop_maj ~ growth, data = country)
            summary(model)
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
