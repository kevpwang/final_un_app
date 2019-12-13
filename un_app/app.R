#### libraries ####

library(shiny)
library(scales)
library(ggthemes)
library(gt)
library(broom)
library(vembedr)
library(tidyverse)

#### load relevant data ####

majs_gdps <- read_rds("majs_gdps.rds")
issues_majs <- read_rds("issues_majs.rds")
all_majs <- read_rds("all_majs.rds")

#### menu choices ####

country_choices_issues <- issues_majs %>% 
    distinct(countryname)

country_choices_gdps <- majs_gdps %>% 
    distinct(country_name) %>% 
    arrange()

issue_choices <- issues_majs %>% 
    distinct(issue) %>% 
    add_row(issue = "All") %>% 
    filter(issue != "other") %>% 
    arrange()

#### ui ####

ui <- fluidPage(
    navbarPage(
        "UN General Assembly Voting Patterns",
        
        #### Key Findings ####
        
        tabPanel(
            imageOutput("unga", width = "100%", height = "100%"),
            title = "Key Findings",
            fluidRow(column(2), column(8,
            h2("Analysis of UN General Assembly Voting Patterns", align = "center"),
            p(em("Roll call votes, 1946 - 2018"), align = "center"),
            h3("Background"),
            p(
                'The United Nations General Assembly (UNGA) is "the main deliberative, policymaking and representative 
                organ of the UN" [1]. It meets in annual session in New York and is the only UN body in which all member 
                countries enjoy equal representation.'
            ),
            p(
                "To be sure, the actual power of the UNGA is limited. Resolutions adopted by the UNGA are not binding on member 
                countries. In that sense, other institutions like the UN Security Council, comprised of five permanent 
                voting members with unilateral veto power, are considered more substantial actors on the global stage."
            ),
            p(
                "Nevertheless, the votes of the UNGA are a unique record of how the international community 
                has viewed the most important and persistant global issues in modern times. In particular, 
                this project focuses on how often and on what issues different countries vote in the majority.
                How often do different countries agree with the international consensus? Does the United States act as 
                the global leader that we believe ourselves to be? What factors influence how majorities are formed and 
                whom they include?"
            ),
            h3("America and the UNGA"),
            p(
                "This project uses a dataset compiled by Erik Voeten of Georgetown University containing all roll-call 
                votes in the UNGA from 1946 to 2018 [2]. To begin, I visualized how often the United States has
                voted in UNGA majorities. Though many resolutions may be symbolic, they reflect some degree of international 
                consensus on a given issue. Over the history of the UNGA, the US has often diverged from that consensus."
            ),
            plotOutput("us_majs_plot"),
            br(),
            p(
                "This voting pattern is even more striking when we zoom in on more recent years and compare it to 
                other major countries."
            ),
            plotOutput("comp_plot"),
            br(),
            p(
                'This data does not necessarily suggest that the US lacks global influence more generally. Nor does it suggest
                that the US fundamentally disagrees with basic international values of human rights and democracy. North Korea, 
                known as the "Hermit Kingdom" and hardly a paragon of the liberal international order, votes with the majority far more 
                frequently than the US. Thus, a more plausible intepretation is that the UNGA frequently chooses to consider 
                very specific issues on which the US has taken a minority stance. For instance, the US has consistently voted 
                against the majority on resolutions pertaining to the Palestinian conflict.'
            ),
            plotOutput("palestinian_plot"),
            br(),
            p(
                'On the "UNGA Majorities" page, you can see how often every member country has voted in the majority and
                on what kinds of issues.'
            ),
            h3("GDP model"),
            p(
                "I examined whether there is a relationship between a country's economy in a given year, as measured by GDP growth, 
                and how often it voted with in UNGA majorities. I used a dataset from the World Bank containing annual percentage change 
                in GDP since 1961 for every country [3]. I then ran a linear regression with GDP change as the explanatory
                variable and frequency in the majority as the response variable. I limited the analysis to the approximately
                one hundred countries that have continuous GDP data available dating back to 1961."
            ),
            plotOutput("gdp_coefs_plot", height = "600px"),
            p(
                "A bootstrap replication of these results revealed very little uncertainty surrounding the GDP 
                coefficient. As the plot shows, for the vast majority of countries, the coefficient associated 
                with GDP change ranges from slightly negative to slightly positive, with most countries 
                displaying essentially no relationship. The extreme outlier of South Africa is attributable to 
                unique historical circumstances. South Africa was suspended from the UNGA in 1974 over apartheid and reinstated 
                in 1994 following its transition to democracy. Owing to this two-decade gap in the data, no conclusions 
                should be drawn from the regression with regard to South Africa."
            ),
            p(
                "The results of this analysis are not surprising. While economic prosperity or decline might plausibly
                affect a country’s global power (very generally defined), the specific relationship 
                between GDP change and frequency in UNGA majorities is more tenuous. GDP itself is a highly constructed 
                metric, and minute differences in GDP change—a 3.1 percent increase this year versus 3.2 percent the 
                previous year—are hardly likely to significantly affect the fundamentals of a country’s international 
                standing. Along the same lines, the UNGA and “one country, one vote” do not reflect the realities of 
                international engagement. Many small countries with negligible influence vote 100 percent of the time 
                with the UNGA majority. For such countries, any relationship my analysis may indicate is almost certainly
                a result of random chance."
            ),
            p(
                "Some of the analysis can be seen as indicative of real-world trends. For instance, Israel 
                shows the highest coefficient of GDP change. This likely reflects the historical fact that its 
                economic strength has risen in tandem with other factors like military strength and diplomatic 
                standing, which then indirectly translate into greater power in the UN. The same reasoning could 
                be applied to China, which also shows a fairly positive relationship. For other countries like 
                the former European colonial powers, a negative relationship could suggest economic decline
                accompanied by a broader loss in international status, but further case-by-case investigation 
                is needed to determine the exact historical circumstances."
            ),
            p(
                "Future research on UNGA voting patterns may focus more specifically on the influence of countries like 
                the United States. For instance, there may be a relationship between US foreign aid to a country, 
                or military presence near a country, and the frequency at which that country votes with the US 
                in the UNGA."
            ),
            h4("Sources"),
            "[1]", a(href = "https://www.un.org/en/ga/about/index.shtml", "https://www.un.org/en/ga/about/index.shtml"),
            br(),
            "[2] Erik Voeten 'Data and Analyses of Voting in the UN General Assembly' Routledge Handbook of 
            International Organization, edited by Bob Reinalda (published May 27, 2013). Available at SSRN:", 
            a(href = "http://ssrn.com/abstract=2111149", "http://ssrn.com/abstract=2111149"),
            br(),
            "[3]", a(href = "https://data.worldbank.org/indicator/NY.GDP.MKTP.KD.ZG", 
                     "https://data.worldbank.org/indicator/NY.GDP.MKTP.KD.ZG")
            ))
        ),
        
        #### UNGA Majorities ####
        
        tabPanel(
            title = "UNGA Majorities",
            
            # inputId goes into server
            # inputId names must be distinct
            
            sidebarPanel(
                p("Select a country to see how often it votes in the majority."),
                selectInput(
                    inputId = "country_issues",
                    label = "Country:",
                    choices = country_choices_issues,
                    selected = "United States of America"
                ),
                p("Select an issue category."),
                selectInput(
                    inputId = "issue",
                    label = "Issue:",
                    choices = issue_choices,
                    selected = "All"
                )
            ),
            mainPanel(
                plotOutput("issues_majs_plot")
            ),
        ),
        
        #### GDP Model ####
        
        tabPanel(
            title = "GDP Model",
            sidebarPanel(
                p("Select a country to see the relationship between its economy and how often it votes in UNGA majorities."),
                
                # used later in plot section to receive country choice
                # set default value with selected, same with value in checkboxInput()
                
                selectInput(
                    inputId = "country_gdps",
                    label = "Country:",
                    choices = country_choices_gdps,
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
            
            # put all graphical elements in single mainPanel()
            
            mainPanel(
                plotOutput("gdp_plot"),
                br(),
                tableOutput("summary")
            ),
        ),
        
        #### About ####
        
        tabPanel(
            title = "About",
            fluidRow(column(3), column(6, 
                                       h3("Video Introduction to Project", align = "center"), 
                                       column(3))),
            fluidRow(column(4), column(4, 
                                       embed_url("https://youtu.be/l6cz-G4r5bY"),
                                       column(4))),
            fluidRow(column(3), column(6,     
                                       h3("Information", align = "center"),
                                       p("Project for Government 1005: Data", align = "center"),
                                       p("Kevin Wang, Harvard College 2023", align = "center"),
                                       p("Email: kpwang@college.harvard.edu", align = "center"),
                                       p(tags$a(href = "https://github.com/kevinwang13", "GitHub"), align = "center"),
                     column(3)))
        )
    )
)

#### server ####

server <- function(input, output) {
    #### UNGA image ####
    
    output$unga <- renderImage ({
        list(src = "unga.jpg",
             height = 333,
             width = 600, style="display: block; margin-left: auto; margin-right: auto;")
    }, deleteFile = FALSE )
    
    #### US majorities plot ####
    
    output$us_majs_plot <- renderPlot ({
        plot <- all_majs %>% 
            filter(country == "USA") %>% 
            ggplot(aes(x = year, y = prop_maj)) +
            geom_line(color = "blue", size = 1) +
            scale_y_continuous(labels = percent, limits = c(0, 1)) +
            labs(
                title = "America often disagrees with the world...",
                subtitle = "Frequency in UN Majorities, 1946 - 2018",
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
    
    #### Comparison plot ####
    
    output$comp_plot <- renderPlot({
        plot <- all_majs %>% 
            filter(country %in% c("USA", "GBR", "CAN", "RUS")) %>% 
            filter(year >= 1980) %>% 
            ggplot(aes(x = year, y = prop_maj)) +
            geom_line(aes(color = countryname), size = 1) +
            scale_y_continuous(labels = percent, limits = c(0, 1)) +
            labs(
                title = "...especially since the 1980s...",
                caption = 'Source: Erik Voeten, "Data and Analyses of Voting in the UN General Assembly"'
            ) + 
            scale_color_discrete(name = "Country", labels = c("Canada", "USSR/Russia", "United Kingdom", "United States")) +
            theme_fivethirtyeight() +
            theme(
                plot.caption = element_text(margin = margin(20,1,1,1))
            )
        plot
    })
    
    #### Palestinian plot ####
    
    output$palestinian_plot <- renderPlot ({
        plot <- issues_majs %>% 
            filter(country == "USA" & issue == "Palestinian Conflict") %>% 
            ggplot(aes(x = year, y = prop_maj)) +
            geom_point() +
            geom_line(color = "blue", size = 0.75) +
            scale_y_continuous(labels = percent, limits = c(0, 1)) +
            labs(title = "...and especially about Palestine",
                 caption = 'Source: Erik Voeten, "Data and Analyses of Voting in the UN General Assembly"'
            ) + 
            theme_fivethirtyeight() +
            theme(
                plot.caption = element_text(margin = margin(20,1,1,1))
            )
        plot
    })
    
    #### GDP regression plot ####

    output$gdp_plot <- renderPlot({
        plot <- majs_gdps %>% 
            
        # filter country_name based on received input from user
            
            filter(str_detect(country_name, input$country_gdps)) %>% 
            ggplot(aes(x = growth, y = prop_maj)) +
            geom_point() +
            geom_smooth(method = "lm", se = FALSE, size = 1) +
            scale_x_continuous(labels = percent) +
            scale_y_continuous(labels = percent) +
            
            # NB: concatenate strings with paste(), default sep = " "
            # use user input to indicate that the plot is the right country
            
            labs(
                title = "Economic Growth vs. Frequency in UNGA Majorities",
                subtitle = paste(input$country_gdps, ", 1961-2018", sep = ""),
                caption = 'Source: Erik Voeten, "Data and Analyses of Voting in the UN General Assembly"; the World Bank'
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
    
    #### Issue majorities plot ####
    
    output$issues_majs_plot <- renderPlot({
        
        # if all issues selected, use cleaned all_majs data w/correct countryname var
        # use inputId name
        # use user input in plot subtitle
        
        if(input$issue == "All") {
            plot <- all_majs %>% 
                filter(str_detect(countryname, input$country_issues)) %>% 
                ggplot(aes(x = year, y = prop_maj)) +
                geom_point() +
                geom_line(color = "blue", size = 0.75) +
                scale_y_continuous(labels = percent, limits = c(0, 1)) +
                labs(
                    title = "Frequency in UN Majorities",
                    subtitle = input$country_issues,
                    caption = 'Source: Erik Voeten, "Data and Analyses of Voting in the UN General Assembly"'
                ) + 
                theme_fivethirtyeight() +
                theme(
                    plot.caption = element_text(margin = margin(20,1,1,1))
                )
            plot
        }
        
        # if specific issue selected, use issues_majs; same countryname var
        
        else {
            plot <- issues_majs %>% 
                
                # filter countryname and issue based on received input from user
                
                filter(str_detect(countryname, input$country_issues)) %>% 
                filter(str_detect(issue, input$issue)) %>%
                ggplot(aes(x = year, y = prop_maj)) +
                
                # geom_point() neccessary bc. not all issues appear in all years,
                # so geom_line() alone will be misleading.
                # make line thinner to display points
                # limits ensures y-axis scale does not exceed 100%
                
                geom_point() +
                geom_line(color = "blue", size = 0.75) +
                scale_y_continuous(labels = percent, limits = c(0, 1)) +
                
                # NB: concatenate strings with paste(), default sep = " "
                
                labs(
                    title = paste("Frequency in UN Majorities", " on ", input$issue, sep = ""),
                    subtitle = input$country_issues,
                    caption = 'Source: Erik Voeten, "Data and Analyses of Voting in the UN General Assembly"'
                ) + 
                theme_fivethirtyeight() +
                theme(
                    plot.caption = element_text(margin = margin(20,1,1,1))
                )
            plot
        }
    })
    
    #### Regression summary ####
    
    # tidy() in broom pkg is rendered as table
    # give alignment arg in renderTable()
    
    output$summary <- renderTable({
        if (input$summary == TRUE) {
            country <- majs_gdps %>% 
                filter(str_detect(country_name, input$country_gdps))
            model <- lm(prop_maj ~ growth, data = country)
            tidy(model)
        }
    },
    align = "c"
    )
    
    #### GDP coefficients plot ####
    output$gdp_coefs_plot <- renderPlot ({
        
        # nest() and create model for each country
        # interested in coef of `growth`
        
        gdp_models <- majs_gdps %>% 
            group_by(country_name) %>% 
            nest() %>% 
            mutate(model = map(data, ~lm(prop_maj ~ growth, data = .x))) %>% 
            mutate(coefficients = map(model, ~coef(.x))) %>% 
            mutate(growth_coef = map_dbl(coefficients, ~pluck(.x, "growth"))) %>%
            select(-model, -data, -coefficients) %>% 
            ungroup()
        
        # reorder coefs for more helpful visualization
        
        plot <- ggplot(gdp_models, aes(x = growth_coef, y = fct_reorder(country_name, growth_coef, .fun = median))) +
            geom_point() +
            labs(title = "Models of % GDP Change vs. Frequency in UN Majorities",
                 x = "Coefficient of GDP Change",
                 y = "")
        plot
    })
    
}

#### run the application #### 
shinyApp(ui = ui, server = server)
