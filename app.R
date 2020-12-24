library(tidyverse)
library(shiny)
library(shinythemes)

ui <- fluidPage(
    theme = shinytheme('united'),
    
    titlePanel('Most Wanted Industry Skills'),
    p('Match industry with skills, skill with industries.'),
    hr(),
    
    sidebarLayout(
        sidebarPanel(
            width = 6,
            tabsetPanel(
                tabPanel(
                    'Start with Industry',
                    
                    h4('First choose an industry group:'),
                    uiOutput('industry_groups'),
                    
                    h4('Then choose an industry:'),
                    uiOutput('industry_names'),
                    
                    p('The most wanted skills of the industry will show up.')
                ),
                tabPanel(
                    'Start with Skill',
                    
                    h4('First choose a skill group:'),
                    uiOutput('skill_groups'),
                    
                    h4('Then choose a skill:'),
                    uiOutput('skill_names'),
                    
                    p('The industries where the skill is most wanted will show up.')
                )
            )
            
        ),
        
        mainPanel(
            width = 6,
            tableOutput('output_table')
        )
    ),
    
    p(
        a('Data source: LinkedIn Data for Development @ World Bank Group', href = 'https://linkedindata.worldbank.org', target = '_blank'),
        br(),
        a('Project by Ricky Soo | Free and Open Source', href = 'https://github.com/rickysoo', target = '_blank')
    )
)

server <- function(input, output, session) {
    values <- reactiveValues(
        tab = 'Industry'
    )
    
    load_data <- reactive({
        read.csv('public_use-industry-skills-needs.csv') %>%
            filter(year == max(year)) %>%
            select(
                `Industry Group` = isic_section_name,
                `Industry Name` = industry_name,
                `Skill Group` = skill_group_category,
                `Skill Name` = skill_group_name,
                `Rank` = skill_group_rank
            )
    })
    
    output$industry_groups <- renderUI({
        choices <- load_data() %>%
            pull(`Industry Group`) %>%
            unique() %>%
            sort()
        
        selectInput(
            inputId = 'industry_group',
            label = NULL,
            choices = choices
        )
    })  
    
    output$industry_names <- renderUI({
        if (is.null(input$industry_group)) {
            return()
        }
        
        choices <- load_data() %>%
            filter(`Industry Group` == input$industry_group) %>%
            pull(`Industry Name`) %>%
            unique() %>%
            sort()
        
        selectInput(
            inputId = 'industry_name',
            label = NULL,
            choices = choices
        )
    })  
    
    output$skill_groups <- renderUI({
        choices <- load_data() %>%
            pull(`Skill Group`) %>%
            unique() %>%
            sort()
        
        selectInput(
            inputId = 'skill_group',
            label = NULL,
            choices = choices
        )
        
    })  
    
    output$skill_names <- renderUI({
        if (is.null(input$skill_group)) {
            return()
        }
        
        choices <- load_data() %>%
            filter(`Skill Group` == input$skill_group) %>%
            pull(`Skill Name`) %>%
            unique() %>%
            sort()
        
        selectInput(
            inputId = 'skill_name',
            label = NULL,
            choices = choices
        )
        
    })  
    
    output$output_table <- renderTable(
        {
            if (is.null(input$industry_name)) {
                return()
            }
            
            load_data() %>%
                select(`Industry Name`, `Skill Name`, `Rank`) %>%
                { if (values$tab == 'Industry') filter(., `Industry Name` == input$industry_name) else filter(., `Skill Name` == input$skill_name) } %>%
                arrange(Rank)
        },
        
        bordered = TRUE,
        striped = TRUE,
        hover = TRUE
    )
    
    observeEvent(input$industry_name, {
        values$tab <- 'Industry'    
    })
    
    observeEvent(input$skill_name, {
        values$tab <- 'Skill'    
    })
    
}

shinyApp(ui, server)