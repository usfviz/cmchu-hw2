if("ggrepel" %in% rownames(installed.packages()) == FALSE) {install.packages("ggrepel")}

library(shiny)
library(ggplot2)
library(reshape2)
library(tidyr)
library(ggrepel)

#############
# load data #
#############

fert_rate <- read.csv('API_SP.DYN.TFRT.IN_DS2_en_csv_v2.csv', skip=4) 
life_exp <- read.csv('API_SP.DYN.LE00.IN_DS2_en_csv_v2.csv', skip=4)
metadata <- read.csv('Metadata_Country_API_SP.DYN.LE00.IN_DS2_en_csv_v2.csv')
population <- read.csv('API_SP.POP.TOTL_DS2_en_csv_v2.csv')

##############
# clean data #
##############

metadata$SpecialNotes <- NULL
metadata$X <- NULL
metadata$Country.Code <- NULL
life_exp$X <- NULL
life_exp$X2015 <- NULL
life_exp$X2016 <- NULL
fert_rate$X <- NULL
fert_rate$X2015 <- NULL
fert_rate$X2016 <- NULL
population$X <- NULL
population$X2015 <- NULL
population$X2016 <- NULL

names(metadata) <- c("Region", "Income_Group", "Country")
names(life_exp) <- c("Country.Name", "Country.Code", "Indicator.Name", "Indicator.Code", 1960:2014)
names(fert_rate) <- c("Country.Name", "Country.Code", "Indicator.Name", "Indicator.Code", 1960:2014)
names(population) <- c("Country.Name", "Country.Code", "Indicator.Name", "Indicator.Code", 1960:2014)

life_exp_melt <- melt(life_exp, id.vars = "Country.Name", measure.vars = as.character(c(1960:2014)))
fert_rate_melt <- melt(fert_rate, id.vars = "Country.Name", measure.vars = as.character(c(1960:2014)))
population <- melt(population, id.vars = "Country.Name", measure.vars = as.character(c(1960:2014)))

names(life_exp_melt) <- c("Country", "Year", "Life_Expectancy")
names(fert_rate_melt) <- c("Country", "Year", "Fertility_Rate")
names(population) <- c("Country", "Year", "Population")

df <- merge(life_exp_melt, fert_rate_melt, by=c("Country", "Year"))
df <- merge(df, population, by=c("Country", "Year"))
df <- merge(df, metadata, by="Country")

df$Year <- as.numeric(as.character(df$Year))
df$Region[df$Region == ""] <- NA
df <- df %>% drop_na(Country, Year, Life_Expectancy, Fertility_Rate, Region, Population)

########################
# create visualization #
########################

ui <- fluidPage(
  mainPanel(
    selectInput("plot_select", "Continent", 
                c("All", sort(as.character(unique(df$Region))))),
    selectInput("select_country", "Countries to Track",
                sort(as.character(unique(df$Country))), multiple=TRUE),
    div(style='position:relative',
      plotOutput("plot",
            hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce")),
            uiOutput("hover_info")),
    sliderInput("plot_slider", label=NULL, min=1960, 
                max=2014, value=1960, step=1, animate=TRUE,
                ticks=FALSE, sep="", width='82%'), 
    width = 11)
)

server <- function(input, output) {
  output$plot <- renderPlot({
    if (is.null(input$select_country)) {
      if (input$plot_select == "All") {
        ggplot(df[df$Year == input$plot_slider,]) + 
          geom_point(aes(x=Life_Expectancy, y=Fertility_Rate, color=Region, size=Population), alpha=.7) + 
          xlab("Life Expectancy") + ylab("Fertility Rate") + 
          scale_size(range = c(4, 20), breaks=c(1e+06, 1e+07, 5e+07, 1e+08, 3e+08, 6e+08), 
                     labels=c("1,000,000", "10,000,000", "50,000,000", "100,000,000", "300,000,000", "600,000,000")) + 
          scale_x_continuous(breaks=seq(10,90,5)) + 
          scale_y_continuous(breaks=seq(1,9,1)) + 
          scale_colour_manual(values=c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e", "#e6ab02", "#a6761d")) + 
          guides(colour = guide_legend(override.aes = list(size=4)))
      } else {
        ggplot() + 
          geom_point(data = df[(df$Year == input$plot_slider) & (df$Region == input$plot_select),],
                     aes(x=Life_Expectancy, y=Fertility_Rate, color=Region, size=Population), alpha=.7) + 
          geom_point(data = df[(df$Year == input$plot_slider) & (df$Region != input$plot_select),],
                     aes(x=Life_Expectancy, y=Fertility_Rate, color=Region, size=Population), alpha=.1) + 
          xlab("Life Expectancy") + ylab("Fertility Rate") + 
          scale_size(range = c(4, 20), breaks=c(1e+06, 1e+07, 5e+07, 1e+08, 3e+08, 6e+08), 
                     labels=c("1,000,000", "10,000,000", "50,000,000", "100,000,000", "300,000,000", "600,000,000")) + 
          scale_x_continuous(breaks=seq(10,90,5)) + 
          scale_y_continuous(breaks=seq(1,9,1)) + 
          scale_colour_manual(values=c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e", "#e6ab02", "#a6761d")) + 
          guides(colour = guide_legend(override.aes = list(size=4)))
      }
    } else {
      if (input$plot_select == "All") {
        ggplot(df[df$Year == input$plot_slider,]) + 
          geom_point(aes(x=Life_Expectancy, y=Fertility_Rate, color=Region, size=Population), alpha=.7) + 
          geom_line(data = df[(df$Country %in% input$select_country) & (df$Year <= input$plot_slider),],
                    aes(x=Life_Expectancy, y=Fertility_Rate, color=Region), show.legend=FALSE) + 
          geom_point(data = df[(df$Country %in% input$select_country) & (df$Year <= input$plot_slider),],
                     aes(x=Life_Expectancy, y=Fertility_Rate, color=Region, size=Population), alpha=1, show.legend=FALSE) + 
          geom_label_repel(data = df[(df$Country %in% input$select_country) & (df$Year == 1960),],
                     aes(x=Life_Expectancy, y=Fertility_Rate, label=Country, color=Region), fill="white", show.legend=FALSE) + 
          xlab("Life Expectancy") + ylab("Fertility Rate") + 
          scale_size(range = c(4, 20), breaks=c(1e+06, 1e+07, 5e+07, 1e+08, 3e+08, 6e+08), 
                     labels=c("1,000,000", "10,000,000", "50,000,000", "100,000,000", "300,000,000", "600,000,000")) + 
          scale_x_continuous(breaks=seq(10,90,5)) + 
          scale_y_continuous(breaks=seq(1,9,1)) + 
          scale_colour_manual(values=c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e", "#e6ab02", "#a6761d")) + 
          guides(colour = guide_legend(override.aes = list(size=4)))
      } else {
        ggplot() + 
          geom_point(data = df[(df$Year == input$plot_slider) & (df$Region == input$plot_select),],
                     aes(x=Life_Expectancy, y=Fertility_Rate, color=Region, size=Population), alpha=.7) + 
          geom_point(data = df[(df$Year == input$plot_slider) & (df$Region != input$plot_select),],
                     aes(x=Life_Expectancy, y=Fertility_Rate, color=Region, size=Population), alpha=.1, show.legend=FALSE) + 
          geom_line(data = df[(df$Country %in% input$select_country) & (df$Year <= input$plot_slider),],
                    aes(x=Life_Expectancy, y=Fertility_Rate, color=Region), show.legend=FALSE) + 
          geom_point(data = df[(df$Country %in% input$select_country) & (df$Year <= input$plot_slider),],
                     aes(x=Life_Expectancy, y=Fertility_Rate, color=Region, size=Population), alpha=1, show.legend=FALSE) + 
          geom_label_repel(data = df[(df$Country %in% input$select_country) & (df$Year == 1960),],
                     aes(x=Life_Expectancy, y=Fertility_Rate, label=Country, color=Region), fill="white", show.legend=FALSE) + 
          xlab("Life Expectancy") + ylab("Fertility Rate") + 
          scale_size(range = c(4, 20), breaks=c(1e+06, 1e+07, 5e+07, 1e+08, 3e+08, 6e+08), 
                     labels=c("1,000,000", "10,000,000", "50,000,000", "100,000,000", "300,000,000", "600,000,000")) + 
          scale_x_continuous(breaks=seq(10,90,5)) + 
          scale_y_continuous(breaks=seq(1,9,1)) + 
          scale_colour_manual(values=c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e", "#e6ab02", "#a6761d")) + 
          guides(colour = guide_legend(override.aes = list(size=4)))
      }
    }
  })
  
  # inspired by: https://gitlab.com/snippets/16220
  output$hover_info <- renderUI({
    hover <- reactive({input$plot_hover})
    info <- reactive({nearPoints(df[(df$Year == input$plot_slider) | (df$Country %in% input$select_country),], 
                                 hover(), threshold=3, maxpoints=1)})
    
    if (nrow(info()) == 0) {return(NULL)}
    
    # calculate hover position as percentage from left and top of plot
    left_pct <- (info()$Life_Expectancy - hover()$domain$left) / (hover()$domain$right - hover()$domain$left)
    top_pct <- (hover()$domain$top - info()$Fertility_Rate) / (hover()$domain$top - hover()$domain$bottom)
    
    # calculate hover position as number of pixels from left and top of plot
    left_px <- hover()$range$left + left_pct * (hover()$range$right - hover()$range$left)
    top_px <- hover()$range$top + top_pct * (hover()$range$bottom - hover()$range$top)
    
    # create style of hover country text
    style <- paste0("position:absolute; z-index:100; background-color: rgba(255, 255, 255, 1); ",
                    "left:", left_px + 2, "px; top:", top_px - 78, "px; padding: 2px 2px 0px 2px;")

    # display hover country text
    wellPanel(
      style = style,
      p(HTML(paste0("<b>", as.character(info()$Country), "</b><br/>",
                    "Pop: ", info()$Population, "<br/>",
                    "Year: ", info()$Year)))
    )
  })
}

shinyApp(ui = ui, server = server)