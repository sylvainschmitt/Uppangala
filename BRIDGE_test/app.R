#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggfortify)
library(reshape2)
library(cowplot)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = 'BRIDGE'),
  dashboardSidebar(
    textOutput("Levene")
  ),
  dashboardBody(
    fluidPage(
      fluidRow(
        sliderInput("dlim",
                    "Strata diameter limit",
                    min = 0,
                    max = 100,
                    value = 35)
      ),
      fluidRow(
        plotOutput("PCA", height = "800px")
      )
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$Levene <- renderText({
    BRIDGE <- readr::read_tsv("./species_TROLL_Nino.txt") %>% 
      mutate(strata = ifelse(dmax > input$dlim/100, "C", "U"))
    BRIDGE.pca <- princomp(~ LMA + Nmass + Pmass + wsg, data = BRIDGE, cor = T)
    pca.plot <- autoplot(BRIDGE.pca, data = BRIDGE)
    data <- pca.plot$data %>%
      select(Comp.1, Comp.2, strata) %>%
      rename(WES = Comp.1, LES = Comp.2) %>% # ! test !
      melt(id.vars = "strata", variable.name = "spectrum")
    Variance <- list(
      LES = list(
        C = round(var(data[data$strata == "C" & data$spectrum == "LES", "value"], na.rm = T), 3),
        U = round(var(data[data$strata == "U" & data$spectrum == "LES", "value"], na.rm = T), 3)),
      WES = list(
        C = round(var(data[data$strata == "C" & data$spectrum == "WES", "value"], na.rm = T), 3),
        U = round(var(data[data$strata == "U" & data$spectrum == "WES", "value"], na.rm = T) ,3))
    )
    Leven_test <- list(
      C = round(car::leveneTest(value ~ spectrum, data[data$strata == "C",])$`Pr(>F)`[1], 3),
      U = round(car::leveneTest(value ~ spectrum, data[data$strata == "U",])$`Pr(>F)`[1], 3)
    )
    paste("Canopy species WES vs LES:\n",
          Variance$LES$C, "vs", Variance$WES$C, ",\n",
          "Levene's test p-value=", Leven_test$C,
          "Understorey species LES vs WES:\n",
          Variance$LES$U, "vs", Variance$WES$U, ",\n",
          "Levene's test p-value=", Leven_test$U)
})
  
   output$PCA <- renderPlot({
     BRIDGE <- readr::read_tsv("./species_TROLL_Nino.txt") %>% 
       mutate(strata = ifelse(dmax > input$dlim/100, "C", "U"))
     BRIDGE.pca <- princomp(~ LMA + Nmass + Pmass + wsg, data = BRIDGE, cor = T)
     pca.plot <- autoplot(BRIDGE.pca, 
                          data = BRIDGE,
                          colour = "strata",
                          size = "Freg",
                          loadings.label.size = 6,
                          loadings.label.colour = 'black',
                          loadings = T, loadings.label = T, loadings.colour = 'black',
                          loadings.label.vjust = 1.2) +
       coord_equal() +
       geom_hline(aes(yintercept = 0), col = 'green', linetype = "dotted") +
       geom_vline(aes(xintercept = 0), col = 'purple', linetype = "dotted") +
       xlab('Axe 1 − 33.66 %') + ylab('Axe 2 − 29.07 %') +
       theme(legend.position = c(1, 1), 
             legend.justification = c(1, 1))
     spectrum.density <- pca.plot$data %>% 
       select(Comp.1, Comp.2, strata) %>% 
       rename(WES = Comp.1, LES = Comp.2) %>% 
       melt(id.vars = "strata", variable.name = "spectrum") %>% 
       ggplot(aes(value, color = spectrum, fill = spectrum)) +
       geom_density(alpha = 0.5) + 
       facet_wrap(~strata, nrow = 2) +
       coord_flip() +
       scale_fill_manual("PCA axis", 
                         labels = c("WES" = "PCA1", "LES" = "PCA2"),
                         values = c("green", "purple")) +
       scale_color_manual("PCA axis", 
                          labels = c("WES" = "PCA1", "LES" = "PCA2"),
                          values = c("green", "purple"))
     plot_grid(pca.plot, spectrum.density, rel_widths = c(2,1))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

