library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(DT)

# ==================================
# LOAD CLEANED DATASETS
# ==================================

final_data <- read.csv(
  "final_cleaned_dataset.csv"
)

survey_data <- read.csv(
  "cleaned_survey_data.csv"
)

trends_data <- read.csv(
  "cleaned_google_trends.csv"
)

# ==================================
# CUSTOMER SEGMENTATION
# ==================================

customer_data <- final_data[, c(
  "Total.Spend",
  "Items.Purchased",
  "Average.Rating"
)]

customer_data <- scale(customer_data)

set.seed(123)

kmeans_model <- kmeans(
  customer_data,
  centers = 3
)

final_data$Cluster <- factor(
  kmeans_model$cluster
)

# ==================================
# PRODUCT CATEGORY ANALYSIS
# ==================================

product_counts <- survey_data %>%
  select(
    What.product.categories.do.you.most.frequently.purchase.online....check.all.that.applies.to.you.
  ) %>%
  separate_rows(
    What.product.categories.do.you.most.frequently.purchase.online....check.all.that.applies.to.you.,
    sep = ";"
  ) %>%
  count(
    What.product.categories.do.you.most.frequently.purchase.online....check.all.that.applies.to.you.
  ) %>%
  arrange(desc(n))

# ==================================
# GOOGLE TRENDS
# ==================================

trends_data$Time <- as.Date(
  trends_data$Time
)

trend_long <- pivot_longer(
  trends_data,
  cols = c(
    Lazada.Group,
    Carousell,
    tiktok.shop,
    ZALORA,
    Shopee.Philippines
  ),
  names_to = "Platform",
  values_to = "Interest"
)

# ==================================
# UI
# ==================================

ui <- dashboardPage(

  dashboardHeader(
    title = "E-Commerce Analytics Dashboard"
  ),

  dashboardSidebar(

    sidebarMenu(

      menuItem(
        "Sales Performance",
        tabName = "sales",
        icon = icon("chart-line")
      ),

      menuItem(
        "Best Selling Products",
        tabName = "products",
        icon = icon("shopping-cart")
      ),

      menuItem(
        "Customer Segmentation",
        tabName = "segment",
        icon = icon("users")
      ),

      menuItem(
        "Monthly Trends",
        tabName = "trends",
        icon = icon("calendar")
      ),

      menuItem(
        "Recommendations",
        tabName = "recommend",
        icon = icon("lightbulb")
      )

    )
  ),

  dashboardBody(

    tabItems(

# ==================================
# SALES PERFORMANCE
# ==================================

      tabItem(

        tabName = "sales",

        fluidRow(

          valueBox(
            sum(final_data$Total.Spend),
            "Total Revenue",
            icon = icon("peso-sign")
          ),

          valueBox(
            round(mean(final_data$Total.Spend),2),
            "Average Spend",
            icon = icon("money-bill")
          ),

          valueBox(
            nrow(final_data),
            "Total Customers",
            icon = icon("users")
          )

        ),

        fluidRow(

          box(
            width = 12,
            plotlyOutput("membershipPlot")
          )

        )

      ),

# ==================================
# BEST SELLING PRODUCTS
# ==================================

      tabItem(

        tabName = "products",

        fluidRow(

          box(
            width = 12,
            plotlyOutput("productPlot")
          )

        )

      ),

# ==================================
# CUSTOMER SEGMENTATION
# ==================================

      tabItem(

        tabName = "segment",

        fluidRow(

          box(
            width = 12,
            plotlyOutput("clusterPlot")
          )

        )

      ),

# ==================================
# MONTHLY TRENDS
# ==================================

      tabItem(

        tabName = "trends",

        fluidRow(

          box(
            width = 12,
            plotlyOutput("trendPlot")
          )

        )

      ),

# ==================================
# PRODUCT RECOMMENDATIONS
# ==================================

      tabItem(

        tabName = "recommend",

        fluidRow(

          box(
            width = 12,

            DTOutput("recommendTable")

          )

        )

      )

    )

  )

)

# ==================================
# SERVER
# ==================================

server <- function(input, output) {

# SALES PERFORMANCE

  output$membershipPlot <- renderPlotly({

    p <- ggplot(
      final_data,
      aes(
        x = Membership.Type,
        y = Total.Spend,
        fill = Membership.Type
      )
    ) +
      geom_boxplot() +
      theme_minimal() +
      labs(
        title = "Spending by Membership Type"
      )

    ggplotly(p)

  })

# BEST SELLING PRODUCTS

  output$productPlot <- renderPlotly({

    p <- ggplot(
      product_counts,
      aes(
        x = reorder(
          What.product.categories.do.you.most.frequently.purchase.online....check.all.that.applies.to.you.,
          n
        ),
        y = n
      )
    ) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      theme_minimal() +
      labs(
        title = "Best Selling Product Categories",
        x = "Category",
        y = "Count"
      )

    ggplotly(p)

  })

# CUSTOMER SEGMENTATION

  output$clusterPlot <- renderPlotly({

    p <- ggplot(
      final_data,
      aes(
        x = Items.Purchased,
        y = Total.Spend,
        color = Cluster
      )
    ) +
      geom_point(size = 3) +
      theme_minimal() +
      labs(
        title = "Customer Segmentation"
      )

    ggplotly(p)

  })

# MONTHLY TRENDS

  output$trendPlot <- renderPlotly({

    p <- ggplot(
      trend_long,
      aes(
        x = Time,
        y = Interest,
        color = Platform
      )
    ) +
      geom_line(size = 1) +
      theme_minimal() +
      labs(
        title = "Monthly Consumer Interest Trends"
      )

    ggplotly(p)

  })

# RECOMMENDATION TABLE

  output$recommendTable <- renderDT({

    recommendation_data <- data.frame(

      Category = c(
        "Electronics",
        "Fashion",
        "Beauty",
        "Groceries"
      ),

      Recommendation = c(
        "Power Banks, Earbuds",
        "Shoes, Bags",
        "Skincare Products",
        "Household Essentials"
      )

    )

    datatable(
      recommendation_data,
      options = list(pageLength = 10)
    )

  })

}

# ==================================
# RUN DASHBOARD
# ==================================

shinyApp(
  ui = ui,
  server = server
)