library(shiny)
library(ggplot2)
library(readr)

# Read the dataset
cyb_df <- read_csv("Cleaned-Data.csv")

ui <- fluidPage(
  tabsetPanel(
    tabPanel("Introduction", 
             h1("The Dangers of Cyberthreats"),
             p("In an increasingly interconnected world driven by technological advancements,",
               "the significance of cybersecurity cannot be overlooked.",
               "While technology brings countless benefits and innovations to our lives,",
               "it also produces new risks and vulnerabilities that can jeopardize the safety",
               "and privacy of individuals and organizations.",
               "In our ongoing journey through the digital age,",
               "our lives have become more intertwined with technology than ever before.",
               "As a result, it is important to safeguard the vast amount of data we generate and exchange daily.",
               "Each one of us possesses data that holds significant value,",
               "known as Sensitive Personally Identifiable Information (SPII).",
               "This SPII encompasses a wide range of sensitive details,",
               "such as Social Security Numbers, credit card information,",
               "and medical records, among others.",
               "This information is stored on both local devices",
               "and the online platforms we rely on daily.",
               "While efforts have been made to secure this data on dedicated servers,",
               "the internet is teeming with individuals and groups seeking to exploit any vulnerabilities and gain unauthorized access.",
               "These threat actors use various hacking techniques,",
               "including distributed denial of service (DDoS) attacks,",
               "phishing attempts, malware distribution, and more.",
               "Once they have this information, they can use it for a variety of illegal acts,",
               "such as financial extortion and identity theft.",
               "This is where the significance of cybersecurity and its protective measures",
               "truly comes to the forefront."),
             h2("Our Project"),
             p("Our project researches further into these cyber threats, delving into the various attacks targeting businesses and individuals alike.",
               "The goal of our project is to examine various datasets, gaining insights into the most common cyber attacks hackers employ and understanding their severity.",
               "By gaining a better understanding of these attack vectors, our goal is to educate others on the most common cyber attacks")
    ),
    tabPanel("Bar Chart", 
             plotOutput(outputId = "bar_chart")),
    tabPanel("Tab 3", 
             # Add content for Tab 3 here
    ),
    tabPanel("Tab 4", 
             # Add content for Tab 4 here
    ),
    tabPanel("Tab 5", 
             # Add content for Tab 4 here
    )
  )
)

server <- function(input, output) {
  # Render the bar chart
  output$bar_chart <- renderPlot({
    count_df <- as.data.frame(table(cyb_df$Attack.Type, cyb_df$Target.Variable))
    
    ggplot(data = count_df, aes(x = Var2, y = Freq, fill = Var2)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(x = "Types of Cyber Attacks", y = "Number of recorded cases from 2020 - Present", fill = "Attack Type") +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)
