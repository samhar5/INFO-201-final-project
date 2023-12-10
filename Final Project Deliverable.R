library(shiny)
library(ggplot2)
library(readr)

# Read the dataset
cyb_df <- read_csv("Cleaned-Data.csv")

ui <- fluidPage(
  tabsetPanel(
    tabPanel("Introduction", 
             h1("Securing the Digital Frontier: Unraveling Cyber Threats through Data Analysis"),
             p("By: Krishang Bairavarsu, Samuel Harris, Amara Herman"),
             
             # Add two images side by side
             div(
               img(src = "https://www.hurix.com/wp-content/uploads/2022/01/Cyber-security.jpg", 
                   width = 400, height = 300),
               img(src = "https://managex.ae/wp-content/uploads/2022/05/What-are-the-seven-types-of-cyber-security-threats-1536x864.jpg", 
                   width = 400, height = 300)
             ),
             
             h2("The Significance of Cybersecurity"),
             
             p("In an increasingly interconnected world driven by technological advancements, the significance of cybersecurity cannot be overlooked. While technology brings countless benefits and innovations to our lives, it also produces new risks and vulnerabilities that can jeopardize the safety and privacy of individuals and organizations."),
             
             p("In our ongoing journey through the digital age, our lives have become more intertwined with technology than ever before. As a result, it is important to safeguard the vast amount of data we generate and exchange daily."),
             
             p("Each one of us possesses data that holds significant value, known as Sensitive Personally Identifiable Information (SPII). This SPII encompasses a wide range of sensitive details, such as Social Security Numbers, credit card information, and medical records, among others. This information is stored on both local devices and the online platforms we rely on daily. While efforts have been made to secure this data on dedicated servers, the internet is teeming with individuals and groups seeking to exploit any vulnerabilities and gain unauthorized access."),
             
             p("These threat actors use various hacking techniques, including distributed denial of service (DDoS) attacks, phishing attempts, malware distribution, and more. Once they have this information, they can use it for a variety of illegal acts, such as financial extortion and identity theft. This is where the significance of cybersecurity and its protective measures truly comes to the forefront."),
             
             h2("Our Project"),
             
             p("Our project researches further into these cyber threats, delving into the various attacks targeting businesses and individuals alike. The goal of our project is to examine various datasets, gaining insights into the most common cyber attacks hackers employ and understanding their severity. By gaining a better understanding of these attack vectors, our goal is to not only educate others on potential cyber threats but to also effectively predict and mitigate potential risks in the ever-evolving realm of cybersecurity."),
             
             # Add a link for further exploration
             p("Explore our datasets in detail:"),
             p("Dataset 1:https://www.kaggle.com/datasets/zunxhisamniea/cyber-threat-data-for-new-malware-attacks."),
             p("Dataset 2:https://www.kaggle.com/datasets/teamincribo/cyber-security-attacks."
             )
    ),
    tabPanel("Bar Chart", 
             h3("About the Bar Charts:"),
             p("The bar charts below provide visual insights into the distribution of cyber attacks."),
             p("The first chart represents the top 3 most popular attack types in recent years, while the second chart displays other common attack types."),
             
             plotOutput(outputId = "bar_chart_attack_type"),
             plotOutput(outputId = "bar_chart_target_variable")
    ),
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
  output$bar_chart_attack_type <- renderPlot({
    count_df_attack_type <- as.data.frame(table(cyb_df$Attack.Type))
    
    ggplot(data = count_df_attack_type, aes(x = Var1, y = Freq, fill = Var1)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = Freq), vjust = -0.5, size = 3) +
      labs(x = "Top 3 Recorded Attack Types", y = "Number of recorded cases from 2020 - Present", fill = "Attack Type") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$bar_chart_target_variable <- renderPlot({
    count_df_target_variable <- as.data.frame(table(cyb_df$Target.Variable))
    
    ggplot(data = count_df_target_variable, aes(x = Var1, y = Freq, fill = Var1)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = Freq), vjust = -0.5, size = 3) +
      labs(x = "Other Common Attack Types", y = "Number of recorded cases from 2020 - Present", fill = "Attack Type") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

shinyApp(ui = ui, server = server)
