# ----------------------------------------------------------------
# Header
# ----------------------------------------------------------------
header <- dashboardHeader(
  title = "Social Media Dashboard"
)

# ----------------------------------------------------------------
# Sidebar
# ----------------------------------------------------------------
sidebar <- dashboardSidebar(
  sidebarMenu(
    # menuItem(text = "Google News",
    #          tabName = "googleNewsItem",
    #          icon = icon("google")),
    menuItem(text = "Overview",
             tabName = "overviewItem",
             icon = icon("eye")),
    menuItem(text = "Instagram",
             tabName = "instagramItem",
             icon = icon("instagram")),
    menuItem(text = "Twitter",
             tabName = "twitterItem",
             icon = icon("twitter")),
    menuItem(text = "Google News",
             tabName = "googleItem",
             icon = icon("google"))
  )
)

# ----------------------------------------------------------------
# Item
# ----------------------------------------------------------------

instagramItem <- tabItem(
  tabName = "instagramItem",
  fluidPage(
    h3("Informasi Dasar Terkait Post"),
    fluidRow(
      box(
        title = "Input dan Selector",
        width = 3,
        height = 420,
        selectInput(
          inputId = "company",
          label = shiny::HTML("<span style='color: #f0f0f0'>Pilih perusahaan: </span>"),
          choices = as.vector(unique(ig_posts$owner_username)),
          selected = "teamalgoritma"
        ),
        radioButtons(
          inputId = "postSelector",
          label = shiny::HTML("<span style='color: #f0f0f0'>Informasi: </span>"),
          choices = c("Jumlah Post", "Jumlah Post Video"),
          selected = "Jumlah Post"
        ),
        radioButtons(
          inputId = "postSelector2",
          label = shiny::HTML("<span style='color: #f0f0f0'>Tampilkan per: </span>"),
          choices = c("Hari", "Bulan", "Tahun"),
          selected = "Hari"
        )
      ),
      box(
        width = 9,
        echarts4rOutput(outputId = "infoPost"),
      )
    ),
    h3("Informasi Lanjut Terkait Post"),
    fluidRow(
      box(
        width = 3,
        height = 420,
        selectInput(
          inputId = "company2",
          label = shiny::HTML("<span style='color: #f0f0f0'>Pilih perusahaan: </span>"),
          choices = as.vector(unique(ig_posts$owner_username)),
          selected = "teamalgoritma"
        ),
        radioButtons(
          inputId = "postInfoSelector",
          label = shiny::HTML("<span style='color: #f0f0f0'>Informasi: </span>"),
          choices = c("Jumlah Likes", "Jumlah Comments"),
          selected = "Jumlah Likes"
        )
      ),
      box(
        width = 9,
        echarts4rOutput(outputId = "infoLanjutanPost")
      )
    ),
    fluidRow(
      box(
        title = 'Outlier',
        width = 12,
        collapsible = F,
        collapsed = T,
        dataTableOutput(outputId = "tableOutlier")
      )
    )
  )
)

overviewItem <- tabItem(
  tabName = "overviewItem",
  fluidPage(
    tabBox(
      title = "Overview",
      width = 12,
      id = "tabset1",
      tabPanel(
        title = "Instagram",
        icon = icon("instagram"),
        box(
          width = 4,
          height = "400px",
          radioButtons(
            inputId = "igSelector",
            label = shiny::HTML("<span style='color: #f0f0f0'>Informasi: </span>"),
            choices = c("Jumlah Post", "Jumlah Postingan Video","Jumlah IG TV", "Followers", "Followees"),
            selected = "Jumlah Post",
            inline = F
          ),
          p("Keterangan"),
          shiny::HTML("<ul>
                      <li><b>Jumlah post</b>: Jumlah postingan pada instagram</li>
                      <li><b>Jumlah post video</b>: Jumlah postingan yang berupa video</li>
                      <li><b>Jumlah IGTV</b>: Jumlah konten IG TV</li>
                      <li><b>Followers</b>: Jumlah pengikut pada instagram</li>
                      <li><b>Followees</b>: Jumlah akun yang diikuti pada instagram</li>
                    </ul>
                   ")
        ),
        valueBoxOutput(outputId = "overviewIg",
                      width = 8),
        ),
      tabPanel(
        title = "Linkedin",
        icon = icon("linkedin"),
        box(
          width = 12,
          echarts4rOutput(
            outputId = "linkedinFollowers"
          )
        ),
        box(
          width = 12,
          echarts4rOutput(
            outputId = "linkedinEmployees"
          )
        )
      ),
      tabPanel(
        title = "Facebook",
        icon = icon("facebook"),
        box(
          width = 12,
          echarts4rOutput(
            outputId = "facebookFollowers"
          )
        ),
        box(
          width = 12,
          echarts4rOutput(
            outputId = "facebookLikes"
          )
        )
      ),
      tabPanel(
        title = "Youtube",
        icon = icon("youtube"),
        box(
          width = 12,
          echarts4rOutput(
            outputId = "youtubeVideos"
          )
        ),
        box(
          width = 12,
          echarts4rOutput(
            outputId = "youtubeSubscribers"
          )
        )
      ),
      tabPanel(
        title = "Twitter",
        icon = icon("twitter"),
        box(
          width = 4,
          height = "400px",
          radioButtons(
            inputId = "twitterSelector",
            label = shiny::HTML("<span style='color: #f0f0f0'>Informasi: </span>"),
            choices = c("Jumlah Tweet", "Followers", "Friends"),
            selected = "Jumlah Tweet",
            inline = F
          ),
          p("Keterangan"),
          shiny::HTML("<ul>
                      <li><b>Jumlah tweet</b>: Jumlah tweet yang pernah diposting di Twitter</li>
                      <li><b>Followers</b>: Jumlah pengikut di Twitter</li>
                      <li><b>Friends</b>: Jumlah akun yang mengikuti dan diikuti di Twitter.</li>
                    </ul>
                   ")
        ),
        valueBoxOutput(outputId = "overviewTwitter",
                       width = 8),
      ),
      tabPanel(
        title = "Alumni",
        icon = icon("graduation-cap"),
        valueBoxOutput(
          width = 6,
          outputId = "alumniAlgoritma"
        ),
        valueBoxOutput(
          width = 6,
          outputId = "alumniPurwadhika"
        ),
        valueBoxOutput(
          width = 6,
          outputId = "alumniHacktiv8"
        ),
        valueBoxOutput(
          width = 6,
          outputId = "alumniDibimbiing"
        )
      )
    ),
  )
)

twitterItem <- tabItem(
  tabName = "twitterItem",
  fluidPage(
    fluidRow(
      box(
        width = 3,
        height = 420,
        title = "Input dan Selector",
        radioButtons(
          inputId = "twSelector2",
          label = shiny::HTML("<span style='color: #f0f0f0'>Informasi: </span>"),
          choices = c("Total Retweet", "Total Favorite"),
          selected = "Total Retweet",
          inline = F
        ),
        p("Keterangan"),
        shiny::HTML("<ul>
                      <li><b>Total Retweet</b>: Total retweet di setiap perusahaan berdasaarkan 300+ tweet terakhir.</li>
                      <li><b>Total Favorite</b>: Total favorite di setiap perusahaan berdasaarkan 300+ tweet terakhir.</li>
                    </ul>
                   ")
      ),
      box(
        width = 9,
        echarts4rOutput(
          outputId = "retweetFavoriteInfo"
        )
      )
    ),
    fluidRow(
      box(
        width = 3,
        height = 420,
        selectInput(
          inputId = "twitterCompany",
          label = shiny::HTML("<span style='color: #f0f0f0'>Pilih perusahaan: </span>"),
          choices = as.vector(unique(tweets$username)),
          selected = "teamalgoritma"
        ),
        radioButtons(
          inputId = "twSelector3",
          label = shiny::HTML("<span style='color: #f0f0f0'>Informasi: </span>"),
          choices = c("Retweet", "Favorite"),
          selected = "Retweet",
          inline = F
        ),
      ),
      box(
        width = 9,
        echarts4rOutput(
          outputId = "tweetMore"
        )
      )
    ),
    fluidRow(
      box(
        width = 12,
        radioButtons(
          inputId = "twGroupBy",
          label = shiny::HTML("<span style='color: #f0f0f0'>Group by: </span>"),
          choices = c("Hari", "Bulan", "Tahun"),
          selected = "Hari",
          inline = T
        )
      )
    ),
    fluidRow(
      box(
        width = 12,
        echarts4rOutput(outputId = "infoTweet"),
      )
    )
  )
)

googleItem <- tabItem(
  tabName = "googleItem",
  fluidPage(
    fluidRow(
      box(
        width = 4,
        height = 170,
        selectInput(
          inputId = "companyNews",
          label = shiny::HTML("<span style='color: #f0f0f0'>Pilih perusahaan: </span>"),
          choices = as.vector(unique(news2$company)),
          selected = "Algoritma"
        )
      ),
      box(
        width = 4,
        height = 170,
        valueBoxOutput(
          outputId = "cardNews"
        )
      ),
      box(
        width = 4,
        height = 170,
        valueBoxOutput(
          outputId = "cardNews2"
        )
      )
    ),
    fluidRow(
      box(
        width = 12,
        echarts4rOutput(
          outputId = "news"
        )
      )
    )
  )
)

body <- dashboardBody(
  # CSS
  tags$link(rel = "stylesheet", 
            type = "text/css",
            href = "style.css"),
  tags$script(HTML("$('body').addClass('fixed');")),
  tabItems(
    overviewItem,
    instagramItem,
    twitterItem,
    googleItem
  )
)

ui <- dashboardPage(header, sidebar, body)