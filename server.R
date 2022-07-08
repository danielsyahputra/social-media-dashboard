server <- function(input, output) {
  
  # VALUE BOX SPARK ---------------------------------------------
  # Reference: https://jkunst.com/blog/posts/2020-06-26-valuebox-and-sparklines/
  
  valueBoxSpark <- function(value, title, sparkobj = NULL, subtitle, info = NULL, 
                            icon = NULL, color = "teal", width = 12, href = NULL){
    shinydashboard:::validateColor(color)
    if (!is.null(icon))
      shinydashboard:::tagAssert(icon, type = "i")
    
    info_icon <- tags$small(
      tags$i(
        class = "fa fa-info-circle fa-lg",
        title = info,
        `data-toggle` = "tooltip",
        style = "color: rgba(255, 255, 255, 0.75);"
      ),
      # bs3 pull-right 
      # bs4 float-right
      class = "pull-right float-right"
    )
    
    boxContent <- div(
      class = paste0("info-box small-box bg-", color),
      div(
        class = "inner",
        tags$small(title),
        if (!is.null(sparkobj)) info_icon,
        h3(value),
        if (!is.null(sparkobj)) sparkobj,
        p(subtitle)
      ),
      # bs3 icon-large
      # bs4 icon
      if (!is.null(icon)) div(class = "icon-large icon", icon, style = "z-index; 0")
    )
    
    if (!is.null(href)) 
      boxContent <- a(href = href, boxContent)
    
    div(
      class = if (!is.null(width)) paste0("col-sm-", width), 
      boxContent
    )
  }
  # VALUE BOX SPARK - END --------------------------------------------
  
  
  # INSTAGRAM TAB - START -----------------------------------------------
  
  getCompanyFullName <- function(account) {
    if (account == "teamalgoritma") {
      return("Algoritma")
    } else if (account == "purwadhikaschool") {
      return("Purwadhika")
    } else if (account == "hacktiv8id") {
      return("Hacktiv8")
    } else if (account == "dibimbing.id") {
      return("Dibimbing.id")
    } else if (account == "revou_id") {
      return("Revo-u")
    } else if (account == "shiftacademy.id") {
      return("Shift Academy")
    } else if (account == "digital.skola") {
      return("Digital Skola")
    } else if (account == "pacmannai") {
      return("Pacmann AI")
    } else {
      return("None")
    }
  }
  
  output$igBasicInfo <- renderEcharts4r({
    selector <- input$igSelector
    
    df <- ""
    if (selector == "Jumlah Post") {
      df <-  profiles %>% 
        select(username, mediacount) %>% 
        arrange(desc(mediacount))
    } else if (selector == "Jumlah IG TV") {
      df <- profiles %>% 
        select(username, igtvcount) %>% 
        arrange(desc(igtvcount))
    } else if (selector == "Followers") {
      df <- profiles %>% 
        select(username, followers) %>% 
        arrange(desc(followers))
    } else if (selector == "Followees") {
      df <- profiles %>% 
        select(username, followees) %>% 
        arrange(desc(followees))
    }
    
    plot <- df %>% 
      e_chart(username)
    
    if (selector == "Jumlah Post") {
      plot <- plot %>% 
        e_bar(mediacount)
    } else if (selector == "Jumlah IG TV") {
      plot <- plot %>% 
        e_bar(igtvcount)
    } else if (selector == "Followers") {
      plot <- plot %>% 
        e_bar(followers)
    } else if (selector == "Followees") {
      plot <- plot %>% 
        e_bar(followees)
    }
    
    plot %>% 
      e_theme_custom("www/chart_theme.json") %>% 
      e_title(
        text = glue("{selector} dari Setiap Company"),
        left = "center",
        top = "0"
      ) %>% 
      e_legend(show = F) %>% 
      e_x_axis(axisLabel = list(interval = 0, rotate = 15)) %>% 
      e_tooltip(
        trigger = "item",
        formatter = JS(
          "
           function(params){return(
           '<b>@' + params.name + '</b>' + ' : ' 
           + echarts.format.addCommas(params.value[1])
           )}
           "
        )
      )
  })
  
  output$videoCount <- renderEcharts4r({
    ig_posts %>% 
      mutate(is_video = as.logical(is_video)) %>% 
      group_by(owner_username) %>% 
      summarise(VideoCount = sum(is_video)) %>% 
      arrange(desc(VideoCount)) %>% 
      e_chart(owner_username) %>% 
      e_bar(VideoCount) %>% 
      e_theme_custom("www/chart_theme.json") %>% 
      e_title(
        text = "Jumlah Postingan Video di Platform Instagram",
        left = "center",
        top = "0"
      ) %>% 
      e_legend(show = F) %>% 
      e_x_axis(axisLabel = list(interval = 0, rotate = 15)) %>% 
      e_tooltip(
        trigger = "item",
        formatter = JS(
          "
           function(params){return(
           '<b>@' + params.name + '</b>' +' : ' 
           + echarts.format.addCommas(params.value[1]) + ' video'
           )}
           "
        )
      )
  })
  
  output$infoPost <- renderEcharts4r({
    company <- input$company
    companyName <- getCompanyFullName(company)
    postSelector <- input$postSelector
    groupBy <- input$postSelector2
    
    ig_posts <- ig_posts %>% 
      filter(month != "Nov")
    
    if (postSelector == "Jumlah Post Video") {
      df <- ig_posts %>% 
        mutate(is_video = as.logical(is_video)) %>% 
        filter(is_video)
    } else {
      df <- ig_posts
    }
    
    plot <- df %>% 
      filter(owner_username == company)
    
    if (groupBy == "Hari") {
      plot <- plot %>% 
        group_by(dayname) %>% 
        mutate(dayname = as.factor(as.vector(dayname)))
    } else if (groupBy == "Bulan") {
      plot <- plot %>% 
        group_by(month) %>% 
        mutate(month = as.factor(as.vector(month)))
    } else {
      plot <- plot %>% 
        group_by(year) %>% 
        mutate(year = as.factor(as.vector(year)))
    }
    
    plot <- plot %>% 
      summarise(JumlahPost = n())
    
    if (groupBy == "Hari") {
      plot <- plot %>% 
        e_chart(dayname)
    } else if (groupBy == "Bulan") {
      plot <- plot %>% 
        e_chart(month)
    } else {
      plot <- plot %>% 
        e_chart(year)
    }
    
    title <- glue("{postSelector} Instagram per {groupBy} dari {companyName}")
    
    plot %>% 
      e_line(JumlahPost) %>% 
      e_theme_custom("www/chart_theme.json") %>% 
      e_title(
        text = title,
        left = "center",
        top = "0"
      ) %>% 
      e_legend(show = F) %>% 
      e_x_axis(axisLabel = list(interval = 0, rotate = 15)) %>% 
      e_tooltip(
        trigger = "item",
        formatter = JS(
          "
           function(params){return(
           '<b>' + params.name+ '</b>'+ ' : ' 
           + echarts.format.addCommas(params.value[1]) + ' post'
           )}
           "
        )
      )
  })
  
  
  output$infoLanjutanPost <- renderEcharts4r({
    infoSelector <- input$postInfoSelector
    company <- input$company2
    companyFullName <- getCompanyFullName(company)
    
    plot <- ig_posts %>% 
      filter(owner_username == company) %>% 
      e_charts()
    
    if (infoSelector == "Jumlah Likes") {
      plot <- plot %>% 
        e_boxplot(likes, itemStyle = list(color = "#AE2024"))
    } else {
      plot <- plot %>% 
        e_boxplot(comments, itemStyle = list(color = "#AE2024"))
    }
    plot %>% 
      e_theme_custom("www/chart_theme.json") %>% 
      e_title(
        text = glue("Boxplot {infoSelector} Postingan Instagram {companyFullName}"),
        left = "center",
        top = "0"
      ) %>% 
      e_axis_labels(y = glue("{infoSelector}")) %>% 
      e_tooltip(trigger = c("item", "axis"))
  })
  
  true_false_formatter <-
    formatter("span",
              style = x ~ style(
                font.weight = "bold",
                color = ifelse(x == TRUE, "forestgreen", ifelse(x == FALSE, "red", "black"))
              ))
  
  output$tableOutlier <- renderDataTable({
    selector <- input$postInfoSelector
    company <- input$company2
    companyFullName <- getCompanyFullName(company)
    
    p <- data %>% 
      filter(owner_username == company)
    
    outliers <- ""
    if (selector == "Jumlah Likes") {
      
      p <- p %>% 
        arrange(desc(likes))
      Q1 <- quantile(p$likes, .25)
      Q3 <- quantile(p$likes, .75)
      IQR <- IQR(p$likes)
      
      outliers <- subset(p, p$likes < (Q1 - 1.5*IQR) | p$likes > (Q3 + 1.5*IQR))
      
      outliers <- outliers %>% 
        mutate(url = paste0("<a href='https://www.", url,"' target='_blank'>", url,"</a>"))
      
      formattable(
        outliers,
        list(
          ## colour this column in a white to pink gradiant by value
          `likes` = color_tile("gray", "pink"),
          
          ## use custom formatter for TRUE/FALSE values
          orderable = true_false_formatter
        )
      ) %>%
        as.datatable(escape = FALSE,
                     options = list(
                       scrollX=T,
                       scrollY = 400,
                       initComplete = JS(
                         "function(settings, json) {",
                         "$(this.api().table().header()).css({'background-color': '#1c1c1c', 'color': 'white'});",
                         "}")
                     ),
                     rownames = FALSE,
        ) 
    } else {
      p <- p %>% 
        arrange(desc(comments))
      
      Q1 <- quantile(p$comments, .25)
      Q3 <- quantile(p$comments, .75)
      IQR <- IQR(p$comments)
      
      outliers <- subset(p, p$comments < (Q1 - 1.5*IQR) | p$comments > (Q3 + 1.5*IQR))
      
      outliers <- outliers %>% 
        mutate(url = paste0("<a href='https://www.", url,"' target='_blank'>", url,"</a>"))
      
      formattable(
        outliers,
        list(
          ## colour this column in a white to pink gradiant by value
          `comments` = color_tile("gray", "pink"),
          
          ## use custom formatter for TRUE/FALSE values
          orderable = true_false_formatter
        )
      ) %>%
        as.datatable(escape = FALSE,
                     options = list(
                       scrollX=T,
                       scrollY = 400,
                       initComplete = JS(
                         "function(settings, json) {",
                         "$(this.api().table().header()).css({'background-color': '#1c1c1c', 'color': 'white'});",
                         "}")
                     ),
                     rownames = FALSE,
        )
    }
    
    
    
  })
  
  # INSTAGRAM TAB - EMD -----------------------------------------------

  # OVERVIEW TAB - START -----------------------------------------------
  
  output$overviewIg <- renderValueBox({
    selector <- input$igSelector
    
    df <- ""
    if (selector == "Jumlah Post") {
      df <-  profiles %>% 
        select(username, mediacount) %>% 
        arrange(desc(mediacount))
    } else if (selector == "Jumlah IG TV") {
      df <- profiles %>% 
        select(username, igtvcount) %>% 
        arrange(desc(igtvcount))
    } else if (selector == "Followers") {
      df <- profiles %>% 
        select(username, followers) %>% 
        arrange(desc(followers))
    } else if (selector == "Followees") {
      df <- profiles %>% 
        select(username, followees) %>% 
        arrange(desc(followees))
    } else {
      df <- ig_posts %>% 
        mutate(is_video = as.logical(is_video)) %>% 
        group_by(owner_username) %>% 
        summarise(VideoCount = sum(is_video)) %>% 
        arrange(desc(VideoCount))
      
    }
    my_colors <- ""
    if (selector != "Jumlah Postingan Video") {
      usernames <- df$username
      idx <- which(usernames == "teamalgoritma")
      my_colors <- rep(c("#eb4d60"),each=8)
      my_colors[idx] <- "#AE2024"
      
    } else {
      usernames <- df$owner_username
      idx <- which(usernames == "teamalgoritma")
      my_colors <- rep(c("#eb4d60"),each=8)
      my_colors[idx] <- "#AE2024"
    }
    
    
    hcIg <- ""
    if (selector == "Jumlah Post") {
      hcIg <- df %>% 
        mutate(colors = my_colors) %>% 
        hchart("column", hcaes(x = username, y = mediacount, color = colors))
    } else if (selector == "Jumlah IG TV") {
      hcIg <- df %>% 
        mutate(colors = my_colors) %>% 
        hchart("column", hcaes(x = username, y = igtvcount, color = colors))
    } else if (selector == "Followers") {
      hcIg <- df %>% 
        mutate(colors = my_colors) %>% 
        hchart("column", hcaes(x = username, y = followers, color = colors))
    } else if (selector == "Followees") {
      hcIg <- df %>% 
        mutate(colors = my_colors) %>% 
        hchart("column", hcaes(x = username, y = followees, color = colors))
    } else {
      hcIg <- df %>% 
        mutate(colors = my_colors) %>% 
        hchart("column", hcaes(x = owner_username, y = VideoCount, color = colors))
    }
    
    hcIg <- hcIg %>% 
      hc_size(height = 300) %>% 
      hc_title(text = glue("{selector}"),
               style = list(color = "#f0f0f0", fontSize =16)) %>% 
      hc_tooltip(crosshairs = TRUE,
                 borderWidth = 1,
                 pointFormat = '<span style="color:{series.color}">Jumlah</span>:
                       <b>{point.y}</b><br/>')
    
    value <- ""
    
    if (selector == "Jumlah Post") {
      temp <- df %>% 
        filter(username == "teamalgoritma") %>% 
        select(mediacount)
      
      value <- temp[1, "mediacount"]
    } else if (selector == "Jumlah IG TV") {
      temp <- df %>% 
        filter(username == "teamalgoritma") %>% 
        select(igtvcount)
      
      value <- temp[1, "igtvcount"]
    } else if (selector == "Followers") {
      temp <- df %>% 
        filter(username == "teamalgoritma") %>% 
        select(followers)
      
      value <- temp[1, "followers"]
    } else if (selector == "Followees") {
      temp <- df %>% 
        filter(username == "teamalgoritma") %>% 
        select(followees)
      
      value <- temp[1, "followees"]
    } else {
      temp <- df %>% 
        filter(owner_username == "teamalgoritma") %>% 
        select(VideoCount)
      value <- temp[1, "VideoCount"]
    }
    
    vbIg <- valueBoxSpark(
      value = glue("{selector} di Instagram"),
      title = toupper("Jumlah Post"),
      sparkobj = hcIg,
      info = "",
      subtitle = NULL,
      icon = icon("instagram"),
      href = NULL
    )
    
    vbIg
  })
  
  
  output$linkedinFollowers <- renderEcharts4r({
    
    df <- profiles_linkedin %>% 
      arrange(desc(followers))
    
    usernames <- df$nama
    idx <- which(usernames == "Algoritma Data Science School")
    my_colors <- rep(c("#eb4d60"),each=8)
    my_colors[idx] <- "#AE2024"
    
    df$colors <- my_colors
    
    df %>% 
      mutate(color = my_colors) %>% 
      e_chart(nama) %>% 
      e_bar(followers) %>% 
      e_theme_custom("www/chart_theme.json") %>% 
      e_add("itemStyle", color) %>% 
      e_title(
        text = "Jumlah Followers di Linkedin",
        left = "center",
        top = "0"
      ) %>% 
      e_legend(show = F) %>% 
      e_x_axis(axisLabel = list(interval = 0, rotate = 15)) %>% 
      e_tooltip(
        trigger = "item",
        formatter = JS(
          "
           function(params){return(
           '<b>' + params.name+ '</b>'+ ' : ' 
           + echarts.format.addCommas(params.value[1]) + ' Followers'
           )}
           "
        )
      )
  })
  
  output$linkedinEmployees <- renderHighchart({
    df <- profiles_linkedin %>% 
      arrange(desc(employees))
    
    usernames <- df$nama
    idx <- which(usernames == "Algoritma Data Science School")
    my_colors <- rep(c("#eb4d60"),each=8)
    my_colors[idx] <- "#AE2024"
    
    df$colors <- my_colors
    
    df %>% 
      mutate(color = my_colors) %>% 
      e_chart(nama) %>% 
      e_bar(employees) %>% 
      e_theme_custom("www/chart_theme.json") %>% 
      e_add("itemStyle", color) %>% 
      e_title(
        text = "Jumlah Pekerja di Linkedin",
        left = "center",
        top = "0"
      ) %>% 
      e_legend(show = F) %>% 
      e_x_axis(axisLabel = list(interval = 0, rotate = 15)) %>% 
      e_tooltip(
        trigger = "item",
        formatter = JS(
          "
           function(params){return(
           '<b>' + params.name+ '</b>'+ ' : ' 
           + echarts.format.addCommas(params.value[1]) + ' Pekerja'
           )}
           "
        )
      )
  })
  
  output$facebookFollowers <- renderEcharts4r({
    df <- profiles_facebook %>% 
      arrange(desc(followers))
    
    usernames <- df$nama
    idx <- which(usernames == "Algoritma")
    my_colors <- rep(c("#eb4d60"), 8)
    my_colors[idx] <- "#AE2024"
    
    df %>% 
      mutate(color = my_colors) %>% 
      e_chart(nama) %>% 
      e_bar(followers) %>% 
      e_theme_custom("www/chart_theme.json") %>% 
      e_add("itemStyle", color) %>% 
      e_title(
        text = "Jumlah Followers",
        left = "center",
        top = "0"
      ) %>% 
      e_legend(show = F) %>% 
      e_x_axis(axisLabel = list(interval = 0, rotate = 15)) %>% 
      e_tooltip(
        trigger = "item",
        formatter = JS(
          "
           function(params){return(
           '<b>' + params.name+ '</b>'+ ' : ' 
           + echarts.format.addCommas(params.value[1]) + ' Followers'
           )}
           "
        )
      )
  })
  
  output$facebookLikes <- renderEcharts4r({
    df <- profiles_facebook %>% 
      arrange(desc(total_likes))
    
    usernames <- df$nama
    idx <- which(usernames == "Algoritma")
    my_colors <- rep(c("#eb4d60"), 8)
    my_colors[idx] <- "#AE2024"
    
    df %>% 
      mutate(color = my_colors) %>% 
      e_chart(nama) %>% 
      e_bar(total_likes) %>% 
      e_theme_custom("www/chart_theme.json") %>% 
      e_add("itemStyle", color) %>% 
      e_title(
        text = "Total Likes",
        left = "center",
        top = "0"
      ) %>% 
      e_legend(show = F) %>% 
      e_x_axis(axisLabel = list(interval = 0, rotate = 15)) %>% 
      e_tooltip(
        trigger = "item",
        formatter = JS(
          "
           function(params){return(
           '<b>' + params.name+ '</b>'+ ' : ' 
           + echarts.format.addCommas(params.value[1]) + ' Likes'
           )}
           "
        )
      )
  })
  
  output$youtubeVideos <- renderEcharts4r({
    df <- profiles_youtube %>% 
      arrange(desc(videos_count))
    
    usernames <- df$nama
    idx <- which(usernames == "Algoritma Data Science School")
    my_colors <- rep(c("#eb4d60"), 8)
    my_colors[idx] <- "#AE2024"
    
    
    df %>% 
      mutate(color = my_colors) %>% 
      e_chart(nama) %>% 
      e_bar(videos_count) %>% 
      e_theme_custom("www/chart_theme.json") %>% 
      e_add("itemStyle", color) %>% 
      e_title(
        text = "Total Video pada Setiap Perusahaan",
        left = "center",
        top = "0"
      ) %>% 
      e_legend(show = F) %>% 
      e_x_axis(axisLabel = list(interval = 0, rotate = 15)) %>% 
      e_tooltip(
        trigger = "item",
        formatter = JS(
          "
           function(params){return(
           '<b>' + params.name+ '</b>'+ ' : ' 
           + echarts.format.addCommas(params.value[1]) + ' Video'
           )}
           "
        )
      )
  })
  
  output$youtubeSubscribers <- renderEcharts4r({
    df <- profiles_youtube %>% 
      arrange(desc(subscribers))
    
    usernames <- df$nama
    idx <- which(usernames == "Algoritma Data Science School")
    my_colors <- rep(c("#eb4d60"), 8)
    my_colors[idx] <- "#AE2024"
    
    
    df %>% 
      mutate(color = my_colors) %>% 
      e_chart(nama) %>% 
      e_bar(subscribers) %>% 
      e_theme_custom("www/chart_theme.json") %>% 
      e_add("itemStyle", color) %>% 
      e_title(
        text = "Total Subscribers pada Setiap Perusahaan",
        left = "center",
        top = "0"
      ) %>% 
      e_legend(show = F) %>% 
      e_x_axis(axisLabel = list(interval = 0, rotate = 15)) %>% 
      e_tooltip(
        trigger = "item",
        formatter = JS(
          "
           function(params){return(
           '<b>' + params.name+ '</b>'+ ' : ' 
           + echarts.format.addCommas(params.value[1]) + ' Subscribers'
           )}
           "
        )
      )
  })
  
  output$overviewTwitter <- renderValueBox({
    selector <- input$twitterSelector
    
    df <- ""
    if (selector == "Jumlah Tweet") {
      df <- profiles_twitter %>% 
        arrange(desc(statuses_count))
    } else if (selector == "Followers") {
      df <- profiles_twitter %>% 
        arrange(desc(followers_count))
    } else {
      df <- profiles_twitter %>% 
        arrange(desc(friends_count))
    }
    
    usernames <- df$username
    idx <- which(usernames == "teamalgoritma")
    my_colors <- rep(c("#eb4d60"),each=8)
    my_colors[idx] <- "#AE2024"
    
    hcTwitter <- ""
    if (selector == "Jumlah Tweet") {
      hcTwitter <- df %>% 
        mutate(colors=my_colors) %>% 
        hchart("column",
               hcaes(x = username, y = statuses_count, color=colors))
    } else if (selector == "Followers") {
      hcTwitter <- df %>% 
        mutate(colors=my_colors) %>% 
        hchart("column",
               hcaes(x = username, y = followers_count, color=colors))
    } else {
      hcTwitter <- df %>% 
        mutate(colors=my_colors) %>% 
        hchart("column",
               hcaes(x = username, y = friends_count, color=colors))
    }
    
    hcTwitter <- hcTwitter %>% 
      hc_title(text=glue("{selector} di Setiap Perusahaan"),
               style = list(color = "#f0f0f0", fontSize =16)) %>% 
      hc_size(height = 300) %>% 
      hc_tooltip(crosshairs = TRUE,
                 borderWidth = 1,
                 pointFormat = '<span style="color:{series.color}">Nilai</span>:
                   <b>{point.y}</b><br/>')
    
    vbTwitter <- valueBoxSpark(
      value = "2022",
      title = toupper("Jumlah Post"),
      sparkobj = hcTwitter,
      info = "Test",
      subtitle = NULL,
      icon = icon("twitter"),
      href = NULL
    )
  })
  
  # OVERVIEW TAB - EMD -----------------------------------------------
  
  
  # TWITTER TAB - START -----------------------------------------------
  
  output$retweetFavoriteInfo <- renderEcharts4r({
    selector <- input$twSelector2
    
    df <- tweets %>% 
      group_by(username)
    
    if (selector == "Total Retweet") {
      df <- df %>% 
        summarise(Total = sum(retweet_count))
    } else {
      df <- df %>% 
        summarise(Total = sum(favorite_count))
    }
    
    df <- df %>% arrange(desc(Total))
    
    usernames <- df$username
    idx <- which(usernames == "teamalgoritma")
    my_colors <- rep(c("#eb4d60"),each=8)
    my_colors[idx] <- "#AE2024"
    
    df %>% 
      mutate(color = my_colors) %>% 
      e_chart(username) %>% 
      e_bar(Total) %>% 
      e_theme_custom("www/chart_theme.json") %>% 
      e_add("itemStyle", color) %>% 
      e_title(
        text = glue("{selector} di Setiap Perusahaan"),
        left = "center",
        top = "0"
      ) %>% 
      e_legend(show = F) %>% 
      e_x_axis(axisLabel = list(interval = 0, rotate = 15)) %>% 
      e_tooltip(
        trigger = "item",
        formatter = JS(
          "
           function(params){return(
           '<b>' + params.name+ '</b>'+ ' : ' 
           + echarts.format.addCommas(params.value[1])
           )}
           "
        )
      )
    
  })
  
  
  output$infoTweet <- renderEcharts4r({
    groupBy <- input$twGroupBy
    
    if (groupBy == "Hari") {
      algoritma <- tweets %>% 
        filter(username == "teamalgoritma") %>% 
        group_by(dayname) %>% 
        summarise(Algoritma = n())
      
      purwadhika <- tweets %>% 
        filter(username == "PurwadhikaClass") %>% 
        group_by(dayname) %>% 
        summarise(Purwadhika = n()) 
      
      hacktiv8id <- tweets %>% 
        filter(username == "hacktiv8id") %>% 
        group_by(dayname) %>% 
        summarise(hacktiv8id = n()) 
      
      pacmannai <- tweets %>% 
        filter(username == "pacmannai") %>% 
        group_by(dayname) %>% 
        summarise(pacmannai = n()) 
      
      revoudotco <- tweets %>% 
        filter(username == "revoudotco") %>% 
        group_by(dayname) %>% 
        summarise(revoudotco = n())
      
      DibimbingId <- tweets %>% 
        filter(username == "DibimbingId") %>% 
        group_by(dayname) %>% 
        summarise(DibimbingId = n())
      
      digital_skola <- tweets %>% 
        filter(username == "digital_skola") %>% 
        group_by(dayname) %>% 
        summarise(digital_skola = n())
      
      Shiftacademyid <- tweets %>% 
        filter(username == "Shiftacademyid") %>% 
        group_by(dayname) %>% 
        summarise(Shiftacademyid = n()) 
      
      test <- merge(algoritma, purwadhika,by = c("dayname"), all = T)
      test <- merge(test, hacktiv8id,by = c("dayname"), all = T)
      test <- merge(test, pacmannai,by = c("dayname"), all = T)
      test <- merge(test, revoudotco,by = c("dayname"), all = T)
      test <- merge(test, DibimbingId,by = c("dayname"), all = T)
      test <- merge(test, digital_skola,by = c("dayname"), all = T)
      test <- merge(test, Shiftacademyid,by = c("dayname"), all = T)
      test[is.na(test)] <- 0
      
      test %>% 
        mutate(dayname = as.factor(as.vector(dayname))) %>% 
        e_charts(dayname) %>% 
        e_line(Algoritma) %>% 
        e_line(Purwadhika) %>% 
        e_line(hacktiv8id) %>% 
        e_line(pacmannai) %>% 
        e_line(revoudotco) %>% 
        e_line(DibimbingId) %>% 
        e_line(digital_skola) %>% 
        e_line(Shiftacademyid) %>% 
        e_title(
          text = "Jumlah Tweet Per Hari Pada Setiap Perusahaan",
          left = "center",
          top = "0"
        ) %>% 
        e_legend(top = "30") %>% 
        e_theme_custom("www/chart_theme.json") %>% 
        e_tooltip(
          trigger = "item",
          formatter = JS(
            "
           function(params){return(
           params.name + '<br><b>Jumlah Tweet</b>'
           + ' : ' 
           + params.value[1]
           )}
           "
          )
        )
    } else if (groupBy == "Bulan") {
      algoritma <- tweets %>% 
        filter(username == "teamalgoritma") %>% 
        group_by(month) %>% 
        summarise(Algoritma = n())
      
      purwadhika <- tweets %>% 
        filter(username == "PurwadhikaClass") %>% 
        group_by(month) %>% 
        summarise(Purwadhika = n()) 
      
      hacktiv8id <- tweets %>% 
        filter(username == "hacktiv8id") %>% 
        group_by(month) %>% 
        summarise(hacktiv8id = n()) 
      
      pacmannai <- tweets %>% 
        filter(username == "pacmannai") %>% 
        group_by(month) %>% 
        summarise(pacmannai = n()) 
      
      revoudotco <- tweets %>% 
        filter(username == "revoudotco") %>% 
        group_by(month) %>% 
        summarise(revoudotco = n())
      
      DibimbingId <- tweets %>% 
        filter(username == "DibimbingId") %>% 
        group_by(month) %>% 
        summarise(DibimbingId = n())
      
      digital_skola <- tweets %>% 
        filter(username == "digital_skola") %>% 
        group_by(month) %>% 
        summarise(digital_skola = n())
      
      Shiftacademyid <- tweets %>% 
        filter(username == "Shiftacademyid") %>% 
        group_by(month) %>% 
        summarise(Shiftacademyid = n()) 
      
      test <- merge(algoritma, purwadhika,by = c("month"), all = T)
      test <- merge(test, hacktiv8id,by = c("month"), all = T)
      test <- merge(test, pacmannai,by = c("month"), all = T)
      test <- merge(test, revoudotco,by = c("month"), all = T)
      test <- merge(test, DibimbingId,by = c("month"), all = T)
      test <- merge(test, digital_skola,by = c("month"), all = T)
      test <- merge(test, Shiftacademyid,by = c("month"), all = T)
      test[is.na(test)] <- 0
      
      test %>% 
        mutate(month = as.factor(as.vector(month))) %>% 
        e_charts(month) %>% 
        e_line(Algoritma) %>% 
        e_line(Purwadhika) %>% 
        e_line(hacktiv8id) %>% 
        e_line(pacmannai) %>% 
        e_line(revoudotco) %>% 
        e_line(DibimbingId) %>% 
        e_line(digital_skola) %>% 
        e_line(Shiftacademyid) %>% 
        e_title(
          text = "Jumlah Tweet Per Bulan Pada Setiap Perusahaan",
          left = "center",
          top = "0"
        ) %>% 
        e_legend(top = "30") %>% 
        e_theme_custom("www/chart_theme.json") %>% 
        e_tooltip(
          trigger = "item",
          formatter = JS(
            "
           function(params){return(
           params.name + '<br><b>Jumlah Tweet</b>'
           + ' : ' 
           + params.value[1]
           )}
           "
          )
        )
    } else {
      algoritma <- tweets %>% 
        filter(username == "teamalgoritma") %>% 
        group_by(year) %>% 
        summarise(Algoritma = n())
      
      purwadhika <- tweets %>% 
        filter(username == "PurwadhikaClass") %>% 
        group_by(year) %>% 
        summarise(Purwadhika = n()) 
      
      hacktiv8id <- tweets %>% 
        filter(username == "hacktiv8id") %>% 
        group_by(year) %>% 
        summarise(hacktiv8id = n()) 
      
      pacmannai <- tweets %>% 
        filter(username == "pacmannai") %>% 
        group_by(year) %>% 
        summarise(pacmannai = n()) 
      
      revoudotco <- tweets %>% 
        filter(username == "revoudotco") %>% 
        group_by(year) %>% 
        summarise(revoudotco = n())
      
      DibimbingId <- tweets %>% 
        filter(username == "DibimbingId") %>% 
        group_by(year) %>% 
        summarise(DibimbingId = n())
      
      digital_skola <- tweets %>% 
        filter(username == "digital_skola") %>% 
        group_by(year) %>% 
        summarise(digital_skola = n())
      
      Shiftacademyid <- tweets %>% 
        filter(username == "Shiftacademyid") %>% 
        group_by(year) %>% 
        summarise(Shiftacademyid = n()) 
      
      test <- merge(algoritma, purwadhika,by = c("year"), all = T)
      test <- merge(test, hacktiv8id,by = c("year"), all = T)
      test <- merge(test, pacmannai,by = c("year"), all = T)
      test <- merge(test, revoudotco,by = c("year"), all = T)
      test <- merge(test, DibimbingId,by = c("year"), all = T)
      test <- merge(test, digital_skola,by = c("year"), all = T)
      test <- merge(test, Shiftacademyid,by = c("year"), all = T)
      test[is.na(test)] <- 0
      
      test %>% 
        mutate(year = as.factor(as.vector(year))) %>% 
        e_charts(year) %>% 
        e_line(Algoritma) %>% 
        e_line(Purwadhika) %>% 
        e_line(hacktiv8id) %>% 
        e_line(pacmannai) %>% 
        e_line(revoudotco) %>% 
        e_line(DibimbingId) %>% 
        e_line(digital_skola) %>% 
        e_line(Shiftacademyid) %>% 
        e_title(
          text = "Jumlah Tweet Per Tahun Pada Setiap Perusahaan",
          left = "center",
          top = "0"
        ) %>% 
        e_legend(top = "30") %>% 
        e_theme_custom("www/chart_theme.json") %>% 
        e_tooltip(
          trigger = "item",
          formatter = JS(
            "
           function(params){return(
           params.name + '<br><b>Jumlah Tweet</b>'
           + ' : ' 
           + params.value[1]
           )}
           "
          )
        )
    }
    
    
  })
  
  output$news <- renderEcharts4r({
    companyNews <- input$companyNews
    
    if (companyNews == "Algoritma") {
      news2 %>% 
        filter(company == companyNews,
               Total > 2) %>% 
        arrange(desc(Total)) %>% 
        e_chart(media) %>% 
        e_bar(Total) %>% 
        e_theme_custom("www/chart_theme.json") %>% 
        e_title(
          text = glue("Media di {companyNews}"),
          left = "center",
          top = "0"
        ) %>% 
        e_legend(show = F) %>% 
        e_x_axis(axisLabel = list(interval = 0, rotate = 15)) %>% 
        e_tooltip(
          trigger = "item",
          formatter = JS(
            "
           function(params){return(
           '<b>' + params.name+ '</b>'+ ' : ' 
           + echarts.format.addCommas(params.value[1])
           )}
           "
          )
        )
    } else {
      news2 %>% 
        filter(company == companyNews) %>% 
        arrange(desc(Total)) %>% 
        e_chart(media) %>% 
        e_bar(Total) %>% 
        e_theme_custom("www/chart_theme.json") %>% 
        e_title(
          text = glue("Media di {companyNews}"),
          left = "center",
          top = "0"
        ) %>% 
        e_legend(show = F) %>% 
        e_x_axis(axisLabel = list(interval = 0, rotate = 15)) %>% 
        e_tooltip(
          trigger = "item",
          formatter = JS(
            "
           function(params){return(
           '<b>' + params.name+ '</b>'+ ' : ' 
           + echarts.format.addCommas(params.value[1])
           )}
           "
          )
        )
    }
    
  })
  
  output$cardNews <- renderValueBox({
    companyNews <- input$companyNews
    
    x <- news2 %>% 
      filter(company == companyNews) 
    x
    
    numMedia <- length(unique(x[,"media"]))
    
    box <- valueBoxSpark(
      value = glue("{numMedia}+ Media"),
      title = glue("Media dari {companyNews}"),
      sparkobj = NULL,
      info = "Average customer income based on marketing data",
      subtitle = NULL,
      icon = icon("newspaper"),
      href = NULL
    )
    
    box
  })
  
  output$cardNews2 <- renderValueBox({
    companyNews <- input$companyNews
    
    x <- news2 %>% 
      group_by(company) %>% 
      summarise(Total = sum(Total)) %>% 
      filter(company == companyNews) %>% 
      select(Total)
    
    totalPublikasi <- as.numeric(x[1])
    

    box <- valueBoxSpark(
      value = glue("{totalPublikasi}+ Publikasi"),
      title = glue("Publikasi dari {companyNews}"),
      sparkobj = NULL,
      info = "",
      subtitle = NULL,
      icon = icon("newspaper"),
      href = NULL
    )
    
    box
  })
  
  output$tweetMore <- renderEcharts4r({
    company <- input$twitterCompany
    selector <- input$twSelector3

    if (selector == "Retweet") {
      tweets %>% 
        filter(username == company) %>% 
        e_charts() %>% 
        e_boxplot(retweet_count, itemStyle = list(color = "#AE2024")) %>% 
        e_theme_custom("www/chart_theme.json") %>% 
        e_title(
          text = glue("Boxplot {selector} di Twitter Milik {company}"),
          left = "center",
          top = "0"
        ) %>% 
        e_axis_labels(y = "{selector}") %>% 
        e_tooltip(trigger = c("item", "axis"))
    } else {
      tweets %>% 
        filter(username == company) %>% 
        e_charts() %>% 
        e_boxplot(favorite_count, itemStyle = list(color = "#AE2024")) %>% 
        e_theme_custom("www/chart_theme.json") %>% 
        e_title(
          text = glue("Boxplot {selector} di Twitter Milik {company}"),
          left = "center",
          top = "0"
        ) %>% 
        e_axis_labels(y = "{selector}") %>% 
        e_tooltip(trigger = c("item", "axis"))
    }
    
  })
  
  # TWITTER TAB - END -----------------------------------------------
  
  # ALUMNI TAB - START -----------------------------------------------
  
  output$alumniAlgoritma <- renderValueBox({
    algoritmaBox <- valueBoxSpark(
      value = glue("{format(round(as.numeric(2000), 0), nsmall=0, big.mark=',')}+"),
      title = toupper("Jumlah Alumni di Algoritma"),
      sparkobj = NULL,
      info = "Average customer income based on marketing data",
      subtitle = NULL,
      icon = icon("graduation-cap"),
      href = NULL
    )
    
    algoritmaBox
  })
  
  output$alumniPurwadhika <- renderValueBox({
    box <- valueBoxSpark(
      value = glue("{format(round(as.numeric(20000), 0), nsmall=0, big.mark=',')}+"),
      title = toupper("Jumlah Alumni di Purwadhika"),
      sparkobj = NULL,
      info = "Average customer income based on marketing data",
      subtitle = NULL,
      icon = icon("graduation-cap"),
      href = NULL
    )
    
    box
  })
  
  
  output$alumniHacktiv8 <- renderValueBox({
    box <- valueBoxSpark(
      value = glue("{format(round(as.numeric(500), 0), nsmall=0, big.mark=',')}+"),
      title = toupper("Jumlah Alumni di Hacktiv8"),
      sparkobj = NULL,
      info = "Average customer income based on marketing data",
      subtitle = NULL,
      icon = icon("graduation-cap"),
      href = NULL
    )
    
    box
  })
  
  
  output$alumniDibimbiing <- renderValueBox({
    box <- valueBoxSpark(
      value = glue("{format(round(as.numeric(30000), 0), nsmall=0, big.mark=',')}+"),
      title = toupper("Jumlah Alumni di Dibimbing.id"),
      sparkobj = NULL,
      info = "Average customer income based on marketing data",
      subtitle = NULL,
      icon = icon("graduation-cap"),
      href = NULL
    )
    
    box
  })
  
  
  # ALUMNI TAB - END -----------------------------------------------
  
  
}