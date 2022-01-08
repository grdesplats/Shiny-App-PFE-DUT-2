volcano_image <- function (volcano_Name){
  
  require(rvest)
  require(htmltools)
  
  volcano_WikiUrl <- gsub(" ","_",paste0("https://en.wikipedia.org/wiki/",volcano_Name))
  
  volcano_Img <- tryCatch(
    {
    volcano_Img <- read_html(volcano_WikiUrl)
    volcano_Img <- volcano_Img %>% html_elements("img") 
    
    
    list_volcano_Img <- (grepl("This is a featured article", volcano_Img) |grepl("Mergefrom.svg/50px-Mergefrom.svg.png", volcano_Img) | grepl("Ambox current red Americas.svg", volcano_Img) | grepl("Translation_to_english_arrow.svg", volcano_Img) | grepl("OOjs_UI_icon_edit-ltr-progressive.svg", volcano_Img) | grepl("Wiki_letter_w.svg/40px-Wiki_letter_w.svg.png", volcano_Img) | grepl("Question_book-new.svg.png", volcano_Img) | grepl("Listen to this article", volcano_Img) | grepl("This is a good article", volcano_Img)| grepl("Page semi-protected", volcano_Img) | grepl("Ambox current red Asia Australia.svg", volcano_Img) |grepl("Text_document_with_red_question_mark.svg.png", volcano_Img) | grepl("Ambox_current_red.svg/42px-Ambox_current_red.svg.png", volcano_Img))
    volcano_Img <- volcano_Img[min(which(list_volcano_Img == FALSE))]
    
    volcano_Img <- gsub("\"","'",volcano_Img)
    volcano_Img <- gsub("//upload.wikimedia.org","https://upload.wikimedia.org",volcano_Img)
    volcano_Img <- sub("<img","<img style = 'width:100%; height:100%; border-top-left-radius: 5%;border-top-right-radius: 5%;'",volcano_Img)
    
    volcano_Img
    },
    error = function(cond){
      return("")
    }
  )
  closeAllConnections()
  return(volcano_Img)

}


volcano_text <- function (volcano_Name){
  
  
  volcano_WikiUrl <- gsub(" ","_",paste0("https://en.wikipedia.org/wiki/",volcano_Name))
  
  volcano_Img <- tryCatch(
    {
  volcano_t <- read_html(volcano_WikiUrl)
  volcano_t <- volcano_t %>% html_elements("p") %>% html_text2()
  
  
  volcano_t = volcano_t %>% substr(1,200) %>% htmlEscape()
  volcano_t = paste0(volcano_t,"...")
  
  if(grepl("mw-empty-elt",volcano_t[1]) | volcano_t[1] == "..."){
    if(grepl("mw-empty-elt",volcano_t[2]) | volcano_t[2] == "..."){
      
      return(volcano_t[3])
      
    } else {
      
      return(volcano_t[2])
      
    }
    
  } else {
    
    return(volcano_t[1])
    
  }
  
  closeAllConnections()
    },
  error = function(cond){
    errorText = paste0("Wikipedia Url do not exist : you may find what you want on ",tags$a(href= paste0("https://www.volcanodiscovery.com/",volcano_Name),paste0("https://www.volcanodiscovery.com/",volcano_Name),target='_blank'))
    urlExist = FALSE
    return(errorText)
  }
  )
  
}

volcano_card <- function (volcano_Name,volcano_VEI,Volcano_type,volcano_elevation,volcano_eruption) {
  
  
  card_content <- paste0("<style>div.leaflet-popup-content {width:automatic !important;}</style>",
                         "<div class= 'card'>",
                         "<div class='card-image'>",volcano_image(volcano_Name),"</div>",
                         "<div class='card-text'>",
                         "<span class='date'>last eruption : ",volcano_eruption," </span>",
                         "<h2>",tags$b(volcano_Name),"</h2>",
                         "<p>",volcano_text(volcano_Name),"</p>",
                         "<p><a href='https://en.wikipedia.org/wiki/",volcano_Name,"' target='_blank'>Learn more on Wikipedia</i><p></a>",
                         "</div>",
                         "<div class='card-stats'>",
                         "<div class='stat'>",
                         "<div class='value'>", volcano_VEI
                         ,"</div>",
                         "<div class='type'> VEI Max ",
                         tipify(icon("question-circle", lib = "font-awesome",id = "question-1"), "TEST TEST TEST ") ," </div>",
                         "</div>",
                         "<div class='stat'>",
                         "<div class='value'>", Volcano_type ,"</div>",
                         "<div class='type'> type </div>",
                         "</div>",
                         "<div class='stat'>",
                         "<div class='value'>", volcano_elevation ,"</div>",
                         "<div class='type'> height </div>",
                         "</div>",
                         "</div>",
                         "</div>"
                         
  )
  return(card_content)
  
}