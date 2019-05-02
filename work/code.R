library("rvest")
library("stringr")

scrapeBacteria <- function(mic = TRUE, dir = tempdir()) {
    stopifnot(is.logical(mic), length(mic) == 1)
    stopifnot(is.character(dir), length(dir) == 1)
    
    ## setup webscraping 
    url <- "https://mic.eucast.org/Eucast2/SearchController/search.jsp?action=init"
    session1 <- html_session(url)
    f <- html_form(session1)[[1]]
    antibiotics <- f$fields[[6]]$options # get options for antibiotics
    antibiotics <- antibiotics[antibiotics != "-1"]
    
    ## There is no submit-button in the html, therefore we create one, so submit_form() works
    ## source: https://stackoverflow.com/questions/33885629/submit-form-with-no-submit-button-in-rvest
    fake_submit <- list(name = NULL, type = "submit", value = NULL, checked = NULL, 
                        disabled = NULL, readonly = NULL, required = FALSE)
    attr(fake_submit, "class") <- "input"
    
    ## setup output df
    DF <- if (mic) {
              data.frame("Antimicrobial" = character(),
                         "Bacterium" = character(),
                         "M0.002" = numeric(), "M0.004" = numeric(),
                         "M0.008" = numeric(), "M0.016" = numeric(),
                         "M0.032" = numeric(), "M0.064" = numeric(),
                         "M0.125" = numeric(), "M0.25" = numeric(),
                         "M0.5" = numeric(), "M1" = numeric(),
                         "M2" = numeric(), "M4" = numeric(),
                         "M8" = numeric(), "M16" = numeric(),
                         "M32" = numeric(), "M64" = numeric(),
                         "M128" = numeric(), "M256" = numeric(),
                         "M512" = numeric(), "ECOFF" = numeric(),
                         "Distributions" = numeric(),
                         "Observations" = numeric())
          } else {
              data.frame("Antimicrobial" = character(),
                         "Bacterium" = character(),
                         "DiskContent" = numeric(), "Z6" = numeric(),
                         "Z7" = numeric(), "Z8" = numeric(),
                         "Z9" = numeric(), "Z10" = numeric(),
                         "Z11" = numeric(), "Z12" = numeric(),
                         "Z13" = numeric(), "Z14" = numeric(),
                         "Z15" = numeric(), "Z16" = numeric(),
                         "Z17" = numeric(), "Z18" = numeric(),
                         "Z19" = numeric(), "Z20" = numeric(),
                         "Z21" = numeric(), "Z22" = numeric(),
                         "Z23" = numeric(), "Z24" = numeric(),
                         "Z25" = numeric(), "Z26" = numeric(),
                         "Z27" = numeric(), "Z28" = numeric(),
                         "Z29" = numeric(), "Z30" = numeric(),
                         "Z31" = numeric(), "Z32" = numeric(),
                         "Z33" = numeric(), "Z34" = numeric(),
                         "Z35" = numeric(), "Z36" = numeric(),
                         "Z37" = numeric(), "Z38" = numeric(),
                         "Z39" = numeric(), "Z40" = numeric(),
                         "Z41" = numeric(), "Z42" = numeric(),
                         "Z43" = numeric(), "Z44" = numeric(),
                         "Z45" = numeric(), "Z46" = numeric(),
                         "Z47" = numeric(), "Z48" = numeric(),
                         "Z49" = numeric(), "Z50" = numeric(),
                         "ECOFF" = numeric(),
                         "Distributions" = numeric(),
                         "Observations" = numeric())
          }  
  
    ## loop through options 
    for (i in seq_along(antibiotics)) {
        f <- html_form(session1)[[1]]
        f$fields[[3]]$checked <- NULL
        f$fields[[4]]$checked <- "checked"
        f$fields[[5]]$value <- "1000"
        f$fields[[6]]$value <- antibiotics[i] 
        f$fields[["submit"]] <- fake_submit

        session1 <- suppressMessages(submit_form(session1, f)) 
        if (!mic) {
            session1 <- html_session(str_replace(session1$url, regex("=mic"), "=dif"))
        }
        
        ## sometimes no data available = > table doesn't exist = > tryCatch necessary
        d <- tryCatch(html_table(html_node(read_html(session1), xpath = "/html/body/table[3]"), header = TRUE), 
                      error = function(e) NA)
        
        if (!is.null(dim(d)) && ncol(d) > 1) { 
            colnames(d)[1] <- "Bacterium"
            d <- subset(d, Bacterium != "", colnames(d) != "")
            ## as.numeric() leads to warnings with missing data
            D <- suppressWarnings(data.frame(Antimicrobial = names(antibiotics)[i], Bacterium = d[, 1],
                                             as.data.frame(apply(d[, -1, drop = FALSE], 2, function(x) list(as.numeric(x)))),
                                             stringsAsFactors = FALSE))
            colnames(D) <- colnames(DF)
        } else {
            D <- NULL
        }
        ## add data to DF
        DF <- rbind(DF, D)
    }
    ## write csv
    filename <- if (mic) "MIC.csv" else "ZD.csv"
    write.table(x = DF, file = file.path(dir, filename),
                sep = ";", row.names = FALSE, na = "")
}

dir.create(file.path("..", "data"), showWarnings = FALSE)
scrapeBacteria(TRUE, dir = file.path("..", "data")) # for mic data (~75sek)
scrapeBacteria(FALSE, dir = file.path("..", "data")) # for zone data (~125sek)

