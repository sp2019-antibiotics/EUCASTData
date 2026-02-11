library("rvest")
library("stringr")

scrapeBacteria <- function(mic = TRUE, dir = tempdir()) {
    stopifnot(is.logical(mic), length(mic) == 1)
    stopifnot(is.character(dir), length(dir) == 1)
    
    ## setup webscraping 
    url <- "https://mic.eucast.org/Eucast2/SearchController/search.jsp?action=init"
    session1 <- html_session(url)
    f <- html_form(session1)[[1]]
    antibiotics <- f$fields[["search[antibiotic]"]]$options
    names(antibiotics) <- trimws(gsub("\\\n", "", names(antibiotics)))
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
                         "0.002" = numeric(), "0.004" = numeric(),
                         "0.008" = numeric(), "0.016" = numeric(),
                         "0.03" = numeric(), "0.06" = numeric(),
                         "0.125" = numeric(), "0.25" = numeric(),
                         "0.5" = numeric(), "1" = numeric(),
                         "2" = numeric(), "4" = numeric(),
                         "8" = numeric(), "16" = numeric(),
                         "32" = numeric(), "64" = numeric(),
                         "128" = numeric(), "256" = numeric(),
                         "512" = numeric(), 
                         "Distributions" = numeric(),
                         "Observations" = numeric(),
                         "(T)ECOFF" = character(),
                         "Confidence interval" = character(),
                         check.names = FALSE)
          } else {
              data.frame("Antimicrobial" = character(),
                         "Bacterium" = character(),
                         "Disk content" = numeric(), "6" = numeric(),
                         "7" = numeric(), "8" = numeric(),
                         "9" = numeric(), "10" = numeric(),
                         "11" = numeric(), "12" = numeric(),
                         "13" = numeric(), "14" = numeric(),
                         "15" = numeric(), "16" = numeric(),
                         "17" = numeric(), "18" = numeric(),
                         "19" = numeric(), "20" = numeric(),
                         "21" = numeric(), "22" = numeric(),
                         "23" = numeric(), "24" = numeric(),
                         "25" = numeric(), "26" = numeric(),
                         "27" = numeric(), "28" = numeric(),
                         "29" = numeric(), "30" = numeric(),
                         "31" = numeric(), "32" = numeric(),
                         "33" = numeric(), "34" = numeric(),
                         "35" = numeric(), "36" = numeric(),
                         "37" = numeric(), "38" = numeric(),
                         "39" = numeric(), "40" = numeric(),
                         "41" = numeric(), "42" = numeric(),
                         "43" = numeric(), "44" = numeric(),
                         "45" = numeric(), "46" = numeric(),
                         "47" = numeric(), "48" = numeric(),
                         "49" = numeric(), "50" = numeric(),
                         "Distributions" = numeric(),
                         "Observations" = numeric(),
                         "(T)ECOFF" = character(),
                         "Confidence interval" = character(),
                         check.names = FALSE)
          }  

    nums <- sapply(DF, class) == "numeric"
    
    ## loop through options 
    for (i in seq_along(antibiotics)) {
        f <- html_form(session1)[[1]]

        f$fields[["search[antibiotic]"]]$value <- antibiotics[i]
        f$fields[["search[limit]"]]$value <- "1000"
        
        ## f$fields[[3]]$checked <- NULL
        ## f$fields[[4]]$checked <- "checked"
        ## f$fields[[5]]$value <- "1000"
        ## f$fields[[6]]$value <- antibiotics[i] 
        ## f$fields[["submit"]] <- fake_submit

        f$action <- "https://mic.eucast.org/search/"

        if (mic) {
            f$fields[[2]] <- NULL
        } else {
            f$fields[[1]] <- NULL
        }
        session1 <- suppressMessages(submit_form(session1, f))
        
        ## sometimes no data available = > table doesn't exist = > tryCatch necessary
        d <- tryCatch(html_table(html_node(read_html(session1), xpath = "/html/body"), header = TRUE), 
                      error = function(e) NA)
        D <- NULL
        if (!is.null(dim(d)) && ncol(d) > 1) {
            colnames(d)[1] <- "Bacterium"
            stopifnot(colnames(DF)[-1] == colnames(d))
            d <- subset(d, Bacterium != "")
            if (mic) {
                d <- subset(d, !`0.002` %in% c("mic.eucast.org", "Provider/service", "No data available"))
            } else {
                d <- subset(d, !`Disk content` %in% c("mic.eucast.org", "Provider/service", "No data available"))
            }
            if (nrow(d) > 1) {
                D <- cbind(Antimicrobial = names(antibiotics)[i], d)
                ## as.numeric() leads to warnings with missing data
                D[nums] <- suppressWarnings(lapply(D[, nums, drop = FALSE], function(x) as.numeric(x)))
            }
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

