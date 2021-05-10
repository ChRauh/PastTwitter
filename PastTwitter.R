########################################################################################
# Functions to extract and plot info from account dashboard of specific Twitter handles
# for past points in time via archive.org snapshots
# Author: @ChRauh (10.05.2021)
########################################################################################


# Packages ####
if (!require(tidyverse)) {print("Package \"tidyverse\" (>= 1.3.0) required for these functions")}
library(tidyverse)
if (!require(rjson)) {print("Package \"rjson\" (>= 0.2.20) required for these functions")}
library(rjson)
if (!require(rvest)) {print("Package \"rvest\" (>= 0.3.6) required for these functions")}
library(rvest)
if (!require(httr)) {print("Package \"httr\" (>= 1.4.2) required for these functions")}
library(httr)


# Function to extract archive.org snapshots for a given Twitter handle ####
# Expected input: character vector of one tweitter handle (without @ or URL)

handleSnapshots <- function(handle = character(0)) {
  # Construct URL of Twitter Account
  turl = paste0("https://twitter.com/", handle)
  
  # Construct link to archive.org snapshot table of turl  
  # json version, txt available as well but I encountered faulty separation
  snapurl <- paste0("http://web.archive.org/cdx/search/cdx?url=", turl, "*&output=json") 
  
  # Download snapshot list 
  print(paste0("Downloading archive.org snapshot list for ", turl, " ... Please wait."))
  snapshots <- fromJSON(file = snapurl) # Read in file from web
  
  # Turn resulting list into data frame
  snapshots <- matrix(unlist(snapshots), nrow = length(snapshots), byrow = T) %>% 
    as.data.frame()
  
  # Header is in first row
  names(snapshots) <- snapshots[1,]
  snapshots <- snapshots[-1, ]
  
  # Clean snapshot list
  print(paste0("Cleaning archive.org snapshot list for ", turl, " ... Please wait."))
  snapshots <- snapshots %>% 
    # Internal URL key not needed
    select(-urlkey) %>% 
    # Only keep snapshots of main account, 
    # drops numerous snapshots of individual statuses etc. 
    # which may improve temporal resolutions, but would put a large burden on archive.org server
    # down the line
    filter(original == turl) %>% 
    # Only keep snapshots with actual content
    filter(mimetype == "text/html") %>% 
    filter(statuscode == "200") %>% 
    # Keep only the first snapshot per day
    # Again in order to limit later requests to archive.org
    arrange(timestamp) %>% 
    mutate(day = str_extract(timestamp, "^[0-9]{8}")) %>% 
    mutate(dday = !duplicated(day)) %>% 
    filter(dday) %>% 
    select(-dday) %>% 
    # Construct link to individual snapshots
    mutate(surl = paste0("https://web.archive.org/web/", timestamp, "/", original))
  
  # Return result
  return(snapshots)
}



# Function to extract account info for each snapshot ###
# Expected input: data frame structured along the output of handleSnapshots() output


extractAccountInfo <- function(snapshot_df = data.frame(0)) {
  
  # Establish data frame with target variables
  df <- snapshot_df
  df$follower_count <- as.numeric(NA)
  df$following_count <- as.numeric(NA)
  df$likes_count <- as.numeric(NA)
  df$tweet_count <- as.numeric(NA)
  
  # Note source URL
  turl <- unique(df$original)
  
  # Loop through snapshots and extract info
  
  for (i in 1:nrow(df)) {
    
    # Show progress
    print(paste0("Extract info from snapshot ", i, "/", nrow(df), " of ", turl, " ... Please wait."))
    
    # Read HTML of snapshot page
    # archive.org takes some time
    # wrapped in httr:GET to increase timeout tolerance
    page <- read_html(df$surl[i])
    # page <- df$surl[i] %>% GET(., timeout(120)) %>% read_html()
    
    # Select Profile Navigation node
    prfnav <- html_node(page, xpath = "//div[@class=\"ProfileNav\"]") 
    
    # Select relevant list items
    items <- html_nodes(prfnav, xpath = '//li[contains(@class, "ProfileNav-item")]')
    
    # Select follower node and extract 'data_count' attribute
    # Walking systematically through the DOM not possible as the <a> element contains a re-direction
    # thus extracting infor directly from HTML source code of li element
    followers <-
      html_node(items, xpath = '//li[@class=\"ProfileNav-item ProfileNav-item--followers\"]')[1] %>% 
      as.character() %>% # html source code
      str_extract("data-count.*?( |>)") %>%  # String match 'data-count' attribute until next whitepsace or tag end
      str_extract("[0-9]+") %>% # Extract the number
      as.numeric()
    
    # Select following node and extract 'data_count' attribute
    following <-
      html_node(items, xpath = '//li[@class=\"ProfileNav-item ProfileNav-item--following\"]')[1] %>% 
      as.character() %>% # html source code
      str_extract("data-count.*?( |>)") %>%  # String match 'data-count' attribute until next whitepsace or tag end
      str_extract("[0-9]+") %>% # Extract the number
      as.numeric()  
    
    # Select tweet count node and extract 'data_count' attribute
    tweets <-
      html_node(items, xpath = '//li[@class=\"ProfileNav-item ProfileNav-item--tweets is-active\"]')[1] %>% 
      as.character() %>% # html source code
      str_extract("data-count.*?( |>)") %>%  # String match 'data-count' attribute until next whitepsace or tag end
      str_extract("[0-9]+") %>% # Extract the number
      as.numeric() 
    
    # Select favorite count node and extract 'data_count' attribute
    favorites <-
      html_node(items, xpath = '//li[@class=\"ProfileNav-item ProfileNav-item--favorites\"]')[1] %>% 
      as.character() %>% # html source code
      str_extract("data-count.*?( |>)") %>%  # String match 'data-count' attribute until next whitepsace or tag end
      str_extract("[0-9]+") %>% # Extract the number
      as.numeric() 
    
    # Prior to April 2017, the Twitter profile page apparently 
    # didn't contain the 'data_count' attribute, often only the actually displayed number
    # I capture those case here
    
    # Note: if follower value is empty at this stage, the respective node
    # didn't exist in the snapshot at all (Twitter loading errors)
    
    if (length(followers) != 0) {
      
      if (is.na(followers) & as.numeric(df$day[i]) <= 20170401) {
        
        # Select follower node and extract literal data point 
        # Walking systematically through the DOM not possible as the <a> element contains a re-direction
        # thus extracting infor directly from HTML source code of li element
        followers <-
          html_node(items, xpath = '//li[@class=\"ProfileNav-item ProfileNav-item--followers\"]')[1] %>% 
          as.character() %>% # html source code
          str_extract("<span class=\"ProfileNav-value\".*?</span>") %>% # The relvant span tag
          str_remove(".*?>") %>% # Isolate printed content of span tag
          str_remove("<.*$") %>% 
          str_remove_all("(\\.|,)") %>% # Remove commas or decimal points
          as.numeric() # Note that this removes data entries for which 'data-is-compact="true"' in the span tag (e.g. 2,6M), those will be captured only for the data-count approach above
        
        # Select following node and extract 'data_count' attribute
        following <-
          html_node(items, xpath = '//li[@class=\"ProfileNav-item ProfileNav-item--following\"]')[1] %>% 
          as.character() %>% # html source code
          str_extract("<span class=\"ProfileNav-value\".*?</span>") %>% # The relvant span tag
          str_remove(".*?>") %>% # Isolate printed content of span tag
          str_remove("<.*$") %>% 
          str_remove_all("(\\.|,)") %>% # Remove commas or decimal points
          as.numeric()  
        
        # Select tweet count node and extract 'data_count' attribute
        tweets <-
          html_node(items, xpath = '//li[@class=\"ProfileNav-item ProfileNav-item--tweets is-active\"]')[1] %>% 
          as.character() %>% # html source code
          str_extract("<span class=\"ProfileNav-value\".*?</span>") %>% # The relvant span tag
          str_remove(".*?>") %>% # Isolate printed content of span tag
          str_remove("<.*$") %>% 
          str_remove_all("(\\.|,)") %>% # Remove commas or decimal points
          as.numeric()
        
        # Select tweet count node and extract 'data_count' attribute
        favorites <-
          html_node(items, xpath = '//li[@class=\"ProfileNav-item ProfileNav-item--favorites\"]')[1] %>% 
          as.character() %>% # html source code
          str_extract("<span class=\"ProfileNav-value\".*?</span>") %>% # The relvant span tag
          str_remove(".*?>") %>% # Isolate printed content of span tag
          str_remove("<.*$") %>% 
          str_remove_all("(\\.|,)") %>% # Remove commas or decimal points
          as.numeric()
      }
    }
    
    # Where data presentation is set to compact for that period of the Twitter profile design
    # the actual number is contained in the title attribute of the respective a tag
    # I extract those here, again only if it has not been captured by the earlier approaches
    
    if (length(followers) != 0) {
      
      if (is.na(followers) & as.numeric(df$day[i]) <= 20170401) {
        
        # Extract follower count from respective a title attribute
        followers <-
          html_node(items, xpath = '//li[@class=\"ProfileNav-item ProfileNav-item--followers\"]')[1] %>%
          html_node(xpath = '//a[(@data-nav="followers")]') %>% 
          html_attr("title") %>% 
          str_remove_all("[^0-9]") %>% # Remove all non-numeric content
          as.numeric()
        
        # Extract following count from respective a title attribute
        following <-
          html_node(items, xpath = '//li[@class=\"ProfileNav-item ProfileNav-item--followers\"]')[1] %>%
          html_node(xpath = '//a[(@data-nav="following")]') %>% 
          html_attr("title") %>% 
          str_remove_all("[^0-9]") %>% # Remove all non-numeric content
          as.numeric() 
        
        # Extract tweet count from respective a title attribute
        tweets <-
          html_node(items, xpath = '//li[@class=\"ProfileNav-item ProfileNav-item--followers\"]')[1] %>%
          html_node(xpath = '//a[(@data-nav="tweets")]') %>% 
          html_attr("title") %>% 
          str_remove_all("[^0-9]") %>% # Remove all non-numeric content
          as.numeric()
        
        # Extract favorites count from respective a title attribute
        favorites <-
          html_node(items, xpath = '//li[@class=\"ProfileNav-item ProfileNav-item--followers\"]')[1] %>%
          html_node(xpath = '//a[(@data-nav="favorites")]') %>% 
          html_attr("title") %>% 
          str_remove_all("[^0-9]") %>% # Remove all non-numeric content
          as.numeric()
      }
    }
    
    
    # Then there was an intermediate period ~2013 when the page design looked different
    # without profile-nav-items, (so length(followers) == 0 here)
    # collect this here if not yet found
    
    if(length(followers)==0) {
      
      # Extract follower count from respective a tag
      followers <-
        html_node(page, xpath = '//a[(@data-element-term="follower_stats")]') %>%
        as.character() %>% 
        str_extract("strong>.*?<") %>% 
        str_remove_all("(strong>|,|<)") %>% 
        as.numeric()
      
      # Extract following count from respective a tag
      tweets <-
        html_node(page, xpath = '//a[(@data-element-term="following_stats")]') %>%
        as.character() %>% 
        str_extract("strong>.*?<") %>% 
        str_remove_all("(strong>|,|<)") %>% 
        as.numeric()
      
      # Extract tweet count from respective a tag
      following <-
        html_node(page, xpath = '//a[(@data-element-term="tweet_stats")]') %>%
        as.character() %>% 
        str_extract("strong>.*?<") %>% 
        str_remove_all("(strong>|,|<)") %>% 
        as.numeric()
    }
    
    
    # Finally The very old design of the twitter profile page
    # Exact break point somewhen before 20120829
    
    if(as.numeric(df$day[i]) <= 20120829) {
    # if(length(followers)==0 & as.numeric(df$day[i]) <= 20120829) {
      
      # Extract follower count from respective span tag
      followers <-
        html_node(page, xpath = '//span[@id="follower_count"]') %>%
        html_text() %>% 
        str_remove_all(",") %>% 
        as.numeric()
      
      # Extract following count from respective span tag
      following <-
        html_node(page, xpath = '//span[@id="following_count"]') %>%
        html_text() %>% 
        str_remove_all(",") %>% 
        as.numeric()
      
      # Extract tweets from respective span tag
      tweets <-
        html_node(page, xpath = '//span[@id="update_count"]') %>%
        html_text() %>% 
        str_remove_all(",") %>% 
        as.numeric()
    }
    
    
    # Write data to target data frame
    if (length(followers)>0) df$follower_count[i] <- followers
    if (length(following)>0) df$following_count[i] <- following
    if (length(favorites)>0) df$likes_count[i] <- favorites
    if (length(tweets)>0) df$tweet_count[i] <- tweets
    
    # Clean up
    rm(list = c('followers','following', 'favorites', 'tweets'))
    
  }
  
  # Clean up
  rm(list = c('i','turl'))
  df <- df %>% 
    rename(date = day,
           screen_name = original) %>% 
    mutate(screen_name = str_remove(screen_name, fixed("https://twitter.com/"))) %>% 
    select(c(screen_name, date, surl, tweet_count, follower_count, following_count, likes_count)) %>% 
    # When none of the target vars existed, snapshot just reflext a Twitter loading error
    # Example: https://web.archive.org/web/20201210145145/https://twitter.com/vonderleyen
    # Remove those cases here
    # filter(!(is.na(follower_count) & is.na(following_count) & is.na(tweet_count) & is.na(likes_count))) %>% 
    arrange(date)
    
  
  # Output
  print("Completed!")
  return(df)
  
}



# Convenience function for time series plot of follower counts ####
# Expected input: data frame as structured by extractAccountInfo() function above

plotFollowers <- function(account_df = data.frame(0)) {
  
  # Copy of data
  df <- account_df
  
  # Full sequence of days between earliest and latest archive.org snapshot
  ts <- seq.Date(as.Date(min(df$date), format = "%Y%m%d"), 
                 as.Date(max(df$date), format = "%Y%m%d"), by = "days") %>% 
    as.data.frame() %>% 
    rename(date = 1) %>% 
    mutate(date = as.character(date))
  
  # Store handle for plotting
  handle <- unique(df$screen_name)
  
  # Adapt date var in main data
  df$date <- df$date %>% 
    as.Date(format = "%Y%m%d") %>% 
    as.character()
  
  # Reduce main data
  df <- df %>% 
    select(c(date, follower_count))
  
  # Merge full timeline with available main data
  ts <- ts %>% 
    left_join(df, by = "date")
  
  # X-axis params
  breaks <- ts %>% 
    # filter(str_detect(date, "-01$")) %>% # First of every  month
    filter(str_detect(date, "(-01-01$)|(-07-01$)")) %>% # First of January and July (roughly half-years)
    select(date)
  breaks <- breaks[,1] # Atomic character vector
  labels <- breaks
  
  # Plot the time series
  pl.followers <-
    ggplot(ts, aes(x = date, y = follower_count, group = 1)) +
    geom_bar(stat = "identity", fill = "#1DA1F2") + # Twitter blue ...
    geom_line(data = ts[!is.na(ts$follower_count), ], linetype = "dashed", color = "#1DA1F2")+ # Implicit linear interpolation
    scale_x_discrete(breaks = breaks, labels = labels)+
    scale_y_continuous(labels = function(x) format(x, digits = 0, scientific = F, big.mark = "."))+
    labs(title = paste0("Followers of @", handle, " on Twitter"),
         subtitle = paste0("As seen through all available snapshots of \'https://twitter.com/", handle, "\' on archive.org"),
         y= "Count of profile followers\n",
         x = "\nDay",
         caption = "\nVertical bars indicate that a snapshot is available on that day, the dashed line presents the linear interpolation.\nData extraction scripts available at: https://github.com/ChRauh/PastTwitter")+
    theme_light()+
    theme(axis.text = element_text(color = "black"),
          axis.text.x = element_text(angle = 90, vjust = .5),
          plot.title = element_text(face = "bold", color = "#1DA1F2"),
          plot.subtitle = element_text(face = "italic", color = "grey20"))
  
  # Return plot
  return(pl.followers)
}



  