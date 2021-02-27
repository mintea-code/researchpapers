#code final for upload



#### SCRAPING #############

#initialise URL components
url_base <- "https://core.ac.uk/api-v2/articles/search/repositories.id:517?"

url_key <- "apiKey=***REMOVED***"

url_page <- "&page=" #the page number will be added later when looping

url_pagesize <- "&pageSize=100"

url_fulltext <- "&fulltext=TRUE"


#1st batch: pages 1-100

#initialise list to collect records
batch1 <- vector(mode = "list", length = 0)

#extract records from pages 1-100
for (n in c(1:100)){
  url_scrape <- paste(url_base, 
                      url_key, #to replace "REDACTED" with actual key 
                      url_page, 
                      n, #this is the page number
                      url_pagesize, 
                      url_fulltext, sep = "")
  try({
    resp <- httr::GET(url_scrape) #get up to 100 records
    cont_raw <- httr::content(resp) #change from JSON to list of lists
    #extract records from element "data" 
    
    for (record in cont_raw$data){
      batch1 <- batch1 %>% append(list(record))
    }
    print(
      paste("Extracted page", n, sep=" ")
    )
  })
  
  
  Sys.sleep(2) #To limit the request rate per CORE rules
  
}


#extracting missing pages
batch2 <- vector(mode = "list", length = 0)

for (n in c(2, 5, 20, 43,
            51, 52, 53, 54,
            64, 70, 74, 76,
            77, 78, 94, 98, 99)){
  url_scrape <- paste(url_base, 
                      url_key, #to replace "REDACTED" with actual key 
                      url_page, 
                      n, #this is the page number
                      url_pagesize, 
                      url_fulltext, sep = "")
  try({
    resp <- httr::GET(url_scrape) #get up to 100 records
    cont_raw <- httr::content(resp) #change from JSON to list of lists
    #extract records from element "data" 
    
    for (record in cont_raw$data){
      batch2 <- batch2 %>% append(list(record))
    }
    print(
      paste("Extracted page", n, sep=" ")
    )
  })
  
  
  Sys.sleep(2) #To limit the request rate per CORE rules
  
}

#all pages were extracted except page 2, so we'll try to extract it again

batch3 <- vector(mode = "list", length = 0)

for (n in c(2)){
  url_scrape <- paste(url_base, 
                      url_key, #to replace "REDACTED" with actual key 
                      url_page, 
                      n, #this is the page number
                      url_pagesize, 
                      url_fulltext, sep = "")
  try({
    resp <- httr::GET(url_scrape) #get up to 100 records
    cont_raw <- httr::content(resp) #change from JSON to list of lists
    #extract records from element "data" 
    
    for (record in cont_raw$data){
      batch3 <- batch3 %>% append(list(record))
    }
    print(
      paste("Extracted page", n, sep=" ")
    )
  })
  
}
#page 2 extracted successfully


#combine all batches into one list
batches_all <- append(batch1, c(batch2, batch3))


batches_all %>% str(max.level = 3, list.len = 4) # list of 10,000

#### RESTRUCTURING ###############################

# Function for tidying

TidyDF <- function(x){
  # Some code adapted from
  # https://www.r-bloggers.com/2018/10/converting-nested-json-to-a-tidy-data-frame-with-r/
  
  x <- unlist(x) #flatten into vector
  x <- enframe(x) #turn into df
  x <- t(x) #transpose df
  x <- as.data.frame(x)
  rownames(x) <- NULL #reset row names to default numbering
  colnames(x) <- x[1,] #set column names as correct variable names
  x <- x[-1,] #delete original row of variable names
  
  return(x)
}



##Initialise empty df with key columns
record_df <- data.frame(id=character(),
                        year=character(),
                        #character for now to match data type of raw data.
                        #will be changed to integer type in filled df later
                        title=character(), 
                        fullText=character(),
                        authors=character(),
                        subjects=character(),
                        publisher=character(),
                        topics=character(),
                        description=character(),
                        stringsAsFactors=FALSE)


#tidy all records and combine into single df
for (item in batches_all){
  item_tidy <- item %>% TidyDF()
  record_df <- full_join(record_df,item_tidy)
}
#----------------------

#inspect df
record_df %>%  str(max.level = 3, list.len = 4)
record_df %>% View()

##check for duplicate IDs
length(unique(record_df$id)) == nrow(record_df) #returns TRUE if all IDs are unique

#write df to csv
write.csv(record_df, "data/record_df.csv")


#code for markdown document
# ##read in record_df data frame
#record_df <- data.table::fread("data/record_df.csv", stringsAsFactors = FALSE)
# 
# #summarise data
# print(dfSummary(record_df, valid.col = FALSE, graph.magnif = 0.75), 
#       max.tbl.height = 300, method = "render")




######## SUBSETTING THE DATA ###################


# trim variables
cut_vars <- c("id","year","title","fullText","authors","subjects","publisher")

# record_cut<- record_df[cut_vars] #alternative
record_cut <- record_df %>% select(all_of(cut_vars))


# add helper column indicating fulltext status
record_cut$fullTextStatus <- ifelse(is.na(record_cut$fullText), "MissingFullText", "HasFullText")

# frequencies of full-text status
record_cut$fullTextStatus %>% freq() #66% of records have full text

#write.csv(record_cut, "data/record_cut.csv")

# extract full-text records
record_cut_full <- record_cut %>% subset(fullTextStatus == "HasFullText")

record_cut_full$fullTextStatus %>% freq() #100% of records have full text



## randomly select 100 full-text records from each year from 2016-2020

set.seed(123) # for repeatable selection
record_2020 <- record_cut_full %>% subset(year=="2020") %>% sample_n(100)

set.seed(123)
record_2019 <- record_cut_full %>% subset(year=="2019") %>% sample_n(100)

set.seed(123)
record_2018 <- record_cut_full %>% subset(year=="2018") %>% sample_n(100)

set.seed(123)
record_2017 <- record_cut_full %>% subset(year=="2017") %>% sample_n(100)

set.seed(123)
record_2016 <- record_cut_full %>% subset(year=="2016") %>% sample_n(100)

#compile records into list
record_1620 <-list(
  record_2020,
  record_2019,
  record_2018,
  record_2017,
  record_2016)

#initialise empty df with key columns
sample_yr_df <- data.frame(id=character(),
                           year=character(),
                           title=character(), 
                           fullText=character())

#combine records into df
for (item in record_1620){
  sample_yr_df <- full_join(sample_yr_df,item)
}

#write to csv
write.csv(sample_yr_df, "data/sample_yr_df.csv")

# #for markdown document
#sample_yr_df <- data.table::fread("data/sample_yr_df.csv", stringsAsFactors = FALSE)

#inspect df
sample_yr_df %>%  str(max.level = 3, list.len = 4)

##check for duplicate IDs
length(unique(sample_yr_df$id)) == nrow(sample_yr_df) #returns TRUE if all IDs are unique


########TEXT CLEANING####################

#tokenise full texts and put tokens in $word column
record_tok <- unnest_tokens(sample_yr_df, word, fullText)

##note: this removes $fullText column

record_tok$word %>% head()


#clean tokens
## Note: lowercasing and punctuation removal are
## already done by unnest_tokens)

CleanseAcadText <- function(x){
  x <- rm_number(x) # remove numbers (inc. numbers with commas, decimals and negatives)
  x <- rm_non_words(x) #remove anything that's not a letter or apostrophe
  x <- rm_non_ascii(x) # remove non_ascii characters
  x <- str_replace_all(x, fixed(" "), "") #remove all spaces
  return(x)
}

record_tok$cleanword <- record_tok$word %>% CleanseAcadText()

#remove $word column, the 8th column
record_tok <- record_tok[,-8]

#check
record_tok %>% names() #$word column has been removed


#remove stop words

##load stop words from tidytext
data("stop_words") 

##remove stop words in $cleanword col
record_nostop <- record_tok %>% anti_join(stop_words, by= c("cleanword" ="word"))

#remove all NAs and empty cells in $cleanword col
record_noempty <- record_nostop %>%
  drop_na(c("cleanword")) %>% 
  dplyr:: filter(cleanword != "")


#save
saveRDS(record_noempty, "record_noempty.rds")
write.csv(record_noempty, "data/record_noempty.csv")




########### most frequent terms #######
#code adapted from https://www.tidytextmining.com/


#visualise top 20 terms
record_noempty %>% 
  dplyr::count(cleanword, sort = TRUE) %>% #count cleanwords
  head(20) %>%
  mutate(cleanword = reorder(cleanword, n)) %>% #reorder by count of words
  ggplot(aes(n, cleanword)) +
  geom_col(fill="purple")+
  labs(title = "20 most frequent terms")+
  theme(plot.title = element_text(size=12))


########### topic modelling ########

#code adapted from https://www.tidytextmining.com/

#turn into document term matrix (not tidy)
record_dtm <- record_noempty %>% 
  count(id, cleanword) %>% #turn into document, term, count structure (tidy)
  cast_dtm(id, cleanword, n)

# record_dtm

#create 6-topic LDA model
## (set a seed for repeatable model)

record_lda <- LDA(record_dtm, k = 6, control = list(seed = 42)) 
# record_lda

#put back into tidy format
record_topics <- tidy(record_lda, matrix = "beta") #beta: per-topic-per-word probability
# record_topics

#see top 10 terms for each topic
record_top_terms <- record_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

#visualise top terms per topic
record_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") + #wrap facets into rectangular layout, scale varies per topic
  scale_y_reordered()



###### selected terms over time ###################

#generate frequencies of selected words, put in df
word_time <- record_noempty %>% 
  select(c("year","cleanword")) %>% 
  filter(cleanword %in% c("optimal", "user","law","firms", "social", "sector")) %>%
  table() %>% 
  data.frame()

word_time$year %>% as.integer()

#generate plot
ggplot(word_time, aes(x=year, y=Freq, group=cleanword, colour=cleanword)) +
  geom_line(size=1)+
  labs(title='Word frequency 2016-2020')+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  annotate(geom="text", x="2019", y=750, 
           label="'sector' bucks trend")+
  annotate(geom="point", x="2019", y=270, size=10, shape=21, fill="transparent")
  

#### diff between full-text and no-text records

# Freq of full-text status over years
fullstatus_time <- record_cut %>% 
  select(c("year","fullTextStatus")) %>%
  table() %>% 
  data.frame()

fullstatus_time$year %>% as.integer()


#generate plot from freq table (turned df)
ggplot(fullstatus_time, 
       aes(x=year, y=Freq, group=fullTextStatus, colour=fullTextStatus)) +
  geom_line()+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+ #to remove decimal places
  labs(title='Full-text counts over time (2012-2021)', y="count")

# Distribution of publication types

##generate a df of publication types ($subjects) and their fullTextStatus frequencies
pub_freq <- record_cut %>% 
  select(c("subjects","fullTextStatus")) %>%
  table() %>% 
  data.frame()

##generate plot for publication types with counts >200
ggplot(subset(pub_freq, subjects=="Book Chapter"|
                subjects=="Case"|
                subjects=="Conference Paper"|
                subjects=="Conference Proceeding Article"|
                subjects=="Journal Article"|
                subjects=="Magazine Article"|
                subjects=="Working Paper"),
       aes(x=subjects, y=Freq, fill=subjects)) +
  geom_col()+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  labs(title='Distribution of publication types', y="count", x="publication type")+
  facet_grid(rows = vars(fullTextStatus))+
  theme(legend.position="none", axis.text.x=element_text(angle=40, hjust=1))


