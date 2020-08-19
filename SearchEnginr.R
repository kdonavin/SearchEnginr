if (!require("pacman")) install.packages("pacman")
p_load(dplyr, pbapply, R6, tidytext)

SearchEnginr <- R6::R6Class("SearchEnginr", list(
  #Class variables
    doc_list = NULL
    , doc_stems = NULL
    , invert_index = list()
  #Utility functions
    , import_doc_list = function(file){
      doc_file <- file(file, 'r')
      self$doc_list <- readLines(doc_file) %>%
        stringr::str_split(pattern = '\n')
      close(doc_file) #Cleanup
      invisible(self)
    }, doc_count = function(){
      return(length(self$doc_list))
    }, save = function(path){
      save(self, file = path)
    }, save_preproc_stems = function(path){ #Broken, just saves the character "ds"
      doc_stems <- self$doc_stems  #name must match with load function
      save(doc_stems, file = path)
      invisible(doc_stems)
    }, load_preproc_stems = function(path){
      tmp_env <- new.env()
      load(file = path, envir = tmp_env)
      self$doc_stems <- tmp_env[['doc_stems']] #name must match with save function
    }, save_index = function(path){
      index <- self$invert_index #name must match with load function
      save(index, file = path)
    }, load_index = function(path){
      tmp_env <- new.env() 
      load(file = path, envir = tmp_env) 
      self$invert_index <- tmp_env[['index']] #name must match with save function
    },
  #Text Processing Functions
    sanitize_text = function(text){
        text <- stringr::str_to_lower(text)
        text <- stringr::str_replace_all(text, pattern = '[\\-/]', replacement = ' ') #Replace hyphens
        text <- stringr::str_remove_all(text, pattern = '[^A-Za-z\\h]') #Remove all non-characters and spaces
        text <- stringr::str_replace_all(text, pattern = '\\h{2,}', replacement = ' ') #Condense white space
        return(text)
    }, tokenizer = function(text){
      words <- stringr::str_split(text, pattern = ' ')[[1]]
      words <- words[words %nin% c("", tidytext::stop_words$word)] ##stop-word removal
      return(words)
    }, stemmer = function(token_array){ ##Need to suppress print out
      stemmed_array <- tm::stemDocument(token_array)
      stemmed_df <- data.frame(stem = stemmed_array) %>%
        group_by(stem) %>%
        summarise(
          cnt = n()
          , .groups = "drop"
        )
      return(stemmed_df)
  #Document Processing Functions
    }, pre_process_docs = function(doc_list = self$doc_list){
      message("Sanitizing text...")
      self$doc_stems <- pbapply::pblapply(self$doc_list, FUN = self$sanitize_text)
      message("Tokenizing...")
      self$doc_stems <- pbapply::pblapply(self$doc_stems, FUN = self$tokenizer)
      message("Stemming...")
      self$doc_stems <- pbapply::pblapply(self$doc_stems, FUN = self$stemmer)
      invisible(self$doc_stems)},
    #' @details A function that indexes every document ID
    #' that contains a given stem in the collection of documents,
    #' as well as the count of those terms per document.
    #' @param doc_stems - list of of tibbles that contain all stems per doc along
    #' with a count. Fields: "stem", "cnt"
    index_docs = function(){
      
      #Progress Bar
      doc_count <- self$doc_count() #For time estimation
      message("Indexing...")
      pb <- progress_bar$new(format = "[:bar] :current/:total (:percent) - ETA: :eta"
                             , total = doc_count)
      
      # Processing Stems - For each document, for each stem:
      for(i in 1:doc_count){
        stem_list <- self$doc_stems[[i]]
        for(j in 1:length(stem_list$stem)){
          stem <- stem_list[j, 'stem'] %>% as.character #Forces tibble to vector
          
          # Row contains Doc ID + Count for a given stem
          index_row <- data.frame(id = i, cnt = self$doc_stems[[i]][j,'cnt']) #Perhaps more elegant to draw "cnt" from the saved "stem_list"
          
          #' If the stem is not already in the index, save the row to the
          #' indexed stem. Else, append the row to the rows already saved.
          if(self$invert_index[[stem]] %>% is.null){
            self$invert_index[[stem]] <- index_row
          } else {
            self$invert_index[[stem]] <- union(self$invert_index[[stem]], index_row)
          }
        }
        pb$tick() #Progress bar tick
      }
      invisible(self$invert_index)
    }, 
  #Query Functions
  retrieve_doc_ids = function(query){
    
    query__processed <- (query %>% self$sanitize_text() %>% self$tokenizer() %>% self$stemmer())$stem
    message("processed query")
    return_index <- self$invert_index[query__processed] 
    ids <- NULL
    for(stem in names(return_index)){
      new_ids <- return_index[[stem]]$id
      ids <- c(ids, new_ids) 
      ids %<>% unique()
    }
    return(ids)
  }, retrieve_docs = function(query){
     ids <- self$retrieve_doc_ids(query)
     return(self$doc_list[ids])
  }
  )
)

