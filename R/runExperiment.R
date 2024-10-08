#' Run an Experiment Based on the Configuration
#'
#' Executes the experiment and saves the results to an Excel file.
#'
#' @param gptConfig A list containing the configuration for the language model, including the system prompt,
#' model specifications, and token settings.
#' @param savePath The file path where the experiment results will be saved in Excel format.
#' Defaults to './output.xlsx' in the current working directory.
#'
#' @return This function does not return a value but saves the experiment results to the specified Excel file.
#' Upon completion, "Done." will be printed to the console.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' runExperiment(Experiment_config,"./output.xlsx")
#'
#' #The first argument Experiment_config is generated by preCheck() function.
#'
#' Experiment_config <- preCheck(data)
#' }
runExperiment <- function (gptConfig, savePath = "./output.xlsx") {
  switch(
    Sys.getenv("llm"),
    "openai" = run_LLMs(gptConfig, savePath),
    "baidubce" = run_LLMs(gptConfig, savePath),
    "llama-2" = run_LLMs(gptConfig, savePath),
    "llama-3" = run_LLMs(gptConfig, savePath),
    "claude" = run_claude(gptConfig, savePath),
    "gemini" = run_gemini(gptConfig, savePath),
    "baichuan" = run_baichuan(gptConfig, savePath),
    "aimlapi_test" = run_LLMs(gptConfig, savePath),
    "custom" = run_LLMs(gptConfig, savePath),
    stop("Failed to the interact with the LLM.")
  )
  
}



#' Internal Execution of the Experiment Scenarios
#'
#' @description
#' This internal function manages the execution of the experiment scenarios based on the gptConfig settings.
#' It iteratively processes the data for each run and trial, interacts with the llama model,
#' and appends the results in the designated Excel file. It is utilized within the 'runExperiment' function.
#' @importFrom utils setTxtProgressBar txtProgressBar write.csv modifyList
#' @import openxlsx
#' @param gptConfig A list containing the configuration for the llama model, including the system prompt,
#' model specifications, token settings, and experiment mode.
#' @param savePath The file path where the experiment results will be saved in Excel format.
#'
#' @return This function does not return a value but executes the experiment scenarios and
#' compiles the results in an Excel file. It prints "Done." to the console upon completion.
#'
#' @noRd
run_claude <- function(gptConfig, savePath) {
  data <- as.data.frame(gptConfig[1])
  version <- gptConfig$version
  systemPrompt <- gptConfig$systemPrompt
  model <- gptConfig$model
  args <- gptConfig$args
  n <- 1
  csvFlag = FALSE
  if (sub(".*\\.", "", savePath) == 'csv') {
    savePath <- paste0(sub("\\.[^.]*$", "", savePath), ".xlsx")
    csvFlag = TRUE
  }
  
  
  
  
  
  total_iterations <- nrow(data) * n
  current_progress <- 0
  progress_bar <- utils::txtProgressBar(min = 0, max = total_iterations, style = 3)
  utils::setTxtProgressBar(progress_bar, current_progress)
  
  
  
  
  data$Response <- NA
  data$N <- NA
  data$Trial <- NA
  data$Message <- NA
  data$rawResponse <- NA
  data$...... <- NA
  
  column_names <- names(data)
  new_order <- column_names[c(6, 1, 2, 3, 9, 4, 5, 7, 8, 10, 11, 12)]
  
  data <- data[, new_order]
  
  wb <- createWorkbook()
  addWorksheet(wb, "Sheet1")
  writeData(
    wb,
    sheet = 1,
    x = t(colnames(data)),
    startRow = 1,
    startCol = 1,
    colNames = FALSE
  )
  row_num <- 2
  
  for (s in unique(data$Session)) {
    s_data <- data[data$Session == s, ]
    # Run loop
    for (r in unique(data$Run)) {
      r_data <- s_data[s_data$Run == r, ]
      messages = list()
      
      message(r)
      
      for (it in unique(data$Item)) {
        it_data <- r_data[r_data$Item == it, ]
        # messages=list()
        
        # Trial loop
        for (i in seq_len(nrow(it_data))) {
          messages = addMessage(messages, "user", it_data$Prompt[i])
          t_data <- it_data[i, ]
          
          repeat {
            result <- tryCatch({
              #content_list = claude_chat(version=version,messages=messages, system = systemPrompt, model=model, temperature = temperature,max_tokens =  max_tokens, stop_sequences = stop_sequences)
              result_list  <- do.call("claude_chat", modifyList(
                list(
                  messages = messages,
                  model = model,
                  version = version,
                  system = systemPrompt
                ),
                args
              ))
              content_list <- result_list$content_list
              raw_temp <- result_list$raw_response
              
              TRUE
              
            }, error = function(e) {
              warning(paste("wariming:", e))
              FALSE
            })
            
            if (!isTRUE(result)) {
              Sys.sleep(6)
            } else {
              break
            }
          }
          
          
          if (length(content_list) == 1) {
            content_str <- content_list[[1]]
          } else {
            content_str <- paste(content_list, collapse = "\n")
          }
          
          
          # N responses loop
          for (nr in seq_along(content_list)){{
            t_data$Response <- content_list[nr]
            t_data$N <- nr
            
            if (n==1){
              messages=addMessage(messages,"assistant",content_str)
              
            }
            cMessage <- paste(messages, collapse = " ")
            
            
            # new_row <- c(t_data$Run, t_data$Item,t_data$Event, t_data$Condition,t_data$Prompt, t_data$Session,t_data$Response, t_data$N,i,cMessage)
            new_row <- c(t_data$Session, t_data$Run,t_data$Item, t_data$Event,i,t_data$Condition, t_data$Prompt,t_data$Response, t_data$N,cMessage)
            for (i in seq(1, nchar(raw_temp), by = 25000)) {
              new_row <- c(new_row, substring(raw_temp, i, min(i + 24999, nchar(raw_temp))))
            }
            writeData(wb, sheet = 1, x = t(new_row), startRow = row_num, startCol = 1,colNames = FALSE)
            row_num <- row_num + 1
            current_progress <- current_progress + 1
            setTxtProgressBar(progress_bar, current_progress)
            
          }
            saveWorkbook(wb, savePath, overwrite = TRUE)
          }
        }
      }
    }
  }
  
  if (csvFlag) {
    csvPath <- paste0(sub("\\.[^.]*$", "", savePath), ".csv")
    
    data_xlsx <- read.xlsx(savePath)
    
    utils::write.csv(data_xlsx, csvPath, row.names = FALSE)
    fileDeleted <- file.remove(savePath)
  }
  
  
  close(progress_bar)
  message("Done.")
}



#' Internal Execution of the Experiment Scenarios
#'
#' @description
#' This internal function manages the execution of the experiment scenarios based on the gptConfig settings.
#' It iteratively processes the data for each run and trial, interacts with the llama model,
#' and appends the results in the designated Excel file. It is utilized within the 'runExperiment' function.
#' @importFrom utils setTxtProgressBar txtProgressBar write.csv modifyList
#' @import openxlsx
#' @param gptConfig A list containing the configuration for the llama model, including the system prompt,
#' model specifications, token settings, and experiment mode.
#' @param savePath The file path where the experiment results will be saved in Excel format.
#'
#' @return This function does not return a value but executes the experiment scenarios and
#' compiles the results in an Excel file. It prints "Done." to the console upon completion.
#'
#' @noRd
run_gemini <- function(gptConfig, savePath) {
  data <- as.data.frame(gptConfig[1])
  model <- Sys.getenv("model")
  args <- gptConfig$args
  n <- 1
  csvFlag = FALSE
  if (sub(".*\\.", "", savePath) == 'csv') {
    savePath <- paste0(sub("\\.[^.]*$", "", savePath), ".xlsx")
    csvFlag = TRUE
  }
  
  
  
  
  
  total_iterations <- nrow(data) * n
  current_progress <- 0
  progress_bar <- utils::txtProgressBar(min = 0, max = total_iterations, style = 3)
  utils::setTxtProgressBar(progress_bar, current_progress)
  
  
  
  
  data$Response <- NA
  data$N <- NA
  data$Trial <- NA
  data$Message <- NA
  data$rawResponse <- NA
  data$...... <- NA
  
  column_names <- names(data)
  new_order <- column_names[c(6, 1, 2, 3, 9, 4, 5, 7, 8, 10, 11, 12)]
  
  data <- data[, new_order]
  
  
  wb <- createWorkbook()
  addWorksheet(wb, "Sheet1")
  writeData(
    wb,
    sheet = 1,
    x = t(colnames(data)),
    startRow = 1,
    startCol = 1,
    colNames = FALSE
  )
  row_num <- 2
  
  for (s in unique(data$Session)) {
    s_data <- data[data$Session == s, ]
    # Run loop
    for (r in unique(data$Run)) {
      r_data <- s_data[s_data$Run == r, ]
      messages = list()
      
      message(r)
      # Trial loop
      for (it in unique(data$Item)) {
        it_data <- r_data[r_data$Item == it, ]
        # messages=list()
        
        for (i in seq_len(nrow(it_data))) {
          messages = addMessage(messages, "user", it_data$Prompt[i])
          t_data <- it_data[i, ]
          
          repeat {
            result <- tryCatch({
              #content_list = gemini_chat(messages=messages,model=model, temperature = temperature,max_tokens =  max_tokens, stop_sequences = stop_sequences,topK=topK,topP=topP)
              result_list  <- do.call("gemini_chat", modifyList(list(
                messages = messages, model = model
              ), args))
              content_list <- result_list$content_list
              raw_temp <- result_list$raw_response
              TRUE
              
            }, error = function(e) {
              warning(paste("wariming:", e))
              FALSE
            })
            
            if (!isTRUE(result)) {
              Sys.sleep(6)
            } else {
              break
            }
          }
          
          
          if (length(content_list) == 1) {
            content_str <- content_list[[1]]
          } else {
            content_str <- paste(content_list, collapse = "\n")
          }
          
          
          # N responses loop
          for (nr in seq_along(content_list)){{
            t_data$Response <- content_list[nr]
            t_data$N <- nr
            
            if (n==1){
              messages=addMessage(messages,"model",content_str)
              
            }
            cMessage <- paste(messages, collapse = " ")
            
            # new_row <- c(t_data$Run, t_data$Item,t_data$Event, t_data$Condition,t_data$Prompt, t_data$Session,t_data$Response, t_data$N,i,cMessage)
            new_row <- c(t_data$Session, t_data$Run,t_data$Item, t_data$Event,i,t_data$Condition, t_data$Prompt,t_data$Response, t_data$N,cMessage)
            for (i in seq(1, nchar(raw_temp), by = 25000)) {
              new_row <- c(new_row, substring(raw_temp, i, min(i + 24999, nchar(raw_temp))))
            }
            writeData(wb, sheet = 1, x = t(new_row), startRow = row_num, startCol = 1,colNames = FALSE)
            row_num <- row_num + 1
            current_progress <- current_progress + 1
            setTxtProgressBar(progress_bar, current_progress)
            
          }
            saveWorkbook(wb, savePath, overwrite = TRUE)
          }
        }
      }
    }
  }
  if (csvFlag) {
    csvPath <- paste0(sub("\\.[^.]*$", "", savePath), ".csv")
    
    data_xlsx <- read.xlsx(savePath)
    
    utils::write.csv(data_xlsx, csvPath, row.names = FALSE)
    fileDeleted <- file.remove(savePath)
  }
  
  
  close(progress_bar)
  message("Done.")
}


#' Internal Execution of the Experiment Scenarios
#'
#' @description
#' This internal function manages the execution of the experiment scenarios based on the gptConfig settings.
#' It iteratively processes the data for each run and trial, interacts with the llama model,
#' and appends the results in the designated Excel file. It is utilized within the 'runExperiment' function.
#' @importFrom utils setTxtProgressBar txtProgressBar write.csv modifyList
#' @import openxlsx
#' @param gptConfig A list containing the configuration for the llama model, including the system prompt,
#' model specifications, token settings, and experiment mode.
#' @param savePath The file path where the experiment results will be saved in Excel format.
#'
#' @return This function does not return a value but executes the experiment scenarios and
#' compiles the results in an Excel file. It prints "Done." to the console upon completion.
#'
#' @noRd
run_baichuan <- function(gptConfig, savePath) {
  data <- as.data.frame(gptConfig[1])
  model <- Sys.getenv("model")
  args <- gptConfig$args
  n <- args[["n"]]
  if (is.null(n)) {
    n <- 1
  }
  csvFlag = FALSE
  if (sub(".*\\.", "", savePath) == 'csv') {
    savePath <- paste0(sub("\\.[^.]*$", "", savePath), ".xlsx")
    csvFlag = TRUE
  }
  
  
  
  
  
  total_iterations <- nrow(data) * n
  current_progress <- 0
  progress_bar <- utils::txtProgressBar(min = 0, max = total_iterations, style = 3)
  utils::setTxtProgressBar(progress_bar, current_progress)
  
  
  
  
  data$Response <- NA
  
  data$N <- NA
  data$Trial <- NA
  data$Message <- NA
  data$rawResponse <- NA
  data$...... <- NA
  
  column_names <- names(data)
  new_order <- column_names[c(6, 1, 2, 3, 9, 4, 5, 7, 8, 10, 11, 12)]
  
  data <- data[, new_order]
  
  wb <- createWorkbook()
  addWorksheet(wb, "Sheet1")
  writeData(
    wb,
    sheet = 1,
    x = t(colnames(data)),
    startRow = 1,
    startCol = 1,
    colNames = FALSE
  )
  row_num <- 2
  
  for (s in unique(data$Session)) {
    s_data <- data[data$Session == s, ]
    # Run loop
    for (r in unique(data$Run)) {
      r_data <- s_data[s_data$Run == r, ]
      messages = list()
      message(r)
      
      for (it in unique(data$Item)) {
        it_data <- r_data[r_data$Item == it, ]
        # messages=list()
        
        # Trial loop
        for (i in seq_len(nrow(it_data))) {
          messages = addMessage(messages, "user", it_data$Prompt[i])
          t_data <- it_data[i, ]
          
          repeat {
            result <- tryCatch({
              #content_list = baichuan_chat(messages = messages , model = model, temperature = temperature, max_tokens = max_tokens, top_k = top_k ,top_p = top_p)
              result_list  <- do.call("baichuan_chat", modifyList(list(
                messages = messages, model = model
              ), args))
              content_list <- result_list$content_list
              raw_temp <- result_list$raw_response
              
              TRUE
              
            }, error = function(e) {
              warning(paste("wariming:", e))
              FALSE
            })
            
            if (!isTRUE(result)) {
              Sys.sleep(6)
            } else {
              break
            }
          }
          
          
          if (length(content_list) == 1) {
            content_str <- content_list
          } else {
            content_str <- paste(content_list, collapse = "\n")
          }
          
          
          # N responses loop
          for (nr in seq_along(content_list)){{
            t_data$Response <- content_list[nr]
            t_data$N <- nr
            
            if (n==1){
              messages=addMessage(messages,"assistant",content_list[nr])
              
            }
            cMessage <- paste(messages, collapse = " ")
            
            # new_row <- c(t_data$Run, t_data$Item,t_data$Event, t_data$Condition,t_data$Prompt, t_data$Session,t_data$Response, t_data$N,i,cMessage)
            new_row <- c(t_data$Session, t_data$Run,t_data$Item, t_data$Event,i,t_data$Condition, t_data$Prompt,t_data$Response, t_data$N,cMessage)
            for (i in seq(1, nchar(raw_temp), by = 25000)) {
              new_row <- c(new_row, substring(raw_temp, i, min(i + 24999, nchar(raw_temp))))
            }
            writeData(wb, sheet = 1, x = t(new_row), startRow = row_num, startCol = 1,colNames = FALSE)
            row_num <- row_num + 1
            current_progress <- current_progress + 1
            setTxtProgressBar(progress_bar, current_progress)
            
          }
            saveWorkbook(wb, savePath, overwrite = TRUE)
          }
        }
      }
    }
  }
  if (csvFlag) {
    csvPath <- paste0(sub("\\.[^.]*$", "", savePath), ".csv")
    
    data_xlsx <- read.xlsx(savePath)
    
    utils::write.csv(data_xlsx, csvPath, row.names = FALSE)
    fileDeleted <- file.remove(savePath)
  }
  
  
  close(progress_bar)
  message("Done.")
}

#' Internal Execution of the Experiment Scenarios
#'
#' @description
#' This internal function manages the execution of the experiment scenarios based on the gptConfig settings.
#' It iteratively processes the data for each run and trial, interacts with the GPT model,
#' and appends the results in the designated Excel file. It is utilized within the 'runExperiment' function.
#' @importFrom utils setTxtProgressBar txtProgressBar write.csv modifyList
#' @import openxlsx
#' @param gptConfig A list containing the configuration for the GPT model, including the system prompt,
#' model specifications, token settings, and experiment mode.
#' @param savePath The file path where the experiment results will be saved in Excel format.
#' @param log A logical value indicating whether to log the experiment results. Defaults to FALSE.
#'
#' @return This function does not return a value but executes the experiment scenarios and
#' compiles the results in an Excel file. It prints "Done." to the console upon completion.
#'
#' @noRd
run_LLMs <- function(gptConfig, savePath, log = FALSE) {
  initializeData <- function(data) {
    data$Response <- NA
    data$N <- NA
    data$Trial <- NA
    data$Message <- NA
    data$rawResponse <- NA
    data$...... <- NA
    data <- data[, c(6, 1, 2, 3, 9, 4, 5, 7, 8, 10, 11, 12)]
    return(data)
  }
  url <- Sys.getenv("url")
  # Completion
  if (grepl("/chat/", Sys.getenv("url"))) {
    Completion_mode = FALSE
  } else {
    Completion_mode = TRUE
  }
  
  if (log) {
    log_file_name <- paste0("request_log_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".txt")
    Sys.setenv(LOG_FILE = log_file_name)
  }
  
  # Create work book and sheet
  createWorkbookAndSheet <- function(data) {
    wb <- createWorkbook()
    addWorksheet(wb, "Sheet1")
    writeData(
      wb,
      sheet = 1,
      x = t(colnames(data)),
      startRow = 1,
      startCol = 1,
      colNames = FALSE
    )
    return(list(wb = wb, row_num = 2))
  }
  
  # Progress bar
  updateProgressBar <- function(progress_bar,
                                current_progress,
                                total_iterations) {
    current_progress <- current_progress + 1
    setTxtProgressBar(progress_bar, current_progress)
    return(current_progress)
  }
  
  
  
  # Send request to models
  callModel <- function(messages, model, args) {
    # Request method list
    ## ToDo: add more models
    model_request <- list(
      openai = list(chat = "openai_chat", completion = "openai_completion"),
      "llama-3" = list(chat = "llama_chat", completion = "llama_chat"),
      "llama-2" = list(chat = "llama_chat", completion = "llama_chat"),
      baidubce = list(chat = "wenxin_chat", completion = "wenxin_chat")
      # example_model = list(chat = "example_chat_function", completion = "example_completion_function")
    )
    chat_request <- model_request[[Sys.getenv("llm")]]$chat
    completion_request <- model_request[[Sys.getenv("llm")]]$completion
    repeat {
      result <- tryCatch({
        if (Completion_mode != TRUE) {
          # message("run_LLMs_model: ",model)
          # message("run_LLMs_chat_request: ",chat_request)
          result_list <- do.call(chat_request, modifyList(list(
            messages = messages, model = model
          ), args))
        } else {
          message("callModel: ",messages)
          result_list <- do.call(completion_request, modifyList(list(
            prompt = messages, model = model
          ), args))
        }
        # message(result_list$content_list)
        # message(result_list$raw_response)
        list(
          content_list = result_list$content_list,
          raw_response = result_list$raw_response,
          success = TRUE
        )
      }, error = function(e) {
        warning(paste("warning:", e))
        list(success = FALSE)
      })
      
      if (result$success) {
        return(result)
      } else {
        Sys.sleep(6)
      }
    }
  }
  
  #
  ## ToDo: Add more models' response format
  handle_response <- function(response) {
    if (Sys.getenv("llm") == "llama-3") {
      return(substring(response, 12))
    } else {
      return(response)
    }
  }
  
  saveResults <- function(wb, savePath, csvFlag) {
    saveWorkbook(wb, savePath, overwrite = TRUE)
    if (csvFlag) {
      csvPath <- paste0(sub("\\.[^.]*$", "", savePath), ".csv")
      data_xlsx <- read.xlsx(savePath)
      utils::write.csv(data_xlsx, csvPath, row.names = FALSE)
      file.remove(savePath)
    }
  }
  
  data <- as.data.frame(gptConfig[1])
  systemPrompt <- gptConfig$systemPrompt
  model <- Sys.getenv("model")
  imgDetail <- gptConfig$imgDetail
  args <- gptConfig$args
  n <- ifelse(is.null(args[["n"]]), 1, args[["n"]])
  
  csvFlag <- FALSE
  if (sub(".*\\.", "", savePath) == 'csv') {
    savePath <- paste0(sub("\\.[^.]*$", "", savePath), ".xlsx")
    csvFlag <- TRUE
  }
  
        
  if (grepl("^sk", Sys.getenv("key"))&&Completion_mode == FALSE) {
      total_iterations <- nrow(data) * n
    } else {
      total_iterations <- nrow(data)
    }

  current_progress <- 0
  progress_bar <- utils::txtProgressBar(min = 0, max = total_iterations, style = 3)
  utils::setTxtProgressBar(progress_bar, current_progress)
  
  data <- initializeData(data)
  wb_info <- createWorkbookAndSheet(data)
  wb <- wb_info$wb
  row_num <- wb_info$row_num
  
  # Role name
  ## ToDo: Add more models' roles
  model_roles <- list(
    openai = list(
      user = "user",
      system = "system",
      assistant = "assistant"
    ),
    baidubce= list(
      user = "user",
      system = "system",
      assistant = "assistant"
    ),
    "llama-2" = list(
      user = "user",
      system = "system",
      assistant = "assistant"
    ),
    "llama-3" = list(
      user = "user",
      system = "system",
      assistant = "assistant"
    )
    # example_model = list(chat = "example_chat_function", completion = "example_completion_function")
  )
  user <- model_roles[[Sys.getenv("llm")]]$user
  system <- model_roles[[Sys.getenv("llm")]]$system
  assistant <- model_roles[[Sys.getenv("llm")]]$assistant
  
  # Beginning message
  ## ToDo: Add more models' beginning message

  Beginning_messages_list <- list(openai = list(),
                                  "llama-2" = "<s>[INST] ",
                                  "llama-3" = "<|begin_of_text|>")
  if (Completion_mode == T) {
    Beginning_messages <- Beginning_messages_list[[Sys.getenv("llm")]]
  } else {
    Beginning_messages <-list()
  }
  
  for (s in unique(data$Session)) {
    s_data <- data[data$Session == s, ]
    for (r in unique(data$Run)) {
      r_data <- s_data[s_data$Run == r, ]
      # Beginning message
      #message(2222)
      messages <- Beginning_messages
      if (systemPrompt != "" && Completion_mode != TRUE) {
        messages <- addMessage(messages, system, systemPrompt)
      }
      # message(r)
      for (it in unique(data$Item)) {
        it_data <- r_data[r_data$Item == it, ]
        for (i in seq_len(nrow(it_data))) {
          
          if (Completion_mode != TRUE) {
            messages <- addMessage(messages, user, it_data$Prompt[i], imgDetail)
          }else {
            messages <- it_data$Prompt[i]
          }
          t_data <- it_data[i, ]
          # message("run_LLMs_before_callmodel: ",messages)
          result <- callModel(messages, model, args)
          # message(result)
          content_list <- result$content_list
          raw_temp <- result$raw_response
          message("run_LLMs_content_list: ",content_list)
          
          for (nr in seq_along(content_list)) {
            t_data$Response <- handle_response(content_list[nr])
            # message("run_LLMs_Response: ",t_data$Response)
            t_data$N <- nr
            if (n == 1 && Completion_mode != TRUE) {
              messages <- addMessage(messages, assistant, content_list[nr])
            }
            cMessage <- paste(messages, collapse = " ")
            new_row <- c(
              t_data$Session,
              t_data$Run,
              t_data$Item,
              t_data$Event,
              i,
              t_data$Condition,
              t_data$Prompt,
              t_data$Response,
              t_data$N,
              cMessage
            )
            for (j in seq(1, nchar(raw_temp), by = 25000)) {
              new_row <- c(new_row, substring(raw_temp, j, min(j + 24999, nchar(
                raw_temp
              ))))
            }
            writeData(
              wb,
              sheet = 1,
              x = t(new_row),
              startRow = row_num,
              startCol = 1,
              colNames = FALSE
            )
            row_num <- row_num + 1
            current_progress <- updateProgressBar(progress_bar, current_progress, total_iterations)
          }
          saveWorkbook(wb, savePath, overwrite = TRUE)
        }
      }
    }
  }
  saveResults(wb, savePath, csvFlag)
  close(progress_bar)
  message("Done.")
  closeAllConnections()
}