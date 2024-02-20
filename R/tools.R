#' magicTokenizer
#'
#' @description
#' This function provides the number of tokens in a specified text, acting as a wrapper for an internal tokenizer function.
#'
#' @param text A character string: the text for which the number of tokens is required.
#'
#' @return Returns the number of tokens in the provided text.
#'
#' @export
  magicTokenizer <- function(text) {
  return(tiktokenTokenizer(text))
}

#' Internal Token Counter
#'
#' @description
#' This internal function interacts with an API to determine the number of tokens in a specified text.
#' This is an essential part of managing interactions with the GPT-3 API to ensure requests remain within token limits.
#' @import httr
#' @importFrom httr POST
#' @param text A character string: the text for which the number of tokens is required.
#'
#' @return Returns the number of tokens in the provided text or stops with an error message if the API call fails.
#'
#' @noRd
  tiktokenTokenizer <- function(textList){
    response <- POST("http://chat.cuhklpl.com/tokenList", body = list(textList = textList), encode = "json")

    if (status_code(response) == 200) {

      return(content(response)$tokenList)
    } else {
      warning("There is a mistake for token check. Please make sure that the max token number does not exceed the model's limitation.")
    }
  }



#######################################Tokenizer#############################################





#######################################addContent#############################################
addContent <- function(content, modality, imgDetail) {
  switch(modality,
         "base" = content,

         "img" = {
           # a="question1$$imgUrl1$$question2$$imgUrl2$$question2$$imgUrl3"

           pattern <- "\\$\\$[^\\$]+\\$\\$|[^\\$]+"
           elements <- unlist(regmatches(content, gregexpr(pattern, content)))
           content_str=list()

           # 遍历并处理每个元素
           for (element in elements) {
             if (grepl("^\\$\\$", element)) {
               # 去除 $$ 并处理图片
               imgUrl <- gsub("\\$\\$", "", element)
               content_str=append(content_str, list(list(type = "image_url", image_url = list(url = imgUrl, detail = imgDetail))))
             } else {
               # 处理文本
               content_str=append(content_str, list(list(type = "text", text = element)))
             }
           }

           content_str
         },


         "audio" = {
           message("audio1")
           content
         },
         stop("Modality input error")
  )
}

#######################################addContent#############################################











#######################################addMessage############################################
#' Internal Message Adder
#'
#' @description
#' This internal function is used to append a new message (composed of role and content) to an existing list of messages.
#' This is used internally to manage dialog flows within the experimentation platform.
#'
#' @param messages A list of existing messages, each as a list containing role and content.
#' @param role A character string indicating the role for the new message.
#' @param content A character string containing the actual text of the new message.
#'
#' @return Returns the updated list of messages after adding the new message.
#'
#' @noRd
addMessage <-function (messages,role="user",content="",modality='base',imgDetail="low"){

  if(Sys.getenv("model")=="llama"){
    new_message <- switch(role,
                          "system" = paste0("<s>[INST] <<SYS>>\n", content, "\n<</SYS>>\n"),
                          "user" = paste0(content, " [/INST] "),
                          "assistant" = paste0(content, " </s><s>[INST] "),
                          ""
    )
    return(paste0(messages, new_message))
  }

  else{
      messages=append(messages,
                      list(
                        list(
                          role = role,
                          content = addContent(content,modality,imgDetail)
                        )
                      )
      )

    return(messages)
  }
}

#######################################addMessage############################################


#######################################setkey############################################
#' Step1: Set model's API key and url.
#'
#' @description
#' This function allows users to set and verify an API key for data collection. You can change the default api_url for open-source models' API.
#'
#' todo
#' @param api_key A character string: the user's OpenAI/huggingface/other API key.Please fill 'NA' for self-deployed models.
#' @param api_url A character string: the user's OpenAI/huggingface/other url .default is OpenAI.
#' @param model  A character string: specify the model version.
#' @return Prints a message to the console indicating whether the API key setup was successful.
#' If the setup fails, the function stops with an error message.
#'
#' @examples
#' \dontrun{
#' set_key(api_key="YOUR_API_KEY", api_url="api.openai.com/v1/chat/completions",model="gpt-3.5-turbo")
#' }
#' @export
setKey <- function(api_key,api_url="https://api.openai.com/v1/chat/completions",model){
  if (!is.null(api_key) && is.character(api_key)) {

    Sys.setenv(key=api_key)
    Sys.setenv(url=api_url)
    Sys.setenv(model=model)

   if(grepl("openai", api_url)){
     if(grepl("audio", api_url)){

       tempUrl=api_url
       tempModel=model


       Sys.setenv(url="https://api.openai.com/v1/chat/completions")
       Sys.setenv(model="gpt-3.5-turbo")

       message(openai_chat(list(
         list(
           role = "user",
           content = "this is a test,please say 'Setup api_key successful!'"
         )
       ),max_tokens = 10,temperature = 0.1,model="gpt-3.5-turbo"))
       message(paste("your api_key:",Sys.getenv("key")))

       Sys.sleep(2)

       Sys.setenv(url=tempUrl)
       Sys.setenv(model=tempModel)


     }else{
       message(openai_chat(list(
          list(
            role = "user",
            content = "this is a test,please say 'Setup api_key successful!'"
          )
        ),max_tokens = 10,temperature = 0.1,model=Sys.getenv("model")))
       message(paste("your api_key:",Sys.getenv("key")))
      }
    }
    else if(grepl("llama", api_url)){
      message(llama_chat("<s>[INST] <<SYS>>
You are a helpful AI, please answer according to my requirements, do not output other irrelevant content
<</SYS>>

please say 'Setup api_key successful!' [/INST]",max_tokens = 30,temperature = 0.1))
      message(paste("your api_key:",Sys.getenv("key")))

      Sys.setenv(model = "llama")
    }
    else if(grepl("NA", api_key)) {
      message(openai_chat(list(
        list(
          role = "user",
          content = "this is a test,please say 'Model test successful!'"
        )
      ),max_tokens = 10,temperature = 0.1,model=Sys.getenv("model")))
    }

  } else {
    stop("Failed to the interact with the LLM.")
  }
}

#######################################setkey############################################
