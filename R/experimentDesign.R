#' Step3: Generate the experimental design matrix.
#'
#' Defines the experiment setup based on the stimuli loaded.
#'
#' @param data A data frame that has been processed through the 'loadData' function, containing the experimental items and their attributes.
#' @param Step An integer indicating how many sessions (the whole set of trials) should be run. Default is 1, meaning no repetition.
#' @param random A logical indicating whether the trials should be randomized. Default is FALSE, meaning trials will occur in the order provided.
#' @return A data.frame with the designed structure for the experiment, including any repetitions and randomizations as specified. Each row corresponds to a single trial or instance in the experiment.
#' @export
#'
#' @examples
#'
#' df <- data.frame(
#' Run = c(1,2),
#' Item = c(1,2),
#' Condition = c(1,2),
#' TargetPrompt = c("1","2")
#' )
#'
#' ExperimentItem=loadData(df$Run,df$Item,df$Condition,promptList = df$TargetPrompt)
#'
#' Design=experimentDesign(ExperimentItem,Step=1,random = TRUE)
#'

experimentDesign <-function(data,Step=1,random=FALSE){


  new_data <- matrix(vector(), 0, 0)
  data_random<- matrix(vector(), 0, 0)

  data$Session <- 1
  sindex_num = 1
  if(random){


    for(i in 1:Step){
      for (r in unique(data$Run)) {

        r_data <- data[data$Run == r, ]

        data_random <- r_data[sample(nrow(r_data)),]
        data_random$Session = i
        new_data <- rbind(new_data, data_random)

      }

    }


    return(new_data)
  }

  if(!random){
    new_data <- data
    for(i in seq_len(Step)){
      temp_data <- data
      temp_data$Session = i
      if (i>1){
        new_data <- rbind(new_data,temp_data)
      }
    }

    return(new_data)
  }

}







