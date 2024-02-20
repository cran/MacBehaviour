#' Step2: Load and format data
#'
#' Prepares the stimuli data for the experiment.
#'
#' @param runList A numeric vector of data representing the 'Run' column in the experiment.
#' @param itemIDList A numeric vector of data representing the 'itemID' column in the experiment.
#' @param conditionList A numeric/character vector of data representing the 'Condition' column in the experiment.
#' @param promptList A character vector of the main prompt (usually experiment items).
#' @param header A logical value indicating if the output data.frame should include column headers (default is TRUE).
#' @return A data frame with the processed columns 'Run', 'Trial', 'Item', 'Condition', and 'Prompt', ready for use in experiments.
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
#' ExperimentItem=loadData(df$Run,df$Item,df$Condition,promptList = df$TargetPrompt)
#'
loadData <- function(runList,itemIDList,conditionList,promptList,header=TRUE){


  runs <- as.numeric(runList)
  items <- as.numeric(itemIDList)
  conditions <- conditionList
  prompts <- promptList

  # parameter test
  vectors <- list(runs=runs,  items=items, conditions=conditions , prompts=prompts)
  for (name in names(vectors)) {
    if (ncol(as.matrix(vectors[[name]])) != 1) {
      stop(paste("The size of second dimension in ", name, " is not one (expected a single column)"))
    }
  }

  lengths <- sapply(vectors, length)
  if (length(unique(lengths)) > 1) {
    stop("Inconsistent number of rows between run, list, item and condition.")
  }




  data <- data.frame(
    Run = runs,
    ItemID = items,
    Condition = conditions,
    Prompt = prompts
  )
  return(data)
}

