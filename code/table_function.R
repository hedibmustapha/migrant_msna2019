library(questionr)
library(tidyverse)

#' Create table for question
#' 
#' @param question character of name of vectr in data.frame
#' @param in_questionnaire logical vector indicating if question is in questionnaire or not
#' @param data data frame
#' @param weighting_function function applied to dataset to generate weights
#' @param name name for overall data in the area
#' @param ... additional stratification variables to be applied
analyzer <- function(x, in_questionnaire, data, weighting_function = NULL, main_col_name = "Libya", ...) {
  #print(x)
  cat(blue(paste0("----",round(match(x, colnames(data)) / ncol(data) *100,2),"%\n\n")))
  strata <- list(...)
  strata <- strata[!is.na(strata)]
  data <- filter(data, !is.na(!!sym(x)))
  if (is.null(weighting_function)) {
    weights <- rep(1, nrow(data))
  } else {
    weights <- weighting_function(data)
  }
  
  x_data <- data[[x]]
  
  if (class(x_data) %in% c("logical", "numeric", "integer")) {
    if (in_questionnaire) {
      avg <- wtd.mean(x_data, weights = weights)
    } else if (min(x_data) >= 0 & max(x_data) <= 1) {
      avg <- 100 * round(wtd.mean(x_data, weights = weights), 2)
    } else {
      avg <- wtd.mean(x_data, weights = weights)
    }
    table <- tibble("data" = c(x, "Average"), !!main_col_name := c(main_col_name, avg))
  } else {
    table <- wtd.table(x_data, rep(1, length(x_data)), weights = weights)
    if(is_empty(table)) {
      table <- tibble("data" = "", !!main_col_name := "")
    } else {
      table <- prop(table)
      table <- as.data.frame.matrix(table)
      table[,1] <- row.names(table)
      table <- table[-nrow(table),]
      table <- table[order(as.numeric(table[,ncol(table)]),
                           decreasing = T), c(1, ncol(table))] %>%
        mutate(Total = round(Total, 1))
      table <- rbind(c(x, main_col_name), table)
      table <- as.data.frame(table)
      names(table) <- c("data", main_col_name)
    }
  } 
  if (length(strata) > 0) {
    for (i in 1:length(strata)) {
      groups <- unique(unlist(data[strata[[i]]]))
      groups <- groups[!is.na(groups)]
      for (j in 1:length(groups)) {
        new_data <- filter(data, !!sym(strata[[i]]) == groups[j])
        new_table <- analyzer(x, in_questionnaire, new_data, weighting_function, groups[j], NA)
        table <- left_join_NA(table, new_table, by = "data")
      }
    }
  }
  return(table)
}

left_join_NA <- function(x, y, ...) {
  left_join(x = x, y = y, by = ...) %>% 
    mutate_each(list(~replace(., which(is.na(.)), 0)))
}

table_maker <- function(data, questionnaire, choices, weighting_function = NULL, labels = T, language = NULL, 
                        main_col_name = "Libya", ...) {
  # collecting strata information
  strata <- list(...)
  strata <- strata[!is.na(strata)]
  strata <- unlist(strata)
  
  # remove the text column for select multiple
  sel_mul <- filter(questionnaire, str_detect(type, "select_multiple"))$name
  data <- select(data, -one_of(sel_mul))
  
  # getting analysis names, don't analyze strata
  if (!is.null(strata)) {
    analysis_names <- names(select(data, -one_of(strata)))
  } else {
    analysis_names <- names(select(data))
  }
  
  # detect which are in the questionnaire
  in_questionnaire <- analysis_names %in% questionnaire$name
  
  # get initial table output
  
  if (!is.null(strata)) {
    table_output <- bind_rows(map2(analysis_names, 
                                   in_questionnaire, 
                                   analyzer, 
                                   data, 
                                   weighting_function,
                                   main_col_name,
                                   strata))
  } else {
    table_output <- bind_rows(map2(analysis_names, 
                                   in_questionnaire, 
                                   analyzer, 
                                   data, 
                                   weighting_function,
                                   main_col_name))
  }
  
  # Editing select multiple binary options
  # Removes extra rows so that we end up with # of rows for # of options
  
  sel_mul_rgx <- paste0(sel_mul, "(\\/|\\.)")
  sel_mul_extract_rgx <- paste0("(", str_c(sel_mul, collapse = "|"), ")")
  sel_mul_remove_rgx <- paste0("(", str_c(sel_mul_rgx, collapse = "|"), ")")

  table_output <- table_output %>%
    mutate(sel_mul = str_extract(data, sel_mul_extract_rgx))
  avg_indices <- which(table_output[, 1] == "Average")
  avg_indices <- avg_indices[str_detect(table_output[avg_indices - 1, 1], sel_mul_remove_rgx)]
  
  table_output[avg_indices, 1] <- table_output[avg_indices - 1, 1]
  match_previous <- (table_output[avg_indices - 3, "sel_mul"] == table_output[avg_indices - 1, "sel_mul"])
  match_previous[is.na(match_previous)] <- FALSE
  rem_avg_indices <- avg_indices[match_previous]
  table_output <- table_output[-(rem_avg_indices - 1), ] %>%
    select(-sel_mul)
  
  # Removing select_multiple question names from binary vars
  # Also removing select multiple binary option from the main question
  
  table_output[,1] <- ifelse(table_output[,2] == main_col_name & str_detect(table_output[,1], sel_mul_remove_rgx),
                             str_extract(table_output[,1], sel_mul_extract_rgx),
                             table_output[,1])
  
  table_output[,1] <- ifelse(table_output[,2] != main_col_name,
                             str_remove_all(table_output[,1], sel_mul_remove_rgx),
                             table_output[,1])
  
  # Getting question labels if requested
  
  if (labels) {
    if (is.null(language)) {
      label_col <- "label"
    } else {
      cols <- names(questionnaire)
      label_col <- str_detect(cols, paste0("label[\\W]{2}(?i)", language))
      label_col <- cols[label_col]
    }
    
    
    choice_indices <- match(table_output$data, choices$name)
    choice_labels <- choices[[label_col]]
    question_indices <- match(table_output$data, questionnaire$name)
    question_labels <- questionnaire[[label_col]]
    
    table_output <- table_output %>%
      mutate(data = ifelse(is.na(question_indices), 
                           data, 
                           ifelse(is.na(question_labels[question_indices]) | question_labels[question_indices] == "",
                                  data,
                                  question_labels[question_indices])),
             data = ifelse(is.na(choice_indices),
                           data,
                           choice_labels[choice_indices]))
    
    # Fixing select multiple labels
    
    sel_mul_indices <- match(sel_mul, questionnaire$name)
    
    for (i in 1:length(sel_mul_indices)) {
      table_output[,1] <- str_replace(table_output[,1], paste0(questionnaire$name[sel_mul_indices[i]],"\\b"), question_labels[sel_mul_indices[i]])
    }
  }
  
  # Cleaning rows with question names
  
  split_rows <- table_output[,2] == main_col_name
  table_output[split_rows, 2:ncol(table_output)] <- ""

  return(table_output)
}
