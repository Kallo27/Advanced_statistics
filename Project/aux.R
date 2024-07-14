library(tm)
library(textstem)
library(SnowballC)
library(dplyr)

####################
# DATASET CLEANING #
####################

change_labels <- function(labels) {
  label_map <- c("0" = 2, "1" = 1, "2" = 3, "3" = 4, "4" = 0, "5" = 5)
  new_labels <- sapply(labels, function(label) label_map[as.character(label)])
  return(new_labels)
}

lemmatize_text <- function(text) {
  lemmatized <- textstem::lemmatize_words(unlist(strsplit(text, "\\s+")))
  lemmatized <- SnowballC::wordStem(lemmatized, language = "en")

  return(paste(lemmatized, collapse = " "))
}

filter_non_english_words <- function(text) {
  tokens <- unlist(strsplit(text, "\\s+"))
  is_english <- hunspell::hunspell_check(tokens)
  english_tokens <- tokens[is_english]
  cleaned_text <- paste(english_tokens, collapse = " ")
  return(cleaned_text)
}

remove_numbers_inside_words <- function(text) {
  words <- unlist(strsplit(text, "\\s+"))

  clean_words <- lapply(words, function(word) {
    if (grepl("\\d", word)) {
      word <- gsub("\\d", "", word)
    }
    return(word)
  })

  cleaned_text <- paste(clean_words, collapse = " ")
  return(cleaned_text)
}

to_space <- tm::content_transformer(function(x, pattern) {
  return(gsub(pattern, " ", x))
})

clean <- function(document, tokenize = TRUE, lemmatize = TRUE) {
  clean_doc <- tm::VCorpus(tm::VectorSource(document))

  if (tokenize) {
    clean_doc <- tm::tm_map(clean_doc, tm::content_transformer(tolower))
    clean_doc <- tm::tm_map(clean_doc, tm::removePunctuation)
    clean_doc <- tm::tm_map(clean_doc, tm::removeWords, tm::stopwords("en"))
    clean_doc <- tm::tm_map(clean_doc, tm::content_transformer(filter_non_english_words))
    clean_doc <- tm::tm_map(clean_doc, tm::content_transformer(remove_numbers_inside_words))
    clean_doc <- tm::tm_map(clean_doc, tm::stripWhitespace)
  }

  if (lemmatize) {
    clean_doc <- tm::tm_map(clean_doc, tm::content_transformer(lemmatize_text))
  }

  return(sapply(clean_doc, NLP::content))
}

clean_empty_rows <- function(dataframe) {
  empty_rows <- which(nchar(trimws(dataframe$Text)) == 0)
  if (length(empty_rows) != 0) {
    dataframe <- dataframe[-empty_rows, ]
  }
  return(dataframe)
}


################
# VOCABULARIES #
################

get_vocabulary_six <- function(document, threshold) {
  words <- unlist(strsplit(document, "\\s+"))
  words <- words[words != ""]
  words_table <- table(words)

  words_freq <- as.data.frame(words_table, stringsAsFactors = FALSE)
  colnames(words_freq) <- c("word", "occurrencies")

  vocabulary <- words_freq[words_freq$occurrencies >= threshold, ]$word
  return(vocabulary)
}

get_vocabulary_two <- function(document, threshold) {
  words <- unlist(strsplit(document, "\\s+"))
  words <- words[words != ""]
  words_table <- table(words)

  words_freq <- as.data.frame(words_table, stringsAsFactors = FALSE)
  colnames(words_freq) <- c("word", "occurrencies")

  total_words <- sum(words_freq$occurrencies)
  words_freq$occurrencies <- words_freq$occurrencies / total_words

  vocabulary <- words_freq[words_freq$occurrencies >= threshold, ]$word
  return(list(voc = vocabulary, df = words_freq))
}

get_vocabulary_tags <- function(df, threshold) {
  tag_texts <- list()
  all_tags <- unique(unlist(strsplit(df$Tag, ",")))

  for (tag in all_tags) {
    matching_docs <- df[grep(tag, df$Tag), "Text"]
    doc <- paste(matching_docs, collapse = " ")

    voc <- get_vocabulary_six(doc, threshold)
    tag_texts <- append(tag_texts, voc)
  }

  return(sort(unique(unlist(tag_texts))))
}


############
# TRAINING #
############

train_multinomial_nb <- function(classes, data, threshold, type) {
  n <- length(data$Text)

  if (type == "Six") {
    vocabulary <- get_vocabulary_six(paste(data$Text, collapse = " "), threshold)
  } else if (type == "Two") {
    vocabulary <- get_vocabulary_two(paste(data$Text, collapse = " "), threshold)$voc
  } else if (type == "Tags") {
    vocabulary <- get_vocabulary_tags(data, threshold)
  } else {
    stop("Invalid type specified")
  }

  prior <- numeric(length(classes))
  names(prior) <- classes
  post <- matrix(0, nrow = length(vocabulary), ncol = length(classes), dimnames = list(vocabulary, classes))

  for (c in seq_along(classes)) {
    class_label <- classes[c]
    docs_in_class <- data[data$Label == class_label, "Text"]
    prior[c] <- length(docs_in_class) / n

    textc <- paste(docs_in_class, collapse = " ")
    tokens <- table(strsplit(tolower(textc), "\\W+")[[1]])
    vocab_counts <- sapply(vocabulary, function(t) if (t %in% names(tokens)) tokens[t] else 0)

    post[, c] <- (vocab_counts + 1) / (sum(vocab_counts) + length(vocabulary))
  }

  return(list(vocab = vocabulary, prior = prior, condprob = post))
}

##################
# LOG-LIKELIHOOD #
##################

apply_multinomial_nb <- function(classes, vocab, prior, condprob, doc) {
  tokens <- intersect(unlist(strsplit(doc, "\\s+")), vocab)

  score_matrix <- matrix(0, nrow = length(tokens), ncol = length(classes))
  rownames(score_matrix) <- tokens
  colnames(score_matrix) <- classes

  for (c in seq_along(classes)) {
    for (t in seq_along(tokens)) {
      term <- tokens[t]
      score_matrix[t, c] <- log(condprob[term, c])
    }
  }

  scores <- colSums(score_matrix) + log(prior)

  return(names(which.max(scores)))
}

###########################
# K-FOLD CROSS VALIDATION #
###########################

kfold_cross_validation <- function(dataset, k = 5, thresholds, type) {
  n <- nrow(dataset)
  fold_size <- floor(n / k)

  accuracies <- matrix(0, nrow = k, ncol = length(thresholds))
  classes <- as.integer(sort(unique(dataset$Label)))

  for (fold in 1:k) {
    validation_indices <- ((fold - 1) * fold_size + 1):(fold * fold_size)
    train_indices <- setdiff(1:n, validation_indices)
    training_set <- dataset[train_indices, ]
    validation_set <- dataset[validation_indices, ]

    for (i in seq_along(thresholds)) {
      model <- train_multinomial_nb(classes, training_set, thresholds[i], type)

      pred_labels <- sapply(validation_set$Text, function(doc) {
        apply_multinomial_nb(classes, model$vocab, model$prior, model$condprob, doc)
      })

      correct_predictions <- sum(validation_set$Label == pred_labels)
      total_predictions <- length(validation_set$Label)
      accuracies[fold, i] <- correct_predictions / total_predictions
    }
  }

  mean_accuracies <- colMeans(accuracies)
  return(data.frame(threshold = thresholds, mean_accuracy = mean_accuracies))
}