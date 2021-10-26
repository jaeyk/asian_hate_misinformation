
conText <- function(formula, data, text_var = 'text', pre_trained, transform = TRUE, transform_matrix, bootstrap = TRUE, num_bootstraps = 20, stratify_by = NULL, permute = TRUE, num_permutations = 100, getcontexts = TRUE, window = 6, valuetype = "fixed", case_insensitive = TRUE, hard_cut = FALSE, verbose = TRUE){

        # extract target word
        target <- as.character(formula[[2]])
        # extract covariates
        covariates <- attr(terms(formula), which = "term.labels")
        # if getcontexts is specified, get context around target word
        if(getcontexts){
            # subset data to where target is present (this step helps speed up the get_context function)
            if(valuetype == 'fixed') target_present <- grep(target, dplyr::pull(data, text_var), fixed = TRUE)
            if(valuetype != 'fixed') target_present <- grep(target, dplyr::pull(data, text_var), ignore.case = case_insensitive)
            data <- data[target_present,]
            # add document id variable (used for merging back with covariates)
            data <- data %>% dplyr::mutate(docname = paste0("text", 1:nrow(.)))
            # apply get_context function (see get_context documentation)
            context <- get_context(x = dplyr::pull(data, text_var), target = target, window = window, valuetype = valuetype, case_insensitive = case_insensitive, hard_cut = hard_cut, verbose = verbose)
            # merge with metadata
            context <- dplyr::left_join(context, data[,c("docname", covariates)], by = "docname") %>% dplyr::mutate('(Intercept)' = 1)
            # otherwise take the full text as the context to be embedded
        }else{
            # add intercept
            context <- data %>% dplyr::mutate('(Intercept)' = 1) %>% dplyr::mutate(context = dplyr::pull(data, text_var))
        }

        # embed context to get dependent variable (note, aggregate is set to FALSE as we want an embedding for each instance)
        embeds_out <- embed_target(context$context, pre_trained = pre_trained, transform_matrix = transform_matrix, transform = transform, aggregate = FALSE, verbose)
        Y <- embeds_out$target_embedding
        if(verbose) cat('total observations included in regression:', nrow(Y), '\n')

        # regressors
        X <- context[embeds_out$obs_included,c('(Intercept)', covariates)]
        # run full sample ols
        full_sample_out <- run_ols(Y = Y, X = X)
        # outputs
        beta_cofficients <- full_sample_out$betas
        norm_tibble <- dplyr::tibble(Coefficient = names(full_sample_out$normed_betas), Normed_Estimate = unname(full_sample_out$normed_betas))
        # -------------------
        # bootstrapping
        # -------------------
        if(bootstrap){
            if(verbose) cat('starting bootstrapping \n')
            # bootstrapped ols
            bootstrap_out <- replicate(num_bootstraps, bootstrap_ols(Y = Y, X = X, stratify_by = stratify_by), simplify = FALSE)
            # average betas
            betas <- lapply(bootstrap_out, '[[', 'betas')
            bs_betas <- Reduce("+",betas)/length(betas)
            # summary statistics for normed betas
            normed_betas <- lapply(bootstrap_out, '[[', 'normed_betas') %>% do.call(rbind,.)
            mean_normed_betas <- apply(normed_betas, 2, mean)
            stderror_normed_betas <- 1/sqrt(num_bootstraps) * apply(normed_betas, 2, sd)
            bs_normed_betas <- dplyr::tibble(Coefficient = names(mean_normed_betas), Normed_Estimate = unname(mean_normed_betas), Std.Error = unname(stderror_normed_betas))
            # output
            beta_cofficients <- bs_betas
            norm_tibble <- bs_normed_betas
            # notice
            if(verbose) cat('done with bootstrapping \n')
        }
        # -------------------
        # permutation
        # -------------------
        if(permute){
            if(verbose) cat('starting permutations \n')
            # permuted ols
            permute_out <- replicate(num_permutations, permute_ols(Y = Y, X = X), simplify = FALSE)
            # compute empirical p-value
            permuted_normed_betas <- lapply(permute_out, '[[', 'normed_betas') %>% do.call(rbind,.)
            empirical_pvalue <- sweep(permuted_normed_betas, 2, 1/full_sample_out$normed_betas, '*')
            empirical_pvalue <- apply(empirical_pvalue, 2, function(x) sum(x>1)/length(x))
            # bind with results
            norm_tibble <- cbind(norm_tibble, Empirical_Pvalue = unname(empirical_pvalue))
            if(verbose) cat('done with permutations \n')
        }
        # -------------------
        # output
        # -------------------
        return(list(betas = beta_cofficients, normed_betas = norm_tibble))
    }
    # -----------------------------
    #
    # SUB-FUNCTIONS
    #
    # -----------------------------
    #' Permute OLS
    #'
    #' Estimate empirical p-value using permutated regression
    #'
    #' @param Y vector of regression model's dependent variable (embedded context)
    #' @param X data.frame of model independent variables (covariates)
    #'
    #' @return list with two elements, `betas` = list of beta_cofficients (D dimensional vectors);
    #' `normed_betas` = tibble with the norm of the non-intercept coefficients
    #'
    permute_ols <- function(Y = NULL, X = NULL){
        # shuffle Y
        Y_permuted <- Y[sample(1:nrow(Y), replace = FALSE),]
        # run ols
        ols_out <- run_ols(Y = Y_permuted, X = X)
        # output
        return(ols_out)
    }
    #' Bootstrap OLS
    #'
    #' Bootstrap model coefficients and standard errors
    #'
    #' @param Y vector of regression model's dependent variable (embedded context)
    #' @param X data.frame of model independent variables (covariates)
    #' @param stratify_by covariates to stratify when bootstrapping
    #'
    #' @return list with two elements, `betas` = list of beta_cofficients (D dimensional vectors);
    #' `normed_betas` = tibble with the norm of the non-intercept coefficients
    #'
    bootstrap_ols <- function(Y = NULL, X = NULL, stratify_by = NULL){
        # label instances
        X_bs <- cbind(obs = 1:nrow(X), X)
        # sample observations with replacement
        if(is.null(stratify_by)) X_bs <- dplyr::sample_n(X_bs, size = nrow(X_bs), replace = TRUE)else{
            X_bs <- X_bs %>% dplyr::group_by_at(stratify_by) %>% dplyr::sample_n(size = dplyr::n(), replace = TRUE) %>% dplyr::ungroup()
        }
        # subset Y to sampled observations
        Y_bs <- Y[X_bs$obs,]
        # remove observation label
        X_bs <- X_bs[,-1]
        # run ols
        ols_out <- run_ols(Y = Y_bs, X = X_bs)
        # output
        return(ols_out)
    }
    #' Run OLS
    #'
    #' Bootstrap model coefficients and standard errors
    #'
    #' @param Y vector of regression model's dependent variable (embedded context)
    #' @param X data.frame of model independent variables (covariates)
    #'
    #' @return list with two elements, `betas` = list of beta_cofficients (D dimensional vectors);
    #' `normed_betas` = tibble with the norm of the non-intercept coefficients
    #'
    run_ols <- function(Y = NULL, X = NULL){
        # convert X to a matrix
        X_mat <- as.matrix(X, ncol = ncol(X))
        # compute OLS bets hats
        betas <- solve(t(X_mat)%*%X_mat)%*%t(X_mat)%*%Y
        # normed betas
        vars <- setdiff(colnames(X), "(Intercept)") # identify non-intercept covariates (norm of intercept is not all that informative)
        normed_betas <- apply(matrix(betas[vars,], nrow = length(vars)), 1, function(x) norm(matrix(x, nrow = 1))) %>% setNames(vars)
        # output
        return(list('betas' = betas, 'normed_betas' = normed_betas))
    }

## for bootstrapping 95% confidence intervals; Borrowed from Nick Camp's code from Jaren, Nick, and my shared project

theta <- function(x, xdata, na.rm = T) {
  mean(xdata[x], na.rm = na.rm)
}

ci.low <- function(x, na.rm = T) {
  mean(x, na.rm = na.rm) - quantile(bootstrap::bootstrap(1:length(x), 1000, theta, x, na.rm = na.rm)$thetastar, .025, na.rm = na.rm)
}

ci.high <- function(x, na.rm = T) {
  quantile(bootstrap::bootstrap(1:length(x), 1000, theta, x, na.rm = na.rm)$thetastar, .975, na.rm = na.rm) - mean(x, na.rm = na.rm)
}

# preprocessing

get_word_count <- function(data, stem = TRUE) {

  tidy_df <- data %>%
    # Tokenize
    unnest_tokens("word", value) %>%
    # Remove stop words
    anti_join(get_stopwords(), by = "word")

  if (stem == TRUE) {

    # Stemming
    tidy_df <- tidy_df %>% mutate(stem = wordStem(word))

    df_words <- tidy_df %>%
      count(date, stem, sort = TRUE)

    total_words <- df_words %>%
      group_by(date) %>%
      summarize(total = sum(n))

    joined_words <- left_join(df_words, total_words)

    tf_idf <- joined_words %>%
      # TF-IDF
      bind_tf_idf(stem, date, n)

  } else {

    df_words <- tidy_df %>% count(date, word, sort = TRUE)

    total_words <- df_words %>%
      group_by(date) %>%
      summarize(total = sum(n))

    joined_words <- left_join(df_words, total_words)

    tf_idf <- joined_words %>%
      # TF-IDF
      bind_tf_idf(word, date, n)

  }

  return(tf_idf)

}

clean_text <- function(full_text) {

  vec <- tolower(full_text) %>%
    # Remove all non-alpha characters
    gsub("[^[:alpha:]]", " ", .) %>%
    # remove 1 letter words
    str_replace_all("\\b\\w{1,2}\\b", "") %>%
    # remove excess white space
    str_replace_all("^ +| +$|( ) +", "\\1")

  vec <- vec %>%
    replace_html() %>%
    replace_url() %>%
    replace_contraction() %>%
    replace_hash() %>%
    replace_tag()

  vec <- tm::removeWords(vec, words = c(stopwords(source = "snowball"), "moron", "asshole", "fuckin", "wtf"))

  return(vec)

}

con2nplot <- function(corpus, keyword, local_glove, local_transform) {
  # Latino context
  contextPre <- get_context(x = corpus$clean_text[corpus$Label == 1], target = keyword)

  # Asian context
  contextPost <- get_context(x = corpus$clean_text[corpus$Label == 0], target = keyword)

  # bind contexts
  contexts_corpus <- rbind(cbind(contextPre, group = "Hate speech"), cbind(contextPost, group = "Counterspech"))

  # embed each instance using a la carte
  contexts_vectors <- embed_target(
    context = contexts_corpus$context,
    pre_trained = local_glove,
    transform_matrix = local_transform,
    transform = TRUE,
    verbose = TRUE)

  # get local vocab
  local_vocab <- get_local_vocab(c(contextPre$context, contextPost$context), pre_trained = local_glove)

  # exclude the keyword
  local_vocab <- setdiff(local_vocab, c(keyword))

  contrast_target <- contrast_nns(
    context1 = contextPre$context,
    context2 = contextPost$context,
    pre_trained = local_glove,
    transform_matrix = local_transform,
    transform = TRUE,
    bootstrap = TRUE,
    num_bootstraps = 100,
    permute = TRUE,
    num_permutations = 100,
    candidates = local_vocab,
    norm = "l2")

  # define top N of interest
  N <- 40

  # first get each party's nearest neighbors (output by the contrast_nns function)
  nnsPre <- contrast_target$nns1
  nnsPost <- contrast_target$nns2

  # subset to the union of top N nearest neighbors for each party
  top_nns <- union(nnsPre$Term[1:N], nnsPost$Term[1:N])

  # identify which of these are shared
  shared_nns <- intersect(nnsPre$Term[1:N], nnsPost$Term[1:N])

  # subset nns_ratio (output by contrast_nns) to the union of the top nearest
  # neighbors
  nns_ratio <- contrast_target$nns_ratio %>%
    dplyr::filter(Term %in% top_nns) %>%
    mutate(group = case_when(
      (Term %in% nnsPre$Term[1:N]) & !(Term %in% nnsPost$Term[1:N]) ~ "Hate speech",
      !(Term %in% nnsPre$Term[1:N]) & (Term %in% nnsPost$Term[1:N]) ~ "Counterpspeech",
      (Term %in% shared_nns) ~ "Shared"),
      significant = if_else(Empirical_Pvalue < 0.01, "yes", "no"))

  # order Terms by Estimate
  nns_ratio <- nns_ratio %>%
    mutate(absdev = abs(1 - Estimate)) %>%
    arrange(-absdev) %>%
    mutate(tokenID = 1:nrow(.)) %>%
    mutate(Term_Sig = if_else(significant == "yes", paste0(Term, "*"), Term))

  # plot
  nns_ratio %>%
    ggplot() +
    geom_point(aes(x = Estimate, y = tokenID, color = group, shape = group), size = 2) +
    geom_vline(xintercept = 1, colour = "black", linetype = "dashed",
               size = 0.5) +
    geom_text(
      aes(x = Estimate, y = tokenID, label = Term_Sig), hjust = if_else(nns_ratio$Estimate > 1, -0.2, 1.2), vjust = 0.25, size = 5) +
    scale_color_brewer(palette = "Dark2") +
    xlim(-30, 30) +
    ylim(0, 50) +
    labs(y = "", x = "cosine similarity ratio \n (Hate speech/Counterspeech)",
         col = "Category", shape = "Category") +
    theme(legend.position = "bottom")
}


df2cm <- function(corpus, count_min = 10, window_size = 6) {

  ############################### Create VOCAB ###############################

  # Create iterator over tokens
  tokens <- space_tokenizer(corpus$clean_text)

  # Create vocabulary. Terms will be unigrams (simple words).
  it <- itoken(tokens, progressbar = TRUE)

  vocab <- create_vocabulary(it)

  # Filter words
  vocab_pruned <- prune_vocabulary(vocab, term_count_min = count_min)

  # use quanteda's fcm to create an fcm matrix
  fcm_cr <- quanteda::tokens(corpus$clean_text) %>%
    quanteda::fcm(context = "window", count = "frequency",
                  window = window_size, weights = rep(1, window_size), tri = FALSE)

  # subset fcm to the vocabulary included in the embeddings
  fcm_cr <- fcm_select(fcm_cr, pattern = vocab_pruned$term, selection = "keep")

  return(fcm_cr)
}

df2ltm <- function(corpus, local_glove, count_min = 10, window_size = 6) {

  ############################### Create VOCAB ###############################

  # Create iterator over tokens
  tokens <- space_tokenizer(corpus$clean_text)

  # Create vocabulary. Terms will be unigrams (simple words).
  it <- itoken(tokens, progressbar = TRUE)

  vocab <- create_vocabulary(it)

  # Filter words
  vocab_pruned <- prune_vocabulary(vocab, term_count_min = count_min)

  # use quanteda's fcm to create an fcm matrix
  fcm_cr <- quanteda::tokens(corpus$clean_text) %>%
    quanteda::fcm(context = "window", count = "frequency",
                  window = window_size, weights = rep(1, window_size), tri = FALSE)

  # subset fcm to the vocabulary included in the embeddings
  fcm_cr <- fcm_select(fcm_cr, pattern = vocab_pruned$term, selection = "keep")

  local_transform <- compute_transform(context_fcm = fcm_cr, pre_trained = local_glove,
                                       vocab = vocab_pruned, weighting = 1000)

  return(local_transform)
}

df2vec <- function(corpus, count_min = 10, window_size = 6, dims = 50) {

  ############################### Create VOCAB ###############################

  # Create iterator over tokens
  tokens <- space_tokenizer(corpus$clean_text)

  # Create vocabulary. Terms will be unigrams (simple words).
  it <- itoken(tokens, progressbar = TRUE)

  vocab <- create_vocabulary(it)

  # Filter words
  vocab_pruned <- prune_vocabulary(vocab, term_count_min = count_min)

  ############################### Create Term Co-occurence Matrix ###############################

  # Use our filtered vocabulary
  vectorizer <- vocab_vectorizer(vocab_pruned)

  # Use window of 10 for context words
  tcm <- create_tcm(it, vectorizer, skip_grams_window = window_size, skip_grams_window_context = "symmetric", weights = rep(1, window_size))

  ############################### Set Model Parameters ###############################

  glove <- GlobalVectors$new(rank = dims, x_max = 10)

  ############################### Fit Model ###############################

  wv_main <- glove$fit_transform(tcm, n_iter = 1000, convergence_tol = 0.001, n_threads = RcppParallel::defaultNumThreads())

  ############################### Get Output ###############################

  wv_context <- glove$components

  word_vectors <- wv_main + t(wv_context)

  return(word_vectors)
}

get_bt_terms <- function(speech_n, label_n, keyword, word_n) {

  contexts <- get_context(x = subset(corpus, trump_speech == speech_n & Label == label_n)$clean_text, target = keyword,
                          window = 6, valuetype = "fixed", case_insensitive = TRUE, hard_cut = FALSE, verbose = FALSE)

  local_vocab <- get_local_vocab(contexts$context, local_glove)

  local_vocab <- setdiff(local_vocab, keyword)

  out <- bootstrap_nns(
    context = contexts$context,
    pre_trained = local_glove,
    transform_matrix = local_transform,
    transform = TRUE,
    candidates = local_vocab,
    bootstrap = TRUE,
    num_bootstraps = 100,
    N = word_n,
    norm = "l2")

  if (speech_n == 1 & label_n == 1) {

    out <- out %>%
      mutate(trump_speech = "Post",
             label = "Hate")

  }

  if (speech_n == 1 & label_n == 0) {

    out <- out %>%
      mutate(trump_speech = "Post",
             label = "Counterhate")

  }

  if (speech_n == 0 & label_n == 1) {

    out <- out %>%
      mutate(trump_speech = "Pre",
             label = "Hate")

  }

  if (speech_n == 0 & label_n == 0) {

    out <- out %>%
      mutate(trump_speech = "Pre",
             label = "Counterhate")

  }

  return(out)
}

get_candidates <- function(corpus, keyword, local_glove, local_transform) {

  # get contexts
  contexts_corpus <- get_context(x = corpus$clean_text, target = keyword)

  # embed each instance using a la carte
  contexts_vectors <- embed_target(
    context = contexts_corpus$context,
    pre_trained = local_glove,
    transform_matrix = local_transform,
    transform = TRUE,
    aggregate = FALSE,
    verbose = TRUE)

  # get local vocab
  local_vocab <- get_local_vocab(c(contextL$context, contextA$context), pre_trained = local_glove)

  return(local_vocab)
}

get_contexs <- function(group_n, period_n, key_word) {

  out <- get_context(x = subset(corpus, latino == group_n & pre == period_n)$clean_text, target = key_word,
                     window = 6, valuetype = "fixed", case_insensitive = TRUE, hard_cut = FALSE, verbose = FALSE)

  return(out)
}

get_context_con <- function(group_n, period, period_n, key_word) {

  out <- get_context(x = subset(corpus, latino == group_n & get(period) == period_n)$clean_text,
                     target = key_word,
                     window = 6,
                     valuetype = "fixed",
                     case_insensitive = TRUE,
                     hard_cut = FALSE,
                     verbose = FALSE)

  return(out)

}

get_word_count <- function(data, stem = TRUE) {

  tidy_df <- data %>%
    # Tokenize
    unnest_tokens("word", clean_text) %>%
    # Remove stop words
    anti_join(get_stopwords(), by = "word")

  if (stem == TRUE) {

    # Stemming
    tidy_df <- tidy_df %>% mutate(stem = wordStem(word))

    df_words <- tidy_df %>%
      count(date, stem, sort = TRUE)

    total_words <- df_words %>%
      group_by(date) %>%
      summarize(total = sum(n))

    joined_words <- left_join(df_words, total_words)

    tf_idf <- joined_words %>%
      # TF-IDF
      bind_tf_idf(stem, date, n)

  } else {

    df_words <- tidy_df %>% count(date, word, sort = TRUE)

    total_words <- df_words %>%
      group_by(date) %>%
      summarize(total = sum(n))

    joined_words <- left_join(df_words, total_words)

    tf_idf <- joined_words %>%
      # TF-IDF
      bind_tf_idf(word, date, n)

  }

  return(tf_idf)

}

key2convec <- function(corpus, keyword) {

  contexts <- get_context(x = corpus$clean_text, target = keyword, window = 6, valuetype = "fixed", case_insensitive = TRUE, hard_cut = FALSE, verbose = FALSE)

  contexts_vectors <- embed_target(context = contexts$context, pre_trained = local_glove, transform_matrix = local_transform, transform = TRUE, aggregate = TRUE, verbose = TRUE)

  return(contexts_vectors)
}

key2sim <- function(vectors, key_word, n = 30) {

  out <- vectors %>%
    nearest_neighbors(key_word) %>%
    filter(item1 != key_word) %>%
    top_n(n, abs(clean_text)) %>%
    mutate(clean_text = round(clean_text,2)) %>%
    rename(word = item1,
           similarity = clean_text) %>%
    mutate(keyword = key_word)

  return(out)

}

keyword2plot <- function(word_vector, keywords, n, custom_title = NULL){

  out <- purrr::map_dfr(keywords,
                        possibly(~vec2sim(word_vector, ., n),
                                 # This sorts out the keyword not in the word embedding
                                 otherwise =
                                   data.frame(word = NA,
                                              similarity = NA,
                                              keyword = NA)))

  if (is.null(custom_title)) {

    out <- plot_embed(out)

    return(out)

  }

  else {

    out <- plot_embed(out) + labs(title = custom_title)

    return(out)

  }
}

models2df <- function(models) {

  plot_tibble <- lapply(models, '[[', 'normed_betas') %>%
    do.call(rbind, .) %>%
    mutate(year = factor(unique(corpus$year), levels = unique(corpus$year)))

  return(plot_tibble)
}

models2plot_seq <- function(models, key_word) {

  # combine results
  plot_tibble <- lapply(models, '[[', 'normed_betas') %>%
    do.call(rbind, .) %>%
    mutate(year = factor(unique(corpus$year), levels = unique(corpus$year)))

  plot <- ggplot(plot_tibble,
                 aes(x = year, y = Normed_Estimate, group = 1)) +
    geom_line(color = 'blue', size = 0.5) +
    geom_pointrange(aes(
      x = year,
      y = Normed_Estimate,
      ymin = Normed_Estimate - 1.96*Std.Error,
      ymax = Normed_Estimate + 1.96*Std.Error),
      lwd = 0.5,
      position = position_dodge(width = 1/2)) +
    labs(x = "",
         y = "Norm of beta hat",
         title = glue("Keyword = {key_word}")) +
    scale_color_manual(values = c('no' = 'grey', 'yes' = 'blue')) +
    theme(axis.text.x = element_text(size = 10, angle = 90, vjust = 0.5, hjust = 1),
          axis.text.y = element_text(size = 10))

  return(plot)
}

nearest_neighbors <- function(df, token) {
  df %>%
    widely(
      ~ {
        y <- .[rep(token, nrow(.)), ]
        res <- rowSums(. * y) /
          (sqrt(rowSums(. ^ 2)) * sqrt(sum(.[token, ] ^ 2)))

        matrix(res, ncol = 1, dimnames = list(x = names(res)))
      },
      sort = TRUE
    )(item1, dimension, clean_text) %>%
    select(-item2)
}

plot_embed <- function(embed) {

  out <- embed %>%
    filter(!is.na(keyword)) %>%
    group_by(keyword) %>%
    slice_max(similarity, n = 10) %>%
    mutate(word = factor(word, levels = rev(unique(word)))) %>%
    ggplot(aes(similarity, word, fill = keyword)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~keyword, ncol = 5, scales = "free") +
    labs(x = "Cosine similiarity",
         y = "")

  return(out)
}

terms2plot <- function(df1, df2, keyword, year) {

  bind_rows(df1, df2) %>%
    group_by(feature) %>%
    filter(n() > 1) %>%
    ggplot(aes(x = fct_reorder(feature, value), y = value,
               ymax = value + 1.96*std.error,
               ymin = value - 1.96*std.error, col = label)) +
    geom_pointrange() +
    facet_wrap(~trump_speech) +
    coord_flip() +
    labs(subtitle = glue("Keyword: {keyword}"),
         title = glue("{year}"),
         x = "",
         y = "Bootstrapped estimate",
         col = "Label") +
    theme(legend.position = "bottom") +
    scale_color_brewer(palette = "Dark2")

}

terms2plot_sep <- function(df1, df2, keyword, year) {

  bind_rows(df1, df2) %>%
    group_by(label) %>%
    top_n(25, value) %>%
    ggplot(aes(x = fct_reorder(feature, value), y = value,
               ymax = value + 1.96*std.error,
               ymin = value - 1.96*std.error, col = label)) +
    geom_pointrange() +
    facet_wrap(~trump_speech) +
    coord_flip() +
    labs(subtitle = glue("Keyword: {keyword}"),
         title = glue("{year}"),
         x = "",
         y = "Bootstrapped estimate",
         col = "Label") +
    theme(legend.position = "bottom") +
    scale_color_brewer(palette = "Dark2")

}