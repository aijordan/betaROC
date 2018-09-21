#' #' Observation forecast object
#' #'
#' #' \code{observationforecast} constructs and returns an object of class
#' #' \code{obsforc}.
#' #'
#' #' @param observation An vector containing observations where zeros represent
#' #'   misses and ones represent hits.
#' #' @param forecast A vector of type \code{numeric} (or \code{character} when
#' #'   convertible to \code{numeric}) that contains per forecast and observed
#' #'   value one predicted value
#' #'
#' #' @param dataset optional; name of the dataset
#' #' @param forecastname optional; name of the forecast
#' #'
#' #' @return The output is an object of class \code{obsforc}, which is a list
#' #'   containing the following components: \tabular{ll}{ \code{obs} \tab a vector
#' #'   of observations \cr \code{forc} \tab a vector or matrix of forecasts\cr }
#' #' @export
#' observationforecast <- function(
#'   observation, forecast, dataset = NULL, forecastname = NULL){
#'
#'   if(is.null(dataset)) dataset <- "unknown"
#'   if(is.null(forecastname)) forecastname <- "unknown"
#'
#'   if(length(observation) != length(forecast)){
#'     stop("observation and forecast have different lengths")
#'   }
#'
#'   if(is.factor(forecast)){
#'     tx <- type.convert(levels(forecast))
#'     if(is.factor(tx)){
#'       stop("forecast can not be transformed into numeric vector")
#'     }
#'     x <- as.numeric(type.convert(as.character.factor(forecast)))
#'   }else{
#'     x <- type.convert(forecast)
#'     if(is.character(x) || is.factor(x)){
#'       stop("forecast can not be transformed into numeric vector")
#'     }
#'     x <- as.numeric(type.convert(x))
#'   }
#'
#'   if(all(is.na(x) | is.infinite(x))){
#'     stop("forecast contains only NA, NaN, or infinite values")
#'   }
#'
#'   if(is.factor(observation)){
#'     ty <- type.convert(levels(observation))
#'     if(is.factor(ty)){
#'       stop("observation can not be transformed into numeric vector")
#'     }
#'     y <- as.numeric(type.convert(as.character.factor(observation)))
#'   }else{
#'     y <- type.convert(observation)
#'     if(is.character(y) | is.factor(y)){
#'       stop("observation can not be transformed into numeric vector")
#'     }
#'     y <- as.numeric(type.convert(y))
#'   }
#'
#'   if(all(is.na(y))){
#'     stop("observation contains only NA or NaN values")
#'   }
#'
#'   if(!all(y[!is.na(y)] %in% c(0,1))) stop("observation is not binary")
#'
#'   ind_x <- is.na(x) | is.infinite(x)
#'   ind_y <- is.na(y)
#'
#'   if(all(ind_x | ind_y)){
#'     stop("no forecast obserservation pairs found that are permissible")
#'   }
#'
#'   of <- list(
#'     obs      = y[!(ind_x | ind_y)],
#'     forc     = x[!(ind_x | ind_y)]
#'   )
#'
#'   class(of) <- "obsforc"
#'
#'   attr(of, "dataset") <- dataset
#'   attr(of, "forecastname") <- forecastname
#'
#'   return(of)
#' }
#'
#'
#' #' S3 method for an \code{obsforc}
#' #'
#' #' @param obj Element of class \code{obsforc}
#' #' @return The output is an object of class \code{data.frame}
#' #'
#' #' @export
#' as.data.frame.obsforc <- function(obj){
#'
#'   df <- data.frame(
#'     obs  = obj$obs,
#'     forc = obj$forc
#'   )
#'   attr(df, "dataset") <- attributes(obj)$dataset
#'   attr(df, "forecastname") <- attributes(obj)$forecastname
#'
#'   return(df)
#' }
#'
#' #' S3 method for an \code{obsforc} object
#' #'
#' #' @param obj Element of class \code{obsforc}
#' #'
#' #' @export
#' print.obsforc <- function(obj){
#'   print(as.data.frame(obj))
#' }
#'
#' #' S3 method for an \code{obsforc} object
#' #'
#' #' @param obj Element of class \code{obsforc}
#' #' @return Return a summary of an \code{obsforc} object by specifying important
#' #' characteristics, such as the empirical frequency of a 0 or 1 observation
#' #' and summary measures about the conditional distributions of the forecast
#' #' under 0 and 1 observations
#' #'
#' #' @export
#' summary.obsforc <- function(obj){
#'
#'   p0 <- mean(obj$obs == 0)
#'   p1 <- mean(obj$obs == 1)
#'
#'   F0 <- obj$forc[obj$obs == 0]
#'   F1 <- obj$forc[obj$obs == 1]
#'
#'   sF0 <- summary(F0)
#'   sF1 <- summary(F1)
#'
#'   sob <- round(rbind(c(p0, p1), cbind(sF0, sF1)), 2)
#'   dimnames(sob) <- list(c("Prob", dimnames(sob)[[1]][-1]), c("Y=0", "Y=1"))
#'
#'   return(sob)
#' }
#'
#' #' Plotting an obsforc object
#' #'
#' #' @param obj Element of class \code{obsforc}
#' #' @param caption Character specifying the plot caption
#' #' @param subtitle Character specifying the plot subtitle
#' #'
#' #' @export
#' plot.obsforc <- function(obj, caption = NULL, subtitle = NULL){
#'
#'   if(requireNamespace("RColorBrewer", quietly = TRUE)){
#'     color <- RColorBrewer::brewer.pal(n = 3, name = "Set1")[1:2]
#'   }else{
#'     color <- c("red", "blue")
#'   }
#'
#'   if(attributes(of)$forecastname == "unknown"){
#'     xlabs <- "Forecast"
#'   }else{
#'     xlabs <- attributes(of)$forecastname
#'   }
#'
#'   obsforc_df <- as.data.frame.obsforc(obj) %>% as.tibble()
#'   obsforc_df0 <- obsforc_df %>% filter(obs == 0)
#'   obsforc_df1 <- obsforc_df %>% filter(obs == 1)
#'
#'   xlim <- c(min(obsforc_df$forc), max(obsforc_df$forc))
#'   breaks <- seq(xlim[1], xlim[2], length.out = 31)
#'   bins <- sapply(obsforc_df$forc, function(x) sum(x <= breaks))
#'
#'   binabs <- obsforc_df %>%
#'     add_column(bins) %>%
#'     group_by(obs, bins) %>%
#'     summarize(n = n()) %>%
#'     ungroup() %>%
#'     select(obs, n)
#'
#'   corfac <- c(nrow(obsforc_df0), nrow(obsforc_df1)) * max(diff(breaks))
#'
#'   binrel <-max(
#'       binabs %>% filter(obs == 0) %>% pull(n) / corfac[1],
#'       binabs %>% filter(obs == 1) %>% pull(n) / corfac[2])
#'
#'   ylim <- 1.2 * binrel * c(-1, 1)
#'
#'   support <- obsforc_df %>%
#'     group_by(obs) %>%
#'     summarize(minv = min(forc), maxv = max(forc)) %>%
#'     ungroup %>%
#'     gather(type, x, -obs) %>%
#'     mutate(y = ifelse(obs == 1, ylim[2], ylim[1]),
#'            obs = as.factor(obs)) %>%
#'     select(x,y,obs)
#'
#'   phist_raw <- ggplot() +
#'     geom_histogram(data = obsforc_df0, aes(x = forc, y = -..density..),
#'                    fill = color[1], alpha = 1, breaks = breaks) +
#'     geom_histogram(data = obsforc_df1, aes(x = forc, y = ..density..),
#'                    fill = color[2], alpha = 1, breaks = breaks) +
#'     geom_line(data = support, aes(x = x, y = y, col = obs), lwd = 1) +
#'     scale_color_manual(values = color[1:2])
#'
#'   phist_layout <- phist_raw +
#'     theme_minimal(base_size = 16) +
#'     geom_hline(yintercept = 0) +
#'     xlab(xlabs) +
#'     ylab("") +
#'     theme(legend.position = "none") +
#'     scale_fill_manual(name = "Observation", values = colors[1:2]) +
#'     scale_y_continuous(
#'       labels = NULL,
#'       breaks = seq(ylim[1], ylim[2], length.out = 11),
#'       minor_breaks = NULL) +
#'     coord_cartesian(ylim = ylim)
#'
#'   phist_anno <- phist_layout +
#'     annotate("text",
#'              x = xlim[1] + 0.95 * diff(xlim),
#'              y = ylim[1] + 0.05 * diff(ylim),
#'              label = "F[0]", size = 7, col = color[1], parse = TRUE) +
#'     annotate("text",
#'              x = xlim[1] + 0.95 * diff(xlim),
#'              y = ylim[1] + 0.95 * diff(ylim),
#'              label = "F[1]", size = 7, col = color[2], parse = TRUE)
#'
#'   if(is.null(subtitle)){
#'     if(is.null(caption)){
#'       phist <- phist_anno
#'     }else{
#'       if(requireNamespace("cowplot", quietly = TRUE)){
#'         phist <- align_caption(
#'           p       = phist_anno,
#'           caption = caption)
#'       }else{
#'         phist <- phist_anno + ggtitle(caption)
#'       }
#'     }
#'   }else{
#'     if(is.null(caption)){
#'       warning("subtitle specified, but caption missing")
#'     }else{
#'       if(requireNamespace("cowplot", quietly = TRUE)){
#'         phist <- align_caption(
#'           p        = phist_anno,
#'           caption  = caption,
#'           subtitle = subtitle)
#'       }else{
#'         phist <- phist_anno + ggtitle(label = caption, subtitle = subtitle)
#'       }
#'     }
#'   }
#'
#'   return(phist)
#' }
