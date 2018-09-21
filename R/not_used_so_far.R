#' Converting an \code{obsforc} object into a data.frame
#'
#' @param obj Element of class \code{obsforc}
#'
#' @export
# as.data.frame.obsforc <- function(obj){
#
#   df <- data.frame(
#     obs  = obs,
#     forc = forc
#   )
#
#   return(df)
# }


#' Printing a obsforc object
#'
#' @param obj Element of class \code{obsforc}
#'
#' @export
# print.obsforc <- function(obj){
#
# }

#' Plotting an obsforc object
#'
#' @param obj Element of class \code{obsforc}
#' @param caption Character specifying the plot caption
#' @param subtitle Character specifying the plot subtitle
#'
#' @export
# plot.obsforc <- function(obj, caption = NULL, subtitle = NULL){
#
#   if(requireNamespace("RColorBrewer", quietly = TRUE)){
#     color <- RColorBrewer::brewer.pal(n = 8, name = "Set1")
#   }else{
#     color <- c("red", "blue")
#   }
#
#   obsforc_df  <- as.data.frame.obsforc(obj) %>% as.tibble()
#   obsforc_df0 <- obsforc_df %>% filter(obs == 0)
#   obsforc_df1 <- obsforc_df %>% filter(obs == 1)
#
#   xlim <- c(min(obsforc_df$forc), max(obsforc_df$forc))
#
#   line_vals <- obsforc %>%
#     group_by(obs) %>%
#     summarize(minv = min(forc), maxv = max(forc)) %>%
#     ungroup
#
#   dat_line  <- tibble(
#     x  = c(line_vals %>% filter(obs == 0) %>% select(-obs) %>% as.numeric,
#            line_vals %>% filter(obs == 1) %>% select(-obs) %>% as.numeric),
#     y   = c(ylim[1], ylim[1], ylim[2], ylim[2]),
#     obs = as.factor(c(0,0,1,1)))
#
#   phistogramm <- ggplot() +
#     geom_histogram(data = obsforc_df0, aes(x = forc, y = -..density..),
#                    fill = colors[1], alpha = 1, breaks = breaks) +
#     geom_histogram(data = obsforc_df1, aes(x = forc, y = ..density..),
#                    fill = colors[2], alpha = 1, breaks = breaks) +
#     geom_line(data = dat_line, aes(x = x, y = y, col = obs), lwd = 1) +
#     scale_color_manual(values = colors[1:2]) +
#
#     geom_hline(yintercept = 0) +
#     xlab(xlabs) + ylab("") + ggtitle("", subtitle = "") +
#     theme(legend.position = "none") +
#     scale_fill_manual(name = "Observation", values = colors[1:2]) +
#
#     scale_y_continuous(
#       labels = NULL,
#       breaks = seq(ylim[1], ylim[2], length.out = 11),
#       minor_breaks = NULL) +
#
#     coord_cartesian(ylim = ylim) +
#
#     annotate("text", x = xlim[1] + 0.95 * diff(xlim), y = ylim[1] + 0.1 * diff(ylim),
#              label = "F[0]", size = 7, col = colors[1], parse = TRUE) +
#
#     annotate("text", x = xlim[1] + 0.95 * diff(xlim), y = ylim[1] + 0.9 * diff(ylim),
#              label = "F[1]", size = 7, col = colors[2], parse = TRUE)
#
#   phistogramm <- move_caption_afterwards(p = phistogramm, caption = caption, subtit = subtit)
#
#
# }




#' #' Transforms predictions and observations into ROC curve
#' #'
#' #' @param observation A numeric vector with zeros representing misses and
#' #'   ones representing hits. Alternatively, logical or character vectors can
#' #'   be provided, that can be converted in a (0,1) vector
#' #' @param prediction  A numeric vector of the some length as \code{observation},
#' #'   containing the predicted value for each observation.
#' #' @param
#'
#' roc <- function(observation, prediction, )
#'
#'
#' wrapper_ROC_obsforc <- function(obsforc, thresholds = NULL, method, noncal = FALSE, cal = FALSE, conhull = FALSE,
#'                                 roc_fawcett = TRUE, cal_fawcett = FALSE, debug = FALSE){
#'
#'   # if(debug) print("")
#'   # if(debug) print("-------------------------------------------------------------------------")
#'   # if(debug) print("Starting wrapper_ROC_obsforc()")
#'   #
#'   # if(debug) print("")
#'   # if(debug) print("NO DEBUG INFORMATION SO FAR")
#'   #
#'   # # Checks and warnings
#'   # if(!noncal & !cal & !conhull) cat("There are no ROC values to compute!")
#'   #
#'   # # Set up
#'   # TFPR  <- tibble(TPR = as.numeric(), FPR = as.numeric(), type = as.character(), forecast = as.character())
#'   #
#'   # # Iteratives Ermitteln der ROC-Werte
#'   # for(i in 1:length(method)){
#'   #
#'   #   print(paste("Start computing ROC values for", method[i], "(", i, "of", length(method), ")."))
#'   #
#'   #   obs      <- pull(obsforc, obs)
#'   #   forc_raw <- pull(obsforc, method[i])
#'   #
#'   #   if(noncal){
#'   #     TFPR_raw  <- wrapper_roc(obs = obs, forc = forc_raw, rocmethod = ifelse(roc_fawcett, "fawcett", "own"))
#'   #     TFPR      <- TFPR_raw %>% as.tibble() %>% add_column(type = "noncal", forecast = method[i]) %>% bind_rows(TFPR)
#'   #   }
#'   #
#'   #   if(cal){
#'   #     forc_cal <- wrapper_PAV(obs = obs, forc = forc_raw, calmethod = ifelse(cal_fawcett, "fawcett", "isotone"))
#'   #     TFPR_cal  <- wrapper_roc(obs = obs, forc = forc_cal, rocmethod = ifelse(roc_fawcett, "fawcett", "own"))
#'   #     TFPR      <- TFPR_cal %>% as.tibble() %>% add_column(type = "cal", forecast = method[i]) %>% bind_rows(TFPR)
#'   #   }
#'   #
#'   #   if(conhull){
#'   #     if(!exists("TFPR_raw")){
#'   #       TFPR_raw <- wrapper_roc(obs = obs, forc = forc_raw, rocmethod = ifelse(roc_fawcett, "fawcett", "own"))
#'   #       print("New computation of TFPR_raw")
#'   #     }
#'   #     TFPR_conhull <- ROC_convex_hull_fawcett(TFPR = TFPR_raw)
#'   #     TFPR         <- TFPR_conhull %>% as.tibble() %>%
#'   #       add_column(type = "conhull", forecast = method[i]) %>% bind_rows(TFPR)
#'   #   }
#'   #
#'   #
#'   #   # Sortieren des TPFR
#'   #   TFPR <- TFPR %>% arrange(type, forecast, TPR, FPR)
#'   #
#'   #   # Remove objects
#'   #   rmlist <- c("TFPR_raw", "TFPR_cal", "TFPR_conhull")
#'   #   rmlist <- rmlist[sapply(rmlist, exists)]
#'   #   rm(list = rmlist)
#'   # }
#'
#'   return(TFPR)
#' }