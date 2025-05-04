library(dplyr)
library(survival)
library(ggsurvfit)
library(swimplot)

tev <- function(x){
    as.character(stringi::stri_extract_all_regex(x, "\\w.+")[[1]])
}

dav <- function(x){
  as.character(stringi::stri_extract_all_regex(x, "\\d{4}-\\d{2}-\\d{2}")[[1]])
}


div <- function(x){
  as.numeric(stringi::stri_extract_all_regex(x, "\\d+")[[1]])
}

sb <- function(df, by, decreasing=FALSE){
  df[order(df[,by], decreasing = decreasing),]
}

prettyplot_for_one <- function(survival_for_the_plot, x_max = 60,
                               survivaltitle=c('',''), plotxlab='Years',
                               plotcol='black', vertlines = c(12, 36),
                               survivaltitle_suffix=NULL, years=TRUE,
                               x_intervals=12, vertlinescol='grey',
                               annotateshift=12, textsize=16,
                               cens_size=3.5, ci=FALSE){
  library(survminer)
  tunit <- ifelse(years, 12, 1)
  survival_percent <- round((summary(survival_for_the_plot, vertlines))[[6]]*100, 1)
  median <- round(surv_median(survival_for_the_plot)$median,1)
  lower <- round(surv_median(survival_for_the_plot)$lower,1)
  upper <- round(surv_median(survival_for_the_plot)$upper,1)
  datalength <- nrow(tidy_survfit(survival_for_the_plot))

  plot <-  ggsurvfit(survival_for_the_plot, linewidth=1.25, color = plotcol)+

    labs(title = paste(survivaltitle[1], 'Survival', survivaltitle_suffix)) +
    xlab(plotxlab) + scale_y_continuous(
      limits = c(0, 1),
      labels = scales::percent,
      expand = c(0.01, 0))+

    add_censor_mark(size = cens_size, shape = "|", color = plotcol)+
    {if(ci)add_confidence_interval(type = 'ribbon')}+

    scale_x_continuous(breaks = seq(0, x_max, by = x_intervals),
                       limits = c(0, NA),
                       labels = seq(0, x_max/x_intervals, by = 1)*(x_intervals)/tunit,
                       expand = c(0.01, 0.01)) +

    theme(plot.title = element_text(size=textsize),
          axis.title = element_text(size = textsize*0.5625),
          axis.text.x = element_text(size = textsize*0.6875),
          axis.text.y = element_text(size = textsize*0.5625),
          legend.text = element_text(size = textsize*0.5625),
          legend.title = element_text(size = textsize*0.5625)
    )+

    theme(panel.grid = element_blank())+
    geom_vline(xintercept = vertlines,
               linetype = "dashed",
               linewidth = 2,
               color = vertlinescol)+


    geom_rect(aes(xmin = vertlines[1:datalength]-1,
                  xmax= vertlines[1:datalength]+1,
                  ymin = 0.96,
                  ymax = 1),
              color = 'white',
              fill = 'white',
              size=7)+
    geom_text(aes(vertlines[1:datalength],
                  y = 0.965,
                  label = paste(survival_percent[1:datalength], '%', sep='')),
              size = textsize*0.375,
              color = plotcol)+
    add_quantile(0.5, linewidth = 1, color = 'grey')+
    annotate(label = paste('Median ', survivaltitle[2], ':
', median, ' months (95% CI: ', lower, ' - ', upper, ')', sep=''),
             x=max(vertlines + annotateshift), y=0.75, size=textsize*0.3125,
             geom = 'text')

  plot
}


pretty_plot_for2 <- function(time, status, var, survivaltitle,
                             plotxlab, survivaltitle_suffix = NULL,
                             x_max = 60, legend_tit='Legend', curve1name='A',
                             curve2name='B',curve3name='C', curve4name='D',
                             curve1col='#963480',curve2col='#9cb640', ci=FALSE,
                             curve3col='blue', curve4col='red', x_intervals=12,
                             years=TRUE, cens_size=3.5, textsize=16,add_pvalue=TRUE
){
  tunit <- ifelse(years, 12, 1)

  plot <- survfit2(Surv(time, status)~var) %>%

    ggsurvfit(linewidth=1.25)+

    scale_color_manual(values = c(curve1col, curve2col, curve3col, curve4col),
                       labels = c(curve1name, curve2name, curve3name, curve4name))+
    scale_fill_manual(values = c(curve1col, curve2col, curve3col, curve4col),
                      labels = c(curve1name, curve2name, curve3name, curve4name))+
    labs(title = paste(survivaltitle[1], 'Survival', survivaltitle_suffix)) +
    xlab(plotxlab) + scale_y_continuous(
      limits = c(0, 1),
      labels = scales::percent,
      expand = c(0.01, 0))+

    add_censor_mark(size = cens_size, shape = "|")+
    {if(ci)add_confidence_interval(type = 'ribbon')}+

    scale_x_continuous(breaks = seq(0, x_max, by = x_intervals),
                       limits = c(0, NA),
                       labels = seq(0, x_max/x_intervals, by = 1)*(x_intervals)/tunit,
                       expand = c(0.01, 0.01)) +



    {if(add_pvalue)add_pvalue(caption = "Log-rank {p.value}",
                              location = "annotation",
                              x = 0.85*x_max, y = 0.85,
                              size = textsize/4)} +
    add_legend_title(legend_tit)+

    theme(plot.title = element_text(size=textsize),
          axis.title = element_text(size = textsize*0.5625),
          axis.text.x = element_text(size = textsize*0.6875),
          axis.text.y = element_text(size = textsize*0.5625),
          legend.text = element_text(size = textsize*0.5625),
          legend.title = element_text(size = textsize*0.5625)
    )+

    theme(panel.grid = element_blank())
  plot
}

tb <- function(x, name, sort=TRUE, pick=NA){
  library(dplyr)
  d <- data.frame(table(x), stringsAsFactors = FALSE) %>%
    `colnames<-`(c('variable', 'n'))
  if(sort){d <- sb(d,'n', decreasing = TRUE)}
  d <- d %>% mutate(n=paste(n, ' (', round(100*n/length(x),1), '%)',sep=''))
  d <- rbind(data.frame(variable=paste('**',name, '**', sep=''), n=''),d)
  if(all(is.na(pick))){d}else{
    r <- match(pick, d[,1])
    d <- rbind(d[1,],d[r,])
    d
  }
}

tbcon <- function(x, name, range=TRUE, mean=FALSE){
  rbind(data.frame(variable=paste('**',name, '**', sep=''), n=''),
        if(mean){
          data.frame(variable='Mean (SD)',
                     n=paste(round(mean(x, na.rm=TRUE),2), ' (',
                             round(sd(x),2), ')', sep=''))}else{
        if(range){
         data.frame(variable='Median (range)',
         n=paste(round(median(x, na.rm=TRUE),2), ' (',
                 round(min(x, na.rm=TRUE),2), ' - ',
                 round(max(x, na.rm=TRUE),2), ')', sep=''))}else{
         data.frame(variable='Median (IQR)',
                    n=paste(round(median(x, na.rm=TRUE),2), ' (',
                            round(quantile(x, 0.25),2), ' - ',
                            round(quantile(x, 0.75),2), ')', sep=''))}})

}

fisherizer <- function(df, x, y, name='Name', bold=TRUE, chisq=FALSE, rmp=FALSE){
  library(dplyr)
  df0 <- df[df[,y]==0,]
  df1 <- df[df[,y]==1,]
  df2 <- df

  x2 <- as.data.frame(table(df2[,x]))
  x1 <- as.data.frame(table(df1[,x]))
  x0 <- as.data.frame(table(df0[,x]))
  result <- merge(x2, x1, all.x = TRUE, all.y = TRUE, by.x = 'Var1', by.y = 'Var1')
  result <- merge(result, x0, all.x = TRUE, all.y = TRUE, by.x = 'Var1', by.y = 'Var1')
  result[is.na(result)] <- 0

  p <- ifelse(chisq, chisq.test(result[,3:4])[[3]], fisher.test(result[,3:4])[[1]])
  p <- round(p, 5)

  if(bold){
    p <- paste(ifelse(p<0.05, '**', ''), p, ifelse(p<0.05, '**', ''), sep='')
  }

  p <- ifelse(rmp, ' ', p)

  result <- cbind(result, c(rep('', times=nrow(result)-1),p))
  result <- result %>% mutate(Freq.x=paste(Freq.x, ' (', round(100*Freq.x/sum(result$Freq.x),1), '%)',sep=''),
                              Freq.y=paste(Freq.y, ' (', round(100*Freq.y/sum(result$Freq.y),1), '%)',sep=''),
                              Freq=paste(Freq, ' (', round(100*Freq/sum(result$Freq),1), '%)',sep=''))

  colnames(result) <- c('value', 'All', paste(y,'=1'), paste(y,'=0'), 'p-value')

  remove(df0, df1, df2, x0, x1, x2)
  nam <- t(data.frame(c(paste('**',name, '**', sep=''),rep('',4))))
  colnames(nam) <- colnames(result)
  rbind(nam,result)

}

wilcoxinator <- function(df, x, by, name='Name', bold=TRUE, range=FALSE, round=2){
  library(dplyr)
  df0 <- df[df[,by]==0,]
  df1 <- df[df[,by]==1,]

  x2 <- na.omit(df[,x])
  x0 <- na.omit(df0[,x])
  x1 <- na.omit(df1[,x])

  wil <- wilcox.test(x0, x1)[[3]]
  wil <- round(wil,5)
  if(bold){
    wil <- paste(ifelse(wil<0.05, '**', ''), wil, ifelse(wil<0.05, '**', ''), sep='')
  }

  rnd <- function(x){round(x, round)}

result <- list(data.frame('value'= 'median (range)',
                            'All'=paste(median(x2) %>% rnd,
                                        ' (',
                                        min(x2) %>% rnd,
                                        '-',
                                        max(x2) %>% rnd,
                                        ')',sep=''),
                            'by+'=paste(median(x1) %>% rnd,
                                       ' (',
                                       min(x1) %>% rnd,
                                       '-',
                                       max(x1) %>% rnd,
                                       ')',sep=''),
                            'by-'=paste(median(x0) %>% rnd,
                                       ' (',
                                       min(x0) %>% rnd,
                                       '-',
                                       max(x0) %>% rnd,
                                       ')',sep=''),
                            'pvalue'=wil),
                 data.frame('value'='median (IQR)',
                            'All'=paste(median(x2) %>% rnd,
                                        ' (',
                                        quantile(x2, 0.25) %>% rnd,
                                        '-',
                                        quantile(x2, 0.75) %>% rnd,
                                        ')',sep=''),
                            'by+'=paste(median(x1) %>% rnd,
                                       ' (',
                                       quantile(x1, 0.25) %>% rnd,
                                       '-',
                                       quantile(x1, 0.75) %>% rnd,
                                       ')',sep=''),
                            'by-'=paste(median(x0) %>% rnd,
                                       ' (',
                                       quantile(x0, 0.25) %>% rnd,
                                       '-',
                                       quantile(x0, 0.75) %>% rnd,
                                       ')',sep=''),
                            'pvalue'=wil)
                 )[[2-range]]

   colnames(result) <- c('value', 'All', paste(by,'=1'), paste(by,'=0'), 'p-value')
   remove(df0, df1, x0, x1, x2)
   nam <- t(data.frame(c(paste('**',name, '**', sep=''),rep('',4))))
   colnames(nam) <- colnames(result)
   rbind(nam,result)
}

# Modified swimplot::swimmer_plot
swimmer_plot_piotra <- function (df, id = "id", end = "end", start = "start", name_fill = NULL,
                                 name_col = NULL, name_alpha = NULL, increasing = TRUE, id_order = NULL,
                                 stratify = FALSE, base_size = 11, identifiers = TRUE, ncol=1,...)
{
  if (!is.null(id_order)) {
    if (id_order[1] %in% c("increasing", "decreasing")) {
      warning("Increasing/decreasing have been deprecated as options for id_order use increasing=TRUE or increasing=FALSE instead",
              call. = FALSE)
      if (id_order[1] == "increasing")
        increasing = TRUE
      if (id_order[1] == "decreasing")
        increasing = FALSE
      id_order = NULL
    }
  }
  df[, id] <- as.character(df[, id])
  if (is.null(id_order)) {
    max_df <- stats::aggregate(df[, end] ~ df[, id], FUN = max,
                               na.rm = T)
    names(max_df) <- c(id, "MAX_TIME_FOR_EACH_ID")
    if (increasing) {
      id_order <- max_df[order(max_df$MAX_TIME_FOR_EACH_ID),
                         id]
    }
    else id_order <- max_df[order(max_df$MAX_TIME_FOR_EACH_ID,
                                  decreasing = T), id]
  }
  if (id_order[1] %in% names(df)) {
    max_df <- stats::aggregate(df[, end] ~ df[, id], FUN = max,
                               na.rm = T)
    names(max_df) <- c(id, "MAX_TIME_FOR_EACH_ID")
    merged_df_with_max <- merge(max_df, df, all = F)
    starting_df <- stats::aggregate(df[, end] ~ df[, id],
                                    FUN = min, na.rm = T)
    names(starting_df) <- c(id, end)
    starting_information <- merge(starting_df, merged_df_with_max,
                                  all = F)
    if (increasing) {
      id_order <- starting_information[order(starting_information[,
                                                                  id_order[1]], -rank(starting_information$MAX_TIME_FOR_EACH_ID),
                                             decreasing = TRUE), id]
    }
    else id_order <- starting_information[order(starting_information[,
                                                                     id_order[1]], rank(starting_information$MAX_TIME_FOR_EACH_ID),
                                                decreasing = TRUE), id]
  }
  df <- df[order(df[, id], df[, end]), ]
  if (start %in% names(df)) {
    add_in <- function(id_fix, df, start, end) {
      df_fix <- df[df[, id] == id_fix, ]
      end_blank <- df_fix[, start][c(0, dplyr::lag(df_fix[,
                                                          end])[-1]) != df_fix[, start]]
      start_blank <- c(0, dplyr::lag(df_fix[, end])[-1])[c(0,
                                                           dplyr::lag(df_fix[, end])[-1]) != df_fix[, start]]
      df_fixed <- data.frame(id_fix, start_blank, end_blank)
      names(df_fixed) <- c(id, start, end)
      merge(df_fixed, df_fix, all = T)
    }
    df <- do.call(rbind.data.frame, sapply(unique(df[, id]),
                                           add_in, df = df, start = start, end = end, simplify = F))
  }
  else {
    start = "starting_bars_variable"
    df$starting_bars_variable <- stats::ave(df[, end], df[,
                                                          id], FUN = dplyr::lag)
    df$starting_bars_variable[is.na(df$starting_bars_variable)] <- 0
  }
  temp_end <- df[, end] - stats::ave(df[, end], df[, id], FUN = dplyr::lag)
  df[, end][!is.na(temp_end)] <- temp_end[!is.na(temp_end)]
  df <- data.frame(df)
  starting_times <- sort(unique(as.numeric(as.character(df[, start]))), decreasing = TRUE)
  df[, start] <- factor(df[, start], starting_times)
  df[, id] <- factor(df[, id], levels = id_order)
  plot <- ggplot2::ggplot(df) + ggplot2::geom_col(position = "stack",
                                                  ggplot2::aes_string(fill = name_fill, col = name_col,
                                                                      alpha = name_alpha, group = start, x = id, y = end),
                                                  ...) + ggplot2::coord_flip() + ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank())
  if (stratify[1] != FALSE)
    plot <- plot + ggplot2::facet_wrap(stats::as.formula(paste("~",
                                                               paste(stratify, collapse = "+"))), scales = "free_y", ncol = ncol) +
    ggplot2::theme(strip.background = ggplot2::element_rect(colour = "black",
                                                            fill = "white"))
  if (identifiers == FALSE)
    plot <- plot + ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                                  axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank())
  return(plot)
}

#  This is a modification of a function 'ggforest' from the package survminer_0.4.9

ggforest_piotra <- function (model, label_map, data = NULL, main = "Hazard ratio", cpositions = c(0.02,
                                                                                                  0.22, 0.4), fontsize = 0.7, refLabel = "reference", noDigits = 2, annotations=FALSE)
{
  conf.high <- conf.low <- estimate <- NULL
  stopifnot(inherits(model, "coxph"))
  data <- survminer:::.get_data(model, data = data)
  terms <- attr(model$terms, "dataClasses")[-1]
  coef <- as.data.frame(tidy(model, conf.int = TRUE))
  gmodel <- glance(model)
  allTerms <- lapply(seq_along(terms), function(i) {
    var <- names(terms)[i]
    if (terms[i] %in% c("factor", "character")) {
      adf <- as.data.frame(table(data[, var]))
      cbind(var = var, adf, pos = 1:nrow(adf))
    }
    else if (terms[i] == "numeric") {
      data.frame(var = var, Var1 = "", Freq = nrow(data),
                 pos = 1)
    }
    else {
      vars = grep(paste0("^", var, "*."), coef$term, value = TRUE)
      data.frame(var = vars, Var1 = "", Freq = nrow(data),
                 pos = seq_along(vars))
    }
  })
  allTermsDF <- do.call(rbind, allTerms)
  colnames(allTermsDF) <- c("var", "level", "N", "pos")
  inds <- apply(allTermsDF[, 1:2], 1, paste0, collapse = "")
  rownames(coef) <- gsub(coef$term, pattern = "`", replacement = "")
  toShow <- cbind(allTermsDF, coef[inds, ])[, c("var", "level",
                                                "N", "p.value", "estimate", "conf.low", "conf.high",
                                                "pos")]
  toShowExp <- toShow[, 5:7]
  toShowExp[is.na(toShowExp)] <- 0
  toShowExp <- format(exp(toShowExp), digits = noDigits)
  toShowExpClean <- data.frame(toShow, pvalue = signif(toShow[,
                                                              4], noDigits + 1), toShowExp)
  toShowExpClean$stars <- paste0(round(toShowExpClean$p.value,
                                       noDigits + 1), " ", ifelse(toShowExpClean$p.value < 0.05,
                                                                  "*", ""), ifelse(toShowExpClean$p.value < 0.01, "*",
                                                                                   ""), ifelse(toShowExpClean$p.value < 0.001, "*", ""))
  toShowExpClean$ci <- paste0("(", toShowExpClean[, "conf.low.1"],
                              " - ", toShowExpClean[, "conf.high.1"], ")")
  toShowExpClean$estimate.1[is.na(toShowExpClean$estimate)] = refLabel
  toShowExpClean$stars[which(toShowExpClean$p.value < 0.001)] = "<0.001 ***"
  toShowExpClean$stars[is.na(toShowExpClean$estimate)] = ""
  toShowExpClean$ci[is.na(toShowExpClean$estimate)] = ""
  toShowExpClean$estimate[is.na(toShowExpClean$estimate)] = 0
  toShowExpClean$var = as.character(toShowExpClean$var)
  toShowExpClean$var[duplicated(toShowExpClean$var)] = ""
  toShowExpClean$N <- paste0("(N=", toShowExpClean$N, ")")
  toShowExpClean <- toShowExpClean[nrow(toShowExpClean):1,
  ]
  rangeb <- range(toShowExpClean$conf.low, toShowExpClean$conf.high,
                  na.rm = TRUE)
  breaks <- axisTicks(rangeb/2, log = TRUE, nint = 7)
  rangeplot <- rangeb
  rangeplot[1] <- rangeplot[1] - diff(rangeb)
  rangeplot[2] <- rangeplot[2] + 0.15 * diff(rangeb)
  width <- diff(rangeplot)
  y_variable <- rangeplot[1] + cpositions[1] * width
  y_nlevel <- rangeplot[1] + cpositions[2] * width
  y_cistring <- rangeplot[1] + cpositions[3] * width
  y_stars <- rangeb[2]
  x_annotate <- seq_len(nrow(toShowExpClean))
  annot_size_mm <- fontsize * as.numeric(grid::convertX(unit(theme_get()$text$size,
                                                             "pt"), "mm"))

  var_names <- label_map[toShowExpClean$var]
  replace(var_names, is.na(var_names), '')
  toShowExpClean$var <- var_names
  p <- ggplot(toShowExpClean, aes(seq_along(var), exp(estimate))) +
    geom_rect(aes(xmin = seq_along(var) - 0.5, xmax = seq_along(var) +
                    0.5, ymin = exp(rangeplot[1]), ymax = exp(rangeplot[2]),
                  fill = ordered(seq_along(var)%%2 + 1))) + scale_fill_manual(values = c("#FFFFFF33",
                                                                                         "#00000033"), guide = "none") + geom_point(pch = 15,
                                                                                                                                    size = 4) + geom_errorbar(aes(ymin = exp(conf.low), ymax = exp(conf.high)),
                                                                                                                                                              width = 0.15) + geom_hline(yintercept = 1, linetype = 3) +
    coord_flip(ylim = exp(rangeplot)) + ggtitle(main) + scale_y_log10(name = "",
                                                                      labels = sprintf("%g", breaks), expand = c(0.02, 0.02),
                                                                      breaks = breaks) + theme_light() + theme(panel.grid.minor.y = element_blank(),
                                                                                                               panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(),
                                                                                                               legend.position = "none", panel.border = element_blank(),
                                                                                                               axis.title.y = element_blank(), axis.text.y = element_blank(),
                                                                                                               axis.ticks.y = element_blank(), plot.title = element_text(hjust = 0.5)) +
    xlab("") + annotate(geom = "text", x = x_annotate, y = exp(y_variable),
                        label = toShowExpClean$var, fontface = "bold", hjust = 0,
                        size = annot_size_mm) + annotate(geom = "text", x = x_annotate,
                                                         y = exp(y_nlevel), hjust = 0, label = toShowExpClean$level,
                                                         vjust = -0.1, size = annot_size_mm) + annotate(geom = "text",
                                                                                                        x = x_annotate, y = exp(y_nlevel), label = toShowExpClean$N,
                                                                                                        fontface = "italic", hjust = 0, vjust = ifelse(toShowExpClean$level ==
                                                                                                                                                         "", 0.5, 1.1), size = annot_size_mm) + annotate(geom = "text",
                                                                                                                                                                                                         x = x_annotate, y = exp(y_cistring), label = toShowExpClean$estimate.1,
                                                                                                                                                                                                         size = annot_size_mm, vjust = ifelse(toShowExpClean$estimate.1 ==
                                                                                                                                                                                                                                                "reference", 0.5, -0.1)) + annotate(geom = "text",
                                                                                                                                                                                                                                                                                    x = x_annotate, y = exp(y_cistring), label = toShowExpClean$ci,
                                                                                                                                                                                                                                                                                    size = annot_size_mm, vjust = 1.1, fontface = "italic") +
    annotate(geom = "text", x = x_annotate, y = exp(y_stars),
             label = toShowExpClean$stars, size = annot_size_mm,
             hjust = -0.2, fontface = "italic")
  if (annotations) {p <- p +annotate(geom = "text",
                                     x = 0.5, y = exp(y_variable), label = paste0("# Events: ",
                                                                                  gmodel$nevent, "; Global p-value (Log-Rank): ", format.pval(gmodel$p.value.log,
                                                                                                                                              eps = ".001"), " \nAIC: ", round(gmodel$AIC,
                                                                                                                                                                               2), "; Concordance Index: ", round(gmodel$concordance,
                                                                                                                                                                                                                  2)), size = annot_size_mm, hjust = 0, vjust = 1.2,
                                     fontface = "italic")}
  gt <- ggplot_gtable(ggplot_build(p))
  gt$layout$clip[gt$layout$name == "panel"] <- "off"
  ggpubr::as_ggplot(gt)
}
