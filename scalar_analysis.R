rm(list=ls())
#install.packages("tidyr")
#install.packages("dplyr")
#install.packages("scales")
#install.packages("ggplot2")
#install.packages("gt")
library(gt)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)
library(modelsummary)
library(flextable)

dir.create("out", showWarnings = FALSE, recursive = TRUE)

# load merged monthly scalars
merged_scalar <- readRDS("data/merged_scalar_monthly.rds")

# annual aggregation
annual <- merged_scalar %>%
  mutate(year = as.integer(format(ym, "%Y"))) %>%
  group_by(year) %>%
  summarise(
    queries_total = sum(query_total, na.rm = TRUE),
    .groups = "drop"
  )

# 1. Queries ---------------------------------------------------------------------------------------------------------------------

# 1.1 Queries - public plot

x_min <- min(annual$year, na.rm = TRUE)
x_max <- max(annual$year, na.rm = TRUE)

year_lines <- tibble(year = seq(x_min, x_max, by = 1))

# plot in billions (keeps the same “clean axis” feel)
annual_q <- annual %>%
  mutate(queries_b = queries_total / 1e9)

# choose y grid to look like your other plot
y_max  <- ceiling(max(annual_q$queries_b, na.rm = TRUE) * 2) / 2  # rounds to 0.5
y_step <- 0.5                                                    # similar density

p_queries <- ggplot(annual_q, aes(x = year, y = queries_b)) +
  geom_vline(
    data = year_lines,
    aes(xintercept = year),
    color = "gray90",
    linewidth = 0.4
  ) +
  geom_hline(
    yintercept = seq(0, y_max, by = y_step),
    color = "gray90",
    linewidth = 0.4
  ) +
  geom_line(
    aes(color = "Queries"),
    linewidth = 0.5
  ) +
  geom_point(
    aes(color = "Queries"),
    size = 2
  ) +
  scale_color_manual(
    values = c("Queries" = "black"),
    name = NULL
  ) +
  scale_x_continuous(breaks = seq(x_min, x_max, by = 2)) +
  scale_y_continuous(
    limits = c(0, y_max),
    labels = label_number(suffix = " B", accuracy = 0.1)
  ) +
  labs(
    #title = "Annual volume of X-Road queries",
    #subtitle = "Combined series of various X-Road versions",
    x = "Year",
    y = "Volume (billions)",
    #caption = "Source: X-Road operational statistics. Analytics pipeline by Kristjan Vassil, 2026"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    aspect.ratio = 0.9,
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    plot.caption = element_text(size = 9, color = "gray40"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 11),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

print(p_queries)

ggsave(
  "out/xtee_annual_queries.png",
  p_queries,
  dpi = 300,
  bg = "white",
  width = 6,
  height = 5.5
)

ggsave(
  "out/plosone/Fig1.tif",
  p_queries,
  device      = "tiff",
  type        = "cairo",
  compression = "lzw",
  dpi         = 300,
  bg          = "white",
  width       = 6,
  height      = 5.5,
  units       = "in"
) 


# 2. Services ---------------------------------------------------------------------------------------------------------------------

# 2.1 Services (raw)---------------------------------------------------------------------------------------------------------------------

# monthly services_total (stock => max over old/v6)
merged_scalar <- merged_scalar %>%
  mutate(
    service_total = pmax(service_count_old, service_count_v6, na.rm = TRUE),
    year = as.integer(format(ym, "%Y"))
  )

# annual services (stock => end-of-year level, i.e., last available month)
annual_services <- merged_scalar %>%
  group_by(year) %>%
  arrange(ym, .by_group = TRUE) %>%
  summarise(
    services_eoy = service_total[which.max(ym)],
    .groups = "drop"
  )

##Plot simple raw services (with data holes)

services_holes <- ggplot(annual_services, aes(x = year, y = services_eoy)) +
  geom_line() +
  geom_point()

print(services_holes)


# 2.2 Services (imputed 2013, 2017)----------------------------------------------------------------------------------------------------------------

annual_services <- annual_services %>%
  arrange(year) %>%
  mutate(
    services_eoy_imputed = services_eoy
  )

# impute 2013 using mean(2012, 2014)
annual_services$services_eoy_imputed[annual_services$year == 2013] <-
  mean(
    annual_services$services_eoy[annual_services$year %in% c(2012, 2014)],
    na.rm = TRUE
  )

# impute 2017 using mean(2016, 2018)
annual_services$services_eoy_imputed[annual_services$year == 2017] <-
  mean(
    annual_services$services_eoy[annual_services$year %in% c(2016, 2018)],
    na.rm = TRUE
  )

#plot simple

services_imputed <- ggplot(annual_services, aes(x = year, y = services_eoy_imputed)) +
  geom_line() +
  geom_point()

print(services_imputed)


# 3. Producers (Data repositories)------------------------------------------------------------------------------------------------------------- 
 
 
 # monthly producer_total (stock => max over old/v6)
 merged_scalar <- merged_scalar %>%
   mutate(
     producer_total = pmax(producer_count_old, producer_count_v6, na.rm = TRUE),
     year = as.integer(format(ym, "%Y"))
   )
 
 # annual EOY series
 annual_producers <- merged_scalar %>%
   group_by(year) %>%
   arrange(ym, .by_group = TRUE) %>%
   summarise(
     producers_eoy = producer_total[which.max(ym)],
     .groups = "drop"
   ) %>%
   arrange(year) %>%
   mutate(producers_eoy_imputed = producers_eoy)
 
 
 # optional: impute 2013 and 2017
 annual_producers$producers_eoy_imputed[annual_producers$year == 2013] <-
   mean(annual_producers$producers_eoy[annual_producers$year %in% c(2012, 2014)], na.rm = TRUE)
 
 annual_producers$producers_eoy_imputed[annual_producers$year == 2017] <-
   mean(annual_producers$producers_eoy[annual_producers$year %in% c(2016, 2018)], na.rm = TRUE)
 
 #plot (simple)
 
 #not imputed
 ggplot(annual_producers, aes(x = year, y = producers_eoy)) +
   geom_line() +
   geom_point()
 
 #imputed
  ggplot(annual_producers, aes(x = year, y = producers_eoy_imputed)) +
   geom_line() +
   geom_point()
 
 
# 4. Combined graph for services and producers ------------------------------------------------------------------------------------------------------------- 
 
 annual_sp <- annual_services %>%
   select(year, services = services_eoy_imputed) %>%
   left_join(
     annual_producers %>%
       select(year, producers = producers_eoy_imputed),
     by = "year"
   )
 
 annual_long <- annual_sp %>%
   pivot_longer(
     cols = c(services, producers),
     names_to = "series",
     values_to = "value"
   )
 
 # Plot

 annual_sp_long <- annual_services %>%
   select(year, value = services_eoy_imputed) %>%
   mutate(series = "Services") %>%
   bind_rows(
     annual_producers %>%
       select(year, value = producers_eoy_imputed) %>%
       mutate(series = "Data repositories")
   )
 
 
 p_services_producers <- ggplot(annual_sp_long, aes(x = year, y = value)) +
   geom_vline(
     data = year_lines,
     aes(xintercept = year),
     color = "gray90",
     linewidth = 0.4
   ) +
   geom_hline(
     yintercept = seq(0, 4000, by = 500),
     color = "gray90",
     linewidth = 0.4
   ) +
   geom_line(
     aes(color = series),
     linewidth = 0.5
   ) +
   geom_point(
     aes(color = series),
     size = 2
   ) +
   scale_color_manual(
     values = c(
       "Data repositories" = "grey60",
       "Services"  = "black"
     ),
     name = NULL
   ) +
   scale_x_continuous(breaks = seq(x_min, x_max, by = 2)) +
   scale_y_continuous(limits = c(0,4000), labels = label_number(big.mark = " ")) +
   labs(
     #title = "X-Road services and data repositories",
     #subtitle = "End-of-year counts (imputed where missing)",
     x = "Year",
     y = "Volume",
     linetype = NULL,
     shape = NULL,
     #caption = "Source: X-Road operational statistics. Analytics pipeline by Kristjan Vassil, 26.01.2026"
   ) +
   labs(color = NULL) +
   theme_minimal(base_size = 13) +
   theme(
     aspect.ratio = 0.9,
     plot.title = element_text(face = "bold", size = 16),
     plot.subtitle = element_text(size = 12, color = "gray40"),
     plot.caption = element_text(size = 9, color = "gray40"),
     axis.title = element_text(size = 12),
     axis.text = element_text(size = 10),
     panel.grid.major.x = element_blank(),
     panel.grid.minor.x = element_blank(),
     panel.grid.minor.y = element_blank(),
     legend.position = "bottom",
     legend.text = element_text(size = 11),
     plot.background  = element_rect(fill = "white", color = NA),
     panel.background = element_rect(fill = "white", color = NA)
   )
 
 print(p_services_producers)
 
 ggsave(
   "out/xtee_annual_services_producers.png",
   p_services_producers,
   dpi = 300,
   bg = "white",
   width = 6,
   height = 5.5
 )  
 
 ggsave(
   "out/plosone/Fig2.tif",
   p_services_producers,
   device      = "tiff",
   type        = "cairo",
   compression = "lzw",
   dpi         = 300,
   bg          = "white",
   width       = 6,
   height      = 5.5,
   units       = "in"
 ) 
 
 
 #5. Queries per service - annual load------------------------------------------------------------------------------------------------------------------------------------X!X- 
 
 annual_merged <- annual %>%
   select(year, queries_total) %>%
   left_join(
     annual_services %>% 
       select(year, services_eoy_imputed),
     by = "year"
   )
 
 # inspect
 # print(annual_merged)
 # str(annual_merged)
 # 
 annual_merged <- annual_merged %>%
   mutate(
     queries_per_service = queries_total / services_eoy_imputed
   )

 p_qps <- ggplot(
   annual_merged,
   aes(x = year, y = queries_per_service)
 ) +
   geom_vline(
     data = year_lines,
     aes(xintercept = year),
     color = "gray90",
     linewidth = 0.4
   ) +
   geom_hline(
     yintercept = seq(0, y_max, by = y_step),
     color = "gray90",
     linewidth = 0.4
   ) +
   geom_line(
     #aes(color = "Queries"),
     color ="black",
     linewidth = 0.5
   ) +
   geom_point(
     #aes(color = "Queries"),
     size = 2
   ) +
   # scale_color_manual(
   #   values = c("Queries" = "black"),
   #   name = NULL
   # ) +
   scale_x_continuous(breaks = seq(x_min, x_max, by = 2)) +
   scale_y_continuous(
  #    limits = c(0, 1000000),
      labels = label_number()
    ) +
   labs(
    #title = "Annual service load: queries per service",
     #subtitle = "y=queries/services",
     x = "Year",
     y = "Queries per service",
    # caption = "Source: X-Road operational statistics. Analytics by Kristjan Vassil, 2026"
   ) +
   theme_minimal(base_size = 13) +
   theme(
     aspect.ratio = 0.9,
     plot.title = element_text(face = "bold", size = 16),
     plot.subtitle = element_text(size = 12, color = "gray40"),
     plot.caption = element_text(size = 9, color = "gray40"),
     axis.title = element_text(size = 12),
     axis.text = element_text(size = 10),
     panel.grid.major.x = element_blank(),
     panel.grid.minor.x = element_blank(),
     panel.grid.minor.y = element_blank(),
     legend.position = "bottom",
     legend.text = element_text(size = 11),
     plot.background  = element_rect(fill = "white", color = NA),
     panel.background = element_rect(fill = "white", color = NA)
   )
 
 print(p_qps)
 
 ggsave(
   "out/xtee_annual_queries_per_services.png",
   p_qps,
   dpi = 300,
   bg = "white",
   width = 6,
   height = 5.5
 )   
 
 ggsave(
   "out/plosone/Fig4.tif",
   p_qps,
   device      = "tiff",
   type        = "cairo",
   compression = "lzw",
   dpi         = 300,
   bg          = "white",
   width       = 6,
   height      = 5.5,
   units       = "in"
 )

 

 # #Explore ratios between queries-services and queries-repositories
 # 
 # 
  df_q_services <- annual %>%
    inner_join(
      annual_services,
      by = "year"
    )
  
 # p_q_services <- ggplot(
 #   df_q_services,
 #   aes(
 #     x = services_eoy_imputed,
 #     y = queries_total
 #   )
 # ) +
 #   geom_point(size = 3, color = "black") +
 #   geom_smooth(
 #     method = "lm",
 #     formula = y ~ exp(x),
 #     se = FALSE,
 #     linewidth = 0.8
 #   ) +
 #   scale_y_continuous(labels = label_number(scale = 1e-9, suffix = "B")) +
 #   scale_x_continuous(labels = label_number()) +
 #   labs(
 #     x = "Service stock (end-of-year)",
 #     y = "Total X-Road queries (annual)",
 #     title = "Annual association between X-Road queries and service stock"
 #   ) +
 #   theme_minimal()
 # 
 # p_q_services
 # 
 # library(dplyr)
 # library(ggplot2)
 # library(scales)
 
   
  df_q_producers <- annual %>%
    inner_join(annual_producers, by = "year")
  df_q_producers <- annual %>%
    inner_join(annual_producers, by = "year")
 
  
 # p_q_producers_linear <- ggplot(
 #   df_q_producers,
 #   aes(
 #     x = producers_eoy_imputed,
 #     y = queries_total
 #   )
 # ) +
 #   geom_point(size = 3, color = "black") +
 #   geom_smooth(
 #     method = "lm",
 #     se = FALSE,
 #     linewidth = 0.8
 #   ) +
 #   scale_y_continuous(
 #     labels = label_number(scale = 1e-9, suffix = "B")
 #   ) +
 #   scale_x_continuous(labels = label_number()) +
 #   labs(
 #     x = "Data repositories (end-of-year)",
 #     y = "Total X-Road queries (annual)",
 #     title = "Annual association between X-Road queries and data repositories (linear)"
 #   ) +
 #   theme_minimal()
 # 
 # p_q_producers_linear
 # 
 # ##
 # library(dplyr)
 # library(ggplot2)
 # library(scales)
 # 
 df_prod_services <- annual_producers %>%
   inner_join(
     annual_services,
     by = "year"
   )
 # 
 # p_prod_services <- ggplot(
 #   df_prod_services,
 #   aes(
 #     x = producers_eoy_imputed,
 #     y = services_eoy_imputed
 #   )
 # ) +
 #   geom_point(size = 3, color = "black") +
 #   geom_smooth(
 #     method = "lm",
 #     formula = y ~ log(x),
 #     se = FALSE,
 #     linewidth = 0.9
 #   ) +
 #   scale_x_continuous(labels = label_number()) +
 #   scale_y_continuous(labels = label_number()) +
 #   labs(
 #     x = "Data repositories (end-of-year)",
 #     y = "Service stock (end-of-year)",
 #     title = "Annual association between data repositories and service stock"
 #   ) +
 #   theme_minimal()
 # 
 # p_prod_services
 
 ##Marginal services per repository---------------------------------------------------------------------------------------------------------------------------------------
 
 df_prod_services <- annual_producers %>%
   inner_join(annual_services, by = "year")
 
 m_log <- lm(
   services_eoy_imputed ~ log(producers_eoy_imputed),
   data = df_prod_services
 )
 
 summary(m_log)
 
  modelsummary(
   m_log,
   coef_map = c(
     "(Intercept)" = "Constant",
     "log(producers_eoy_imputed)" = "Log data repositories"
   ),
   statistic = "std.error",
   stars = TRUE,
   gof_map = c(
     "nobs",
     "r.squared",
     "adj.r.squared"
   ),
   output = "flextable"
 ) |>
   save_as_docx(
     path = "out/supply_side_regression.docx"
   )
  
###Plose one format table###

  plos_table <- function(model,
                         dv     = NULL,
                         labels = NULL,
                         digits = 3,
                         level  = 0.95) {
    
    s  <- summary(model)
    co <- s$coefficients
    ci <- confint(model, level = level)
    
    fmt_p <- function(p) ifelse(p < 0.001, "< 0.001", formatC(p, format = "f", digits = 3))
    fmt_n <- function(x) formatC(x, format = "f", digits = digits)
    
    term <- rownames(co)
    if (!is.null(labels)) term <- ifelse(term %in% names(labels), labels[term], term)
    
    out <- data.frame(
      Variable = term,
      Estimate = fmt_n(co[, "Estimate"]),
      SE       = fmt_n(co[, "Std. Error"]),
      CI       = paste0(fmt_n(ci[, 1]), ", ", fmt_n(ci[, 2])),
      p        = fmt_p(co[, 4]),
      row.names = NULL, stringsAsFactors = FALSE
    )
    names(out)[4] <- paste0(level * 100, "% CI")
    
    f  <- s$fstatistic
    fp <- pf(f[1], f[2], f[3], lower.tail = FALSE)
    
    note <- sprintf(
      paste0("Ordinary least squares estimates; the dependent variable is %s. n = %d. ",
             "Residual standard error %s on %d degrees of freedom. R2 = %s, adjusted R2 = %s. ",
             "F(%d, %d) = %s, p %s. Confidence intervals are Wald intervals based on the ",
             "t distribution with %d degrees of freedom."),
      if (is.null(dv)) deparse(formula(model)[[2]]) else dv,
      length(residuals(model)),
      formatC(s$sigma, format = "f", digits = 3),
      model$df.residual,
      formatC(s$r.squared,     format = "f", digits = 3),
      formatC(s$adj.r.squared, format = "f", digits = 3),
      f[2], f[3],
      formatC(f[1], format = "f", digits = 1),
      ifelse(fp < 0.001, "< 0.001", paste("=", formatC(fp, format = "f", digits = 3))),
      model$df.residual
    )
    
    structure(list(table = out, note = note), class = "plos_table")
  }
  
  print.plos_table <- function(x, ...) {
    print(x$table, row.names = FALSE, right = FALSE)
    cat("\n", strwrap(x$note, width = 100), sep = "\n")
    invisible(x)
  }
  
  t1 <- plos_table(
    m_log,
    dv     = "the cumulative number of X-Road services",
    labels = c("(Intercept)"                = "Constant",
               "log(producers_eoy_imputed)" = "Log (data repositories)")
  )
  print(t1)
  
  library(flextable)
  library(officer)
  
  plos_table_docx <- function(..., file, captions) {
    
    objs <- list(...)
    stopifnot(length(objs) == length(captions))
    
    rule <- fp_border(color = "black", width = 0.75)
    doc  <- read_docx()
    
    for (i in seq_along(objs)) {
      
      x  <- objs[[i]]
      ft <- flextable(x$table)
      ft <- add_footer_lines(ft, x$note)
      ft <- font(ft, fontname = "Times New Roman", part = "all")
      ft <- fontsize(ft, size = 11, part = "all")
      ft <- fontsize(ft, size = 10, part = "footer")
      ft <- bold(ft, part = "header")
      ft <- align(ft, j = 1,   align = "left",  part = "all")
      ft <- align(ft, j = 2:5, align = "right", part = "all")
      ft <- align(ft, align = "left", part = "footer")
      ft <- padding(ft, padding.top = 2, padding.bottom = 2, part = "all")
      ft <- border_remove(ft)
      ft <- hline_top(ft,    border = rule, part = "header")
      ft <- hline_bottom(ft, border = rule, part = "header")
      ft <- hline_bottom(ft, border = rule, part = "body")
      ft <- width(ft, j = 1:5, width = c(1.55, 1.00, 0.80, 2.05, 0.70))
      
      doc <- body_add_par(doc, captions[i])
      doc <- body_add_flextable(doc, ft)
      if (i < length(objs)) doc <- body_add_par(doc, "")
    }
    
    print(doc, target = file)
    invisible(file)
  }
  
  plos_table_docx(
    t1,
    file     = "out/plosone/Tables.docx",
    captions = "Table 1. Supply-side model: services regressed on log data repositories."
  )
  
  
  
###
 
 
 ###get table out
 
 beta <- coef(m_log)[["log(producers_eoy_imputed)"]]
 
 df_marginal <- df_prod_services %>%
   mutate(
     marginal_services = beta / producers_eoy_imputed
   )
 
 p_marginal <- ggplot(
   df_marginal,
   aes(
     x = producers_eoy_imputed,
     y = marginal_services
   )
 ) +
   geom_point(size = 2, color = "black") +
   geom_line(linewidth = 0.9) +
   geom_hline(
     yintercept = c(1, 5, 10),
     linetype = "dashed",
     color = "grey70",
     linewidth = 0.5
   )+
   geom_text(
     data = data.frame(y = c(1, 5, 10)),
     aes(
       x = Inf,
       y = y,
       label = y
     ),
     hjust = 1.5,
     vjust = -1,
     color = "grey50",
     size = 3
   ) +
   geom_vline(
     xintercept = c(100, 200),
     linetype = "dotted",
     color = "grey70",
     linewidth = 0.5
   ) +
   geom_text(
     data = data.frame(x = c(100, 200), label = c("100", "200")),
     aes(x = x, y = Inf, label = label),
     vjust = 5,
     color = "grey50",
     size = 3
   ) +
    #scale_x_log10() +
   scale_x_continuous(labels = label_number()) +
   scale_y_continuous(labels = label_number(accuracy = 0.1)) +
   labs(
     x = "Data repositories",
     y = "Marginal service yield per added repository",
     #title = "Marginal service yield as a function of data repository expansion"
   ) +
   theme_minimal()
 
 p_marginal
 
  ggsave(
   "out/xtee_marginal_services.png",
   p_marginal,
   dpi = 300,
   bg = "white",
   width = 6,
   height = 5.5
 )   
  
  ggsave(
    "out/plosone/Fig3.tif",
    p_marginal,
    device      = "tiff",
    type        = "cairo",
    compression = "lzw",
    dpi         = 300,
    bg          = "white",
    width       = 6,
    height      = 5.5,
    units       = "in"
  )
 
 #Extract table for marginal services
  # library(gt)
  # table_marginal %>%
 #   gt() %>%
 #   fmt_number(
 #     columns = c(
 #       `Data repositories (EOY)`,
 #       `Service stock (EOY)`
 #     ),
 #     decimals = 0
 #   ) %>%
 #   fmt_number(
 #     columns = `Marginal services per repository`,
 #     decimals = 1
 #   ) %>%
 #   tab_header(
 #     title = "Marginal service yield of data repositories"
 #   )
 # 
 # install.packages("writexl")
 # library(writexl)
 # 
 # write_xlsx(
 #   table_marginal,
 #   "marginal_services_table.xlsx"
 # )
 # 
# -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
 #Why marginal services instead of averages
   
 #   df_compare <- df_marginal %>%
 #   mutate(
 #     avg_services = services_eoy_imputed / producers_eoy_imputed
 #   ) %>%
 #   select(
 #     year,
 #     producers_eoy_imputed,
 #     avg_services,
 #     marginal_services
 #   )
 # 
 # df_long <- df_compare %>%
 #   tidyr::pivot_longer(
 #     cols = c(avg_services, marginal_services),
 #     names_to = "metric",
 #     values_to = "value"
 #   ) %>%
 #   mutate(
 #     metric = recode(
 #       metric,
 #       avg_services = "Average services per repository",
 #       marginal_services = "Marginal services per repository"
 #     )
 #   )
 
 
 # library(ggplot2)
 # library(scales)
 # 
 # p_avg_vs_marginal <- ggplot(
 #   df_long,
 #   aes(
 #     x = producers_eoy_imputed,
 #     y = value,
 #     color = metric
 #   )
 # ) +
 #   geom_vline(
 #   xintercept = 100,
 #   linetype = "dashed",
 #   color = "gray60"
 # ) +
 #   geom_line(linewidth = 1) +
 #   geom_point(size = 3) +
 #   scale_color_manual(
 #     values = c(
 #       "Average services per repository" = "black",
 #       "Marginal services per repository" = "gray40"
 #     ),
 #     name = NULL
 #   ) +
 #   geom_vline(
 #     xintercept = 100,
 #     linetype = "dashed",
 #     color = "gray60"
 #   )
 #   scale_y_continuous(labels = label_number(accuracy = 0.1)) +
 #   labs(
 #     x = "Data repositories (end-of-year)",
 #     y = "Services per repository"
 #   ) +
 #   theme_minimal(base_size = 13) +
 #   theme(
 #     aspect.ratio = 0.8,
 #     legend.position = "bottom",
 #     panel.grid.minor = element_blank()
 #   )
 # 
 # p_avg_vs_marginal
 # 
 # ggsave(
 #   "out/xtee_threshold.png",
 #   p_avg_vs_marginal,
 #   dpi = 300,
 #   bg = "white",
 #   width = 6,
 #   height = 5.5
 # )   
 # 

  
#Demand side model
  m_log2 <- lm(
   log(queries_total) ~ log(services_eoy_imputed),
   data = df_q_services
 )
summary(m_log2)
 
 # tbl <- modelsummary(
 #   m_log,
 #   coef_map = c(
 #     "(Intercept)" = "Constant",
 #     "log(services_eoy_imputed)" = "Log services"
 #   ),
 #   statistic = "std.error",
 #   stars = TRUE,
 #   gof_map = c("nobs", "r.squared", "adj.r.squared"),
 #   output = "flextable"
 # )
 
 # save_as_docx(
 #   tbl,
 #   path = "out/demand_side_regression.docx"
 # )

##Demand side model with Plos One format###

t3 <- plos_table(
  m_log2,
  dv     = "the natural logarithm of annual X-Road queries",
  labels = c("(Intercept)"               = "Constant",
             "log(services_eoy_imputed)" = "Log (services)")
)
print(t3)

plos_table_docx(
  t1, t3,
  file     = "out/plosone/Tables.docx",
  captions = c("Table 1. Supply-side model: services regressed on log data repositories.",
               "Table 3. Demand-side model: query volumes regressed on log services.")
)

###
 
#Plot demand side log-log model effect
  
 df_q_services <- df_q_services %>%
   mutate(
     log_q_hat = predict(m_log),
     queries_hat = exp(log_q_hat)
   )
 
 p_effect <- ggplot(df_q_services,
                    aes(x = services_eoy_imputed)) +
   geom_point(
     aes(y = queries_total),
     color = "black",
     size = 2
   ) +
   geom_line(
     aes(y = queries_hat),
     color = "gray50",
     linewidth = 0.5,
     linetype = "dashed"
   ) +
   scale_y_continuous(
     labels = scales::label_number(scale = 1e-9, suffix = " B")
   ) +
   labs(
     x = "Services",
     y = "Queries (billions)",
     #caption = "Dashed line: fitted values from log–log baseline model"
   ) +
   theme_minimal(base_size = 13) +
   theme(
     aspect.ratio = 0.8,
     panel.grid.minor = element_blank()
   )
 
 print(p_effect)
 
 ggsave(
   "out/xtee_log_log_queries-services.png",
   p_effect,
   dpi = 300,
   bg = "white",
   width = 6,
   height = 5.5
 )  
 
 ggsave(
   "out/plosone/Fig5.tif",
   p_effect,
   device      = "tiff",
   type        = "cairo",
   compression = "lzw",
   dpi         = 300,
   bg          = "white",
   width       = 6,
   height      = 5.5,
   units       = "in"
 )
 
 #coef(m_log)["log(services_eoy_imputed)"]
 
# Descriptive stats----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

  # Queries
 stats <- annual_q |>
   summarise(
     N = n(),
     mean = mean(queries_total, na.rm = TRUE),
     median = median(queries_total, na.rm = TRUE),
     sd = sd(queries_total, na.rm = TRUE),
     min = min(queries_total, na.rm = TRUE),
     max = max(queries_total, na.rm = TRUE)
   )
 
 stats_label <- paste0(
   "N = ", stats$N, "\n",
   "Mean = ", round(stats$mean / 1e9, 2), " bn\n",
   "Median = ", round(stats$median / 1e9, 2), " bn\n",
   "SD = ", round(stats$sd / 1e9, 2), " bn\n",
   "Min = ", round(stats$min / 1e9, 2), " bn\n",
   "Max = ", round(stats$max / 1e9, 2), " bn"
 )
 
 mu  <- stats$mean
 med <- stats$median
 
 queries_hist <- ggplot(annual_q, aes(x = queries_total)) +
   geom_histogram(
     bins = 15,
     fill = "gray85",
     color = "black",
     linewidth = 0.4
   ) +
   geom_vline(
     xintercept = mu,
     linewidth = 0.5,
     linetype = "solid"
   ) +
   geom_vline(
     xintercept = med,
     linewidth = 0.5,
     linetype = "dashed"
   ) +
   # label mean line
   annotate(
     "text",
     x = mu,
     y = Inf,
     label = "Mean",
     angle = 90,
     vjust = -0.8,
     hjust = 2,
     size = 3.5
   ) +
   # label median line
   annotate(
     "text",
     x = med,
     y = Inf,
     label = "Median",
     angle = 90,
     vjust = -0.8,
     hjust = 1.6,
     size = 3.5
   ) +
   # descriptive stats block
   annotate(
     "text",
     x = Inf,
     y = Inf,
     label = stats_label,
     hjust = 1.05,
     vjust = 1.1,
     size = 3.5
   ) +
   scale_x_continuous(
     labels = label_number(scale = 1e-9, suffix = " bn")
   ) +
   labs(
   #  title = "Distribution of annual X-Road query volumes",
     x = "Annual X-Road queries (billions)",
     y = "Number of years"
   ) +
   theme_minimal() +
   theme(
     panel.grid.major = element_line(color = "gray90", linewidth = 0.4),
     panel.grid.minor = element_blank(),
     plot.title = element_text(face = "bold"),
     aspect.ratio = 1
   ) 
 
 print(queries_hist)
 
 ggsave(
   "out/xtee_desc_queries.png",
   queries_hist,
   dpi = 300,
   bg = "white",
   width = 6,
   height = 5.5
 )  
 
 ggsave(
   "out/plosone/S1 Fig.tif",
   queries_hist,
   device      = "tiff",
   type        = "cairo",
   compression = "lzw",
   dpi         = 300,
   bg          = "white",
   width       = 6,
   height      = 5.5,
   units       = "in"
 )
 
 # Services
 
 stats <- annual_services |>
   summarise(
     N = n(),
     mean = mean(services_eoy_imputed, na.rm = TRUE),
     median = median(services_eoy_imputed, na.rm = TRUE),
     sd = sd(services_eoy_imputed, na.rm = TRUE),
     min = min(services_eoy_imputed, na.rm = TRUE),
     max = max(services_eoy_imputed, na.rm = TRUE)
   )
 
 stats_label <- paste0(
   "N = ", stats$N, "\n",
   "Mean = ", round(stats$mean, 2), "\n",
   "Median = ", round(stats$median, 2), "\n",
   "SD = ", round(stats$sd, 2), "\n",
   "Min = ", round(stats$min, 2), "\n",
   "Max = ", round(stats$max, 2)
 )
 
 mu  <- stats$mean
 med <- stats$median
 
 services_hist <- ggplot(annual_services, aes(x = services_eoy_imputed)) +
   geom_histogram(
     bins = 15,
     fill = "gray85",
     color = "black",
     linewidth = 0.4
   ) +
   geom_vline(
     xintercept = mu,
     linewidth = 0.5,
     linetype = "solid"
   ) +
   geom_vline(
     xintercept = med,
     linewidth = 0.5,
     linetype = "dashed"
   ) +
   geom_label(
     data = data.frame(
       x = Inf,
       y = Inf,
       label = stats_label
     ),
     aes(x = x, y = y, label = label),
     hjust = 1.05,
     vjust = 1.1,
     size = 3.5,
     fill = "white",
     alpha = 0.75,
     linewidth = 0,
     inherit.aes = FALSE
   ) +
   # label mean line
   annotate(
     "text",
     x = mu,
     y = Inf,
     label = "Mean",
     angle = 90,
     vjust = -0.4,
     hjust = 2,
     size = 3.5
   ) +
   # label median line
   annotate(
     "text",
     x = med,
     y = Inf,
     label = "Median",
     angle = 90,
     vjust = -0.4,
     hjust = 1.6,
     size = 3.5
   ) +
   # descriptive stats block
   # annotate(
   #   "text",
   #   x = Inf,
   #   y = Inf,
   #   label = stats_label,
   #   hjust = 1.05,
   #   vjust = 1.1,
   #   size = 3.5
   # ) +
   scale_x_continuous(
     labels = label_number()
   ) +
   labs(
    # title = "Distribution of annual X-Road services",
     x = "Services",
     y = "Number of years"
   ) +
   theme_minimal() +
   theme(
     panel.grid.major = element_line(color = "gray90", linewidth = 0.4),
     panel.grid.minor = element_blank(),
     plot.title = element_text(face = "bold"),
     aspect.ratio = 1
   )
 
 print(services_hist)
 
 ggsave(
   "out/xtee_desc_services.png",
   services_hist,
   dpi = 300,
   bg = "white",
   width = 6,
   height = 5.5
 )  
 
 ggsave(
   "out/plosone/S2 Fig.tif",
   services_hist,
   device      = "tiff",
   type        = "cairo",
   compression = "lzw",
   dpi         = 300,
   bg          = "white",
   width       = 6,
   height      = 5.5,
   units       = "in"
 )
 
 
 # Data repositories
 
 stats <- annual_producers |>
   summarise(
     N = n(),
     mean = mean(producers_eoy_imputed, na.rm = TRUE),
     median = median(producers_eoy_imputed, na.rm = TRUE),
     sd = sd(producers_eoy_imputed, na.rm = TRUE),
     min = min(producers_eoy_imputed, na.rm = TRUE),
     max = max(producers_eoy_imputed, na.rm = TRUE)
   )
 
 stats_label <- paste0(
   "N = ", stats$N, "\n",
   "Mean = ", round(stats$mean, 2), "\n",
   "Median = ", round(stats$median, 2), "\n",
   "SD = ", round(stats$sd, 2), "\n",
   "Min = ", round(stats$min, 2), "\n",
   "Max = ", round(stats$max, 2)
 )
 
 mu  <- stats$mean
 med <- stats$median
 
 producers_hist <- ggplot(annual_producers, aes(x = producers_eoy_imputed)) +
   geom_histogram(
     bins = 15,
     fill = "gray85",
     color = "black",
     linewidth = 0.4
   ) +
   geom_vline(
     xintercept = mu,
     linewidth = 0.5,
     linetype = "solid"
   ) +
   geom_vline(
     xintercept = med,
     linewidth = 0.5,
     linetype = "dashed"
   ) +
   # label mean line
   annotate(
     "text",
     x = mu,
     y = Inf,
     label = "Mean",
     angle = 90,
     vjust = -0.8,
     hjust = 2,
     size = 3.5
   ) +
   # label median line
   annotate(
     "text",
     x = med,
     y = Inf,
     label = "Median",
     angle = 90,
     vjust = -0.8,
     hjust = 1.6,
     size = 3.5
   ) +
   # descriptive stats block
   annotate(
     "text",
     x = Inf,
     y = Inf,
     label = stats_label,
     hjust = 1.05,
     vjust = 1.1,
     size = 3.5
   ) +
   scale_x_continuous(
     labels = label_number()
   ) +
   labs(
    # title = "Distribution of annual X-Road data repositories",
     x = "Data repositories",
     y = "Number of years"
   ) +
   theme_minimal() +
   theme(
     panel.grid.major = element_line(color = "gray90", linewidth = 0.4),
     panel.grid.minor = element_blank(),
     plot.title = element_text(face = "bold"),
     aspect.ratio = 1
   )
 
 print(producers_hist)
 
 ggsave(
   "out/xtee_desc_datarep.png",
   producers_hist,
   dpi = 300,
   bg = "white",
   width = 6,
   height = 5.5
 )  
 
 ggsave(
   "out/plosone/S3 Fig.tif",
   producers_hist,
   device      = "tiff",
   type        = "cairo",
   compression = "lzw",
   dpi         = 300,
   bg          = "white",
   width       = 6,
   height      = 5.5,
   units       = "in"
 )
 
 
 #Descriptive table
 
 library(dplyr)
 library(officer)
 library(flextable)
 
 # --- 1) Build one descriptive-stat table (formatted for paper) ----
 
 desc_tbl <- bind_rows(
   annual_q |>
     summarise(
       Variable = "Queries (annual total)",
       Unit = "bn queries",
       N = n(),
       Mean = mean(queries_total, na.rm = TRUE) / 1e9,
       Median = median(queries_total, na.rm = TRUE) / 1e9,
       SD = sd(queries_total, na.rm = TRUE) / 1e9,
       Min = min(queries_total, na.rm = TRUE) / 1e9,
       Max = max(queries_total, na.rm = TRUE) / 1e9
     ),
   
   annual_services |>
     summarise(
       Variable = "Services (end-of-year, imputed)",
       Unit = "count",
       N = n(),
       Mean = mean(services_eoy_imputed, na.rm = TRUE),
       Median = median(services_eoy_imputed, na.rm = TRUE),
       SD = sd(services_eoy_imputed, na.rm = TRUE),
       Min = min(services_eoy_imputed, na.rm = TRUE),
       Max = max(services_eoy_imputed, na.rm = TRUE)
     ),
   
   annual_producers |>
     summarise(
       Variable = "Data repositories (end-of-year, imputed)",
       Unit = "count",
       N = n(),
       Mean = mean(producers_eoy_imputed, na.rm = TRUE),
       Median = median(producers_eoy_imputed, na.rm = TRUE),
       SD = sd(producers_eoy_imputed, na.rm = TRUE),
       Min = min(producers_eoy_imputed, na.rm = TRUE),
       Max = max(producers_eoy_imputed, na.rm = TRUE)
     )
 ) |>
   mutate(
     across(c(Mean, Median, SD, Min, Max), ~ round(.x, 2)),
     N = as.integer(N)
   )
 
 # --- 2) Turn into a Word table and export to .docx ----
 
 ft <- flextable(desc_tbl) |>
   autofit() |>
   bold(part = "header") |>
   align(align = "left", part = "all") |>
   align(j = c("N","Mean","Median","SD","Min","Max"), align = "right", part = "all")
 
 doc <- read_docx() |>
   body_add_par("Table X. Descriptive statistics", style = "heading 2") |>
   body_add_flextable(ft)
 
 print(doc, target = "out/xtee_desc_stats.docx")
 
 
####################################################################################################
   
   
 