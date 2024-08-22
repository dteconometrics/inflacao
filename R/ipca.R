# Pacotes -----------------------------------------------------------------

# Carregar pacotes/dependências
library(magrittr)
library(readr)
library(lubridate)
library(dplyr)
library(purrr)
library(GetBCBData)
library(ipeadatar)
library(sidrar)
library(gtrendsR)
library(rbcb)
library(tidyr)
library(tsibble)
library(stringr)
library(forecast)
library(timetk)
library(caret)
library(glmnet)
library(plotly)
library(ggplot2)
library(feasts)
library(fabletools)
library(scales)
library(ggtext)
library(HDeconometrics)
library(ggthemes)
library(usethis)

#remotes::install_github("gabrielrvsc/HDeconometrics")
# Fontes

foot_ibge <- "Fonte: Elaboração Própria com dados do IBGE"
foot_IPEA <- "Fonte: Elaboração Própria com dados do IPEA"
foot_BCB <- "Fonte: Elaboração Própria com dados do BCB"


# Funções Definidas pelo Usuário ------------------------------------------



report_ndiffs <- function (
    x,
    test  = c("kpss", "adf", "pp"),
    term  = c("level", "trend"),
    alpha = 0.05,
    na_rm = TRUE
) {
  
  # All possible tests and terms
  ndiffs_tests <- purrr::cross(list(test = test, type = term))
  ndiffs_tests <- purrr::set_names(
    x  = ndiffs_tests,
    nm = paste(
      purrr::map_chr(ndiffs_tests, 1),
      purrr::map_chr(ndiffs_tests, 2),
      sep = "_"
    )
  )
  
  # Nested for-loop
  purrr::map(
    .x = if (na_rm) {stats::na.omit(x)} else x,
    .f = ~purrr::map(
      .x = ndiffs_tests,
      .f = function (y) {
        forecast::ndiffs(
          x     = .x,
          alpha = alpha,
          test  = y[[1]],
          type  = y[[2]]
        )
      }
    )
  ) %>%
    purrr::map_df(dplyr::bind_rows, .id = "variable") %>%
    # Create column with most frequent value to differentiate
    dplyr::rowwise() %>%
    dplyr::mutate(
      ndiffs = dplyr::c_across(!dplyr::any_of("variable")) %>%
        table() %>%
        sort(decreasing = TRUE) %>%
        names() %>%
        purrr::chuck(1) %>%
        as.numeric()
    ) %>%
    dplyr::ungroup()
  
}



ym_label <- function(x) {
  
  x <- lubridate::as_date(x)
  
  dplyr::if_else(
    is.na(dplyr::lag(x)) | tsibble::yearmonth(dplyr::lag(x)) != tsibble::yearmonth(x),
    paste(lubridate::month(x, label = TRUE), "\n", lubridate::year(x)),
    paste(lubridate::month(x, label = TRUE))
  )
  
}



get_cv_rmse_hdecon <- function (
    model,
    data,
    y_target,
    date_col,
    init_window = 150,
    step        = 1,
    horizon     = 12,
    ...
) {
  
  cv_train_index <- data %>%
    dplyr::slice(1:(dplyr::n() - horizon)) %>%
    nrow() %>%
    seq_len() %>%
    # function not exported, use with caution!
    tsibble:::stretcher2(.step = step, .init = init_window)
  
  n_fcst <- length(cv_train_index)
  
  point_fcst <- list()
  
  for (i in seq_len(n_fcst)) {
    
    cat(paste0("\nIteration: ", i, "/", n_fcst))
    
    curr_index <- cv_train_index[[i]]
    data_train <- data[curr_index, ]
    
    yy_in <- dplyr::pull(data_train, dplyr::all_of(y_target))
    xx_in <- dplyr::select(data_train, !dplyr::any_of(c(date_col, y_target)))
    
    xx_out <- as.matrix(data[-curr_index, names(xx_in)][1:horizon, ])
    
    if (model == "csr") {
      
      fit_csr <- HDeconometrics::csr(
        x = xx_in,
        y = yy_in,
        ...
      )
      
      fcsts <- predict(object = fit_csr, newdata = xx_out)
      
    } else if (model == "bagging") {
      
      fit_bagging <- HDeconometrics::bagging(
        x = as.matrix(xx_in),
        y = yy_in,
        ...
      )
      
      fcsts <- predict(object = fit_bagging, newdata = xx_out)
      
    } else stop("model must be 'csr' or 'bagging'.")
    
    point_fcst[[i]] <- dplyr::tibble(
      {{ date_col }} := seq.Date(
        from       = max(dplyr::pull(data_train, {{ date_col }})) + months(1),
        by         = "month",
        length.out = length(fcsts)
      ),
      fcst = fcsts
    )
    
  }
  
  fc <- point_fcst %>%
    dplyr::bind_rows(.id = ".id") %>%
    dplyr::mutate(model = dplyr::if_else(model == "csr", "csr", "bagging"))
  
  rmse_tbl <- dplyr::left_join(
    x  = fc,
    y  = dplyr::select(data, dplyr::all_of(c(date_col, y_target))),
    by = {{ date_col }}
  ) %>%
    dplyr::group_by(.id) %>%
    dplyr::mutate(h = dplyr::row_number()) %>%
    dplyr::group_by(h, model) %>%
    dplyr::summarise(
      rmse = sqrt(mean((!!rlang::sym(y_target) - fcst)^2, na.rm = TRUE)),
      .groups = "drop"
    )
  
  return(
    list("rmse" = rmse_tbl, "forecasts" = fc)
  )
  
}



# Coleta de dados ---------------------------------------------------------


# Coleta de dados ---------------------------------------------------------


# Ler metadados/códigos de coleta (CSV)
# Coleta dos Dados ------------------


## Lendo os Metas dados
metadata <- readr::read_csv2(
  file = "./metadados/metadados.csv",
  col_names = TRUE,
  col_types = "c"
)

## Data de Inicio

init_date <- lubridate::ymd("2002-12-01")

# |-- BCB --------

## Código de coleta de dados do SGS/BCB

codes_bcb <- metadata %>%
  dplyr::filter(fonte == "BCB") %>%
  dplyr::summarise(
    purrr::set_names(x = as.numeric(codigo), nm = acronimo)
  ) %>%
  dplyr::pull()


## Coletando da API do BCB

raw_bcb <- GetBCBData::gbcbd_get_series(
  id = codes_bcb,
  first.date = init_date,
  last.date = lubridate::today(),
  use.memoise = FALSE
)

# |-- IPEADATA --------
## Códigp de coleta de dados

codes_ipeadata <- metadata %>%
  dplyr::filter(fonte == "IPEADATA") %>%
  dplyr::summarise(
    purrr::set_names(x = codigo, nm = acronimo)
  ) %>%
  dplyr::pull()

## Coleta

raw_ipeadata <- ipeadatar::ipeadata(code = codes_ipeadata)


# |-- IBGE --------

## Código de coleta de dados do IBGE

codes_ibge <- metadata %>%
  dplyr::filter(fonte == "IBGE") %>%
  dplyr::summarise(
    purrr::set_names(x = codigo, nm = acronimo)
  ) %>%
  dplyr::pull()


# Coleta de dados do IBGE
raw_ibge <- purrr::map(
  .x = codes_ibge,
  .f = ~sidrar::get_sidra(api = .x)
)

# |-- Google Trends ----
# Códigos de coleta de dados do Google Trends
codes_google <- metadata %>%
  dplyr::filter(fonte == "Google Trends") %>%
  dplyr::summarise(
    purrr::set_names(x = codigo, nm = acronimo)
  ) %>%
  dplyr::pull()


# Coleta de dados do Google Trends
raw_google <- gtrendsR::gtrends(
  keyword      = codes_google,
  geo          = "BR",
  time         = "all",
  onlyInterest = TRUE
)


# |-- Focus/BCB ----
# Códigos de coleta de dados do Focus/BCB
codes_focus <- metadata %>%
  dplyr::filter(fonte == "Focus/BCB") %>%
  dplyr::summarise(
    purrr::set_names(x = codigo, nm = acronimo)
  ) %>%
  dplyr::pull()


# Coleta de dados do Focus/BCB
raw_focus <- rbcb::get_market_expectations(
  type       = "monthly",
  indic      = codes_focus,
  start_date = init_date,
  end_date   = lubridate::today()
)

# Tratamento dos Dados -------------------

## Dados do SGS/BCB

df_bcb <- raw_bcb %>%
  dplyr::select(
    "date" = "ref.date",
    "variable" = "series.name",
    "value"
  ) %>%
  tidyr::pivot_wider(
    id_cols     = "date",
    names_from  = "variable",
    values_from = "value"
  )

## Dados do IPEADATA

df_ipeadata <- raw_ipeadata %>%
  dplyr::select("date", "variable" = "code", "value") %>%
  dplyr::left_join(
    y  = dplyr::select(metadata, "codigo", "acronimo"),
    by = c("variable" = "codigo")
  ) %>%
  tidyr::pivot_wider(
    id_cols     = "date",
    names_from  = "acronimo",
    values_from = "value"
  ) %>%
  # Obter média mensal (para séries com freq. diária)
  dplyr::group_by(date = tsibble::yearmonth(.data$date)) %>%
  dplyr::summarise(
    dplyr::across(where(is.numeric), ~mean(.x, na.rm = TRUE))
  ) %>%
  dplyr::mutate(date = lubridate::as_date(.data$date))

## Dados do IBGE

df_ibge <- raw_ibge %>%
  purrr::map_dfr(
    .f  = ~dplyr::select(.x, "date" = "Mês (Código)", "value" = "Valor"),
    .id = "variable"
  )%>%
  tidyr::pivot_wider(
    id_cols     = "date",
    names_from  = "variable",
    values_from = "value"
  ) %>%
  dplyr::mutate(date = lubridate::ym(.data$date)) %>%
  dplyr::filter(date >= init_date) %>%
  dplyr::relocate("date", "ipca")


Sys.sleep(5)

# Dados do Google
df_google <- raw_google %>%
  purrr::pluck("interest_over_time") %>%
  dplyr::mutate(
    date    = lubridate::as_date(.data$date),
    keyword = stringr::str_replace(keyword, " ", "_")
  ) %>%
  tidyr::pivot_wider(
    id_cols     = "date",
    names_from  = "keyword",
    values_from = "hits"
  )


# Dados do Focus/BCB
df_focus <- raw_focus %>%
  dplyr::arrange(Data) %>%
  # Calcula horizonte em meses da expectativa
  dplyr::mutate(
    monthyear = tsibble::yearmonth(Data),
    horizon   = tsibble::yearmonth(DataReferencia, format = "%m/%Y") - monthyear
  ) %>%
  # Agrupar por mês de estatísticas registradas no Focus
  dplyr::group_by(monthyear) %>%
  # Filtra estatísticas tipo 0 (últimos 30 dias) no horizonte de 1 ano e
  # de datas próxima ao dia 15
  dplyr::filter(
    baseCalculo == 0,
    horizon > 0 & horizon < 13,
    lubridate::day(Data) < 16
  ) %>%
  dplyr::filter(
    abs(lubridate::day(Data) - 15) == min(abs(lubridate::day(Data) - 15))
  ) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(
    "date" = lubridate::floor_date(x = Data, unit = "month"),
    horizon,
    .keep_all = TRUE
  ) %>%
  tidyr::pivot_wider(
    id_cols      = date,
    names_from   = horizon,
    values_from  = Mediana,
    names_prefix = "expectativa_ipca_h_"
  )

## Cruzando os dados

df_ipca <- purrr::reduce(
  .x = list(df_ibge, df_bcb, df_ipeadata, df_google, df_focus),
  .f = dplyr::left_join,
  by = "date"
  
)

# |-- Estacionariedade ----

# Aplicar testes de estacionariedade p/ identificar nº de diferenças
# para séries serem estacionárias

## Defasagem automática

vars_ndiffs <- df_ipca %>%
  dplyr::select(- "date") %>%
  report_ndiffs()

# Diferenciar séries que são não estacionárias
df_ipca_diff <- df_ipca %>%
  dplyr::mutate(
    dplyr::across(
      .cols = vars_ndiffs$variable[vars_ndiffs$ndiffs > 0 & vars_ndiffs$variable != "ipca"],
      .fns  = ~tsibble::difference(
        x           = .x,
        differences = vars_ndiffs$ndiffs[vars_ndiffs$variable == dplyr::cur_column()]
      )
    )
  )


# |-- Atrasos de publicação ----

# Epandir base criando defasagens e dummies, preencher valores NA (de baixo),
# filtrar amostra e selecionar variáveis de interesse
df_ipca_lagged <- df_ipca_diff %>%
  timetk::tk_augment_lags(.value = !dplyr::any_of("date"), .lags = 1:4) %>%
  tidyr::fill(!dplyr::any_of(c("date", "ipca")), .direction = "down") %>%
  tidyr::drop_na() %>%
  dplyr::select(
    !dplyr::any_of(
      stringr::str_subset(
        string  = names(df_ipca_diff),
        pattern = "date|ipca|expectativa",
        negate  = TRUE
      )
    )
  )

yy_ts <- stats::ts(
  data = df_ipca_lagged$ipca,
  start = c(
    lubridate::year(min(df_ipca_lagged$date)),
    lubridate::month(min(df_ipca_lagged$date))
  ),
  frequency = 12
)
seasonal_dummies <- forecast::seasonaldummy(yy_ts)

df_ipca_lagged %<>% dplyr::bind_cols(seasonal_dummies)



# Validação Cruzada -------------------------------------------------------


# Especificação de semente para reprodutibilidade
set.seed(1984)


# Número de observações iniciais
init_obs <- 150


# Horizonte de previsão
horizon <- 12



# |-- Modelo Lasso ----

# Criar esquema de validação cruzada
cv_config <- caret::trainControl(
  method          = "timeslice",
  initialWindow   = init_obs,
  horizon         = horizon,
  fixedWindow     = FALSE,
  verboseIter     = TRUE,
  savePredictions = "all"
)


# Treino do modelo
fit_lasso <- caret::train(
  form       = ipca ~ .,
  data       = df_ipca_lagged[-1],
  method     = "glmnet",
  trControl  = cv_config,
  metric     = "RMSE"
)


# Acurácia por horizonte preditivo
acc_lasso <- fit_lasso %>%
  purrr::pluck("pred") %>%
  dplyr::filter(
    alpha == fit_lasso$bestTune$alpha,
    lambda == fit_lasso$bestTune$lambda
  ) %>%
  dplyr::group_by(Resample) %>%
  dplyr::mutate(h = dplyr::row_number(), model = "lasso") %>%
  dplyr::group_by(h, model) %>%
  dplyr::summarise(
    rmse    = sqrt(mean((obs - pred)^2, na.rm = TRUE)),
    .groups = "drop"
  )



# |-- Modelo CSR ----

# Aplica função criada para reportar RMSE por horizonte preditivo e pontos de previsão
acc_csr <- get_cv_rmse_hdecon(
  model          = "csr",
  data           = df_ipca_lagged,
  y_target       = "ipca",
  date_col       = "date",
  init_window    = init_obs,
  step           = 1,
  horizon        = horizon,
  K              = 20,
  k              = 15,
  fixed.controls = colnames(seasonal_dummies)
)



# |-- Modelo Bagging ----

# Aplica função criada para reportar RMSE por horizonte preditivo e pontos de previsão
acc_bagging <- get_cv_rmse_hdecon(
  model          = "bagging",
  data           = df_ipca_lagged,
  y_target       = "ipca",
  date_col       = "date",
  init_window    = init_obs,
  step           = 1,
  horizon        = horizon,
  R              = 500,
  pre.testing    = "group-joint",
  fixed.controls = colnames(seasonal_dummies)
)



# |-- Modelo Ensemble e RW ----

# Tratar dados para obter previsões por amostra de validação cruzada dos 3 modelos
df_ensemble <- fit_lasso %>%
  purrr::pluck("pred") %>%
  dplyr::filter(
    alpha == fit_lasso$bestTune$alpha,
    lambda == fit_lasso$bestTune$lambda
  ) %>%
  dplyr::left_join(
    y = df_ipca_lagged %>%
      dplyr::select("date") %>%
      dplyr::mutate(index = dplyr::row_number(), model = "lasso"),
    by = c("rowIndex" = "index")
  ) %>%
  dplyr::select(".id" = "Resample", "date", "fcst" = "pred", "model") %>%
  dplyr::group_by(.id) %>%
  dplyr::mutate(.id = as.character(dplyr::cur_group_id())) %>%
  dplyr::ungroup() %>%
  dplyr::bind_rows(acc_csr$forecasts, acc_bagging$forecasts) %>%
  tidyr::pivot_wider(
    id_cols     = c(".id", "date"),
    names_from  = "model",
    values_from = "fcst"
  )


# Calcular acurácia do modelo Ensemble (previsão média dos modelos CSR e Bagging) e RW
acc_ensemble_rw <- df_ensemble %>%
  dplyr::left_join(
    y  = dplyr::select(df_ipca_lagged, "date", "ipca", "ipca_lag1"),
    by = "date"
  ) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    ensemble = mean(dplyr::c_across(c("csr", "bagging")), na.rm = TRUE)
  ) %>%
  dplyr::group_by(.id) %>%
  dplyr::mutate(h = dplyr::row_number()) %>%
  dplyr::group_by(h) %>%
  dplyr::summarise(
    ensemble = sqrt(mean((ipca - ensemble)^2, na.rm = TRUE)),
    rw       = sqrt(mean((ipca - ipca_lag1)^2, na.rm = TRUE)),
    .groups  = "drop"
  ) %>%
  tidyr::pivot_longer(cols = -"h", names_to = "model", values_to = "rmse")



# |-- Acurácia do Focus/BCB ----

# Calcular acurácia do Focus por horizonte preditivo
acc_focus <- df_ipca_lagged %>%
  dplyr::select(
    "date",
    "ipca",
    dplyr::matches("expectativa_ipca_h_\\d{1,2}$")
  ) %>%
  tidyr::pivot_longer(
    cols      = -c("date", "ipca"),
    names_to  = "h",
    values_to = "focus"
  ) %>%
  dplyr::mutate(
    h     = as.integer(stringr::str_remove(h, "expectativa_ipca_h_")),
    model = "focus"
  ) %>%
  dplyr::left_join(
    y = df_ensemble %>%
      dplyr::group_by(.id) %>%
      dplyr::mutate(h = dplyr::row_number()) %>%
      dplyr::select(-c("lasso", "csr", "bagging")),
    by = c("date", "h")
  ) %>%
  tidyr::drop_na() %>%
  dplyr::group_by(h, model) %>%
  dplyr::summarise(
    rmse = sqrt(mean((ipca - focus)^2, na.rm = TRUE)),
    .groups  = "drop"
  )


# Previsão fora da amostra ------------------------------------------------


# Criar objetos com vetor da variável dependente e matriz das independentes
yy <- dplyr::pull(df_ipca_lagged, "ipca")
xx <- df_ipca_lagged %>%
  dplyr::select(!dplyr::any_of(c("date", "ipca"))) %>%
  as.matrix()


# Estimar modelo CSR
fit_csr <- HDeconometrics::csr(
  x              = xx,
  y              = yy,
  K              = 20,
  k              = 15,
  fixed.controls = colnames(seasonal_dummies)
)


# Estimar modelo Bagging
fit_bagging <- HDeconometrics::bagging(
  x              = xx,
  y              = yy,
  R              = 500,
  pre.testing    = "group-joint",
  fixed.controls = colnames(seasonal_dummies)
)


# Criar cenários para variáveis independentes, por conveniência serão
# utilizados modelos univariados através de um auto ARIMA
xreg_arima <- purrr::map_df(
  .x = dplyr::select(
    df_ipca_lagged,
    !dplyr::any_of(c("date", "ipca", colnames(seasonal_dummies)))
  ),
  .f = ~{
    forecast::auto.arima(.x) %>%
      forecast::forecast(h = horizon) %>%
      magrittr::extract2("mean") %>%
      as.numeric()
  }
)


# Dummies sazonais de fora da amostra
seasonal_dummies_oos <- forecast::seasonaldummy(x = yy_ts, h = horizon)


# Variáveis exógenas para gerar previsões fora da amostra
xx_oos <- as.matrix(dplyr::bind_cols(xreg_arima, seasonal_dummies_oos))


# Gerar previsões fora da amostra
fcst_csr <- predict(object = fit_csr, newdata = xx_oos)
fcst_bagging <- predict(object = fit_bagging, newdata = xx_oos)
fcst_ensemble <- dplyr::tibble(csr = fcst_csr, bagging = fcst_bagging) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    ensemble = mean(dplyr::c_across(c("csr", "bagging")), na.rm = TRUE)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    date = seq.Date(
      from       = max(df_ipca_lagged$date) + months(1),
      by         = "month",
      length.out = length(fcst_csr)
    ),
    .before = 1
  )

# Visualização de dados ---------------------------------------------------


# Cores para gráficos
colors <- c(
  "#282f6b", # blue
  "#b22200", # red
  "#224f20", # green
  "#eace3f", # yellow
  "#5f487c", # purple
  "#b35c1e", # orange
  "black",
  "#419391", # turquoise
  "#839c56", # light green
  "#3b89bc", # light blue
  "#666666"  # gray
)


# |-- Fanchart ---------------

# Juntar dados observados com pontos de previsão------------------
df_fanchart <- df_ipca_lagged %>%
  dplyr::select("date", "ipca") %>%
  dplyr::full_join(y = fcst_ensemble, by = "date") %>%
  dplyr::mutate(
    ensemble = dplyr::if_else(date == max(df_ipca_lagged$date), ipca, ensemble)
  )


# Gerar gráfico de linha (fanchart)
plt_fanchart <- df_fanchart %>%
  ggplot2::ggplot() +
  ggplot2::aes(x = date) +
  ggplot2::geom_line(
    mapping = ggplot2::aes(y = ipca),
    size    = 1,
    color   = colors[7],
    data    = dplyr::slice_tail(df_ipca_lagged, n = 36)
  ) +
  ggplot2::geom_ribbon(
    mapping = ggplot2::aes(ymin = -Inf, ymax = Inf),
    fill    = colors["darkred"],
    alpha   = 0.35,
    data    = dplyr::filter(df_fanchart, date >= max(df_ipca_lagged$date))
  ) +
  ggplot2::geom_line(
    mapping = ggplot2::aes(y = ensemble),
    size    = 1,
    color   = colors[2],
    data    = dplyr::filter(df_fanchart, date >= max(df_ipca_lagged$date))
  ) +
  ggplot2::geom_vline(
    xintercept = max(df_ipca_lagged$date),
    linetype   = "dashed"
  ) +
  ggplot2::scale_y_continuous(
    labels = scales::number_format(
      suffix       = "%",
      accuracy     = 0.1,
      decimal.mark = ","
    )
  ) +
  ggplot2::scale_x_date(
    breaks = scales::breaks_width("3 months"),
    labels = ym_label
  ) +
  ggplot2::theme_light() +
  ggplot2::labs(
    title    = "Previsão do IPCA (% a.m.)",
    y        = "% a.m.",
    x        = NULL,
    caption  = "Dados: BCB/Google/IBGE/IPEADATA | Elaboração Própria"
  ) +
  ggplot2::theme(
    plot.title       = ggtext::element_markdown(size = 25, colour = colors[1]),
    axis.text        = ggtext::element_markdown(size = 12, face = "bold"),
    axis.title       = ggtext::element_markdown(size = 12, face = "bold"),
    panel.grid.minor = ggplot2::element_blank(),
    plot.caption     = ggtext::element_textbox_simple(
      size   = 10,
      colour = "grey20",
      margin = ggplot2::margin(10, 5.5, 10, 5.5)
    )
  )

# Converter para gráfico interativo usando plotly
plt_fanchart_interactive <- ggplotly(plt_fanchart)

# Visualizar o gráfico interativo
plt_fanchart_interactive


# |-- Gráfico de acurácia ----

# Juntar dados de acurácia de todos os modelos
acc_rmse <- dplyr::bind_rows(
  acc_lasso,
  acc_csr$rmse,
  acc_bagging$rmse,
  acc_ensemble_rw,
  acc_focus
) %>%
  dplyr::mutate(
    model = dplyr::recode(
      model,
      "lasso"    = "LASSO",
      "csr"      = "Complete Subset Regression",
      "bagging"  = "Bagging",
      "ensemble" = "Ensemble",
      "rw"       = "Random Walk",
      "focus"    = "Focus"
    )
  ) %>%
  dplyr::arrange(h, rmse, model) %>%
  dplyr::select("Horizonte" = "h", "Modelo" = "model", "RMSE" = "rmse")


# Gráfico do RMSE por horizonte de previsão
plt_rmse <- acc_rmse %>%
  ggplot2::ggplot(
    ggplot2::aes(x = Horizonte, y = RMSE, colour = Modelo)
  ) +
  ggplot2::geom_line(size = 1.5) +
  ggplot2::geom_point(size = 3) +
  ggplot2::scale_colour_manual(values = colors) +
  ggplot2::scale_y_continuous(
    breaks = scales::breaks_extended(n = 8),
    labels = scales::number_format(
      accuracy     = 0.001,
      decimal.mark = ",",
      big.mark     = "."
    )
  ) +
  ggplot2::scale_x_continuous(
    labels = scales::number_format(accuracy = 1),
    breaks = 1:horizon
  ) +
  ggplot2::theme_light() +
  ggplot2::labs(
    title    = "Acurácia: performance por horizonte preditivo",
    subtitle = "Modelos de previsão do IPCA",
    x        = "Horizonte (meses)",
    color    = NULL,
    caption  = "Elaboração Própria:"
  ) +
  ggplot2::theme(
    plot.title       = ggtext::element_markdown(size = 25, colour = colors[1]),
    plot.subtitle    = ggtext::element_markdown(size = 16),
    axis.text        = ggtext::element_markdown(size = 12, face = "bold"),
    axis.title       = ggtext::element_markdown(size = 12, face = "bold"),
    legend.position  = "bottom",
    legend.text      = ggplot2::element_text(size = 12, face = "bold"),
    legend.key.width = ggplot2::unit(1, "cm"),
    panel.grid.minor = ggplot2::element_blank(),
    plot.caption     = ggtext::element_textbox_simple(
      size   = 8,
      colour = "grey20",
      margin = ggplot2::margin(10, 5.5, 10, 5.5)
    )
  )

# Converter para gráfico interativo usando plotly
plt_rmse_interactive <- ggplotly(plt_rmse)

# Visualizar o gráfico interativo
plt_rmse_interactive

# |-- Tabelas ----

# Tabela de pontos de previsão
fc_tbl <- df_fanchart %>%
  dplyr::filter(date > max(df_ipca_lagged$date)) %>%
  dplyr::mutate(date = lubridate::as_date(date) %>% format("%b/%Y")) %>%
  dplyr::select("Mês" = "date", "Previsão" = "ensemble") %>%
  DT::datatable(
    options = list(dom = "tip", pageLength = 6, scrollX = TRUE, scrollY = "300px"),
    rownames = FALSE
  ) %>%
  DT::formatRound(columns = 2, digits = 2, dec.mark = ",", mark = ".") %>%
  DT::formatStyle(columns = 2, fontWeight = "bold")

# Tabela com valores do RMSE vs. horizonte/modelos
rmse_tbl <- acc_rmse %>%
  DT::datatable(
    options = list(dom = "tip", pageLength = 5, scrollX = TRUE, scrollY = "200px"),
    rownames = FALSE
  ) %>%
  DT::formatRound(columns = 3, digits = 2, dec.mark = ",", mark = ".")

# Dashboard ---------------------------------------------------------------

# Verificar se pasta "docs" existe no projeto
if(!dir.exists("docs")){dir.create("docs")}

# Renderizar dashboard
rmarkdown::render(
  input       = "./docs/dash_ipca.Rmd",
  output_file = "index.html"
)
