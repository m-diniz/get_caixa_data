# Instalação dos pacotes necessários
# install.packages("httr")
# install.packages("readr")

# Carregamento dos pacotes
library(httr)
library(readr)

baixar_dados_caixa <- function(estado, salvar_csv = FALSE, arquivo_saida = NULL) {
  
  base_url <- "https://venda-imoveis.caixa.gov.br"
  url_download <- paste0(base_url, "/listaweb/Lista_imoveis_", estado, ".csv?85791062")
  
  cat("Iniciando download para o estado:", estado, "...\n")
  response <- GET(url = url_download, progress())
  
  if (http_status(response)$category == "Success") {
    
    conteudo_raw <- content(response, as = "raw")
    
    if (length(conteudo_raw) == 0) {
      cat("Erro: O download retornou um conteúdo vazio.\n")
      return(NULL)
    }
    
    dados_df <- readr::read_csv2(conteudo_raw, locale = locale(encoding = "latin1"), show_col_types = FALSE)
    
    cat("Download e importação concluídos. ", nrow(dados_df), " registros importados.\n")
    
    if (salvar_csv) {
      if (is.null(arquivo_saida)) {
        arquivo_saida <- paste0("imoveis_caixa_", tolower(estado), ".csv")
      }
      readr::write_csv2(dados_df, arquivo_saida)
      cat("Arquivo salvo em:", normalizePath(arquivo_saida), "\n")
    }
    
    return(dados_df)
    
  } else {
    
    cat("Falha no download. Código de status HTTP:", status_code(response), "\n")
    stop_for_status(response)
    return(NULL)
    
  }
}

