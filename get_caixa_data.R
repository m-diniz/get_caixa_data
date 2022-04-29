#### Código para importação dos imóveis disponíveis pela caixa
#### Marcos André Diniz
#### Fevereiro de 2022



#### Pacotes -----------------------------------------------------------------
rm(list=ls(all=TRUE))
library(tidyverse)

#### Função para importar dados do site da Caixa -----------------------------
get_caixa_data <- function(estado_busca = c('AC', 'AL', 'AP', 'AM', 'BA', 'CE',
                                            'DF', 'ES', 'GO', 'MA', 'MT', 'MS',
                                            'MG', 'PA', 'PB', 'PR', 'PE', 'PI',
                                            'RJ', 'RN', 'RS', 'RO', 'RR', 'SC',
                                            'SP', 'SE', 'TO')){
  
  ## Função importa dados dos imóveis da caixa
  ## Args: Vetor de texto com as siglas dos estados a serem baixados. 
  ## Se não definido, os dados de todos os estados serão baixados.
  ## Output: Dataframe com dados dos imóveis
  
  if(!is.character(estado_busca)){
    stop('Argument is not a character vector.')
  }
  
  if(prod(str_length(estado_busca) == 2) == 0){
    stop('One or more arguments have a length other than two. 
         See state abbreviation.')
  }
  
  ## Estados a serem baixados
  links <- paste0('https://venda-imoveis.caixa.gov.br/listaweb/Lista_imoveis_',
                  estado_busca, '.htm?933938588%22%22')
  
  ## Ler páginas html
  imoveis_caixa <- list()
  
  for (i in 1:length(links)){
    imoveis_caixa[[i]] <- read_html(links[i]) %>% 
      html_nodes("table") %>%  
      html_table(header = T, fill = T)
  }
  
  ## Formata dados importados
  imoveis_caixa <- imoveis_caixa %>%
    bind_rows() %>% 
    # renomeia variáveis
    rename(link = `Link do Imóvel no site da Caixa`,
           endereco = `Endereço do imóvel`,
           bairro = Bairro,
           descricao = Descrição,
           preco = `Preço (R$)`,
           valor_avaliacao = `Valor de avaliação (R$)`,
           desconto = `Desconto (%)`,
           modalidade_venda = `Modalidade de venda`,
           foto = Foto,
           cidade = Cidade,
           estado = Estado) %>% 
    select(-c(link, foto)) %>% 
    
    # converte variáveis para numérico
    mutate(preco = parse_number(preco, locale = locale(decimal_mark = ",")),
           valor_avaliacao = parse_number(valor_avaliacao, locale = locale(decimal_mark = ",")
                                          )
           )
  
  return(imoveis_caixa)
  
}