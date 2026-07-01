

if (getRversion() >= "2.15.1") {
  utils::globalVariables("Valor")
}

# ================================
# Logomarca do SIDRA Facil
# ================================
load_logo <- function(x){
  img_dahora <- switch(as.character(x),
                       "1" = "ppgr_01.png",
                       "2" = "ppgr_02.png",
                       "3" = "ppgr_03.png",
                       "4" = "ppgr_04.png",
                       "5" = "ppgr_05.png"
  )
  
  logo <- system.file(
    "IDVisual",
    img_dahora,
    package = "SidraFacil"
  )
  
  if(logo == ""){
    warning("Logo nao encontrada.")
    return(invisible(NULL))
  }
  
  
  img <- png::readPNG(logo)
  
  grid::grid.newpage()
  grid::grid.raster(img)
  
  grDevices::dev.flush()
}

# ================================
# Texto entrada
# ================================
visual_texto <- function() {
  cat(texto_titulo())
}

texto_titulo <- function() {
  "===============================================================================================================================================
                                                                 SIDRA FACIL
                                                       Dados do SIDRA. Facil como deve ser.
                                                                                                                                     Versao 1.2

                                                                                                contato:samuelamorimdatascientist@gmail.com
=============================================================================================================================================="
}

# ============================================================
# Selecao, pelo usuario, de pasta para salvamento de arquivos
# ============================================================
visual_interact <- function(){
  svDialogs::dlgMessage(
    message = "Na pr\u00f3xima janela, escolha a pasta onde deseja salvar os arquivos. Os dados ser\u00e3o disponibilizados nela.",
    type = "ok",
    title = "Aten\u00e7\u00e3o"
  )
  pasta <- svDialogs::dlgDir(
    default = getwd(),
    caption = "Escolha a pasta - tabela"
  )$res
  
  
  return(pasta)
  # -----------------------------------------------------------------------
}

# ================================
# Funcao correcao de codificacao
# ================================
converter_encoding <- function(x) {
  
  if (is.character(x)) {
    return(iconv(x, from = "UTF-8", to = "windows-1252", sub = ""))
  }
  
  if (is.factor(x)) {
    lev <- iconv(levels(x), from = "UTF-8", to = "windows-1252", sub = "")
    return(factor(x, levels = lev))
  }
  
  if (is.data.frame(x)) {
    x[] <- lapply(x, converter_encoding)
    return(x)
  }
  
  if (is.list(x)) {
    return(lapply(x, converter_encoding))
  }
  
  return(x)
}

# ==========================
# Download incremental
# ==========================
down_incremental <- function(tabela, anos, variaveis, categorias, geo_escolha, geo_label, cod_classif, pasta, ppgr_num){
  resultado <- NULL
  # Tempo limite mudar propaganda (em segundos)
  tempo_limite <- 180   # 3 minutos
  # Marca tempo inicial da execucao do download
  inicio <- Sys.time()
  
  for(ctgr in categorias){
    
    for(var in variaveis){
      
      for(ano in anos){
        cat(
          "\nBaixando:",
          ano,
          " variavel ",
          var,
          " categoria ",
          ctgr
        )
        
        
        x <- sidrar::get_sidra(
          x = tabela,
          period = as.character(ano),
          variable = var,
          geo = geo_escolha,
          classific = cod_classif,
          category = list(as.character(ctgr))
        )
        
        
        col_cod <- paste0(
          geo_label,
          " (C\u00f3digo)"
        )
        
        col_nome <- geo_label
        
        
        tmp <- x |>
          dplyr::select(
            codigo_ibge = tidyselect::all_of(col_cod),
            localidade = tidyselect::all_of(col_nome),
            Valor
          )
        
        
        nome_coluna <- paste0(
          "ano_", ano,
          "_var_", var,
          "_cat_", ctgr
        )
        
        
        names(tmp)[3] <- nome_coluna
        
        
        if(is.null(resultado)){
          
          resultado <- tmp
          
        } else {
          
          
          resultado <- dplyr::left_join(
            resultado,
            tmp,
            by = c(
              "codigo_ibge",
              "localidade"
            )
          )
          
        }
        
        
        # salva a cada passo
        saveRDS(
          resultado,
          file.path(
            pasta,
            paste0(
              "sidra_",
              tabela,
              "_parcial.rds"
            )
          )
        )
        
        # Verifica o tempo decorrido de execucao
        if (((difftime(Sys.time(), inicio, units = "secs") >= tempo_limite)) && (ppgr_num == 3)) {
          ppgr_num = 4
          load_logo(ppgr_num)# se decorreu o tempo, muda a propaganda
        }
        
      }
    }
  }
  
  
  # ==========================
  # Resultado final
  # ==========================
  
  return(resultado)
  
}


#' SIDRA FACIL - Pacote Framework para baixa automatizada de dados do SIDRA IBGE
#' A proposta deste pacote framework - proporcionar uma ferramenta FACIL e descomplicada para baixa de dados do SIDRA IBGE a partir de simples selecao pelo usuario
#' Solicita um titulo e um texto ao usuario e salva em um arquivo TXT.
#' @return Caminho completo do arquivo criado.
#' @examples
#' \dontrun{
#' dados <- sidra_facil(
#'   tabela = 1419,
#'   variavel = 93
#' )
#' }
#'
#' @export
#'
#' @importFrom sidrar get_sidra info_sidra
#' @importFrom dplyr select left_join
#' @importFrom tidyselect all_of
#' @importFrom svDialogs dlgInput dlgDir dlgMessage
#' @importFrom png readPNG
#' @importFrom grid grid.newpage grid.raster
sidra_facil <- function(){
  ppgr_num = 1
  load_logo(ppgr_num)
  visual_texto()
  
  # ==========================
  # usuario informa tabela
  # ==========================
  tabela <- svDialogs::dlgInput(
    message = "Informe o n\u00famero da tabela SIDRA:"
  )$res
  
  tabela <- as.numeric(tabela)
  
  
  info <- sidrar::info_sidra(tabela)
  #info <- converter_encoding(info)# Corrige a codificacao que vem errada
  cod_classif <- sub(" .*", "", names(info$classific_category)[1])
  print(cod_classif)
  
  # =================================================
  # Mostrar opcoes disponiveis na tabela selecionada
  # =================================================
  cat("\n===== TABELA =====\n")
  print(info$table)
  
  cat("\n===== ANOS DISPONIVEIS =====\n")
  print(info$period)
  
    cat("\n===== VARIAVEIS =====\n")
  print(info$variable)
  
    cat("\n===== CATEGORIAS =====\n")
  
    for(i in seq_along(info$classific_category)){
      print(info$classific_category[[i]])
    }
  
    cat("\n===== GEOGRAFIAS =====\n")
  print(info$geo)
  
  # =======================================================
  # Salva todas as informacoes da tabela em um arquivo txt
  # =======================================================
  saida <- texto_titulo()
  
  saida <- c(saida, "=================================================================== TABELA ===========================================================================")
  saida <- c(saida, utils::capture.output(print(info$table)))
  
  saida <- c(saida, "============================================================== ANOS DISPONIVEIS ======================================================================")
  saida <- c(saida, utils::capture.output(print(info$period)))
  
  saida <- c(saida, "================================================================== VARIAVEIS =========================================================================")
  saida <- c(saida, utils::capture.output(print(info$variable)))
  
  saida <- c(saida, "=================================================================== CATEGORIAS =======================================================================")
  
  for(i in seq_along(info$classific_category)){
    saida <- c(saida, utils::capture.output(print(info$classific_category[[i]])))
  }
  
  saida <- c(saida, "================================================================== GEOGRAFIAS ========================================================================")
  saida <- c(saida, utils::capture.output(print(info$geo)))
  saida <- c(saida, "======================================================================================================================================================")
  
  # ============================================================
  # Selecao, pelo usuario, de pasta para salvamento de arquivos
  # ============================================================
  pasta <- visual_interact()
  if (is.null(pasta) || pasta == "") {
    stop("Nenhuma pasta selecionada.")
  }
  else {
    nome_arquivo <- file.path(
      pasta,
      paste0(
        "Tabela_",
        tabela,
        ".txt"
      )
    )
    con <- file(nome_arquivo, open = "w", encoding = "UTF-8")
    writeLines(saida, con, useBytes = FALSE)
    close(con)
  }
  # -----------------------------------------------------------------------
  ppgr_num = 2
  load_logo(ppgr_num)
  
  # ===========================
  # usuario escolhe o cardapio
  # ===========================
  anos <- as.numeric(
    trimws(
      unlist(
        strsplit(
          svDialogs::dlgInput(
            message = "Digite anos separados por v\u00edrgula:"
          )$res,
          ","
        )
      )
    )
  )
  
  
  variaveis <- as.numeric(
    trimws(
      unlist(
        strsplit(
          svDialogs::dlgInput(
            message = "Digite c\u00f3digos das vari\u00e1veis separados por v\u00edrgula:"
          )$res,
          ","
        )
      )
    )
  )
    
  
  
  categorias <- as.numeric(
    trimws(
      unlist(
        strsplit(
          svDialogs::dlgInput(
            message = "Digite c\u00f3digos das categorias separados por v\u00edrgula: "
          )$res,
          ","
        )
      )
    )
  )
  
  geo_escolha <- svDialogs::dlgInput(
    message = "Escolha n\u00edvel geogr\u00e1fico (ex: City):"
  )$res
  
  geo_map <- c(
    City   = "Munic\u00edpio",
    State  = "Unidade da Federa\u00e7\u00e3o",
    Brazil = "Brasil",
    Region       = "Grande Regi\u00e3o",
    MesoRegion   = "Mesorregi\u00e3o Geogr\u00e1fica",
    MicroRegion  = "Microrregi\u00e3o Geogr\u00e1fica"
  )
  
  geo_label <- geo_map[[geo_escolha]]
  
  ppgr_num = 3
  load_logo(ppgr_num)
  
  # Chama funcao de download incremental
  resultado <- down_incremental(
    tabela = tabela,
    anos = anos,
    variaveis = variaveis,
    categorias = categorias,
    geo_escolha = geo_escolha,
    geo_label = geo_label,
    cod_classif = cod_classif,
    pasta = pasta,
    ppgr_num = ppgr_num
  )
  
  # =====================================
  # Salva em arquivo CSV resultado final
  # =====================================
  anos_ord <- sort(anos)
  
  nomeArquivo <- paste0("T",
                        tabela, "_",
                        "A",anos_ord[1], "-", anos_ord[length(anos_ord)], "_",
                        "V",paste(variaveis, collapse = "-"), "_",
                        "C",paste(categorias, collapse = "-"), "_",
                        "G",geo_escolha,
                        ".csv"
  )
  
  cat("\nNome do Arquivo de dados salvo na pasta selecionada:", nomeArquivo, "\n")
  
  utils::write.csv(
    resultado,
    file.path(pasta, nomeArquivo),
    row.names = FALSE
  )
  print(paste("OBRIGADO POR USAR O SIDRA F\u00c1CIL"))
  ppgr_num = 5
  load_logo(ppgr_num)
  return(resultado)
  
}
