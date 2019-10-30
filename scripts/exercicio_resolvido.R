### Resolução de Exercício

# NOTA: se você usa linux/mac, substitua o "\\r\\n" por "\n".

# define pasta de saída
output_folder <- "~/dados/exercicio"
dir.create( output_folder , recursive = TRUE , showWarnings = FALSE )

### 1. Escolha uma UF: Acre.

### 2. Colete os respectivos microdados de 2010 e 2015

# carrega libraries
library( RCurl )

# 2.a Nascidos Vivos

# define o link do sinasc
ftp_sinasc <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINASC/NOV/DNRES/"

# coleta nomes de arquivos
nasc_links <- getURL( ftp_sinasc , .opts = list( dirlistonly = TRUE ) )

# separa nomes de arquivos
nasc_links <- strsplit( nasc_links , "\\r\\\r\\n" , useBytes = TRUE )[[1]]

# filtra arquivos de 2010 e 2015
nasc_links <- nasc_links[ grepl( "2010|2015" , nasc_links ) ]

# filtra arquivos do Acre
nasc_links <- nasc_links[ grepl( "DNAC" , nasc_links ) ]

# completa link dos arquivos
nasc_links <- paste0( ftp_sinasc , nasc_links )

# baixa arquivos
for (this_link in nasc_links) download.file( this_link , file.path( output_folder , basename( this_link ) ) , mode = "wb" )

# 2.b Óbitos

# define o link do sim
ftp_sim <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DORES/"

# coleta nomes de arquivos
obit_links <- getURL( ftp_sim , .opts = list( dirlistonly = TRUE ) )

# separa nomes de arquivos
obit_links <- strsplit( obit_links , "\\r\\n" , useBytes = TRUE )[[1]]

# filtra arquivos de 2010 e 2015
obit_links <- obit_links[ grepl( "2010|2015" , obit_links ) ]

# filtra arquivos do Acre
obit_links <- obit_links[ grepl( "DOAC" , obit_links ) ]

# completa link dos arquivos
obit_links <- paste0( ftp_sim , obit_links )

# baixa arquivos
for (this_link in obit_links) download.file( this_link , file.path( output_folder , basename( this_link ) ) , mode = "wb" )

### 3. Leia os microdados.

# carrega libraries
library(read.dbc)
library(data.table)
library( magrittr ) # para o operador `%>%`

# lê dados de nascidos vivos
dt_nasc <- lapply( file.path( output_folder , basename( nasc_links ) ) , function( z ) as.data.table( read.dbc( z , as.is = TRUE ) ) ) %>% 
  rbindlist(., use.names = TRUE, fill = TRUE )

# calcula ano de nascimento
dt_nasc[ , ano := substr( DTNASC , 5 , 8 ) ]

# lê dados de óbitos
dt_obit <- lapply( file.path( output_folder , basename( obit_links ) ) , function( z ) as.data.table( read.dbc( z , as.is = TRUE ) ) ) %>% 
  rbindlist(., use.names = TRUE, fill = TRUE )

# calcula ano de óbito
dt_obit[ , ano := substr( DTOBITO , 5 , 8 ) ]

# ajusta idade em anos
dt_obit[ substr( IDADE , 1 , 1 ) %in% 4:5 , idade_anos := ( as.numeric( IDADE ) - 400 ) ]
dt_obit[ substr( IDADE , 1 , 1 ) %in% 0:3 , idade_anos := 0 ]
dt_obit[ IDADE == "000" , idade_anos := as.numeric(NA) ]

### 4. Calcule a Taxa de Mortalidade Infantil.

# calcula e combina totais
dtf <- merge( 
  dt_nasc[ , .( nnasc = .N ) , by = ano ] , # total de nascidos vivos
  dt_obit[ idade_anos < 1 , .( nobit = .N ) , by = ano ] , # total de obitos
  by = "ano" )

# calcula tmi
dtf[ , tmi := 1000 * nobit / nnasc ]