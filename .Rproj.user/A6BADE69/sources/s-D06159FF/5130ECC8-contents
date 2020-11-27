# Pertama-tama load library yang dibutuhkan untuk menjalankan semua script di aplikasi ini.
library(shiny)         # Shiny adalah library untuk membuat aplikasi interaktif di R
library(tidyverse)     # Tidyverse adalah library untuk memanipulasi data seperti filtering, grouping, memilih variabel, menamai ulang variabel, dst.
library(leaflet)       # Leaflet adalah library untuk membuat pemetaan interaktif. Pelajari lebih lanjut di: 
library(highcharter)   # Highcharter adalah library untuk membuat grafik interaktif. Pelajari lebih lanjut di: https://jkunst.com/highcharter/
library(DT)            # DT, singkatan dari DataTable, adalah library untuk formatting tabel
library(rio)           # Rio adalah library untuk membaca berbagai jenis file ke dalam R

# -----------------------------------------------------------Global------------------------------------------------------------

# Selanjutnya dalam global, kita membuat berbagai variabel pembantu yang akan digunakan dalam aplikasi

## Import file dari data-data yang berada di folder data. Fungsi yang digunakan adalah import() dari library "rio" 
data_peta <- import(file = "data/data_peta.xlsx")     # Import data untuk pemetaan dari file excel di folder "data" berjudul "data_peta.xlsx"
data_grafik <- import(file = "data/data_grafik.sav")  # Import data untuk membuat grafik dari file spss (sav) di folder "data" berjudul "data_grafik.sav"
data_tabel <- import(file = "data/data_tabel.csv")    # Import data untuk membuat tabel dari file csv di folder "data" berjudul "data_tabel.csv"


# --------------------------------------------------------------UI--------------------------------------------------------------
# UI atau User Interface adalah bagian yang akan menjadi tampilan aplikasi
ui <- fluidPage(
  title = "Pemantauan Stunting Kecamatan",
  sidebarLayout(  
    sidebarPanel(                                             
      selectInput(
        inputId = "pilihkecamatan", 
        label = "Pilih kecamatan",
        choices = c("Sewon", "Maros Baru"),
        multiple = FALSE       
      ),
      
      selectInput(
        inputId = "pilihbulan", 
        label = "Pilih bulan",
        choices = c("Januari", "Februari", "Maret", "April", "Mei", 
                    "Juni", "Juli", "Agustus", "September", "Oktober"),
        multiple = FALSE
      )
    ),                                                                   
    
    
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel(
          title = "Pemetaan",
          h3("Peta persebaran balita stunting"),                             
          leafletOutput("peta")
        ),
        
        tabPanel(
          title = "Grafik",
          h3("Tren kejadian stunting"),
          highchartOutput("grafik")
        ),
        
        tabPanel(
          title = "Tabel",
          h3("Daftar anak yang masuk kategori stunting"),
          DT::dataTableOutput("tabel")
        )
      )
    )
  )
)


# ------------------------------------------------------------Server------------------------------------------------------------
# Server adalah bagian di mana manipulasi data, komputasi, dan visualisasi dilakukan.
server <- function(input, output) {
  
  output$peta <- renderLeaflet({
    data_peta <- data_peta %>%
      filter(Kecamatan == input$pilihkecamatan, 
             bulan == input$pilihbulan)
    
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(
        lng = data_peta$longitude,
        lat = data_peta$latitude,
        radius = data_peta$Kasus_stunting*2+1, 
        label = paste0(data_peta$Posyandu, ": ", data_peta$Kasus_stunting),
        color = ifelse(data_peta$Kasus_stunting > 0, "red", "blue")
      )
  })
  
  
  output$grafik <- renderHighchart({
    data_grafik <- data_grafik %>%
      filter(Kecamatan == input$pilihkecamatan)
    
    hchart(
      object = data_grafik, 
      type = "line",     
      hcaes(x = tanggal,
            y = Kasus_stunting, 
            group = Posyandu)
    ) %>%
      hc_xAxis(title = list(text = "Bulan")) %>%
      hc_yAxis(title = list(text = "Jumlah kasus stunting")) 
    
  })
  
  
  output$tabel <- DT::renderDataTable({
    datatable({
      data_tabel <- data_tabel %>%
        filter(Kecamatan == input$pilihkecamatan,
               bulan == input$pilihbulan,
               status_stunting == 1) %>%
        select(nomor_KK,	NIK,	nama_anak,	jenis_kelamin,	usiaoktober, Posyandu)
      data_tabel
    })
  })
}

# ---------------------------------------------------Inisialisasi Aplikasi----------------------------------------------------

shinyApp(ui, server)