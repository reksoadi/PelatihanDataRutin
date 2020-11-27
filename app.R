# Pertama-tama load library yang dibutuhkan untuk menjalankan semua script di aplikasi ini.
library(shiny)         # Shiny adalah library untuk membuat aplikasi interaktif di R
library(dplyr)         # dplyr adalah library untuk memanipulasi data seperti filtering, grouping, memilih variabel, menamai ulang variabel, dst.
library(leaflet)       # Leaflet adalah library untuk membuat pemetaan interaktif. Pelajari lebih lanjut di: https://rstudio.github.io/leaflet/
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
  title = "Pemantauan Stunting Kecamatan",                               # Cantumkan judul aplikasi di sini
  sidebarLayout(                                                         # Deklarasikan jenis tampilan Shiny yang ingin dibuat, di sini kita membuat model Sidebar Layout yang terdiri dari sidebar panel dan main panel
    sidebarPanel(                                                        # Memulai menulis kode untuk panel samping yang diisi oleh dua jenis input: pemilihan kecamatan dan pemilihan bulan
      
      ## Buat panel input untuk memilih kecamatan dengan selectInput. Masukkan inputID, label yang akan ditampilkan, dan pilihan jawabannya.
      selectInput(
        inputId = "pilihkecamatan", 
        label = "Pilih kecamatan",
        choices = c("Sewon", "Maros Baru"),
        multiple = FALSE                                                 # TRUE kalau bisa memilih lebih dari satu, FALSE kalau hanya boleh memilih satu
        ),
      
      ## Buat panel input untuk memilih bulan dengan cara yang sama seperti sebelumnya
      selectInput(
        inputId = "pilihbulan", 
        label = "Pilih bulan",
        choices = c("Januari", "Februari", "Maret", "April", "Mei", 
                    "Juni", "Juli", "Agustus", "September", "Oktober"),
        multiple = FALSE
      )
    ),                                                                   
    
    
    mainPanel(                                                           # Memulai menulis kode untuk panel utama yang akan menampilkan peta, grafik, dan tabel
      ## Tampilkan peta persebaran stunting
      h3("Peta persebaran balita stunting"),                             
      leafletOutput("peta"),
      tags$hr(),
      
      ## Tampilkan grafik kejadian stunting
      h3("Tren kejadian stunting"),
      highchartOutput("grafik"),
      tags$hr(),      
      
      ## Tampilkan tabel informasi anak stunting
      h3("Daftar anak yang masuk kategori stunting"),
      DT::dataTableOutput("tabel")
    )
  )
)


# ------------------------------------------------------------Server------------------------------------------------------------
# Server adalah bagian di mana manipulasi data, komputasi, dan visualisasi dilakukan.
server <- function(input, output) {
  
  output$peta <- renderLeaflet({                                        # Output dari pemetaan akan disimpan dalam "wadah" bernama "peta"
    data_peta <- data_peta %>%
      filter(Kecamatan == input$pilihkecamatan,                         # Filter data sesuai dengan pilihan kecamatan dan bulan        
             bulan == input$pilihbulan)
    
    leaflet() %>%                                                       # Gunakan fungsi leaflet() untuk mulai visualisasi spasial
      addTiles() %>%                                                    # addTiles() digunakan untuk menambahkan peta satelit atau peta gambar ke dalam visualisasi
      addCircleMarkers(                                                 # addCircleMarkers() digunakan untuk meletakkan penanda berbentuk lingkaran di titik yang ditentukan
        lng = data_peta$longitude,                                      # Posisi di garis bujur
        lat = data_peta$latitude,                                       # Posisi di garis lintang
        radius = data_peta$Kasus_stunting*2+1,                          # Membuat ukuran garis sesuai dengan jumlah balita stunting di Posyandu
        label = paste0(data_peta$Posyandu, ": ", data_peta$Kasus_stunting),    # Label pada peta menunjukkan nama Posyandu dan jumlah balita stunting-nya
        color = ifelse(data_peta$Kasus_stunting > 0, "red", "blue")     # Beri warna merah untuk Posyandu dengan kasus stunting dan warna biru untuk yang tidak
      )
  })

  
  output$grafik <- renderHighchart({
    data_grafik <- data_grafik %>%
      filter(Kecamatan == input$pilihkecamatan)                         # Filter data sesuai dengan pilihan kecamatan, karena grafik bersifat temporal kita tidak perlu melakukan filter bulan
    
    hchart(                                                             # Gunakan fungsi hchart() dari library highcharter untuk membuat visualisasi grafik
      object = data_grafik, 
      type = "line",                                                    # Masukkan tipe grafik "line" juga dapat diganti dengan "spline" untuk smoothed line atau "column" untuk barchart
      hcaes(x = tanggal,                                                # Definisikan sumbu X, dalam hal ini tanggal
            y = Kasus_stunting,                                         # Definisikan sumbu Y, dalam hal ini Kasus_stunting
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