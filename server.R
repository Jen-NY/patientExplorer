library(DT)
library(RSQLite)
library(dplyr)
library(timevis)
library(plotly)
library(ggplot2)


# Connect to the database
db <- dbConnect(RSQLite::SQLite(), "/Users/huellein/Documents/project/cll/script/github/RLIMS/rlims.db")


# Get column names and tables
# ----
## column names
getColNames <- function(table) {
  colNames <- dbListFields(db, table)
}

# 2. Function to get the tables with exchanged fk
getTbl <- function(table) {
  if (table == "patient") {
    query <- dbGetQuery(db, "SELECT p.patid, p.patpatientid, p.patpseudoid, pr.prjname, p.patdiagnosis, p.patdiagnosissub, p.patsex, p.patcomment
                        FROM patient p
                        LEFT OUTER JOIN project pr ON p.patprjidref = pr.prjid
                        ORDER BY p.patpatientid;")
  } else if (table == "sample") {
    query <- dbGetQuery(db, "SELECT s.smpid, p.patpatientid, s.smpsampleid, s.smpsampledate, s.smpdatereceived,  
                        s.smpleukocytes, s.smppblymphocytes, s.smpcomment
                        FROM sample s
                        LEFT OUTER JOIN patient p ON s.smppatidref = p.patid
                        ORDER BY p.patpatientid, s.smpsampleid;")
  } else if (table == "analysis") {
    query <- dbGetQuery(db,"SELECT a.anlid, s.smpsampleid, o.anuname, a.anlstatus, a.anldate, a.anlrun, a.anltype, a.anlcomment
                        FROM analysis a 
                        LEFT OUTER JOIN sample s ON a.anlsmpidref = s.smpid
                        LEFT OUTER JOIN analysislookup o ON a.anlanuidref = o.anuid
                        ORDER BY s.smpsampleid, o.anuname;")
  } else if (table == "analysislookup") {
    query <- dbGetQuery(db,"SELECT anuid, anuname, anucat, anudescription
                        FROM analysislookup
                        ORDER BY anuname;")
  } else if (table == "storage") {
    query <- dbGetQuery(db, "SELECT stoid, stofreezer, stotype, stotower, stobox, stolayout
                        FROM storage
                        ORDER BY stofreezer, stotower, stobox;")
  } else if (table == "aliquot") {
    query <- dbGetQuery(db,"SELECT a.alqid, s.smpsampleid, a.alqdate, users1.usrinitials AS prepared_by, a.alqsampletype, a.alqcelltype, a.alqcellnumber, a.alqvolume, a.alqconc, a.alqbox, a.alqposition, a.alqempty, a.alqdateused, users2.usrinitials AS used_by, a.alqpurpose, a.alqcomment 
                        FROM aliquot a
                        LEFT OUTER JOIN sample s ON a.alqsmpidref = s.smpid
                        LEFT OUTER JOIN users AS users1 ON a.alqusridref = users1.usrid 
                        LEFT OUTER JOIN users AS users2 ON a.alqusedusridref = users2.usrid
                        ORDER BY alqid;")
  } else if (table == "project") {
    query <- dbGetQuery(db, "SELECT prjid, prjname, prjdisease, prjmaterial, prjfirstname, prjlastname, prjdepartment, prjinstitute, prjcity, prjcountry, prjdescription
                        FROM project
                        ORDER BY prjname;")
  } else if (table == "users") {
    query <- dbGetQuery(db, "SELECT usrid, usrinitials, usrfirstname, usrlastname, usrposition, usrstartdate, usrenddate 
                        FROM users 
                        ORDER BY usrinitials;")
  }
  }

# 3. Generate a dataframe that matches the DB column names with more readible column names.
tblPat_fields <- getColNames("patient")
tblPat_names <- c("AutoID", "Patient ID", "Pseudonym", "Project", "Diagnosis", "Diagnosis subtype", "Gender", "Comment")
tblPat_matchNames <- as.data.frame(cbind(Fields = tblPat_fields, Names = tblPat_names))

tblSmp_fields <- getColNames("sample")
tblSmp_names <- c("AutoID", "Patient ID", "Sample ID", "Date", "Received on", "Leukocyte count", "%-PB-lymphocytes", "Comment")
tblSmp_matchNames <- as.data.frame(cbind(Fields = tblSmp_fields, Names = tblSmp_names))

tblAlq_fields <- names(getTbl("aliquot"))
tblAlq_names <- c("AutoID", "Sample ID", "Prepared on", "Prepared by", "Sample type", "Cell type", "Cell number", "Volume", "Concentration", "Box", "Position", "Empty", "Used on", "Used by", "Used for", "Comment")
tblAlq_matchNames <- as.data.frame(cbind(Fields = tblAlq_fields, Names = tblAlq_names))

tblAnl_fields <- getColNames("analysis")
tblAnl_names <- c("AutoID", "Sample ID", "Analysis", "Status", "Date", "Run", "Specification", "Comment")
tblAnl_matchNames <- as.data.frame(cbind(Fields = tblAnl_fields, Names = tblAnl_names))

tblAnu_fields <- getColNames("analysislookup")
tblAnu_names <- c("AutoID", "Analysis", "Category", "Description")
tblAnu_matchNames <- as.data.frame(cbind(Fields = tblAnu_fields, Names = tblAnu_names))

tblPrj_fields <- getColNames("project")
tblPrj_names <- c("AutoID", "Project", "Disease", "Material", "First name", "Last name", "Department", "Institute", "City", "Country", "Description")
tblPrj_matchNames <- as.data.frame(cbind(Fields = tblPrj_fields, Names = tblPrj_names))

tblUsr_fields <- getColNames("users")
tblUsr_names <- c("AutoID", "Initials", "First name", "Last name", "Position", "Start date", "End date")
tblUsr_matchNames <- as.data.frame(cbind(Fields = tblUsr_fields, Names = tblUsr_names))

tbl_matchNames <- unique(rbind(tblPat_matchNames, tblSmp_matchNames, tblAnl_matchNames, tblAnu_matchNames, tblAlq_matchNames, tblPrj_matchNames, tblUsr_matchNames))
tbl_matchNames$Fields <- as.character(tbl_matchNames$Fields)
tbl_matchNames$Names <- as.character(tbl_matchNames$Names)
# ----

## Retrieve tables
pat <- getTbl("patient")
smp <- getTbl("sample")
sto <- getTbl("storage")
sto$box <- paste0(sto$stofreezer, "_", sto$stotype, "_T", sto$stotower, "_B", sto$stobox)
alq <- getTbl("aliquot")
anl <- getTbl("analysis")
anu <- getTbl("analysislookup")
prj <- getTbl("project")
usr <- getTbl("users")

dbDisconnect(db)


# Sample timeline: Timevis plot
# ----
# Prepare the dataframe
tv_sample <- smp %>%
  mutate(start = as.Date(smpsampledate, format = "%Y-%m-%d")) %>%
  mutate(groupName = "Sample") %>%
  filter(!is.na(start)) %>%
  select(patpatientid, start, content = smpsampleid, groupName)

# Add sample and patient data to the analysis
tv_analysis <- anl %>%
  inner_join(smp[,c("patpatientid", "smpsampleid", "smpsampledate")], by = "smpsampleid") %>%
  inner_join(anu[,c("anuname", "anucat")], by = "anuname") %>%
  mutate(start = as.Date(smpsampledate, format = "%Y-%m-%d")) %>%
  #mutate(anuname = gsub("DS_", "", anuname)) %>%
  #mutate(anuname = gsub("CpG_", "", anuname)) %>%
  filter(!is.na(start)) %>%
  select(patpatientid, start, content = anuname, groupName = anucat)

ttSamples <- rbind(tv_sample, tv_analysis)

# Define the groups
mygroups <- data.frame(
  id = 1:(length(unique(anu$anucat))+1),
  content = c("Sample", unique(anu$anucat))
)

ttSamples$group <- mygroups$id[match(ttSamples$groupName, mygroups$content)]

# -----


# Amplicon seq
# ----
ampliconSeq_snv <- read.csv("/Users/huellein/Documents/project/cll/script/github/patientExplorer/ampliconSeq_random_data.csv")
# Add the patient id
ampliconSeq_snv <- merge(smp[, c("smpsampleid", "patpatientid")], ampliconSeq_snv, by.x = "smpsampleid", by.y = "Sample", all.y = TRUE)
# ----


# WES
# ----
wes_snv <- read.csv("/Users/huellein/Documents/project/cll/script/github/patientExplorer/wes_random_data.csv")
# Add the patient id
wes_snv <- merge(smp[, c("smpsampleid", "patpatientid")], wes_snv, by.x = "smpsampleid", by.y = "Sample", all.y = TRUE)

wes_cnv_plot <- list.files("www/WES")
names(wes_cnv_plot) <- sapply(strsplit(wes_cnv_plot, "_"), `[`, 1 )

# ----


function(input, output, session) {
  
  # Tab Queries
  # ----
  
  ## Generate output tables
  analysis_loop <- sort(unique(anl$anuname))
  
  ### Sample output table
  smp_output <- smp
  for(i in analysis_loop) {
    sub <- filter(anl, anuname == i, anl$anltype != "C")
    smp_output[[i]] <- ifelse(smp_output$smpsampleid %in% sub$smpsampleid[sub$anlstatus %in% c("data received", "screened")], "data received",
                       ifelse(smp_output$smpsampleid %in% sub$smpsampleid[sub$anlstatus %in% c("pre-selected", "selected")], "planned",
                              ifelse(smp_output$smpsampleid %in% sub$smpsampleid[sub$anlstatus %in% c("submitted")], "submitted",
                                     ifelse(smp_output$smpsampleid %in% sub$smpsampleid[sub$anlstatus %in% c("failed", "test")], "failed or test",
                                            NA))))
  }
  
  ### Patient output table
  pat_output <- pat
  for(i in analysis_loop) {
    sub <- select(smp_output)
    pat_output[[i]] <- ifelse(pat_output$patpatientid %in% smp_output$patpatientid[smp_output[[i]] == "data received"], "data received",
                              ifelse(pat_output$patpatientid %in% smp_output$patpatientid[smp_output[[i]] == "planned"], "planned",
                                     ifelse(pat_output$patpatientid %in% smp_output$patpatientid[smp_output[[i]] == "submitted"], "submitted",
                                            ifelse(pat_output$patpatientid %in% smp_output$patpatientid[smp_output[[i]] == "failed or test"], "failed or test", 
                                            NA))))
  }
    
  
  ## Filters
  
  ### Project filter
  output$flt_project <- renderUI({
    values <- prj$prjname
    selectInput("flt_project", "Projects", values, multiple = TRUE, selected = values)
  })
  
  get_flt_project <- reactive({
    input$flt_project  
  })
  
  #### Select all button 
  observe({
    if(input$selectall_flt_project == 0) return(NULL)
    else if (input$selectall_flt_project > 0) {
      values <- prj$prjname
      updateSelectInput(session, "flt_project", selected = values)
    }
  })
  
  #### Deselect all button 
  observe({
    if(input$deselectall_flt_project == 0) return(NULL)
    else if (input$deselectall_flt_project > 0) {
      updateSelectInput(session, "flt_project", selected = "")
    }
  })
  
  
  ### Diagnosis filter
  output$flt_diagnosis <- renderUI({
    values <- sort(unique(pat$patdiagnosis))
    selectInput("flt_diagnosis", "Diagnosis", values, multiple = TRUE, selected = values)
  })
  
  get_flt_diagnosis <- reactive({
    input$flt_diagnosis
  })
  
  #### Select all
  observe({
    if (input$selectall_flt_diagnosis > 0) {
      values <- sort(unique(pat$patdiagnosis))
      updateSelectInput(session, "flt_diagnosis", selected = values)
    } else return(NULL)
  })
  
  #### Deselect all
  observe({
    if(input$deselectall_flt_diagnosis > 0) {
      updateSelectInput(session, "flt_diagnosis", selected = "")
    }
  })
  
  
  ### Aliquot sample type filter
  output$flt_alqsampletype <- renderUI({
    values <- sort(unique(alq$alqsampletype))
    selectInput("flt_alqsampletype", "Cell type", values, multiple = TRUE, selected = values)
  })
  
  get_flt_alqsampletype <- reactive({
    input$flt_alqsampletype
  })
  
  #### Select all
  observe({
    if (input$selectall_flt_alqsampletype > 0) {
      values <- sort(unique(alq$alesampletype))
      updateSelectInput(session, "flt_alqsampletype", selected = values)
    } else return(NULL)
  })
  
  #### Deselect all
  observe({
    if(input$deselectall_flt_alqsampletype > 0) {
      updateSelectInput(session, "flt_alqsampletype", selected = "")
    }
  })
  
  
  ### Aliquot cell type filter
  output$flt_alqcelltype <- renderUI({
    values <- sort(unique(alq$alqcelltype))
    selectInput("flt_alqcelltype", "Cell type", values, multiple = TRUE, selected = values)
  })
  
  get_flt_alqcelltype <- reactive({
    input$flt_alqcelltype
  })
  
  #### Select all
  observe({
    if (input$selectall_flt_alqcelltype > 0) {
      values <- sort(unique(alq$alecelltype))
      updateSelectInput(session, "flt_alqcelltype", selected = values)
    } else return(NULL)
  })
  
  #### Deselect all
  observe({
    if(input$deselectall_flt_alqcelltype > 0) {
      updateSelectInput(session, "flt_alqcelltype", selected = "")
    }
  })
  
  
  ### Analysis
  output$flt_analysis <- renderUI({
    values <- sort(unique(anl$anuname))
    selectInput("flt_analysis", "Analysis", choices = values, multiple = TRUE, selected = values)
  })
  
  get_flt_analysis <- reactive({
    input$flt_analysis
  })
  
  #### Select all
  observe({
    if (input$selectall_flt_analysis > 0) {
      values <- sort(unique(anl$anuname))
      updateSelectInput(session, "flt_analysis", selected = values)
    } else return(NULL)
  })
  
  #### Deselect all
  observe({
    if(input$deselectall_flt_analysis > 0) {
      updateSelectInput(session, "flt_analysis", selected = "")
    }
  })
  
  
  ## Tables
  
  ### Patients
  get_qyr_pat_filtered <- reactive({
    
    # Diagnosis filter
    if(!is.null(input$flt_diagnosis_na)) {
      dgn_filter <- c(get_flt_diagnosis(), NA)
    } else {
      dgn_filter <- get_flt_diagnosis()
    }
    pat_filtered <- filter(pat_output, prjname %in% get_flt_project(), patdiagnosis %in% dgn_filter)
    pat_filtered
    
  })
  
  output$qyr_pat <- DT::renderDataTable({
    tbl <- get_qyr_pat_filtered()
    names(tbl) <- ifelse(names(tbl) %in% tbl_matchNames$Fields, tbl_matchNames$Names[match(names(tbl), tbl_matchNames$Fields)], names(tbl))
    return(tbl)
  }, rownames = FALSE, filter = 'top', options = list(pageLength = 25, lengthMenu = c(10, 25, 50, 100), columnDefs = list(list(visible = FALSE, targets = 0))))
  
  output$download_pat <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), "_patients.csv")
    },
    content = function(file) {
      tbl <- get_qyr_pat_filtered()
      tbl$patid <- NULL
      names(tbl) <- ifelse(names(tbl) %in% tbl_matchNames$Fields, tbl_matchNames$Names[match(names(tbl), tbl_matchNames$Fields)], names(tbl))
      write.csv(tbl, file, row.names = FALSE)
    }
  )
  

  ### Samples
  get_qyr_smp_filtered <- reactive({
    pat_filtered <- get_qyr_pat_filtered()
    smp_filtered <- filter(smp_output, patpatientid %in% pat_filtered$patpatientid)
    smp_filtered
  })
  
  output$qyr_smp <- DT::renderDataTable({
    tbl <- get_qyr_smp_filtered()
    names(tbl) <- ifelse(names(tbl) %in% tbl_matchNames$Fields, tbl_matchNames$Names[match(names(tbl), tbl_matchNames$Fields)], names(tbl))
    return(tbl)
  }, rownames = FALSE, filter = 'top', options = list(pageLength = 25, lengthMenu = c(10, 25, 50, 100), columnDefs = list(list(visible = FALSE, targets = 0))))
  
  output$download_smp <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), "_samples.csv")
    },
    content = function(file) {
      tbl <- get_qyr_smp_filtered()
      tbl$smpid <- NULL
      names(tbl) <- ifelse(names(tbl) %in% tbl_matchNames$Fields, tbl_matchNames$Names[match(names(tbl), tbl_matchNames$Fields)], names(tbl))
      write.csv(tbl, file, row.names = FALSE)
    }
  )

  
  ### Aliquot
  get_qyr_alq_filtered <- reactive({
    
    if(!is.null(input$flt_alqcelltype_na)) {
      celltype_filter <- c(get_flt_alqcelltype(), NA)
    } else {
      celltype_filter <- get_flt_alqcelltype()
    }
    
    empty_filter <- gsub("^$", NA, input$flt_empty)
    
    smp_filtered <- get_qyr_smp_filtered()
    
    alq_filtered <- alq %>%
      filter(smpsampleid %in% smp_filtered$smpsampleid, alqsampletype %in% get_flt_alqsampletype(), alqcelltype %in% celltype_filter) %>%
      left_join(smp[, c("smpsampleid", "patpatientid")], by = "smpsampleid") %>%
      arrange(patpatientid, smpsampleid)
  })
  
  output$qyr_alq <- DT::renderDataTable({
    
    alq_filtered <- get_qyr_alq_filtered()
    names(alq_filtered) <- ifelse(names(alq_filtered) %in% tbl_matchNames$Fields, tbl_matchNames$Names[match(names(alq_filtered), tbl_matchNames$Fields)], names(alq_filtered))
    return(alq_filtered)
  }, rownames = FALSE, filter = 'top', options = list(pageLength = 25, lengthMenu = c(10, 25, 50, 100), columnDefs = list(list(visible = FALSE, targets = 0))))
  
  output$download_alq <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), "_aliquots.csv")
    },
    content = function(file) {
      tbl <- get_qyr_alq_filtered()
      names(tbl) <- ifelse(names(tbl) %in% tbl_matchNames$Fields, tbl_matchNames$Names[match(names(tbl), tbl_matchNames$Fields)], names(tbl))
      write.csv(tbl, file, row.names = FALSE)
    }
  )

  
  ### Analysis
  get_qyr_anl_filtered <- reactive({
    
    smp_filtered <- get_qyr_smp_filtered()
    anl_smp <- merge(smp_filtered, anl, by = "smpsampleid")
    
    anl_filtered <- anl_smp %>%
      filter(anuname %in% input$flt_analysis) %>%
      arrange(patpatientid, smpsampledate, anuname)
  })
  
  output$qyr_anl <- DT::renderDataTable({
    tbl <- get_qyr_anl_filtered()
    names(tbl) <- ifelse(names(tbl) %in% tbl_matchNames$Fields, tbl_matchNames$Names[match(names(tbl), tbl_matchNames$Fields)], names(tbl))
    return(tbl)
    
  }, rownames = FALSE, filter = 'top', options = list(pageLength = 25, lengthMenu = c(10, 25, 50, 100)))
  
  output$download_anl <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), "_analysis.csv")
    },
    content = function(file) {
      tbl <- get_qyr_anl_filtered()
      names(tbl) <- ifelse(names(tbl) %in% tbl_matchNames$Fields, tbl_matchNames$Names[match(names(tbl), tbl_matchNames$Fields)], names(tbl))
      write.csv(tbl, file, row.names = FALSE)
    }
  )
  
  # ----


  # Tab Boxes
  # ----
  
  ## UI select box
  output$select_box <- renderUI({
    values <- sto[["box"]]
    selectInput("select_box", "Select box", choices = c("", values), selected = "")
  })
  
  
  ## Filter aliquot table by box and add patient ID and sample ID
  get_box_filtered <- reactive({
    
    #### If a box is selected, filter aliquot table and format the table
    if(length(input$select_box) > 0) {
      if( !is.null(input$select_box) & input$select_box != "" ) {
        
        alq_filtered <- filter(alq, alqbox == input$select_box)
        
        smp_alq <- merge(smp[,c("patpatientid", "smpsampleid")], alq_filtered, by = "smpsampleid")
        
        alq_format <- smp_alq %>%
          arrange(alqposition) %>%
          select(-alqid)
        
      } 
    }
    
  })
  
  
  ## Box table
  ### Output box table
  output$tbl_box <- DT::renderDataTable({
    
    if(length(input$select_box) > 0) {
      if( !is.null(input$select_box) & input$select_box != "" ) {
        
        tbl <- get_box_filtered()
        names(tbl) <- ifelse(names(tbl) %in% tbl_matchNames$Fields, tbl_matchNames$Names[match(names(tbl), tbl_matchNames$Fields)], names(tbl))
        return(tbl)
      }
    }
    
  }, rownames = FALSE, filter = 'top', options = list(pageLength = 25, lengthMenu = c(10, 25, 50, 100)))
  
  ### Download box table
  output$download_tbl_box <- downloadHandler(
    filename = function() {
      paste0(input$select_box, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      tbl <- get_box_filtered()
      names(tbl) <- ifelse(names(tbl) %in% tbl_matchNames$Fields, tbl_matchNames$Names[match(names(tbl), tbl_matchNames$Fields)], names(tbl))
      write.csv(tbl, file, row.names = FALSE)
    }
  )


  ## Box plot
  ### Output plot
  get_plot_box <- reactive({
    
    #### Get the box layout (e.g. 9er or 10er)
    selected_box <- filter(sto, box == input$select_box)
    x <- as.integer(as.integer(strsplit(selected_box$stolayout, "x")[[1]][[1]]))
    
    #### Generate a table with the positions
    boxlist <- data.frame(
      rowID = factor(rep(LETTERS[1:x], each = x), levels = rev(LETTERS[1:x])), 
      colID = factor(rep(1:x, times = x)), 
      position = 1:x^2)
    
    #### Add the aliquot information, if available
    alq_filtered <- get_box_filtered()
    if(nrow(alq_filtered) > 0) {
      alq_format <- select(alq_filtered, patient = patpatientid, sample = smpsampleid, material = alqsampletype, cells = alqcelltype, cellnumber = alqcellnumber, position = alqposition, empty = alqempty)
      alq_format$material <- gsub("peripheral blood", "PB", alq_format$material)
      alq_format$material <- gsub("bone marrow", "BM", alq_format$material)
      alq_format$material <- gsub("lymph node", "LN", alq_format$material)
      alq_format$material <- gsub("serum", "SE", alq_format$material)
      alq_format$material <- gsub("saliva", "SA", alq_format$material)
      boxlist_alq <- merge(boxlist, alq_format, by = "position", all.x = TRUE)
      boxlist_alq$status <- factor(ifelse(is.na(boxlist_alq$empty), "empty", ifelse(boxlist_alq$empty == TRUE, "used", "filled")), levels = c("empty", "filled", "used"))
    } else {
      boxlist_alq <- boxlist
      boxlist_alq$patient <- NA
      boxlist_alq$sample <- NA
      boxlist_alq$material = NA
      boxlist_alq$cells <- NA
      boxlist_alq$cellnumber <- NA
      boxlist_alq$status <- "empty"
    }
    
    #### Generate the plot
    p <- ggplot(boxlist_alq, aes(colID, rowID)) +
      geom_tile(aes(fill = status)) +
      scale_fill_manual(values = c("empty" = "grey", "filled" = "#F8766D", "used" = "#619CFF")) +
      ggtitle(input$select_box) +
      theme(axis.title=element_blank(),
            axis.text=element_blank(),
            axis.ticks=element_blank(),
            plot.title = element_text(hjust = 0.5, size = 20),
            legend.text = element_text(size = 16)) +
      geom_hline(yintercept = seq(1.5, 9.5, by = 1), color = "white") +
      geom_vline(xintercept = seq(1.5, 9.5, by = 1), color = "white") +
      geom_text(aes(label=sprintf("%s_%s\n%s\n%s\n%s\n%s", position, patient, sample, material, cells, cellnumber)), size = 5)
    
    p
    
  })
  
  output$plot_box <- renderPlot({
    p <- get_plot_box()
    p
  }, width = 1000, height = 1000)
  
  output$download_plot_box <- downloadHandler(
    filename = function() {
      paste0(input$select_box, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      ggsave(file, get_plot_box(), width = 14, height = 14)
    }
  )
  
  # ----
  
  # Tab Genetic data
  
  ### Patient input
  # Select patient ID from dropdown menu
  output$selectPatientID <- renderUI({
    values <- pat$patpatientid
    selectizeInput("selectPatientID", "Enter patient ID", choices = values, selected = "P0001" )
  })
  
  # Output patient table
  output$exp_pat <- DT::renderDataTable({
    
    if(!is.null(input$selectPatientID)) {
      if(input$selectPatientID %in% pat$patpatientid){
        
        pat_filtered <- pat %>%
          filter(patpatientid == input$selectPatientID)
        names(pat_filtered) <- ifelse(names(pat_filtered) %in% tbl_matchNames$Fields, tbl_matchNames$Names[match(names(pat_filtered), tbl_matchNames$Fields)], names(pat_filtered))
        pat_filtered
        
      }
    }
    
  }, rownames = FALSE, options = list(dom = 'F', columnDefs = list(list(visible = FALSE, targets = 0))) )
  
  
  ### Make report
  # ---- Start of report
  
  # Call the report
  observeEvent(input$makeReport, {
    
    patient <- input$selectPatientID
    
    # Format tables
    wes_snv_report <- wes_snv %>% 
      filter(patpatientid == patient) %>%
      mutate(Gene = sapply(strsplit(as.character(Gene.Name), "_"), `[`, 1 ) ) %>%
      arrange(Gene)
    
    # "_" is pre-occupied in latex. Exchange with ".."
      wes_snv_report <- as.data.frame(sapply(wes_snv_report, function(x) gsub("_", "..", x)))
    
    # To shorten the gene names, I removed the ENST IDs
    wes_snv_report <- wes_snv_report %>% 
      select(smpsampleid, Gene, CDS = CDS.Mutation, AA = AA.Mutation, n.control = counts_control, n.tumor = counts_tumor, VAF)
    # Create the report
      rmarkdown::render(input = "report.Rmd",
                        output_format = "pdf_document",
                        output_file = paste0("report_", patient, "_", Sys.Date(), ".pdf"),
                        output_dir = "/Users/huellein/Documents/project/cll/script/github/patientExplorer/report")
  })
  # ---- End of report
  
  
  ### Timevis
  # ----
  # Scale timevis plot
  observeEvent(input$scale, {
    fitWindow("timeline", list(animation = FALSE))
  })
  
  # Samples timeline
  ttSamples_filtered <- reactive({
    
    if(!is.null(input$selectPatientID)) {
      ttSamples %>%
        filter(patpatientid == input$selectPatientID, !is.na(group)) %>% # The group cannot be NA in timevis. If there is a new analysis, adjust the code above.
        select(content, start, group)
    }
  })
  output$timeline <- renderTimevis(
    if(!is.null(input$selectPatientID)) {
      timevis(data = ttSamples_filtered(),
              groups = mygroups)
    }
  )
  # ----
  
  
  ### Sample table
  # ----
  output$exp_smp <- DT::renderDataTable({
    
    if(!is.null(input$selectPatientID)) {
      if(input$selectPatientID %in% smp$patpatientid) {
        
        tbl <- smp %>%
          filter(patpatientid == input$selectPatientID)
        names(tbl) <- ifelse(names(tbl) %in% tbl_matchNames$Fields, tbl_matchNames$Names[match(names(tbl), tbl_matchNames$Fields)], names(tbl))
        tbl 
        
      }
    }
    
  }, selection = 'single', rownames = FALSE, options = list(columnDefs = list(list(visible = FALSE, targets = 0))) )
  # ----

  
  ### Aliquot table
  output$exp_alq <- DT::renderDataTable({
    
    # Catch the input values
    selectPat <- input$selectPatientID
    rowID <- input$exp_smp_rows_selected
    
    if(!is.null(selectPat)){
      
      # Get the selected sample
      smp_filtered <- filter(smp, patpatientid == selectPat)
      smp_selected <- smp_filtered[rowID,] 
      
      if(!is.null(rowID) ){
        
        tbl <- alq %>%
          filter(smpsampleid == smp_selected$smpsampleid)
        names(tbl) <- ifelse(names(tbl) %in% tbl_matchNames$Fields, tbl_matchNames$Names[match(names(tbl), tbl_matchNames$Fields)], names(tbl))
        
        datatable(tbl, rownames = FALSE, filter = 'top', options = list(columnDefs = list(list(visible = FALSE, targets = 0))) ) 
      }
    }
    
  } ) 
  
  
  ### amplicon sequencing
  output$tbl_ampliconSeq_snv <- DT::renderDataTable({
    
    if(input$selectPatientID %in% ampliconSeq_snv$patpatientid) {
      ampliconSeq_snv_filtered <- ampliconSeq_snv %>%
        filter(patpatientid %in% input$selectPatientID) %>%
        arrange(Gene.Name, CDS.Mutation) %>%
        select(Sample = smpsampleid, Gene = Gene.Name, CDS = CDS.Mutation, AA = AA.Mutation, counts_control, counts_tumor, VAF) 
      ampliconSeq_snv_filtered
    } else NULL
    
  }, rownames = FALSE, filter = 'top' )
  
  
  ### WES
  output$tbl_wes_snv <- DT::renderDataTable({
    
    if(input$selectPatientID %in% wes_snv$patpatientid) {
      wes_snv_filtered <- wes_snv %>%
        filter(patpatientid %in% input$selectPatientID) %>%
        arrange(Gene.Name, CDS.Mutation) %>%
        select(Sample = smpsampleid, Gene = Gene.Name, CDS = CDS.Mutation, AA = AA.Mutation, counts_control, counts_tumor, VAF) 
      wes_snv_filtered
    } else NULL
    
  }, rownames = FALSE, filter = 'top' )
  
  # WES CNV plots
  output$plot_wes_cnv <- renderUI({
    
    smpID <- smp$smpsampleid[smp$patpatientid == input$selectPatientID & smp$smpsampleid %in% names(wes_cnv_plot)]
    fileName <- subset(wes_cnv_plot, names(wes_cnv_plot) %in% smpID)
    tags$iframe(src = paste0("WES/", fileName), height = 600, width = 1200)
  })
  
}