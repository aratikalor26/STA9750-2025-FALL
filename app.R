# app.R  — STA 9750 Shiny submission helper (single-file)

# --- Packages ---
if (!require("shiny")) install.packages("shiny"); library(shiny)
if (!require("glue"))  install.packages("glue");  library(glue)
if (!require("yaml"))  install.packages("yaml");  library(yaml)

# --- Course helpers for Step 3 ---
# (Loads mp_submission_verify, mp_feedback_locate, mp_feedback_verify, etc.)
source("https://michael-weylandt.com/STA9750/load_helpers.R")

# --- Global: course variables (repo name, course short title) ---
variables    <- yaml::read_yaml(
  "https://raw.githubusercontent.com/michaelweylandt/STA9750/refs/heads/main/_variables.yml"
)
course_repo  <- variables$course$repo     # e.g., "STA9750-2025-FALL"
course_short <- variables$course$short    # e.g., "STA 9750"

# Helpers to build URLs (tweak if your course uses a different pattern)
mp_two_digit <- function(n) sprintf("%02d", as.integer(n))
course_site  <- "https://michael-weylandt.com/STA9750"  # instructions live here

mp_instructions_url <- function(n) {
  # Example pattern: .../mini-projects/mp0X.html
  glue("{course_site}/mini-projects/mp0{n}.html")
}

student_repo_url <- function(gh) {
  glue("https://github.com/{gh}/{course_repo}")
}

student_site_url <- function(gh) {
  glue("https://{gh}.github.io/{course_repo}")
}

student_mp_page <- function(gh, n) {
  # Example pattern for your website submission page: .../mp0X/
  glue("{student_site_url(gh)}/mp0{n}/")
}

# --- UI ---
ui <- bootstrapPage(
  div(class = "container m-3",
      h3(glue("{course_short}: Shiny Submission Helper")),
      
      # Inputs
      textInput("github_id", "What is your GitHub ID?",
                placeholder = "e.g., aratikalor26"),
      numericInput("mp_num", "Mini-Project #", value = 1, min = 1, max = 4, step = 1),
      
      hr(), h4("Step 1 — Links"),
      uiOutput("gh_link"),        # link to GitHub repo
      uiOutput("profile_link"),   # link to MP#00 homepage
      
      hr(), h4("Step 2 — MP Links (parameterized by the number above)"),
      uiOutput("mp_instructions_link"),
      uiOutput("mp_submission_link"),
      
      hr(), h4("Step 3 — MP Submission Check"),
      uiOutput("mp_check_message"),
      
      hr(), h4("Step 4 — (Optional) Peer Feedback"),
      p(em("Add your peer's GitHub ID below to enable the checks.")),
      textInput("peer_github_id", "Peer GitHub ID (optional)", placeholder = "e.g., someclassmate"),
      uiOutput("peer_feedback_links"),
      uiOutput("peer_feedback_check")
  )
)

# --- Server ---
server <- function(input, output, session) {
  
  gh_ok <- reactive({
    id <- trimws(input$github_id)
    nzchar(id)
  })
  
  mp_ok <- reactive({
    isTRUE(!is.na(input$mp_num)) && input$mp_num >= 1 && input$mp_num <= 4
  })
  
  # ----- Step 1: Links to your repo and MP#00 homepage -----
  output$gh_link <- renderUI({
    req(gh_ok())
    id  <- trimws(input$github_id)
    url <- student_repo_url(id)
    tags$p("My code for this course is on ",
           tags$a(href = url, target = "_blank", url))
  })
  
  output$profile_link <- renderUI({
    req(gh_ok())
    id  <- trimws(input$github_id)
    url <- student_site_url(id)
    tags$p("My MP#00 homepage is at ",
           tags$a(href = url, target = "_blank", url))
  })
  
  # ----- Step 2: Parameterized MP links -----
  output$mp_instructions_link <- renderUI({
    req(mp_ok())
    n   <- as.integer(input$mp_num)
    url <- mp_instructions_url(n)
    tags$p("Mini-Project Instructions: ",
           tags$a(href = url, target = "_blank", url))
  })
  
  output$mp_submission_link <- renderUI({
    req(gh_ok(), mp_ok())
    id  <- trimws(input$github_id)
    n   <- as.integer(input$mp_num)
    url <- student_mp_page(id, n)
    tags$p("My Mini-Project submission page: ",
           tags$a(href = url, target = "_blank", url))
  })
  
  # ----- Step 3: Confirm Mini-Project Submission (helper script) -----
  output$mp_check_message <- renderUI({
    req(gh_ok(), mp_ok())
    id <- trimws(input$github_id)
    n  <- as.integer(input$mp_num)
    
    # run the verifier and capture any diagnostic text
    O <- capture.output({
      E <- tryCatch(mp_submission_verify(n, id), error = function(e) e)
    })
    
    if (isTRUE(E)) {
      tags$p(class = "text-success",
             glue("✅ Mini-Project {n} properly submitted for {id}."))
    } else {
      # O usually has details; show the first couple of lines for clarity
      msg <- "Mini-Project not properly submitted. Issue appears to be:\n"
      msg <- paste0(msg, paste(utils::head(O, 2), collapse = "\n"))
      tagList(
        tags$p(class = "text-danger",
               glue("⚠️ Mini-Project {n} NOT verified for {id}.")),
        tags$pre(msg)
      )
    }
  })
  
  # ----- Step 4 (optional): Peer feedback links & check -----
  output$peer_feedback_links <- renderUI({
    req(gh_ok())
    id <- trimws(input$github_id)
    n  <- if (mp_ok()) as.integer(input$mp_num) else NA_integer_
    
    # Try to list assigned issues (if the cycle exists)
    if (!is.na(n)) {
      # This helper opens browser or returns URLs depending on version.
      # We'll capture links (if printed) for convenience.
      L <- capture.output({
        try(mp_feedback_locate(n, github_id = id), silent = TRUE)
      })
      if (length(L)) {
        tagList(
          tags$p("Peer feedback issue links (if assigned):"),
          tags$pre(paste(L, collapse = "\n"))
        )
      } else {
        tags$p(em("No peer feedback assignments found yet (this is normal for later MPs)."))
      }
    }
  })
  
  output$peer_feedback_check <- renderUI({
    req(gh_ok(), nzchar(trimws(input$peer_github_id)))
    id_self  <- trimws(input$github_id)
    id_peer  <- trimws(input$peer_github_id)
    n        <- if (mp_ok()) as.integer(input$mp_num) else NA_integer_
    
    if (is.na(n)) return(NULL)
    
    L <- capture.output({
      E <- tryCatch(mp_feedback_verify(n, reviewer = id_self, reviewee = id_peer),
                    error = function(e) e)
    })
    
    if (exists("E") && isTRUE(E)) {
      tags$p(class = "text-success",
             glue("✅ Peer feedback for MP {n}: OK ({id_self} → {id_peer})."))
    } else {
      msg <- "Peer feedback not verified. Issue appears to be:\n"
      msg <- paste0(msg, paste(utils::head(L, 3), collapse = "\n"))
      tagList(
        tags$p(class = "text-danger",
               glue("⚠️ Peer feedback for MP {n} not verified ({id_self} → {id_peer}).")),
        tags$pre(msg)
      )
    }
  })
}

shinyApp(ui = ui, server = server)

