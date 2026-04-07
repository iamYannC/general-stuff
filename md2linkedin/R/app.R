# ══════════════════════════════════════════════════════════════════════════════
# md2linkedin – Shiny Web App
# Beautiful UI for converting Markdown to LinkedIn-ready text.
# ══════════════════════════════════════════════════════════════════════════════
# TABLE OF CONTENTS:
# 1. External Imports (Libraries & main.R)
# 2. UI Definition
# 3. Server Logic
# ══════════════════════════════════════════════════════════════════════════════

# ── 1. External Imports ───────────────────────────────────────────────────────
library(shiny)

source(file.path(dirname(sys.frame(1L)$ofile %||% "."), "main.R"), local = TRUE)

# Null-coalescing (in case main.R didn't define it)
if (!exists("%||%")) `%||%` <- function(x, y) if (is.null(x)) y else x

# Explicitly mount 'www' directory so assets load regardless of how the app is launched
local({
  app_dir <- tryCatch(dirname(sys.frame(1L)$ofile), error = function(e) getwd())
  www_dir <- normalizePath(file.path(app_dir, "www"), mustWork = FALSE)
  if (dir.exists(www_dir)) {
    shiny::addResourcePath("app_www", www_dir)
  }
})
# ── 2. UI Definition ──────────────────────────────────────────────────────────
ui <- fluidPage(
  tags$head(
    tags$meta(charset = "UTF-8"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$title("md2linkedin — Markdown to LinkedIn Converter"),
    tags$link(rel = "icon", type = "image/x-icon", href = "app_www/assets/favicon.ico"),
    tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.3/font/bootstrap-icons.min.css"),
    tags$script(type = "module", src = "https://cdn.jsdelivr.net/npm/emoji-picker-element@1/index.js"),
    tags$link(rel = "stylesheet", type = "text/css", href = "app_www/style.css?v=4"),
    tags$script(src = "app_www/app.js?v=5", defer = NA)
  ),


  # Header
  div(class = "app-header",
    div(class = "logo",
      tags$img(src = "app_www/assets/logo.png", class = "logo-icon", alt = "md2linkedin logo"),
      div(class = "logo-text-group",
        span(class = "logo-text", "md2linkedin"),
        span(class = "logo-subtitle", "Convert Markdown to LinkedIn-ready posts")
      )
    ),
    div(class = "header-options",
      tags$label(class = "opt-label",
        tags$input(type = "checkbox", id = "preserve_links", class = "toggle-input",
                   onclick = "Shiny.setInputValue('preserve_links', this.checked, {priority:'event'})"),
        span(class = "toggle-track"), span("Preserve Links")
      ),
      tags$label(class = "opt-label",
        tags$input(type = "checkbox", id = "monospace_code", class = "toggle-input", checked = "checked",
                   onclick = "Shiny.setInputValue('monospace_code', this.checked, {priority:'event'})"),
        span(class = "toggle-track"), span("Monospace Code")
      ),
      tags$button(class = "tool-btn", id = "theme_toggle",
                  title = "Toggle light/dark theme", onclick = "toggleTheme()",
        tags$i(class = "bi bi-sun-fill", id = "theme_icon")
      )
    )
  ),

  # Editor layout
  div(class = "editor-wrap",

    # ── Left panel: Editor ──
    div(class = "panel",
      div(class = "panel-head",
        # span(class = "panel-title", HTML("&#9998; Markdown Editor")),
        div(class = "toolbar",
          tags$button(class = "tool-btn", title = "Bold (Ctrl+B)", onclick = "wrapSelection('**','**')",
            tags$i(class = "bi bi-type-bold")),
          tags$button(class = "tool-btn", title = "Italic (Ctrl+I)", onclick = "wrapSelection('*','*')",
            tags$i(class = "bi bi-type-italic")),
          tags$button(class = "tool-btn", title = "Underline (Ctrl+U)", onclick = "applyCombining(0x0332)",
            tags$i(class = "bi bi-type-underline")),
          tags$button(class = "tool-btn", title = "Strikethrough", onclick = "applyCombining(0x0336)",
            tags$i(class = "bi bi-type-strikethrough")),
          span(class = "tool-sep"),
          tags$select(id = "font_style_select", class = "tool-btn", style = "width: 155px; padding: 0 8px; text-align: left; -webkit-appearance: none; -moz-appearance: none; appearance: none; background: var(--bg-surface-hover); border: 1px solid var(--border);",
                      onchange = "if(this.value !== '') { applyFontStyle(); this.selectedIndex = 0; }",
                      tags$option(value = "", HTML("Font Style&hellip;"), disabled = NA, selected = NA, hidden = "hidden"),
                      tags$option(value = "bold", "𝗦𝗮𝗻𝘀 𝗕𝗼𝗹𝗱"),
                      tags$option(value = "italic", "𝘚𝘢𝘯𝘴 𝘐𝘵𝘢𝘭𝘪𝘤"),
                      tags$option(value = "bold_italic", "𝙎𝙖𝙣𝙨 𝘽𝙤𝙡𝙙 𝙄𝙩𝙖𝙡𝙞𝙘"),
                      tags$option(value = "serif_bold", "𝐒𝐞𝐫𝐢𝐟 𝐁𝐨𝐥𝐝"),
                      tags$option(value = "serif_italic", "𝑆𝑒𝑟𝑖𝑓 𝐼𝑡𝑎𝑙𝑖𝑐"),
                      tags$option(value = "serif_bold_italic", "𝑺𝒆𝒓𝒊𝒇 𝑩𝒐𝒍𝒅 𝑰𝒕𝒂𝒍𝒊𝒄"),
                      tags$option(value = "script", "𝒮𝒸𝓇𝒾𝓅𝓉"),
                      tags$option(value = "script_bold", "𝓢𝓬𝓻𝓲𝓹𝓽 𝓑𝓸𝓵𝓭"),
                      tags$option(value = "fraktur", "𝔉𝔯𝔞𝔨𝔱𝔲𝔯"),
                      tags$option(value = "fraktur_bold", "𝕱𝖗𝖆𝖐𝖙𝖚𝖗 𝕭𝖔𝖑𝖉"),
                      tags$option(value = "double_struck", "𝔻𝕠𝕦𝕓𝕝𝕖-𝕊𝕥𝕣𝕦𝕔𝕜"),
                      tags$option(value = "monospace", "𝙼𝚘𝚗𝚘𝚜𝚙𝚊𝚌𝚎")),
          tags$button(class = "tool-btn", title = "Hyperlink (Ctrl+K)", onclick = "openLinkModal()",
            tags$i(class = "bi bi-link-45deg")),
          span(class = "tool-sep"),
          tags$select(id = "heading_select", class = "tool-btn", title = "Headings (Ctrl+1 to 6)", 
                      style = "width: 48px; text-align-last: center; -webkit-appearance: none; -moz-appearance: none; appearance: none; background: transparent; border: 1px solid transparent; padding: 0; cursor: pointer; color: var(--text-2);",
                      onchange = "if(this.value !== '') { insertAtLine(this.value); this.selectedIndex = 0; }",
                      tags$option(value = "", "H", disabled = NA, selected = NA, hidden = "hidden"),
                      tags$option(value = "# ", "H1"),
                      tags$option(value = "## ", "H2"),
                      tags$option(value = "### ", "H3"),
                      tags$option(value = "#### ", "H4"),
                      tags$option(value = "##### ", "H5"),
                      tags$option(value = "###### ", "H6")),
          tags$button(class = "tool-btn", title = "Inline Code", onclick = "wrapSelection('`','`')",
            tags$i(class = "bi bi-code-slash")),
          tags$button(class = "tool-btn", title = "Bullet List", onclick = "insertAtLine('- ')",
            tags$i(class = "bi bi-list-ul")),
          span(class = "tool-sep"),
          tags$button(class = "tool-btn", id = "emoji_btn", title = "Emoji", onclick = "toggleEmojiPicker()",
            tags$i(class = "bi bi-emoji-smile"))
        )
      ),
      div(class = "editor-body",
        div(id = "emoji_picker_wrap", style = "display:none;",
          HTML('<emoji-picker id="emoji_picker"></emoji-picker>')
        ),
        tags$textarea(id = "md_input", placeholder = "Type or paste your Markdown here...\n\nTip: Use the toolbar above or keyboard shortcuts:\n  Ctrl+B  Bold\n  Ctrl+I  Italic\n  Ctrl+U  Underline\n  Ctrl+K  Hyperlink\n  Ctrl+1-6  H1-H6",
                      oninput = "Shiny.setInputValue('md_input', this.value, {priority:'event'})")
      ),
      div(class = "panel-foot",
        span(class = "char-count", id = "input_count", "0 characters"),
        div(class = "foot-actions",
          tags$label(class = "foot-btn", style = "margin:0; cursor:pointer;",
            tags$i(class = "bi bi-upload"), "Upload .md",
            div(class = "upload-wrapper",
              fileInput("file_upload", NULL, accept = c(".md", ".txt", ".markdown"))
            )
          )
        )
      )
    ),

    # ── Right panel: Preview ──
    div(class = "panel",
      div(class = "panel-head",
        span(class = "panel-title", HTML("&#128065; LinkedIn Preview")),
        div(class = "toolbar",
          tags$button(class = "tool-btn accent", id = "copy_btn", title = "Copy to clipboard",
                      onclick = "copyOutput()",
            tags$i(class = "bi bi-clipboard"), tags$span("Copy")),
          downloadButton("download_txt", label = NULL, class = "tool-btn",
                         icon = icon("download", lib = "glyphicon"))
        )
      ),
      div(class = "preview-body",
        uiOutput("preview_ui")
      ),
      div(class = "panel-foot",
        span(class = "char-count", id = "linkedin_count", "0 / 3,000"),
        div()
      )
    )
  ),

  # Credits footer
  div(class = "credit-footer",
    span("Original source code by"),
    tags$a(href = "https://github.com/IndrajeetPatil/md2linkedin", target = "_blank", "Indrajeet Patil"),
    span(class = "credit-sep", "\u00b7"),
    span("R implementation by"),
    tags$a(href = "https://www.github.com/iamyannc", target = "_blank", "Yann Cohen")
  ),

  # ── Link Modal ──
  div(id = "link_modal", class = "modal-overlay",
    div(class = "modal-box",
      div(class = "modal-title", HTML("&#128279; Insert Hyperlink")),
      div(class = "modal-field",
        tags$label(`for` = "link_text", "Display Text"),
        tags$input(type = "text", id = "link_text", placeholder = "Link text")
      ),
      div(class = "modal-field",
        tags$label(`for` = "link_url", "URL"),
        tags$input(type = "url", id = "link_url", placeholder = "https://example.com")
      ),
      div(class = "modal-actions",
        tags$button(class = "modal-btn cancel", onclick = "closeLinkModal()", "Cancel"),
        tags$button(class = "modal-btn confirm", onclick = "confirmLink()", "Insert Link")
      )
    )
  )
)

# ── 3. Server Logic ───────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # Debounced markdown input
  md_reactive <- reactive({ input$md_input %||% "" })
  md_debounced <- debounce(md_reactive, 300)

  # Convert
  converted <- reactive({
    txt <- md_debounced()
    if (!nzchar(trimws(txt))) return("")
    convert(
      txt,
      preserve_links  = isTRUE(input$preserve_links),
      monospace_code   = isTRUE(input$monospace_code %||% TRUE)
    )
  })

  # Preview output
  output$preview_ui <- renderUI({
    txt <- converted()
    if (!nzchar(txt)) {
      return(span(class = "placeholder", "Your LinkedIn-ready text will appear here..."))
    }
    tags$div(id = "preview_out", style = "white-space:pre-wrap;", txt)
  })

  # Update LinkedIn character count
  observe({
    txt <- converted()
    n <- nchar(txt)
    cls <- if (n > 3000) "char-count over" else if (n > 2700) "char-count warn" else "char-count"
    session$sendCustomMessage("updateCount", list(
      id = "linkedin_count",
      text = sprintf("%s / 3,000", format(n, big.mark = ",")),
      cls = cls
    ))
  })

  # File upload
  observeEvent(input$file_upload, {
    req(input$file_upload)
    content <- paste(readLines(input$file_upload$datapath, encoding = "UTF-8", warn = FALSE),
                     collapse = "\n")
    session$sendCustomMessage("setTextarea", content)
  })

  # Download handler
  output$download_txt <- downloadHandler(
    filename = function() { "linkedin-post.txt" },
    content = function(file) {
      writeLines(converted(), file, useBytes = TRUE)
    }
  )

  # Apply selected font style to user selection
  observeEvent(input$apply_style_req, {
    req(input$apply_style_req$text, input$apply_style_req$style)
    styled_text <- apply_style(input$apply_style_req$text, input$apply_style_req$style)
    session$sendCustomMessage("replaceSelection", styled_text)
  })
}

# Add message handlers via onFlushed
ui2 <- tagList(
  ui,
  tags$script(HTML("
    Shiny.addCustomMessageHandler('updateCount', function(msg) {
      var el = document.getElementById(msg.id);
      if (el) { el.textContent = msg.text; el.className = msg.cls; }
    });
    Shiny.addCustomMessageHandler('setTextarea', function(content) {
      var ta = document.getElementById('md_input');
      if (ta) { ta.value = content; Shiny.setInputValue('md_input', content, {priority:'event'}); }
      updateCounts();
    });
  "))
)

shinyApp(ui = ui2, server = server)
