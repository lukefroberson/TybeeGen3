# ============================================================================
# TAB 2: UNDERSTANDING BEACH WATER QUALITY - EDUCATIONAL CONTENT
# ============================================================================
# This tab provides comprehensive education using Health Belief Model framework
# Focuses on empowering beachgoers with knowledge while maintaining positive tone
# ============================================================================

# Georgia CFU Threshold
GEORGIA_THRESHOLD <- 70

# ===== UI COMPONENT =====

tab2_ui <- tabItem(
  tabName = "education",
  
  # Header
  fluidRow(
    box(
      width = 12,
      title = NULL,
      status = "info",
      solidHeader = FALSE,
      div(
        style = "text-align: center; padding: 20px; background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); border-radius: 8px;",
        h2(style = "margin: 0 0 10px 0; color: white;", 
           icon("graduation-cap"), " Understanding Beach Water Quality"),
        h4(style = "margin: 0; color: white; font-weight: normal;",
           "Knowledge is your best protection for safe beach visits")
      )
    )
  ),
  
  # ===== SECTION 1: THE BASICS =====
  fluidRow(
    box(
      width = 12,
      title = "Water Quality Basics: What You Need to Know",
      status = "primary",
      solidHeader = TRUE,
      icon = icon("book-open"),
      collapsible = TRUE,
      collapsed = FALSE,
      
      fluidRow(
        column(6,
               div(style = "padding: 15px;",
                   h4(icon("bacteria"), " What Are We Measuring?"),
                   p(style = "font-size: 14px; line-height: 1.8;",
                     "We test for ", strong("Enterococcus bacteria"), " - indicator organisms that signal 
                     the possible presence of harmful pathogens. These bacteria come from the intestines 
                     of warm-blooded animals (including humans, birds, and other wildlife)."),
                   
                   p(style = "font-size: 14px; line-height: 1.8;",
                     strong("Important:"), " Finding Enterococcus doesn't mean the water is contaminated 
                     with sewage - these bacteria occur naturally in the environment. But high levels 
                     suggest conditions where harmful pathogens ", em("could"), " also be present."),
                   
                   div(
                     style = "margin-top: 15px; padding: 12px; background-color: #e7f3ff; border-left: 4px solid #0066cc; border-radius: 4px;",
                     p(style = "margin: 0; font-size: 13px;",
                       icon("lightbulb"),
                       " Think of Enterococcus like a ", strong("check engine light"), " in your car - 
                       it signals when conditions need attention, even if nothing's wrong yet.")
                   )
               )
        ),
        
        column(6,
               div(style = "padding: 15px;",
                   h4(icon("ruler"), " Understanding the Numbers"),
                   p(style = "font-size: 14px; line-height: 1.8;",
                     "Bacteria levels are measured in ", strong("CFU/100mL"), 
                     " (Colony Forming Units per 100 milliliters of water)."),
                   
                   tags$ul(
                     style = "font-size: 14px; line-height: 1.8;",
                     tags$li(strong("0-35 CFU:"), " Typical background levels - excellent conditions"),
                     tags$li(strong("36-70 CFU:"), " Normal range - no advisory needed"),
                     tags$li(strong("71-104 CFU:"), " Elevated - Georgia issues advisory at 70 CFU"),
                     tags$li(strong("105+ CFU:"), " High levels - definitely avoid water contact")
                   ),
                   
                   div(
                     style = "margin-top: 15px; padding: 12px; background-color: #d4edda; border-left: 4px solid #28a745; border-radius: 4px;",
                     p(style = "margin: 0; font-size: 13px;",
                       icon("check-circle"),
                       " At Tybee Island, bacteria levels stay below 70 CFU ", 
                       strong("97% of the time"), " - your beaches are well-monitored and typically safe!")
                   )
               )
        )
      )
    )
  ),
  
  # ===== SECTION 2: WHAT AFFECTS WATER QUALITY =====
  fluidRow(
    box(
      width = 12,
      title = "What Affects Beach Water Quality?",
      status = "warning",
      solidHeader = TRUE,
      icon = icon("cloud-rain"),
      collapsible = TRUE,
      collapsed = FALSE,
      
      fluidRow(
        column(4,
               div(style = "padding: 15px; border-right: 1px solid #ddd;",
                   h4(icon("cloud-showers-heavy"), " Rainfall", style = "color: #0066cc;"),
                   p(style = "font-size: 14px; line-height: 1.8;",
                     strong("The #1 factor"), " affecting water quality. Rain washes bacteria from:"),
                   tags$ul(
                     style = "font-size: 13px; line-height: 1.8;",
                     tags$li("Storm drains and urban runoff"),
                     tags$li("Wildlife habitats and bird roosting areas"),
                     tags$li("Boat discharges and marinas"),
                     tags$li("Natural soil and vegetation")
                   ),
                   p(style = "font-size: 13px; color: #666; margin-top: 10px;",
                     "Heavy rain (>1 inch) can raise bacteria levels significantly for 24-48 hours.")
               )
        ),
        
        column(4,
               div(style = "padding: 15px; border-right: 1px solid #ddd;",
                   h4(icon("temperature-high"), " Temperature", style = "color: #dc3545;"),
                   p(style = "font-size: 14px; line-height: 1.8;",
                     "Warm water helps bacteria grow faster:"),
                   tags$ul(
                     style = "font-size: 13px; line-height: 1.8;",
                     tags$li("Water above 78°F accelerates growth"),
                     tags$li("Hot summer days create ideal conditions"),
                     tags$li("Combined with rainfall = higher risk"),
                     tags$li("Cold water naturally reduces bacteria")
                   ),
                   p(style = "font-size: 13px; color: #666; margin-top: 10px;",
                     "This is why summer advisories are more common than winter ones.")
               )
        ),
        
        column(4,
               div(style = "padding: 15px;",
                   h4(icon("water"), " Tides & Currents", style = "color: #17a2b8;"),
                   p(style = "font-size: 14px; line-height: 1.8;",
                     "Ocean mixing helps dilute and flush bacteria:"),
                   tags$ul(
                     style = "font-size: 13px; line-height: 1.8;",
                     tags$li("Strong tides improve water circulation"),
                     tags$li("Outgoing tides carry bacteria offshore"),
                     tags$li("Stagnant water allows bacteria to accumulate"),
                     tags$li("Some beaches naturally flush better")
                   ),
                   p(style = "font-size: 13px; color: #666; margin-top: 10px;",
                     "This is why different beaches have different typical levels.")
               )
        )
      ),
      
      div(
        style = "margin-top: 20px; padding: 15px; background-color: #fff3cd; border-left: 4px solid #ffc107; border-radius: 4px;",
        h5(icon("exclamation-triangle"), " Seasonal Patterns", style = "margin-top: 0;"),
        p(style = "margin: 0; font-size: 14px;",
          strong("Summer months (June-August)"), " have the highest advisory frequency because warm water + 
          afternoon thunderstorms create perfect conditions for bacteria growth. ",
          strong("Winter months"), " typically have the lowest bacteria levels. This is completely normal 
          and expected!")
      )
    )
  ),
  
  # ===== SECTION 3: HEALTH INFORMATION =====
  fluidRow(
    box(
      width = 12,
      title = "Health Information: Know the Risks",
      status = "danger",
      solidHeader = TRUE,
      icon = icon("heartbeat"),
      collapsible = TRUE,
      collapsed = FALSE,
      
      fluidRow(
        column(6,
               div(style = "padding: 15px;",
                   h4(icon("stethoscope"), " Possible Health Effects"),
                   p(style = "font-size: 14px; line-height: 1.8;",
                     "Swimming in water with elevated bacteria levels ", em("may"), " cause:"),
                   
                   tags$ul(
                     style = "font-size: 14px; line-height: 1.8;",
                     tags$li(strong("Gastrointestinal illness:"), " Nausea, vomiting, diarrhea, stomach cramps"),
                     tags$li(strong("Skin infections:"), " Rashes, irritation (especially if you have cuts)"),
                     tags$li(strong("Ear/nose/throat:"), " Infections from water entering these areas"),
                     tags$li(strong("Eye irritation:"), " Redness, discomfort")
                   ),
                   
                   p(style = "font-size: 14px; line-height: 1.8;",
                     strong("Important context:"), " Most people who swim during advisories will NOT get sick. 
                     These symptoms are possible but not guaranteed. Think of it like rainy day driving - 
                     there's increased risk, but most people are fine with precautions."),
                   
                   div(
                     style = "margin-top: 15px; padding: 12px; background-color: #d4edda; border-left: 4px solid #28a745; border-radius: 4px;",
                     p(style = "margin: 0; font-size: 13px;",
                       icon("check-circle"),
                       " Symptoms, if they occur, typically appear within 24-72 hours and 
                       usually resolve on their own in a few days.")
                   )
               )
        ),
        
        column(6,
               div(style = "padding: 15px;",
                   h4(icon("users"), " Who Should Be Extra Careful?"),
                   p(style = "font-size: 14px; line-height: 1.8;",
                     "Some people are more vulnerable to waterborne illness:"),
                   
                   tags$ul(
                     style = "font-size: 14px; line-height: 1.8;",
                     tags$li(strong("Young children:"), " More likely to swallow water, developing immune systems"),
                     tags$li(strong("Elderly individuals:"), " May have weakened immune responses"),
                     tags$li(strong("Immunocompromised:"), " Cancer patients, HIV+, transplant recipients, etc."),
                     tags$li(strong("Pregnant women:"), " Extra precautions recommended"),
                     tags$li(strong("People with open wounds:"), " Direct entry point for bacteria")
                   ),
                   
                   p(style = "font-size: 14px; line-height: 1.8; margin-top: 15px;",
                     strong("If you're in a high-risk group:"), " Take advisories seriously, avoid water 
                     contact during elevated bacteria periods, and consult your doctor if you develop symptoms."),
                   
                   div(
                     style = "margin-top: 15px; padding: 12px; background-color: #f8d7da; border-left: 4px solid #dc3545; border-radius: 4px;",
                     p(style = "margin: 0; font-size: 13px;",
                       icon("exclamation-circle"),
                       " Have a weakened immune system? Talk to your healthcare provider about 
                       beach safety before your visit.")
                   )
               )
        )
      )
    )
  ),
  
  # ===== SECTION 4: HOW MONITORING PROTECTS YOU =====
  fluidRow(
    box(
      width = 12,
      title = "How Our Monitoring System Protects You",
      status = "success",
      solidHeader = TRUE,
      icon = icon("shield-alt"),
      collapsible = TRUE,
      collapsed = FALSE,
      
      fluidRow(
        column(3,
               div(style = "text-align: center; padding: 15px;",
                   icon("vial", class = "fa-3x", style = "color: #28a745; margin-bottom: 15px;"),
                   h4("Regular Testing"),
                   p(style = "font-size: 13px; line-height: 1.8;",
                     "Water samples collected from 5 Tybee Island beaches ",
                     strong("weekly"), " during April-October (peak season) and ",
                     strong("bi-weekly"), " during off-season.")
               )
        ),
        
        column(3,
               div(style = "text-align: center; padding: 15px;",
                   icon("microscope", class = "fa-3x", style = "color: #0066cc; margin-bottom: 15px;"),
                   h4("Lab Analysis"),
                   p(style = "font-size: 13px; line-height: 1.8;",
                     "Samples analyzed at certified laboratory using EPA-approved methods. ",
                     "Results typically available within ",
                     strong("18-24 hours"), ".")
               )
        ),
        
        column(3,
               div(style = "text-align: center; padding: 15px;",
                   icon("chart-line", class = "fa-3x", style = "color: #ffc107; margin-bottom: 15px;"),
                   h4("Predictive Model"),
                   p(style = "font-size: 13px; line-height: 1.8;",
                     "Machine learning model trained on ",
                     strong("20 years"), " of data provides ",
                     strong("same-day"), " predictions when conditions change rapidly.")
               )
        ),
        
        column(3,
               div(style = "text-align: center; padding: 15px;",
                   icon("bullhorn", class = "fa-3x", style = "color: #dc3545; margin-bottom: 15px;"),
                   h4("Public Alerts"),
                   p(style = "font-size: 13px; line-height: 1.8;",
                     "When levels exceed 70 CFU, advisories posted at beaches within ",
                     strong("hours"), ". Dashboard updated in real-time.")
               )
        )
      ),
      
      div(
        style = "margin-top: 20px; padding: 15px; background-color: #d1ecf1; border-left: 4px solid #17a2b8; border-radius: 4px;",
        h5(icon("trophy"), " Track Record", style = "margin-top: 0;"),
        fluidRow(
          column(4,
                 p(style = "margin: 0; font-size: 24px; font-weight: bold; color: #28a745;", "97%"),
                 p(style = "margin: 0; font-size: 13px;", "Of samples below advisory level")
          ),
          column(4,
                 p(style = "margin: 0; font-size: 24px; font-weight: bold; color: #28a745;", "1-2 days"),
                 p(style = "margin: 0; font-size: 13px;", "Average advisory duration")
          ),
          column(4,
                 p(style = "margin: 0; font-size: 24px; font-weight: bold; color: #28a745;", "20+ years"),
                 p(style = "margin: 0; font-size: 13px;", "Continuous monitoring history")
          )
        )
      )
    )
  ),
  
  # ===== SECTION 5: FAQ =====
  fluidRow(
    box(
      width = 12,
      title = "Frequently Asked Questions",
      status = "info",
      solidHeader = TRUE,
      icon = icon("question-circle"),
      collapsible = TRUE,
      collapsed = TRUE,
      
      div(style = "padding: 15px;",
          
          # FAQ 1
          div(style = "margin-bottom: 20px;",
              h4(icon("question"), " Why do bacteria levels change so quickly?"),
              p(style = "font-size: 14px; line-height: 1.8; padding-left: 25px;",
                "Bacteria levels can rise within hours after rainfall and then drop just as quickly. 
                Ocean mixing, sunlight (UV radiation), and tidal flushing all help reduce bacteria naturally. 
                This is why most advisories are short-lived (1-2 days) - the ocean is self-cleaning!")
          ),
          
          # FAQ 2
          div(style = "margin-bottom: 20px;",
              h4(icon("question"), " Are some Tybee beaches safer than others?"),
              p(style = "font-size: 14px; line-height: 1.8; padding-left: 25px;",
                "Yes, beaches closer to urban runoff sources (like Polk Street near tidal creeks) tend to have 
                slightly higher average bacteria levels. However, ALL beaches are monitored equally, and advisories 
                are posted when needed. Beach choice should be based on your activities and preferences, not just 
                water quality - all can be enjoyed safely most of the time.")
          ),
          
          # FAQ 3
          div(style = "margin-bottom: 20px;",
              h4(icon("question"), " Can I swim if it's close to the advisory threshold?"),
              p(style = "font-size: 14px; line-height: 1.8; padding-left: 25px;",
                "This is a personal decision. Georgia's 70 CFU threshold is protective - it's set deliberately 
                conservative to give you a safety buffer. If levels are 60-70 CFU with medium confidence, most 
                people would be fine, but those in high-risk groups might want to wait. Check the confidence 
                score and your personal risk factors.")
          ),
          
          # FAQ 4
          div(style = "margin-bottom: 20px;",
              h4(icon("question"), " Why don't you test every day?"),
              p(style = "font-size: 14px; line-height: 1.8; padding-left: 25px;",
                "Laboratory testing takes 18-24 hours and is resource-intensive. Weekly testing during peak season 
                is the national standard and has proven effective. Our predictive model fills the gaps between 
                physical tests, giving you same-day information when conditions change rapidly (like after heavy rain).")
          ),
          
          # FAQ 5
          div(style = "margin-bottom: 20px;",
              h4(icon("question"), " What about swimming pools vs. ocean water?"),
              p(style = "font-size: 14px; line-height: 1.8; padding-left: 25px;",
                "Pool water is chemically treated (chlorinated) to kill bacteria continuously - that's why it's 
                generally safer. Ocean water is natural and untreated, so bacteria levels fluctuate naturally. 
                However, the ocean is vast and has natural self-cleaning mechanisms. Both are safe when properly 
                monitored!")
          ),
          
          # FAQ 6
          div(style = "margin-bottom: 20px;",
              h4(icon("question"), " I swam during an advisory and feel fine. Was it a false alarm?"),
              p(style = "font-size: 14px; line-height: 1.8; padding-left: 25px;",
                "Not necessarily! Remember, elevated bacteria levels increase your ", em("risk"), " of illness, 
                but don't guarantee it. Many people swim during advisories without getting sick - you were likely 
                lucky. However, the next person might not be, especially if they're in a high-risk group or swallow 
                water. Advisories are precautionary measures to protect the most vulnerable.")
          ),
          
          # FAQ 7
          div(style = "margin-bottom: 20px;",
              h4(icon("question"), " How accurate are the predictions?"),
              p(style = "font-size: 14px; line-height: 1.8; padding-left: 25px;",
                "Our model is trained on 20 years of Tybee Island data and achieves good accuracy, especially 
                with medium to high confidence predictions. However, predictions are tools, not guarantees. 
                When confidence is low, we recommend caution. Physical testing will always be the gold standard - 
                predictions help you make decisions between test days.")
          ),
          
          # FAQ 8
          div(style = "margin-bottom: 0;",
              h4(icon("question"), " How can I stay informed about beach conditions?"),
              p(style = "font-size: 14px; line-height: 1.8; padding-left: 25px;",
                strong("This dashboard!"), " Bookmark it and check before beach visits. You can also:",
                tags$ul(
                  style = "font-size: 14px; line-height: 1.8; margin-top: 10px;",
                  tags$li("Look for posted signs at beach entrances"),
                  tags$li("Sign up for text/email alerts (available at beach kiosks)"),
                  tags$li("Follow Tybee Island social media for updates"),
                  tags$li("Call the beach hotline (if available in your area)")
                )
              )
          )
      )
    )
  ),
  
  # ===== SECTION 6: YOUR ROLE =====
  fluidRow(
    box(
      width = 12,
      title = "Your Role in Beach Safety",
      status = "primary",
      solidHeader = TRUE,
      icon = icon("hands-helping"),
      collapsible = TRUE,
      collapsed = TRUE,
      
      fluidRow(
        column(6,
               div(style = "padding: 15px;",
                   h4(icon("user-check"), " How You Can Help"),
                   tags$ul(
                     style = "font-size: 14px; line-height: 1.8;",
                     tags$li(strong("Check before you go:"), " Use this dashboard before beach visits"),
                     tags$li(strong("Respect advisories:"), " When posted, avoid water contact"),
                     tags$li(strong("Practice good hygiene:"), " Rinse off after swimming, wash hands before eating"),
                     tags$li(strong("Spread awareness:"), " Share this dashboard with friends and family"),
                     tags$li(strong("Report concerns:"), " See suspicious discharges? Call it in"),
                     tags$li(strong("Be a steward:"), " Pack out trash, don't feed wildlife")
                   ),
                   
                   div(
                     style = "margin-top: 15px; padding: 12px; background-color: #d4edda; border-left: 4px solid #28a745; border-radius: 4px;",
                     p(style = "margin: 0; font-size: 13px;",
                       icon("heart"),
                       " Every beachgoer who checks conditions and respects advisories helps protect 
                       others - especially children and vulnerable individuals. You're part of the safety system!")
                   )
               )
        ),
        
        column(6,
               div(style = "padding: 15px;",
                   h4(icon("phone"), " Who to Contact"),
                   p(style = "font-size: 14px; line-height: 1.8;",
                     strong("For water quality questions:")),
                   tags$ul(
                     style = "font-size: 13px; line-height: 1.8;",
                     tags$li("Georgia Coastal Resources Division"),
                     tags$li("Chatham County Health Department"),
                     tags$li("Tybee Island Marine Science Center")
                   ),
                   
                   p(style = "font-size: 14px; line-height: 1.8; margin-top: 15px;",
                     strong("To report concerns:")),
                   tags$ul(
                     style = "font-size: 13px; line-height: 1.8;",
                     tags$li("Sewage spills or suspicious discharges"),
                     tags$li("Large numbers of dead fish"),
                     tags$li("Oil sheens or unusual water color"),
                     tags$li("Missing or damaged advisory signs")
                   ),
                   
                   div(
                     style = "margin-top: 15px; padding: 12px; background-color: #fff3cd; border-left: 4px solid #ffc107; border-radius: 4px;",
                     p(style = "margin: 0; font-size: 13px;",
                       icon("exclamation-triangle"),
                       " If you think you got sick from beach water, see your doctor and report it to 
                       the health department - this helps improve the monitoring program!")
                   )
               )
        )
      )
    )
  ),
  
  # ===== FOOTER MESSAGE =====
  fluidRow(
    box(
      width = 12,
      title = NULL,
      status = "success",
      background = "light-blue",
      div(
        style = "text-align: center; padding: 20px;",
        h3(style = "margin: 0 0 10px 0; color: white;", 
           icon("umbrella-beach"), " Enjoy Your Beach Visit!"),
        p(style = "margin: 0; color: white; font-size: 16px;",
          "Armed with knowledge, you can make informed decisions for safe, fun beach days. ",
          "Tybee Island's beaches are monitored year-round and safe 97% of the time. ",
          "Check conditions, respect advisories, and enjoy the coast!")
      )
    )
  )
)


# ===== SERVER COMPONENT =====
# Tab 2 is primarily static educational content, so minimal server logic needed

tab2_server <- function(input, output, session) {
  # Currently no reactive elements in Tab 2
  # This function is here for future enhancements like:
  # - Interactive quizzes
  # - Downloadable educational materials
  # - User feedback forms
  # - Video content
}


# ===== INTEGRATION NOTES =====
# 
# TO USE:
# 1. Source this file in your main app.R
# 2. Add tab2_ui to your tabItems with tabName = "education"
# 3. Add menu item: menuItem("Understanding Water Quality", tabName = "education", icon = icon("graduation-cap"))
# 4. Call tab2_server in your server function
# 5. No additional data files needed - all content is self-contained
#
# DESIGN PHILOSOPHY:
# - Health Belief Model throughout
# - Positive, empowering tone
# - Factual but not scary
# - Collapsible sections to avoid overwhelming
# - Visual hierarchy with icons and colors
# - Addresses concerns from Tybee Island beachgoer study
# - Emphasizes 97% safe message
