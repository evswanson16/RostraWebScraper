library(RSelenium)

driver <- rsDriver(browser = "firefox", port = 4567L)
remDr <- driver[["client"]]

scrape_page <- function(year, make, model) {

  remDr$navigate("https://www.rostra.com/")

  # Find and interact with the form elements
  year_input <- remDr$findElement(using = "id", value = "Year") 
  make_input <- remDr$findElement(using = "id", value = "Make")
  model_input <- remDr$findElement(using = "id", value = "Model")

  # Fill out the form
  year_input$sendKeysToElement(list(year))
  make_input$sendKeysToElement(list(make))
  
  # Check if the model is available for the selected make
  if (model %in% available_models[[make]]) {
    model_input$sendKeysToElement(list(model))

    # Submit the form
    model_input$submitElement()

    # Wait to ensure the page loads completely
    Sys.sleep(5)

    tryCatch({
      # Find the specific <a> element using the original XPath expression
      a_element <- remDr$findElement(using = "xpath", value = "/html/body/div[3]/div/a[2]")

      if (!is.null(a_element)) {
        # Get the text of the <a> element
        a_text <- a_element$getElementText()
      } else {
        # If the <a> element is not found using the original XPath, try the next XPath
        a_element <- remDr$findElement(using = "xpath", value = "/html/body/div[3]/div/ul[5]/li/a")

        if (!is.null(a_element)) {
          # Get the text of the <a> element
          a_text <- a_element$getElementText()
        } else {
          # Return NULL if neither XPath expression finds the <a> element
          a_text <- "null"
        }
      }

      # Create a data frame with the extracted information
      result_df <- data.frame(
        YearMakeModel = paste(year, make, model, sep = " "),
        ATagContents = ifelse(nchar(a_text) > 0, a_text, "null")
      )
      return(result_df)
    }, error = function(e) {
      return(NULL)  # Return NULL if an error occurs
    })
  } else {
    return(NULL)  # Return NULL if the model is not available for the make
  }
}

# List of inputs for Year
year <- "2023"

# List of Make values
makes <- c(
  "Acura", "Buick", "Chevrolet Cars and SUVs", "Chevrolet Trucks and Vans",
  "Chrysler", "Dodge", "Ford SUVs and Crossovers", "Ford Trucks and Vans",
  "GMC Trucks and Vans", "Honda", "Hyundai", "Jeep", "Kia", "Mazda",
  "Mitsubishi", "Nissan", "RAM Trucks", "Subaru", "Toyota"
)

# Create a lookup table of available models for each make
available_models <- list(
  "Acura" = c("Integra", "TLX", "TDX", "MDX", "RDX"),
  "Buick" = c("Enclave", "Encore", "Envision"),
  "Chevrolet Cars and SUVs" = c("Bolt EV", "Bolt EUV", "Blazer", "Camaro", "Equinox", "Malibu", "Suburban", "Tahoe", "Trailblazer", "Traverse"),
  "Chevrolet Trucks and Vans" = c("Colorado", "Express Van", "Silverado 1500", "Silverado 2500HD", "Silverado 3500HD"),
  "Chrysler" = c("Pacifica", "300", "300C"),
  "Dodge" = c("Charger", "Challenger", "Durango", "Hornet"),
  "Ford SUVs and Crossovers" = c("Bronco", "Bronco Sport", "Edge", "Escape", "Explorer", "Expedition", "Maverick"),
  "Ford Trucks and Vans" = c("F-150", "F-250", "F-350", "F-450", "Ranger", "Transit Full-Size", "Transit Connect"),
  "GMC Trucks and Vans" = c("Canyon", "Savana Van", "Sierra 1500", "Sierra 2500HD", "Sierra 3500HD"),
  "Honda" = c("Accord", "Civic", "CR-V", "HR-V", "Odyssey", "Passport", "Pilot", "Ridgeline"),
  "Hyundai" = c("Elantra", "Kona", "Palisade", "Santa Cruz", "Santa Fe", "Sonata", "Tucson", "Venue"),
  "Jeep" = c("Grand Cherokee", "Compass", "Cherokee", "Renegade", "Wrangler", "Gladiator"),
  "Kia" = c("Forte", "K5", "Rio", "Sedona", "Celts", "Sorento", "Soul", "Sportage", "Stinger", "Telluride"),
  "Mazda" = c("CX-30", "CX-5", "CX-50", "CX-9", "Mazda3 Sedan", "Mazda3 Hatchback", "MX-5 Miata"),
  "Mitsubishi" = c("Eclipse Cross", "Mirage", "Mirage G4", "Outlander", "Outlander PHEV", "Outlander Sport"),
  "Nissan" = c("Altima", "Armada", "Frontier", "Kicks", "Leaf", "Maxima", "Murano", "Pathfinder", "Rogue", "Rogue Sport", "Sentra", "Titan", "Titan XD", "Versa"),
  "RAM Trucks" = c("ProMaster", "Ram 1500", "Ram 2500", "Ram 3500"),
  "Subaru" = c("Ascent", "BRZ", "Crosstrek", "Forester", "Impreza", "Legacy", "Outback", "WRX"),
  "Toyota" = c("4Runner", "C-HR", "Camry", "Camry Hybrid", "Corolla", "Corolla Cross", "Corolla Hatchback", "Corolla Hybrid", "Crown", "GR86", "GR Corolla", "Highlander", "Highlander Hybrid", "Prius", "RAV4", "RAV4 Hybrid", "RAV4 Prime", "Sequoia", "Sienna", "Supra", "Tacoma", "Tundra", "Venza")
)

# List of Model values from list combined
models <- unlist(available_models) 

# Initialize an empty data frame
all_results <- data.frame(YearMakeModel = character(0), ATagContents = character(0))

# Loop through the combinations of Make and Model and scrape each page
for (make in makes) {
  available_models_for_make <- available_models[[make]]
  if (!is.null(available_models_for_make)) {
    for (model in models) {
      if (model %in% available_models_for_make) {
        result <- scrape_page(year, make, model)
        if (!is.null(result)) {
          colnames(result) <- colnames(all_results)
          all_results <- rbind(all_results, result)
        }
      }
    }
  } else {
    cat("No available models found for Make:", make, "\n")
  }
}

# Print the final data frame with all results
print(all_results)

# Close the Selenium driver
driver$server$stop()