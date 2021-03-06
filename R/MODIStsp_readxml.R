
#' MODIStsp_read_xml
#' @description function used to parse the XML file used to store the characteristics of
#' MODIS Land Products and store them in the "prod_opts" data frame
#' @details The function parses the XML file product by product, stores data in a data frame
#' and saves the data frame within the "MODIStsp_previous" RData file as a list of lists
#' @param prodopts_file string filename of the RData in which to store the data 
#' parsed from the XML file
#' @param xml_file string filename of the XML file containing the MODIS products 
#' characteristics
#' @return NULL - retrieved data are stored in the specified RData file
#'
#' @author Lorenzo Busetto, phD (2014-2015) \email{busetto.l@@irea.cnr.it}
#' @author Luigi Ranghetti, phD (2015) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @importFrom XML xmlParse xmlRoot xmlSize xmlToList xmlChildren
#' @importFrom plyr revalue
#' @importFrom hash hash
MODIStsp_read_xml <- function(prodopts_file = prodopts_file, xml_file = xml_file) {
  
  prod_opt_list <- NULL

  xmlfile <- xmlParse(xml_file)  # initialize xml parsing

  xmltop  <- xmlRoot(xmlfile) # gives content of root

  names_products <- names(xmlChildren(xmltop)) # names of the single products
  names_products <- names_products[names_products != "comment"] 
  # cycle on available products
  for (prod in names_products) {
    
    prodopts <- list()	# initialize the prodopts list
    prodopts_name <- xmlToList(xmltop[[prod]][["name"]])
    n_versions <- xmlSize(xmltop[[prod]][["versions"]]) # number of available versions
    
    for (n_version in 1:n_versions) {

      # General info
      version_name <- xmlToList(xmltop[[prod]][["versions"]][[n_version]][["v_number"]])
      prodopts[[version_name]] <- list()	# one element per version
      prodopts[[version_name]]$v_number <- version_name
      prodopts[[version_name]]$main_out_folder <- xmlToList(xmltop[[prod]][["versions"]][[n_version]][["main_out_folder"]])
      prodopts[[version_name]]$native_res      <- xmlToList(xmltop[[prod]][["versions"]][[n_version]][["native_res"]])
      prodopts[[version_name]]$tiled    <- xmlToList(xmltop[[prod]][["versions"]][[n_version]][["tiled"]])
      prodopts[[version_name]]$combined <- xmlToList(xmltop[[prod]][["versions"]][[n_version]][["combined"]])
      prodopts[[version_name]]$cat01 <- xmlToList(xmltop[[prod]][["cat_01"]])
      prodopts[[version_name]]$cat02 <- xmlToList(xmltop[[prod]][["cat_02"]])
      file_prefix_terra <- xmlToList(xmltop[[prod]][["file_prefix_terra"]])
      http_terra <- xmlToList(xmltop[[prod]][["versions"]][[n_version]][["http_terra"]])
      ftp_terra  <- xmlToList(xmltop[[prod]][["versions"]][[n_version]][["ftp_terra"]])
      file_prefix_aqua <- xmlToList(xmltop[[prod]][["file_prefix_aqua"]])
      http_aqua <- xmlToList(xmltop[[prod]][["versions"]][[n_version]][["http_aqua"]])
      ftp_aqua  <- xmlToList(xmltop[[prod]][["versions"]][[n_version]][["ftp_aqua"]])
      prodopts[[version_name]]$www <- xmlToList(xmltop[[prod]][["versions"]][[n_version]][["www"]])
      prodopts[[version_name]]$file_prefix <- hash("Terra" = file_prefix_terra, "Aqua" = file_prefix_aqua)
      prodopts[[version_name]]$http <- hash("Terra" = http_terra, "Aqua" = http_aqua)
      prodopts[[version_name]]$ftp  <- hash("Terra" = ftp_terra, "Aqua" = ftp_aqua)
      prodopts[[version_name]]$multiband_bsq <- T
      
      # Band info
      nbands <- xmlSize(xmltop[[prod]][["versions"]][[n_version]][["bands"]])  # number of original layers
      bandnames <- band_fullname <- datatype <- nodata_in <- nodata_out <- scale_factor <- offset <- NULL
      # get chars of original layers
      for (band in 1:nbands) {
        bandnames     <- c(bandnames, xmlToList(xmltop[[prod]][["versions"]][[n_version]][["bands"]][[band]][["bandname"]]))
        band_fullname <- c(band_fullname, xmlToList(xmltop[[prod]][["versions"]][[n_version]][["bands"]][[band]][["fullname"]]))
        datatype      <- c(datatype, xmlToList(xmltop[[prod]][["versions"]][[n_version]][["bands"]][[band]][["datatype"]]))
        nodata_in     <- c(nodata_in, xmlToList(xmltop[[prod]][["versions"]][[n_version]][["bands"]][[band]][["nodata_in"]]))
        nodata_out    <- c(nodata_out, xmlToList(xmltop[[prod]][["versions"]][[n_version]][["bands"]][[band]][["nodata_out"]]))
        scale_factor  <- c(scale_factor, xmlToList(xmltop[[prod]][["versions"]][[n_version]][["bands"]][[band]][["scale_factor"]]))
        offset        <- c(offset, xmlToList(xmltop[[prod]][["versions"]][[n_version]][["bands"]][[band]][["offset"]]))
      } #End Cycle on band
      prodopts[[version_name]]$bandnames <- bandnames # store in prodopts
      prodopts[[version_name]]$band_fullnames <- band_fullname
      datatype <- as.factor(datatype)
      datatype <- revalue(datatype, c("8-bit signed integer" = "Byte",			# Convert MODIS datatypes to R/Gdal datatypes
                                      "8-bit unsigned integer" = "Byte",
                                      "16-bit signed integer" = "Int16",
                                      "16-bit unsigned integer" = "UInt16",
                                      "32-bit signed integer" = "Int32",
                                      "32-bit unsigned integer" = "UInt32"), 
                          warn_missing = F)
      prodopts[[version_name]]$datatype     <- datatype
      prodopts[[version_name]]$nodata_in    <- nodata_in
      prodopts[[version_name]]$nodata_out   <- nodata_out
      prodopts[[version_name]]$scale_factor <- scale_factor
      prodopts[[version_name]]$offset       <- offset
      
      # Indices info
      nindexes <- xmlSize(xmltop[[prod]][["versions"]][[n_version]][["indexes"]])		# number of Spectral Indexes
      if (nindexes > 0) {
        indexes_bandnames <- indexes_fullnames <- indexes_formulas <- indexes_nodata_out <- NULL
        # get charcteristics of indexes
        for (index in 1:nindexes) {
          indexes_bandnames  <- c(indexes_bandnames,  xmlToList(xmltop[[prod]][["versions"]][[n_version]][["indexes"]][[index]][["indexes_bandname"]]))
          indexes_fullnames  <- c(indexes_fullnames,  xmlToList(xmltop[[prod]][["versions"]][[n_version]][["indexes"]][[index]][["indexes_fullname"]]))
          indexes_formulas   <- c(indexes_formulas,   xmlToList(xmltop[[prod]][["versions"]][[n_version]][["indexes"]][[index]][["indexes_formula"]]))
          indexes_nodata_out <- c(indexes_nodata_out, xmlToList(xmltop[[prod]][["versions"]][[n_version]][["indexes"]][[index]][["indexes_nodata_out"]]))
        } #End Cycle on index
        
        prodopts[[version_name]]$indexes_bandnames  <- indexes_bandnames		# store in prodopts
        prodopts[[version_name]]$indexes_fullnames  <- indexes_fullnames
        prodopts[[version_name]]$indexes_formulas   <- indexes_formulas
        prodopts[[version_name]]$indexes_nodata_out <- indexes_nodata_out
        
      }   #end if on indexes existence
      
      # Quality flag info
      nquality <- xmlSize(xmltop[[prod]][["versions"]][[n_version]][["quality_indicators"]])	# number of QIs
      if (nquality > 0 ) {
        quality_bandnames <- quality_fullnames <- quality_source <- quality_bitN <- NULL
        # get charcteristics of QIs
        for (quality in 1:nquality) {
          quality_bandnames <- c(quality_bandnames, xmlToList(xmltop[[prod]][["versions"]][[n_version]][["quality_indicators"]][[quality]][["quality_bandname"]]))
          quality_fullnames <- c(quality_fullnames, xmlToList(xmltop[[prod]][["versions"]][[n_version]][["quality_indicators"]][[quality]][["quality_fullname"]]))
          quality_source    <- c(quality_source,    xmlToList(xmltop[[prod]][["versions"]][[n_version]][["quality_indicators"]][[quality]][["quality_source"]]))
          quality_bitN      <- c(quality_bitN,      xmlToList(xmltop[[prod]][["versions"]][[n_version]][["quality_indicators"]][[quality]][["quality_bitN"]]))
        } #End Cycle on quality
        prodopts[[version_name]]$quality_bandnames  <- quality_bandnames
        prodopts[[version_name]]$quality_fullnames  <- quality_fullnames
        prodopts[[version_name]]$quality_source     <- quality_source
        prodopts[[version_name]]$quality_bitN       <- quality_bitN
        prodopts[[version_name]]$quality_nodata_in  <- rep("255", length(prodopts[[version_name]]$quality_bandnames))  # nodata in for quality bands (dummy - always 255)
        prodopts[[version_name]]$quality_nodata_out <- rep("255", length(prodopts[[version_name]]$quality_bandnames)) # nodata out for quality bands (always 255)
        
      } # end if on quality existence
      
    } # end of n_versions cycle

    # At each cycle, add product name to mod_prod_list and prodopts to prod_opt_list
    prod_opt_list[[prodopts_name]] <- prodopts

  }  #End Cycle on products

  # Add attributes to these 3 lists (this is used as a check when charging them)
  attr(prod_opt_list, "GeneratedBy")     <- "MODIStsp"
  attr(prod_opt_list, "MODIStspVersion") <- packageVersion("MODIStsp")
  
  # Save the products list and the chars of the products in previous file
  dir.create(dirname(prodopts_file), recursive = TRUE, showWarnings = FALSE)
  save(prod_opt_list, file = prodopts_file)

}
