library(shiny)
library(tidyverse)
library(stringr)
library(imager)
library(png) # For writePNG function
library(colourpicker)

#source('__functions.R')
# https://shiny.rstudio.com/gallery/image-output.html

rgb2hex <- function(r,g,b){
    rgb(r, g, b, maxColorValue = 1)
}

convert_image <- function(img, new_size = NULL){
    
    #img <- EBImage::readImage('../de_beauvoire/sbv2.JPG')
    
    if(!is.null(new_size)){
        
        ### If width or height greater than resize, resize the largest
        if(max(dim(img)[1:2]) > new_size){
            
            if(dim(img)[1] > dim(img)[2]){
                img <- EBImage::resize(img, w = dim(img)[1] * (new_size / 100) )
            }
            
            else{
                img <- EBImage::resize(img, h = dim(img)[2] * (new_size / 100) )
            }
            
        }
        
    }
    
    
    rgb_df <-
        data_frame(rgb_num = c(1, 2, 3),
                   rgb = c('r', 'g', 'b'))
    
    img_melt <-
        reshape::melt(img)
    
    names(img_melt) <-
        c('x', 'y', 'rgb_num', 'value')
    
    img_melt <-
        img_melt %>%
        left_join(rgb_df, by = 'rgb_num') %>%
        select(-rgb_num) %>%
        select(x, y, rgb, value) %>%
        spread(rgb, value) %>%
        select(x, y, r, g, b)
    
    hsv <-
        t(rgb2hsv(r = img_melt$r,
                  g = img_melt$g,
                  b = img_melt$b)) %>%
        as_data_frame() %>%
        mutate(v = (v - min(v)) / (max(v) - min(v))) %>%
        mutate(h = 100 * h,
               s = 100 * s,
               v = 100 * v)
    
    img_final <-
        bind_cols(img_melt, hsv) %>%
        mutate( y = -y + max(y) ) %>%
        arrange(x, y) %>%
        dplyr::mutate(id = 1:n()) %>%
        mutate(id_char = as.character(id)) %>%
        mutate(hex = rgb2hex(r, g, b))
    
    return(img_final)
    
}


options(shiny.maxRequestSize = 20 * 1024^2) # 20MB 

##################################################################
##################################################################
##
##   UI
##
##################################################################
##################################################################


ui <- fluidPage(
    
    # tags$style(type = "text/css", "
    #   .control-label {height: 5px;}
    # "),
    
    titlePanel('Binary Images'),
    
    fluidRow(
        column(5, wellPanel(
            fileInput('load_image', 'Choose image'),
            fluidRow(
                column(6,
                       uiOutput('image_contrast')),
                column(6,
                       uiOutput('image_density'))
            ),
            fluidRow(
                column(6,
                       colourInput(inputId = 'geom_color_bkg',
                                   label = 'Background color:',
                                   value = '#F7F1F0',
                                   showColour = 'both')),
                column(6,
                       colourInput(inputId = 'geom_color_dot',
                                   label = 'Main color:',
                                   value = '#000000',
                                   showColour = 'both'))
            ),
            fluidRow(
                column(6,
                       checkboxInput('all_pixels',
                                     'Activate fill color 
                                     (disables manual / multiple main colors options)',
                                     FALSE)),
                column(6,
                       colourInput(inputId = 'geom_color_dot_2',
                                   label = 'Fill color:',
                                   value = '#979797',
                                   showColour = 'both'))
            ),
            fluidRow(
                column(6,
                       uiOutput('geom_type')),
                column(6,
                       uiOutput('geom_size'))
            ),
            fluidRow(
                column(6,
                       uiOutput('stroke_text')),
                column(6,
                       uiOutput('stroke_size'))
            ),
            fluidRow(
                column(6,
                       uiOutput('geom_alpha')),
                column(6,
                       uiOutput('image_size'))
            ),
            fluidRow(
                column(6,
                       uiOutput('border_size_v')),
                column(6,
                       uiOutput('border_size_h'))
            ),
            fluidRow(
                column(6,
                       uiOutput('jitter_v')),
                column(6,
                       uiOutput('jitter_h'))
            ),
            uiOutput('invert_pixels'),
            fluidRow(
                column(6,
                       uiOutput('manual_dot_colors')),
                column(6,
                       uiOutput('manual_weights'))
            ),
            fluidRow(
                column(6,
                       uiOutput('use_manual_colors')),
                column(6,
                       uiOutput('update_colors'))
            ),
            uiOutput('hex_error'),
            uiOutput('use_image_colors'),
            uiOutput('advanced_filtering'),
            uiOutput('filter_hue'),
            uiOutput('filter_saturation'),
            uiOutput('filter_red'),
            uiOutput('filter_green'),
            uiOutput('filter_blue'),
            uiOutput('transparent_background'),
            fluidRow(
                column(6,
                       uiOutput('activate_density')),
                column(6,
                       uiOutput('invert_density'))
            ),
            uiOutput('geom_boost'),
            uiOutput('base_image')
            #uiOutput('download_plot')
            # radioButtons("picture", "Picture:",
            #              c("chainring", "face"))
        )),
        column(7,
               #imageOutput('image1')
               #imageOutput("image2")
               plotOutput('mixed_graph')
        )
    )
)



##################################################################
##################################################################
##
##   Server
##
##################################################################
##################################################################

server <- function(input, output, session) {
    
    data <- reactiveValues()
    
    observeEvent(input$load_image, {
        
        #data <- list()
        
        #file_path <- '../toro_picasso/picasso_cafe.bmp'
        file_path <-
            input$load_image$datapath
        
        #img <- EBImage::readImage(file_path)
        data$img <-
            EBImage::readImage(file_path)
        
        if(data$img@colormode == 0){
            data$img <- EBImage::toRGB(data$img)
        }
        
        write_rds(data$img, 'test1.rds')
        
        
        max_size <- 1500
        
        ### If width or height greater than 2000, resize the largest
        if(max(dim(data$img)[1:2]) > max_size){
            
            if(dim(data$img)[1] > dim(data$img)[2]){
                data$img <- EBImage::resize(data$img, w = max_size)
            }
            
            else{
                data$img <- EBImage::resize(data$img, h = max_size)
            }
            
        }
        
        
        
        ### Parameters
        
        data$plot_width <-
            dim(data$img)[1]
        
        data$plot_height <-
            dim(data$img)[2]
        
        data$manual_colors <-
            c('#E53B1E', '#000000')
        
        data$manual_weights <-
            c(1, 5)
        
        data$color_input_error <- FALSE
        
    })
    
    
    ############################################
    # 1. Set up contrast and density
    ############################################
    
    output$image_contrast <- renderUI({
        
        if(is.null(data$img)){
        
            sliderInput('image_contrast',
                        'Image contrast:',
                        min = 0, max = 30,
                        value = 0, step = 1)
            
        }
        
        else{
            
            sliderInput('image_contrast',
                        'Image contrast:',
                        min = 0, max = 100,
                        value = c(40, 60), step = 1)
            
        }
        
    })
    
    output$image_density <- renderUI({
        
        if(is.null(data$img)){
            
            sliderInput('image_density',
                        'Image density:',
                        min = 0, max = 100,
                        value = 0, step = 1)
            
        }
        
        else{
            
            sliderInput('image_density',
                        'Image density:',
                        min = 0, max = 100,
                        value = 30, step = 1)
            
        }
        
    })
    
    
    ############################################
    # 2. Set up element type and size
    ############################################
    
    output$geom_type <- renderUI({
        
        if(is.null(data$img)){
            
            selectizeInput(inputId = 'geom_type',
                           label = 'Choose graph element type:',
                           choices = c('circles',
                                       'binary',
                                       'squares',
                                       'diamonds',
                                       'triangles',
                                       'circles (empty)',
                                       'squares (empty)',
                                       'diamonds (empty)',
                                       'triangles (empty)',
                                       '+',
                                       'x',
                                       '*'),
                           selected = '',
                           multiple = FALSE,
                           options = NULL)
            
        }
        
        else{
            
            selectizeInput(inputId = 'geom_type',
                           label = 'Choose graph elemement type:',
                           choices = c('circles',
                                       'binary',
                                       'squares',
                                       'diamonds',
                                       'triangles',
                                       'circles (empty)',
                                       'squares (empty)',
                                       'diamonds (empty)',
                                       'triangles (empty)',
                                       '+',
                                       'x',
                                       '*'),
                           selected = 'circles',
                           multiple = FALSE,
                           options = NULL)
            
        }
        
    })
    
    output$geom_size <- renderUI({
        
        if(is.null(data$img)){
            
            sliderInput('geom_size',
                        'Choose graph element size:',
                        min = 1, max = 100,
                        value = 0, step = 1)
            
        }
        
        else{
            
            sliderInput('geom_size',
                        'Choose graph element size:',
                        min = 1, max = 100,
                        value = c(1, 5), step = 1)
            
        }
        
    })
    
    
    
    ############################################
    # 3. Set up border control
    ############################################
    
    output$stroke_text <- renderUI({
        
        if(is.null(data$img)){
            
            helpText('The element border control only applies to elements
                     that contain a border.')
            
        }
        
        else{
            
            helpText('The element border control only applies to elements
                     that contain a border.')
            
        }
        
    })
    
    output$stroke_size <- renderUI({
        
        if(is.null(data$img)){
            
            sliderInput('stroke_size',
                        'Choose border size:',
                        min = 1, max = 100,
                        value = 0, step = 1)
            
        }
        
        else{
            
            sliderInput('stroke_size',
                        'Choose border size:',
                        min = 1, max = 100,
                        value = 5, step = 1)
            
        }
        
    })
    
    
    
    ############################################
    # 4. Set transparency and image size
    ############################################
    
    output$geom_alpha <- renderUI({
        
        if(is.null(data$img)){
            
            sliderInput('geom_alpha',
                        'Choose transparency:',
                        min = 0, max = 100,
                        value = 0, step = 1)
            
        }
        
        else{
            
            sliderInput('geom_alpha',
                        'Choose transparency:',
                        min = 0, max = 100,
                        value = 10, step = 1)
            
        }
        
    })
    
    output$image_size <- renderUI({
        
        if(is.null(data$img)){
            
            sliderInput('image_size',
                        'Size (% of original):',
                        min = 0, max = 600,
                        value = 0, step = 1)
            
        }
        
        else{
            
            sliderInput('image_size',
                        'Size (% of original):',
                        min = 0, max = 600,
                        value = 100, step = 1)
            
        }
        
    })
    
    
    
    ############################################
    # 5. Set up borders
    ############################################
    
    output$border_size_v <- renderUI({
        
        if(is.null(data$img)){
            
            sliderInput('border_size_v',
                        'Vertical margin:',
                        min = 1, max = 100,
                        value = 0, step = 1)
            
        }
        
        else{
            
            sliderInput('border_size_v',
                        'Vertical margin:',
                        min = 1, max = 100,
                        value = 20, step = 1)
            
        }
        
    })
    
    output$border_size_h <- renderUI({
        
        if(is.null(data$img)){
            
            sliderInput('border_size_h',
                        'Horizontal margin:',
                        min = 1, max = 100,
                        value = 0, step = 1)
            
        }
        
        else{
            
            sliderInput('border_size_h',
                        'Horizontal margin:',
                        min = 1, max = 100,
                        value = 20, step = 1)
            
        }
        
    })
    
    
    
    ############################################
    # 6. Set jitter controls
    ############################################
    
    output$jitter_v <- renderUI({
        
        if(is.null(data$img)){
            
            sliderInput('jitter_v',
                        'Vertical jitter:',
                        min = 0, max = 100,
                        value = 0, step = 1)
            
        }
        
        else{
            
            sliderInput('jitter_v',
                    'Vertical jitter:',
                    min = 0, max = 100,
                    value = 0, step = 1)
            
        }
        
    })
    
    output$jitter_h <- renderUI({
        
        if(is.null(data$img)){
            
            sliderInput('jitter_h',
                        'Horizontal jitter:',
                        min = 0, max = 100,
                        value = 0, step = 1)
            
        }
        
        else{
            
            sliderInput('jitter_h',
                        'Horizontal jitter:',
                        min = 0, max = 100,
                        value = 0, step = 1)
            
        }
        
    })
    
    
    
    ###################################################
    # Advanced settings
    ###################################################
    
    output$invert_pixels <- renderUI({
        
        checkboxInput('invert_pixels',
                      'Invert pixels',
                      FALSE)
        
    })
    
    
    output$manual_dot_colors <- renderUI({
        
        if(input$all_pixels == TRUE) return()
        
        textInput('manual_dot_colors',
                  'Set manual colors - separate (hex) by comma',
                  '#E53B1E, #000000')
        
    })
    
    output$manual_weights <- renderUI({
        
        if(input$all_pixels == TRUE) return()
        
        textInput('manual_weights',
                  'Set color proportions - separate by comma',
                  '1, 4')
        
    })
    
    output$use_manual_colors <- renderUI({
        
        if(input$all_pixels == TRUE) return()
        
        checkboxInput('use_manual_colors',
                      'Use manual colors',
                      FALSE)
        
    })
    
    output$update_colors <- renderUI({
        
        if(input$all_pixels == TRUE) return()
        
        actionButton(inputId = 'update_colors',
                     label = 'Update colors')
        
    })
    
    output$hex_error <- renderUI({
        
        if(is.null(data$img) | input$all_pixels == TRUE) return()
        
        if(data$color_input_error == TRUE){
            helpText('There seems to be an error in you Hex colors or their weights.
                     Can you double check them? :)')
        }
        
        else return()
        
    })
    
    
    
    ### Image colors
    
    output$use_image_colors <- renderUI({
        
        if(is.null(data$img)) return()
        
        checkboxInput('use_image_colors',
                      'Use image colors',
                      FALSE)
        
    })
    
    
    
    
    ### Advanced filtering
    
    output$advanced_filtering <- renderUI({
        
        if(is.null(data$img)) return()
        
        checkboxInput('advanced_filtering',
                      'Activate advanced filtering',
                      FALSE)
        
    })
    
    
    ### Rest of HSV
    
    output$filter_hue <- renderUI({
        
        sliderInput('filter_hue',
                    'Filter hue:',
                    min = 0, max = 100,
                    value = c(0, 100), step = 1)
        
    })
    
    output$filter_saturation <- renderUI({
        
        sliderInput('filter_saturation',
                    'Filter saturation:',
                    min = 0, max = 100,
                    value = c(0, 100), step = 1)
        
    })
    
    
    
    ### RGB
    
    output$filter_red <- renderUI({
        
        sliderInput('filter_red',
                    'Filter red:',
                    min = 0, max = 100,
                    value = c(0, 100), step = 1)
        
    })
    
    output$filter_green <- renderUI({
        
        sliderInput('filter_green',
                    'Filter green:',
                    min = 0, max = 100,
                    value = c(0, 100), step = 1)
        
    })
    
    output$filter_blue <- renderUI({
        
        sliderInput('filter_blue',
                    'Filter blue:',
                    min = 0, max = 100,
                    value = c(0, 100), step = 1)
        
    })
    
    
    
    
    
    
    ### Transparent background
    
    output$transparent_background <- renderUI({
        
        if(is.null(data$img)) return()
        
        checkboxInput('transparent_background',
                      'Transparent background',
                      FALSE)
        
    })
    
    
    
    ### Transparent background
    
    output$activate_density <- renderUI({
        
        if(is.null(data$img)) return()
        
        checkboxInput('activate_density',
                      'Activate density',
                      FALSE)
        
    })
    
    output$invert_density <- renderUI({
        
        if(is.null(data$img)) return()
        
        checkboxInput('invert_density',
                      'Invert density',
                      FALSE)
        
    })
    
    
    ### Square geom
    output$geom_boost <- renderUI({
        
        if(is.null(data$img)) return()
        
        checkboxInput('geom_boost',
                      'Size boost',
                      FALSE)
        
    })
    
    
    ### Base image
    output$base_image <- renderUI({
        
        if(is.null(data$img)) return()
        
        checkboxInput('base_image',
                      'Add base image',
                      FALSE)
        
    })
    
    
    
    observeEvent(input$update_colors, {
        
        if(!is.null(data$img)){
            
            # manual_dot_colors <- '#E53B1H, #000000, #000000'
            colors_temp <-
                input$manual_dot_colors %>%
                #manual_dot_colors %>%
                str_trim() %>%
                str_split(', ?') %>%
                unlist()
            
            # manual_weights <- '1, 4, 3'
            weights_temp <-
                input$manual_weights %>%
                #manual_weights %>%
                str_trim() %>%
                str_split(', ?') %>%
                unlist() %>%
                as.numeric()
            
            verification_df <-
                data_frame(colors_temp,
                           weights_temp) %>%
                mutate(hex =
                           colors_temp %>%
                           str_detect('^#[a-fA-F0-9]{6}'),
                       num =
                           is.numeric(weights_temp) & !is.na(weights_temp)) %>%
                mutate(verification = hex + num)
            
            if(mean(verification_df$verification) == 2){
                
                data$manual_colors <- colors_temp
                data$manual_weights <- weights_temp
                data$color_input_error <- FALSE
                
            }
            
            else{
                data$color_input_error <- TRUE
            }
            
        }
        
    })
    
    
    
    
    
    
    output$mixed_graph <- renderPlot(
        width = function() {
            if (is.null(data$plot_width)) {
                600
            }
            else{
                round(data$plot_width * (input$image_size / 100), 0)
            }
        },
        height = function() {
            if (is.null(data$plot_height)) {
                1000
            }
            else{
                round(data$plot_height * (input$image_size / 100), 0)
            }
        },
        {
            #height = 600, {
            
            if (is.null(data$img)) {
                return()
            }
            
            set.seed(13)
            
            # img_processed_main <-
            #     #convert_image(img, new_size = 30) %>%
            #     convert_image(data$img, new_size = 30) %>%
            #     arrange(id)
            
            img_processed <-
                #convert_image(img, new_size = 30) %>%
                convert_image(data$img, new_size = input$image_density) %>%
                arrange(id)
            
            
            data$processed <- TRUE
            
            
            ### Set binary labels
            set.seed(11)
            labels_v <-
                sample(c(0, 1), nrow(img_processed), replace = TRUE)
            
            img_processed$labels <-
                as.character(labels_v)
            
            
            
            ### Set image multipliers
            width_multip <-
                (data$plot_width / 100) *
                (input$image_size / 100)
            
            height_multip <-
                (data$plot_height / 100) *
                (input$image_size / 100)
            
            
            
            ### Add jitter
            
            jitter <-
                position_jitter(width =
                                    (input$jitter_h / 50) * width_multip,
                                height =
                                    (input$jitter_v / 50) * height_multip)
            
            
            ### Geometry choices
            
            gc <- input$geom_type
            
            geom_choice <- case_when(
                gc == 'circles' ~ 16,
                gc == 'squares' ~ 15,
                gc == 'diamonds' ~ 18,
                gc == 'triangles' ~ 17,
                gc == 'circles (empty)' ~ 1,
                gc == 'squares (empty)' ~ 0,
                gc == 'diamonds (empty)' ~ 5,
                gc == 'triangles (empty)' ~ 2,
                gc == '+' ~ 3,
                gc == 'x' ~ 4,
                gc == '*' ~ 8
            )
            
            geom_point_choices <-
                c(
                    'circles',
                    'squares',
                    'diamonds',
                    'triangles',
                    'circles (empty)',
                    'squares (empty)',
                    'diamonds (empty)',
                    'triangles (empty)',
                    'x',
                    '+',
                    '*'
                )
            
            # par(mar = c(0,0,0,0),
            #     xaxs = 'i', yaxs = 'i')
            
            ### Manual colors
            
            # Get weights
            set.seed(12)
            weights_v <-
                sample(
                    1:length(data$manual_weights),
                    nrow(img_processed),
                    replace = TRUE,
                    prob = data$manual_weights / sum(data$manual_weights)
                )
            
            # Assign colors
            img_processed$manual_colors <-
                data$manual_colors[weights_v]
            
            
            
            ### Invert pixels
            data$invert_pixels <-
                ifelse(
                    is.null(input$invert_pixels),
                    FALSE,
                    ifelse(input$invert_pixels == TRUE, TRUE,
                           FALSE)
                )
            
            
            ### All pixels
            data$all_pixels <-
                ifelse(is.null(input$all_pixels),
                       FALSE,
                       ifelse(input$all_pixels == TRUE, TRUE,
                              FALSE))
            
            
            ### Create geom_size local element
            
            geom_size <-
                (input$geom_size / 3) * (3 ^ input$geom_boost)
            
            
            
            ### Create pixel filter
            
            # Value filter
            value_filter_v <-
                img_processed$v >= input$image_contrast[1] &
                img_processed$v <= input$image_contrast[2]
            
            
            if(input$advanced_filtering == TRUE){
                
                # Hue filter
                hue_filter_v <-
                    img_processed$h >= input$filter_hue[1] &
                    img_processed$h <= input$filter_hue[2]
                
                # Saturation filter
                saturation_filter_v <-
                    img_processed$s >= input$filter_saturation[1] &
                    img_processed$s <= input$filter_saturation[2]
                
                
                
                # Red filter
                red_filter_v <-
                    img_processed$r * 100 >= input$filter_red[1] &
                    img_processed$r * 100 <= input$filter_red[2]
                
                # Green filter
                green_filter_v <-
                    img_processed$g * 100 >= input$filter_green[1] &
                    img_processed$g * 100 <= input$filter_green[2]
                
                # Blue filter
                blue_filter_v <-
                    img_processed$b * 100 >= input$filter_blue[1] &
                    img_processed$b * 100 <= input$filter_blue[2]
                
                
                
                # Final filter - all
                final_filter_v <-
                    (value_filter_v &
                         hue_filter_v &
                         saturation_filter_v &
                         red_filter_v &
                         green_filter_v &
                         blue_filter_v) %>%
                    as.numeric()
                
            }
            
            else{
                
                # Final filter - value only
                final_filter_v <-
                    as.numeric(value_filter_v)
                
            }
            
            
            
            ### Finalize data
            
            img_final <-
                img_processed %>%
                #filter(v < (input$image_contrast / 10000) ) %>%
                mutate(selected_pixel = final_filter_v,
                       invert_pixels = data$invert_pixels) %>%
                mutate(selected_pixel =
                           ifelse(
                               invert_pixels == TRUE,
                               abs(selected_pixel - 1),
                               selected_pixel
                           ))
            
            
            ### Calculate "density"
            
            if(input$activate_density == TRUE){
                
                x_main <-
                    img_final %>%
                    filter(selected_pixel == 1) %>%
                    select(x, y)
                
                message('ja', 1)
                
                x_process <-
                    bind_rows(
                        
                        # Line - 2
                        x_main %>%
                            mutate(xv = x - 2, yv = y + 2),
                        x_main %>%
                            mutate(xv = x - 1, yv = y + 2),
                        x_main %>%
                            mutate(xv = x, yv = y + 2),
                        x_main %>%
                            mutate(xv = x + 1, yv = y + 2),
                        x_main %>%
                            mutate(xv = x + 2, yv = y + 2),
                        
                        # Line - 1
                        x_main %>%
                            mutate(xv = x - 2, yv = y + 1),
                        x_main %>%
                            mutate(xv = x - 1, yv = y + 1),
                        x_main %>%
                            mutate(xv = x, yv = y + 1),
                        x_main %>%
                            mutate(xv = x + 1, yv = y + 1),
                        x_main %>%
                            mutate(xv = x + 2, yv = y + 1),
                        
                        # Center
                        x_main %>%
                            mutate(xv = x - 1, yv = y),
                        x_main %>%
                            mutate(xv = x - 1, yv = y),
                        x_main %>%
                            mutate(xv = x, yv = y),
                        x_main %>%
                            mutate(xv = x + 1, yv = y),
                        x_main %>%
                            mutate(xv = x + 2, yv = y),
                        
                        # Line + 1
                        x_main %>%
                            mutate(xv = x - 2, yv = y - 1),
                        x_main %>%
                            mutate(xv = x - 1, yv = y - 1),
                        x_main %>%
                            mutate(xv = x, yv = y - 1),
                        x_main %>%
                            mutate(xv = x + 1, yv = y - 1),
                        x_main %>%
                            mutate(xv = x + 2, yv = y - 1),
                        
                        # Line + 2
                        x_main %>%
                            mutate(xv = x - 2, yv = y - 2),
                        x_main %>%
                            mutate(xv = x - 1, yv = y - 2),
                        x_main %>%
                            mutate(xv = x, yv = y - 2),
                        x_main %>%
                            mutate(xv = x + 1, yv = y - 2),
                        x_main %>%
                            mutate(xv = x + 2, yv = y - 2)
                        
                    ) %>%
                    right_join(x_main, by = c('xv' = 'x', 'yv' = 'y')) %>%
                    filter(!is.na(x), !is.na(y)) %>%
                    group_by(x, y) %>%
                    dplyr::summarise(density_val = n())
                
                if(input$invert_density == TRUE){
                    
                    x_process <-
                        x_process %>%
                        mutate(density_val = abs(density_val - max(density_val) - 1))
                    
                }
                
                img_final <-
                    img_final %>%
                    left_join(x_process, by = c('x', 'y'))
                
            }
            
            else{
                
                img_final <-
                    img_final %>%
                    mutate(density_val = 1)
                
                message(4)
                
            }
            
            
            
            ###############################################
            #
            # Graphs
            #
            ###############################################
            
            ### Selected or all pixels
            
            if (input$all_pixels == TRUE) {

                img_final <-
                    img_final %>%
                    mutate(
                        mapped_color =
                            ifelse(
                                selected_pixel == 1,
                                input$geom_color_dot,
                                input$geom_color_dot_2
                            )
                    )
                
                img_copy_all <<- img_final
                
            }
            
            else{
                
                img_final <-
                    img_final %>%
                    mutate(mapped_color = input$geom_color_dot) %>%
                    filter(selected_pixel == 1)
                
                img_copy_single <<- img_final
                
            }
            
            
            if (input$use_image_colors == TRUE) {
                img_final <-
                    img_final %>%
                    mutate(mapped_color = hex)
                
            }
            
            
            write_rds(img_final, 'img_final.rds')
            #img_final <- read_rds('img_final.rds')
            
            ### Base image
            
            if(input$base_image == TRUE){
                
                base <-
                    ggplot() +
                    geom_point(
                        data = img_processed,
                        aes(x = x, y = y, col = hex),
                        size = 2,
                        shape = 15) +
                    scale_color_identity()
                
            }
            else{
                base <- ggplot()
            }
            
            
            if (input$geom_type %in% geom_point_choices) {
                if (is.null(input$use_manual_colors)) {
                    
                    img_final <-
                        base +
                        #filter(selected_pixel == 1) %>%
                        geom_point(
                            data = img_final,
                            aes(x = x, y = y)) +
                        geom_point(
                            aes(fill = mapped_color,
                                col = mapped_color,
                                size = density_val),
                            stroke = input$stroke_size / 10,
                            shape = geom_choice,
                            alpha = abs(input$geom_alpha - 100) / 100,
                            position = jitter
                        ) +
                        scale_size_continuous(range =
                                                  c(geom_size[1],
                                                    geom_size[2])) +
                        scale_fill_identity() +
                        scale_color_identity() +
                        scale_y_continuous(
                            limits = c(0, max(img_processed$y)),
                            expand = expand_scale(mult = input$border_size_v / 600)
                        ) +
                        scale_x_continuous(
                            limits = c(0, max(img_processed$x)),
                            expand = expand_scale(mult = input$border_size_h / 600)
                        ) +
                        theme_void() +
                        theme(legend.position = 'none')
                    
                }
                
                else if (input$use_manual_colors == FALSE) {
                    
                    message('woohoo')
                    
                    img_final <-
                        base +
                        #filter(selected_pixel == 1) %>%
                        geom_point(
                            data = img_final,
                            aes(x = x, y = y,
                                fill = mapped_color,
                                col = mapped_color,
                                size = density_val),
                            stroke = input$stroke_size / 10,
                            shape = geom_choice,
                            alpha = abs(input$geom_alpha - 100) / 100,
                            position = jitter
                        ) +
                        scale_size_continuous(range =
                                                  c(geom_size[1],
                                                    geom_size[2])) +
                        scale_fill_identity() +
                        scale_color_identity() +
                        scale_y_continuous(
                            limits = c(0, max(img_processed$y)),
                            expand = expand_scale(mult = input$border_size_v / 600)
                        ) +
                        scale_x_continuous(
                            limits = c(0, max(img_processed$x)),
                            expand = expand_scale(mult = input$border_size_h / 600)
                        ) +
                        theme_void() +
                        theme(legend.position = 'none')
                    
                }
                
                else{
                    
                    img_final <-
                        base +
                        #filter(selected_pixel == 1) %>%
                        geom_point(
                            data = img_final,
                            aes(x = x, y = y)) +
                        geom_point(
                            aes(fill = manual_colors,
                                col = manual_colors,
                                size = density_val),
                            stroke = input$stroke_size / 10,
                            shape = geom_choice,
                            alpha = abs(input$geom_alpha - 100) / 100,
                            position = jitter
                        ) +
                        scale_size_continuous(range =
                                                  c(geom_size[1],
                                                    geom_size[2])) +
                        scale_fill_identity() +
                        scale_color_identity() +
                        scale_y_continuous(
                            limits = c(0, max(img_processed$y)),
                            expand = expand_scale(mult = input$border_size_v / 600)
                        ) +
                        scale_x_continuous(
                            limits = c(0, max(img_processed$x)),
                            expand = expand_scale(mult = input$border_size_h / 600)
                        ) +
                        theme_void() +
                        theme(legend.position = 'none')
                    
                }
                
            }
            
            else if (input$geom_type == c('binary')) {
                
                img_final <-
                    base +
                    #filter(selected_pixel == 1) %>%
                    geom_point(
                        data = img_final,
                        aes(x = x, y = y)) +
                    geom_text(
                        aes(fill = mapped_color,
                            col = mapped_color,
                            size = density_val),
                        stroke = input$stroke_size / 10,
                        fontface = 'bold',
                        alpha = abs(input$geom_alpha - 100) / 100,
                        position = jitter
                    ) +
                    scale_size_continuous(range =
                                              c(geom_size[1],
                                                geom_size[2])) +
                    scale_color_identity() +
                    scale_y_continuous(
                        limits = c(0, max(img_processed$y)),
                        expand = expand_scale(mult = input$border_size_v / 600)
                    ) +
                    scale_x_continuous(
                        limits = c(0, max(img_processed$x)),
                        expand = expand_scale(mult = input$border_size_h / 600)
                    ) +
                    theme_void() +
                    theme(legend.position = 'none')
                
            }
            
            else if (input$geom_type == 'line') {
                set.seed(13)
                
                img_final <-
                    base +
                    #filter(selected_pixel == 1) %>%
                    geom_point(
                        data = img_final,
                        aes(x = x, y = y)) +
                    geom_path(
                        aes(col = mapped_color,
                            size = density_val * (input$geom_size / 20)),
                        stroke = input$stroke_size / 10,
                        alpha = abs(input$geom_alpha - 100) / 100,
                        position = jitter
                    ) +
                    scale_color_identity() +
                    scale_y_continuous(
                        limits = c(0, max(img_processed$y)),
                        expand = expand_scale(mult = input$border_size_v / 600)
                    ) +
                    scale_x_continuous(
                        limits = c(0, max(img_processed$x)),
                        expand = expand_scale(mult = input$border_size_h / 600)
                    ) +
                    theme_void() +
                    theme(legend.position = 'none')
                
            }
            
            
            
            ### Set background
            
            if(input$transparent_background == FALSE){
                
                img_final <-
                    img_final +
                    theme(
                        plot.background =
                            element_rect(
                                fill = input$geom_color_bkg,
                                colour = input$geom_color_bkg
                            )
                    )
                
            }
            
            else{
                
                img_final <-
                    img_final +
                    theme(panel.background =
                              element_rect(fill = 'transparent',
                                           colour = NA),
                          plot.background =
                              element_rect(fill = 'transparent',
                                           colour = NA))
                
            }
            
            data$img_final <-
                img_final
            
            img_final
            
        }, bg = "transparent"
    )
    
    
    # output$download_plot <- renderUI({
    # 
    #     if(is.null(data$img_final)){
    #         return()
    #     }
    # 
    #     actionButton(
    #         inputId = 'download_plot',
    #         label = 'Download plot'
    #     )
    # 
    # })
    # 
    # observeEvent(input$download_plot, {
    #     
    #     # plot_file_name <-
    #     #     paste0(choose.dir(),
    #     #            '\\temp.png', sep='')
    #     # 
    #     # png(plot_file_name, width = 5000, height = 3000)
    #     # print(data$img_final)
    #     # dev.off()
    #     
    #     ggsave(data$img_final,
    #            filename = 'temp.eps',
    #            device = 'eps')
    # 
    # })
    
}




##################################################################
##################################################################
##
##   APP
##
##################################################################
##################################################################

shinyApp(ui = ui, server = server)








