screen_detector_script = '
                                var dimension = [0, 0];
                                
                                var cdsPlotDimensions = [1500, 700];
                                
                                
                                $(document).on("shiny:connected", function(e) {
                                
                                    dimension[0] = cdsPlotDimensions[0];
                                    dimension[1] = cdsPlotDimensions[1];
                                    
                                    Shiny.onInputChange("dimension", dimension);
                                });
                                
                                
                                
                                
                                $(window).resize(function(e) {
                                
                                    console.log("window.innerWidth:", window.innerWidth)
                                    console.log("window.innerHeight:", window.innerHeight)
                                
                                    dimension[0] = window.innerWidth * 0.6 ;
                                    //dimension[0] = cdsPlotDimensions[0];
                                    
                                    dimension[1] = window.innerHeight * 0.3;
                                    
                                    Shiny.onInputChange("dimension", dimension);
                                    
                                });
                            '