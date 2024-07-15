
screen_detector_script = '

var dimension = [0, 0];

let baseHeight = 710;
let baseWidth = 1250;
var cdsPlotDimensions = [baseWidth, baseHeight];


var qpcr_plot_dimension = [baseWidth, baseHeight];

var collectionDatesPlot_cds_dimension = [baseWidth, baseHeight];
var qpcr_ElPaso_reactivePlot_dimension = [baseWidth, baseHeight];
var city_tsnep_dimension = [baseWidth, baseHeight];
var date_tsnep_dimension = [baseWidth, baseHeight];

var virusPlot_dimension = [baseWidth, baseHeight];


var widthScaleFactor = 0.76

var screenWidthRanges = [ [300, 600] , 
                          [600, 1000], 
                          [1000, 1200], 
                          [1200, 1400], 
                          [1400, 1600], 
                          [1600, 1800], 
                          [1800, 2000], 
                          [2000, 2400]  ]


$(document).on("shiny:connected", function(e) {

          
    for (let screenWidthRange of screenWidthRanges) {
      let minimum = screenWidthRange[0]
      let maximum = screenWidthRange[1]

      if (window.innerWidth >= minimum) {
        if(window.innerWidth < maximum) {
          console.log("window.innerWidth:", window.innerWidth)

          let adjustedWidth;
          let adjustedHeight;

          if(maximum === 1200) {
            adjustedWidth =  baseWidth - 200;
            adjustedHeight = baseHeight * 0.85
          }

          else if(maximum === 1000) {
            adjustedWidth =  baseWidth - 400;
            adjustedHeight = baseHeight * 0.85
          }

          else if(maximum === 600) {
            adjustedWidth =  baseWidth - 600;
            adjustedHeight = baseHeight * 0.85
          }

          else {
            adjustedWidth = window.innerWidth * widthScaleFactor;
            adjustedHeight = baseHeight;
          }

          var cdsPlotDimensions = [adjustedWidth, adjustedHeight  ];
          var qpcr_plot_dimension = [adjustedWidth, adjustedHeight];
          var collectionDatesPlot_cds_dimension = [adjustedWidth, adjustedHeight];
          var qpcr_ElPaso_reactivePlot_dimension = [adjustedWidth, adjustedHeight];
          var city_tsnep_dimension = [adjustedWidth, adjustedHeight];
          var date_tsnep_dimension = [adjustedWidth, adjustedHeight];
          var virusPlot_dimension = [adjustedWidth, adjustedHeight];

          break;
        }
      }
    }

    Shiny.onInputChange("dimension", dimension);
    Shiny.onInputChange("cdsPlotDimensions", cdsPlotDimensions);
    Shiny.onInputChange("qpcr_plot_dimension", qpcr_plot_dimension);
    Shiny.onInputChange("collectionDatesPlot_cds_dimension", collectionDatesPlot_cds_dimension);
    Shiny.onInputChange("qpcr_ElPaso_reactivePlot_dimension", qpcr_ElPaso_reactivePlot_dimension);
    Shiny.onInputChange("city_tsnep_dimension", city_tsnep_dimension);
    Shiny.onInputChange("date_tsnep_dimension", date_tsnep_dimension);
    Shiny.onInputChange("virusPlot_dimension", virusPlot_dimension);
});




$(window).resize(function(e) {

    //console.log("window.innerWidth:", window.innerWidth)
    //console.log("window.innerHeight:", window.innerHeight)

    //dimension[0] = window.innerWidth * 0.6 ;
    //dimension[0] = cdsPlotDimensions[0];
    
    //dimension[1] = window.innerHeight * 0.3;
    

    // Shiny.onInputChange("dimension", dimension);
    // Shiny.onInputChange("cdsPlotDimensions", cdsPlotDimensions);
    // Shiny.onInputChange("qpcr_plot_dimension", qpcr_plot_dimension);
    // Shiny.onInputChange("collectionDatesPlot_cds_dimension", collectionDatesPlot_cds_dimension);
    // Shiny.onInputChange("qpcr_ElPaso_reactivePlot_dimension", qpcr_ElPaso_reactivePlot_dimension);
    // Shiny.onInputChange("city_tsnep_dimension", city_tsnep_dimension);
    // Shiny.onInputChange("date_tsnep_dimension", date_tsnep_dimension);
    // Shiny.onInputChange("virusPlot_dimension", virusPlot_dimension);
    
});


                            '
