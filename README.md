# TEPHI WW Dashboard

## Introduction
This Shiny dashboard is designed to generate an interactive dashboard that replicates the TEPHI report and develops/enhances visualization and analysis for the TEPHI WW Project. 

## Features

- **Interactive Texas Map**: Visualize the geographical locations of Wastewater Treatment Plants across Texas with dynamic filtering options.

- **Data Analysis Tools**: Explore CDS and qPCR data through interactive tables and trend plots, allowing for detailed data-driven insights.

- **Customizable Views**: Users can select from multiple interactive view options to tailor the dashboard display to their specific needs.

## Data Sources

This dashboard utilizes data from the TEPHI_WW_RawData under TEPHI_CSMAPS/TEPHI_WW/TEPHI_WW_RawData shared OneDrive folder. 

## Getting Started

To run this dashboard locally, clone this repository and install the required R packages.

 - Clone this repository and install the required R packages in DataPreprocessing.R
 
 - Download the <mark> Team OneDrive folder </mark> from Data Sources and have it in the same path with this clone folder and run global.R, and renemaed that folder as "Data".
 
 - Required files in the <mark>Data</mark> (refer to DatabaseDocumentation for the datatable information). 
   
     - genome_coverage/
     - metadata/
     - qPCR/
     - site_coding/
     - taxonomical_profiles/
     - geographical_files/


## Code Structure

### 1. Data Processing Script (`DataProcessing.R`)

**Functionality**:

- **Data Consolidation**: Consolidates data from various sources, including taxonomical profiles, metadata, genome coverage, and qPCR data.

- **Data Preloading**: Preloads essential data to ensure efficient data management and availability on the Shiny server side.

### 2. User Interface Design (`ui.R`)

**Tabs and Layout**:

- **Main Tabs**:

  - **Collection Site**: Displays an interactive map of Texas, highlighting all counties and cities with participating Wastewater Treatment Plants (WWTP).
  
  - **Comprehensive Deep Sequencing (CDS)**: Provides detailed visualizations and report analyses related to the CDS technique as presented in the TEPHI WW report.
  
  - **qPCR**: Offers visualizations and report analyses pertinent to the qPCR technique, also detailed in the TEPHI WW report.
- **Sub-Tabs**: Includes additional sub-tabs within each main tab to present various visualizations and analyses tailored to the user's selection.

### 3. Server Logic (`server.R`)

**Core Functions**:

- **Dynamic UI Management**: Handles dynamic UI elements that react to user inputs.

- **Data Management**: Manages the processing and filtering of data for effective visualization.

- **Content Generation**: Generates plots, tables, and interactive maps to provide actionable insights and data representations.


## Contributing
