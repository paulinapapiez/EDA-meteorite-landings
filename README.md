# NASA Meteorite Landings Analysis (1900–2000)

## 🌐 Live Demo

The Shiny dashboard is deployed using shinyapps.io:

🔗 https://paulinapapiez.shinyapps.io/Metorite-landings/

You can explore:
- Dynamic filtering
- Interactive maps
- Temporal trend analysis
- Statistical summaries

No local setup required.
Comprehensive data analysis project exploring NASA meteorite landing records between 1900 and 2000.

The repository includes:
- Exploratory Data Analysis (R Markdown report)
- Interactive Shiny dashboard
- Statistical analysis
- Geospatial visualization
- Temporal trend analysis

---

## 🚀 Project Overview

This project analyzes meteorite landings recorded by NASA and focuses on:

- Data cleaning and preprocessing
- Time-based trend analysis
- Mass distribution analysis
- Meteorite class frequency
- Geographic distribution using interactive maps
- Descriptive statistical analysis

The project consists of two main components:
1. 📄 Analytical report (R Markdown → HTML)
2. 📊 Interactive Shiny dashboard

---

## 📊 Dataset

Source: NASA Meteorite Landings dataset (TidyTuesday GitHub)

Filtered to include only records from 1900–2000.

Dataset contains:
- Meteorite name
- Class
- Mass (grams)
- Year
- Latitude & longitude
- Type (Fell / Found)

---

## 📈 Analytical Report (EDA)

The R Markdown report includes:

- Data preprocessing and filtering
- Interactive data table
- Temporal trend visualization
- Top 10 meteorite classes
- Average mass trends by decade
- Statistical summary:
  - Min / Max
  - Mean / Median
  - Standard deviation
  - Skewness
  - Kurtosis
- Interactive Leaflet map

The report ensures full reproducibility.

---

## 🗺 Interactive Shiny Dashboard

The Shiny application provides:

- Dynamic filtering (year range, type, class)
- Interactive map with mass-based color scaling
- Circle markers scaled by meteorite mass
- Plotly interactive charts
- Real-time descriptive statistics
- Clean UI layout with multiple tabs

---

## 📊 Key Insights

- Meteorite observations increased across decades.
- Mass distribution is strongly right-skewed.
- A small number of extremely heavy meteorites dominate total mass.
- Certain classes appear significantly more often.
- Geographic distribution varies across continents.

---

## 🧠 Skills Demonstrated

- Exploratory Data Analysis (EDA)
- Reactive programming (Shiny)
- Data cleaning & transformation
- Statistical analysis
- Geospatial data visualization
- Interactive dashboard development
- Reproducible reporting (R Markdown)
- Working with external data sources (GitHub raw CSV)

---

## 🛠 Tech Stack

- R
- Shiny
- R Markdown
- tidyverse
- Leaflet
- Plotly
- DT
- moments
- knitr
- RColorBrewer

---

## ▶️ How to Run

### Run Shiny App:
```r
shiny::runApp()
Render Report:

Open analysis.Rmd and click Knit to HTML.
```r
shiny::runApp()
