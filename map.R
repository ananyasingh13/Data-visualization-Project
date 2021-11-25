library(plotly)
library(rjson)

url <- 'https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json'
counties <- rjson::fromJSON(file=url)
url2<- "https://raw.githubusercontent.com/plotly/datasets/master/fips-unemp-16.csv"
df <- read.csv(url2, colClasses=c(fips="character"))
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

fig <- plot_ly()

fig <- fig %>% add_trace(
  type="choropleth",
  geojson=counties,
  locations=df$fips,
  z=df$unemp,
  colorscale="Viridis",
  zmin=0,
  zmax=12,
  marker=list(line=list(
    width=0)
  )
)

fig <- fig %>% colorbar(title = " Percentage (%)")
fig <- fig %>% layout(
  title = "Percantage of people having EMV in a particular region"
)

fig <- fig %>% layout(
  geo = g
)

fig
