# README


# Subnational Living Wage Estimation in Mexico

## Introduction

The purpose of this repository is to present the methodology and code
for the subnational living wage estimation in Mexico. This project was
directed by [me](https://hisoto.github.io/), in my position as the Labor
Economics Analysis Coordinator, and in collaboration with my partners at
the [Economic Analysis
Directorate](https://www.gob.mx/conasami/estructuras/damyr) of the
National Minimum Wage Commission
([CONASAMI](https://www.gob.mx/conasami)) in Mexico. This project aims
to provide a comprehensive and transparent approach to estimating living
wages across different regions of Mexico, taking into account national
and international standards for quality of life. It’s purpose is the
serve as a reference in minimum wage, salary and contractual
negotiations. This work is currently under review by different
institutions in Mexico, like the National Institute of Statistics and
Geography ([INEGI](https://www.inegi.org.mx/)), but a preliminary
version of the final results can be found in the [September monthly
report](https://www.gob.mx/cms/uploads/attachment/file/1024349/Informe_Septiembre_2025.pdf)
of CONASAMI.

It is based on the Anker Methodology, which is a widely used approach
for calculating living wages. Particularly, on the [subnational
estimation](https://www.ankerresearchinstitute.org/anker-program-subnational#brazil)
made in Brazil, but in consideration of the differences of the Mexican
context, the main data sources, and the institutional constraints we
faced, it has been modified to include the framework developed by the
Economic Commission for Latin America and the Caribbean (ECLAC) for the
estimation of the cost of [poverty
lines](https://www.cepal.org/en/publications/44920-income-poverty-measurement-updated-methodology-and-results).
The code is written in R and Stata, it is organized into several scripts
and one Do-file that correspond to different components of the living
wage; food, housing and non-food-non-housing (NFNH) costs. The
repository also includes a script for the final estimation of the living
wage, which combines the results from the previous scripts.

## Data sources

The main data source in the National Survey of Household Income and
Expenditure ([ENIGH](https://www.inegi.org.mx/programas/enigh/nc/2024/))
of 2024, which is conducted by the National Institute of Statistics and
Geography (INEGI) in Mexico.

ENIGH is a nationally, state, urban and rural representative
probabilistic survey that collects detailed information on household
income, expenditure, and socio-demographic characteristics. It provides
the necessary data to estimate the cost of living and the components of
the living wage, such as food, housing, and non-food-non-housing costs.

## Repository Structure

The repository is organized into the following folders:

- `data/`: This folder contains the raw data used for the analysis,
  including microdata from national surveys and other relevant datasets.
  **NOTE:** Due the size of the ENIGH microdata, it is not included in
  the repository, but it can be accessed through the INEGI
  [website](https://www.inegi.org.mx/programas/enigh/nc/2024/#microdatos).
- `scripts/`: This folder contains the R and Stata scripts for the
  different components of the living wage estimation, as well as the
  final estimation script.
- `finaldata/`: This folder contains the output of the analysis,
  including tables that summarize the results of the living wage
  estimation.
- `graphs/`: This folder contains figures.
- `README.qmd`: This file, which provides an overview of the project and
  the structure of the repository.

## Methodology

The methodology for estimating the living wage in Mexico is based on the
Anker Methodology, which consists of several steps:

1.  **Defining the living wage**: The living wage is defined as the
    minimum income necessary for a worker to meet their basic needs,
    including food, housing, and other essential expenses.

2.  **Estimating the cost of food**: The cost of food is estimated using
    the ENIGH to identify the cost of a representative food basket for a
    reference quintile of the income distribution, which is selected by
    modifying the methodology used by ECLAC for the estimation of
    poverty lines. The reference household is defined as a household
    with two adults and two children that comply with living adequacy
    criteria. The size and composition of the reference household
    reflect a common family structure in Mexico and that is aligned with
    the objectives set in the National Development Plan of the federal
    government. The cost of the food basket is calculated using the
    prices of food items reported in the ENIGH data at the national,
    state, urban and rural levels plus a 20% extra considering the cost
    of waste (5%) and of food diversity (15%).

The selection of the reference quintile is based on a set of
*Indicadores de Bienestar* developed to identify households with
adequate conditions in the dimensions of housing, food, basic services,
and education. The percentile of the distribution of current per capita
income to which each household belongs was then identified by ordering
households from lowest to highest and calculating the proportion of
households that meet the *Indicador de Bienestar* for each percentile.
When graphing the income distribution against the calculated average,
the indicators that showed a clear positive relationship with current
income were selected. In other words, priority was given to those with
low incidence in the left tail and high incidence in the right tail of
the distribution.

The next graphs shows the indicator for the food dimension. In this
particular case, the indicator *Sin falta de alimentos* tracks the
percentage of households in each percentile that reported having
experience a lack of food.

![Food wellbeing indicators](graphs/indicadores_alimentarios.png)

3.  **Estimating the cost of housing**: The cost of housing is estimated
    using the ENIGH data with a quantile regression approach to estimate
    the cost of a decent housing for the reference household. It is
    based on the characteristics of the dwelling, such as the number of
    rooms and type of construction, following the criteria set by
    ONU-Habitat for housing adequacy. For the selection of the quantile
    a similar approach to the one used for the food basket is used, but
    in this case, the methodology is modified considering the criteria
    set by ONU-Habitat. The cost of housing is estimated at the
    national, state, urban and rural levels.

4.  **Estimating the cost of non-food-non-housing (NFNH) expenses**: The
    cost of NFNH expenses is estimated using the results of the housing
    cost. Using ENIGH, we plot the relationship between housing
    expenditure and the total expenditure probability distribution, and
    we select the percentile that corresponds to the one chosen for the
    housing cost estimation. The average expenditure of the next five
    percentiles above the selected cutoff is then calculated and used as
    the benchmark for NFNH expenses. The next graph shows the plot for
    urban areas at the national level.

![Relation between total expenses probability distribution and housing
expenses](graphs/relacion/nacional/urbano_nacional.png)

This approach is based on the assumption that the NFNH expenses are
correlated with housing expenses, which is a sensible assumption in
living wage estimation methodologies. The cost of NFNH expenses is
estimated at the national, state, urban and rural levels.

5.  **Calculating the living wage**: The living wage is calculated by
    summing the costs of food, housing, and NFNH expenses for the
    reference household plus a 5% in terms of savings.

The end result are 66 living wage estimates, 64 for each combination of
state and urban/rural areas, and two for the national level, one for
urban and one for rural areas.

## Results

    Rows: 66
    Columns: 11
    $ nom_ent                <chr> "Aguascalientes", "Aguascalientes", "Baja Calif…
    $ cve_ent                <int> 1, 1, 2, 2, 3, 3, 4, 4, 7, 7, 8, 8, 9, 9, 5, 5,…
    $ ambito                 <chr> "Rural", "Urbano", "Rural", "Urbano", "Rural", …
    $ rural                  <int> 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0,…
    $ canasta_alimentos      <dbl> 2907.635, 3087.932, 3087.558, 3571.475, 3265.83…
    $ vivienda_mensual       <dbl> 3514.091, 5092.295, 7628.554, 11054.594, 6409.0…
    $ NANV                   <dbl> 3774.439, 4542.543, 5990.807, 6864.653, 4714.58…
    $ alimentos_familiar     <dbl> 11630.54, 12351.73, 12350.23, 14285.90, 13063.3…
    $ nanv_familiar          <dbl> 15097.76, 18170.17, 23963.23, 27458.61, 18858.3…
    $ ingreso_digno_familiar <dbl> 31754.51, 37394.91, 46139.12, 55439.06, 40247.2…
    $ salario_digno          <dbl> 15877.25, 18697.45, 23069.56, 27719.53, 20123.6…

The preliminary results of the living wage estimation can be found in
the [September monthly
report](https://www.gob.mx/cms/uploads/attachment/file/1024349/Informe_Septiembre_2025.pdf)
of CONASAMI. The final results will be published in a forthcoming
report, which will include a detailed analysis of the living wage
estimates and their implications for policy and labor market dynamics in
Mexico.

The following map shows the preliminary results of the living wage
estimation for the urban areas of the 32 states of Mexico. The living
wage estimates are expressed in Mexican pesos per month in August 2024
prices. At the national level the living wage is estimated at 20,010.68
pesos per month, while at the state level, the living wage estimates
range from 15,554.44 pesos per month in Tlaxcala to 27,719.53 pesos per
month in Baja California.

![Living wage estimates by state and urban/rural
areas](graphs/maps/mapa_wage_urbano.png)

In contrast, the living wage estimates for rural areas at the nationa
level is estimated at 17,091.64 pesos per month, while at the state
level, the living wage estimates range from 13,115.46 pesos per month in
Tlaxcala to 24,107.59 pesos per month in Ciudad de México.

![Living wage estimates by state and urban/rural
areas](graphs/maps/mapa_wage_rural.png)
