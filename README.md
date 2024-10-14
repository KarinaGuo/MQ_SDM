<h>Repository for USyd's ENVI5809 course.</h>

**Abstract**  
Conservation is expensive. To maximise resources, efforts in restoration are leaning towards ‘genetic optimisation’ of planting stocks. This comes in the form of maximising the revegetation stock’s traits whether for disease resistance, drought tolerance for short-term resilience, or general genetic diversity for long-term population health. However, these traits often compromise one another. For example, the increase in a population’s average disease resistance has been demonstrated to reduce the population’s genetic diversity. This consequently raises the question – “When maximising multiple objectives, which objective should be given more weight?”. Decisions to such questions are intuitively shaped by the scenario at hand. Here, I present how the intersection of the predicted distribution from Maxent species distribution models can assist practitioners in arriving at a solution that works for them. The pathogen, Myrtle rust (*Austropuccinia psidii* (G. Winter) Beenken), and its host, *Melaleuca quinquenervia* (Cav.) S.T.Blake, is used as a case study that provide the foundation for future applications in other non-model host- pathogen dynamics.  
  <br>
  <br>

**File structure to this repository**  
└── SDM_Maxent.Rmd *; R markdown file to generate present and future Maxent SDMs of MR and MQ*  
└── SDM_maxent.RData *; R datafile generate from SDM_maxent.Rmd*  
└── Mquin_ALA_records.csv *; Melaleuca quinquenervia input data for SDM*  
└── MR_locs.xlsx *; Austropuccinia psidii input data for SDM*  
└── SDM_biomod.Rmd *; R markdown file to generate present biomod SDMs of MR and MQ - note markdown may take a significant time to run, please contact author if .RData is desired*  
└── /Figures/. *; Figures from report*  
└── /output/. *; Maxent and biomod diagnostic outputs from respective .Rmd*  
└── /Shiny/MR_MQ_SDM/. *; Data and scripts to create shiny application. Input information is generated from SDM_Maxent.Rmd*  
&nbsp;&nbsp;&nbsp;&nbsp;├── app.R *; Script used to create file*  
&nbsp;&nbsp;&nbsp;&nbsp;└── data/. *; Input data files* 
