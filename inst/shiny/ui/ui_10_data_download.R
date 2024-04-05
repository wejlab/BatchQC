tabPanel("Data Download",
         # Application title
         titlePanel("Data Download"),
mainPanel('Data Download',
         h5("Download Summarized Experiment"),
         verbatimTextOutput('se_download'),
         downloadButton("downloadData", "Download"),
         br(),
         h4(strong("Usage")),
         h5("The SummarizedExperiment object containing your data may be previewed and downloaded here. The SummarizedExperiment object includes your uploaded data and metadata as well as any normalized and batch corrected versions of your data that were generated while using BatchQC. Please select the Download button to download the SummarizedExperiment object.")
  )
)
