mainView <- function(){
  # Create a conditional panel which olny show his content
  # when sidebar menu id is equal whith menu item id
  conditionalPanel(
    
    ## PANEL HOME
    # show information about NVBI Geo objects
    condition = "input.sidebar == 'home'",
    
    # Create a body tab panel
    tabsetPanel(
      type = "tabs",
      # Create tab and add a title
      tabPanel("NCBI Geo",
               fluidRow(
                 box(width = 12,
                     h1("Overview of GEO"),
                     p("The NCBI Gene Expression Omnibus (GEO) serves as a public repository
                       for a wide range of high-throughput experimental data. These data include
                       single and dual channel microarray-based experiments measuring mRNA,
                       genomic DNA, and protein abundance, as well as non-array techniques
                       such as serial analysis of gene expression (SAGE), mass spectrometry
                       proteomic data, and high-throughput sequencing data.
                       
                       At the most basic level of organization of GEO, there are four basic entity types.
                       The first three (Sample, Platform, and Series) are supplied by users; the fourth,
                       the dataset, is compiled and curated by GEO staff from the user-submitted data.
                       See the",
                       a("GEO home page", href="https://www.ncbi.nlm.nih.gov/geo/"),
                       "for more information.")
                 ),
                 box(
                   width = 12,
                   h2("Platforms"),
                   p("A Platform record describes the list of elements on the array 
                     (e.g., cDNAs, oligonucleotide probesets, ORFs, antibodies) 
                     or the list of elements that may be detected and quantified in that experiment 
                     (e.g., SAGE tags, peptides). Each Platform record is assigned a unique and stable 
                     GEO accession number (GPLxxx). A Platform may reference many Samples that have been 
                     submitted by multiple submitters.")
                   ),
                 box(
                   width = 12,
                   h2("Samples"),
                   p("A Sample record describes the conditions under which an individual Sample was
                     handled, the manipulations it underwent, and the abundance measurement of each
                     element derived from it. Each Sample record is assigned a unique and stable GEO
                     accession number (GSMxxx). A Sample entity must reference only one Platform and
                     may be included in multiple Series.")
                   ),
                 box(
                   width = 12,
                   h2("Series"),
                   p("A Series record defines a set of related Samples considered to be part of a group,
                     how the Samples are related, and if and how they are ordered. A Series provides a
                     focal point and description of the experiment as a whole. Series records may also
                     contain tables describing extracted data, summary conclusions, or analyses.
                     Each Series record is assigned a unique and stable GEO accession number (GSExxx).
                     Series records are available in a couple of formats which are handled by GEOquery
                     independently. The smaller and new GSEMatrix files are quite fast to parse; a simple
                     flag is used by GEOquery to choose to use GSEMatrix files (see below).")
                   ),
                 box(
                   width = 12,
                   h2("Datasets"),
                   p("GEO DataSets (GDSxxx) are curated sets of GEO Sample data. A GDS record represents 
                     a collection of biologically and statistically comparable GEO Samples and forms the 
                     basis of GEO's suite of data display and analysis tools. Samples within a GDS refer 
                       to the same Platform, that is, they share a common set of probe elements. Value 
                       measurements for each Sample within a GDS are assumed to be calculated in an 
                       equivalent manner, that is, considerations such as background processing and 
                       normalization are consistent across the dataset. Information reflecting experimental 
                       design is provided through GDS subsets.")
                   )
                 )
        ),
        tabPanel(
          "Micro arrays",
          fluidRow(
            box(
              width = 12,
              h1("To Do"),
              a("Micro arrays en espanyol",
                href="https://www.cabimer.es/web3/unidades-apoyo/genomica/microarrays-de-affymetrix/")
            )
          )
        )
      )
    )
}