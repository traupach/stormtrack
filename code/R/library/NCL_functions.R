## NCL_functions.R
## Functions to help with running NCL scripts.
## Author: Tim Raupach <timothy.raupach@giub.unibe.ch>

runNCL = function(args, dir="nc", latex=TRUE, figname="", label="", caption="",
                  width="\\textwidth", nclBinary="ncl", shenv=character(),
                  captionFirst=FALSE, figureEnv="figure") {
    ## Run NCL to make a plot, and output latex figure code if required.
    ##
    ## Args:
    ##   args: The arguments to NCL.
    ##   dir: Run in this directory (default: "nc").
    ##   latex: Print latex code? (Default: TRUE).
    ##   figname: The figure name to plot.
    ##   label: Label for latex output.
    ##   caption: Caption for latex output.
    ##   width: Image width in latex output.
    ##   nclBinary: The NCL binary to run.
    ##   shenv: Environment variable ame-value pairs to pass to system2() (Default: none).
    ##   captionFirst: Display the caption before the figure? (Default: TRUE).
    ##   figureEnv: LaTeX figure environment to use.
    ## 
    ## Returns: void.

    if(!dir.exists(dir)) dir.create(dir)
    wd = getwd()
    setwd(dir)
    system2(nclBinary, args, env=shenv)
    setwd(wd)

    ## Include if required.
    if(latex) {
        cat(paste("\\begin{", figureEnv, "}[t]\n", sep=""))
        cat("\\centering\n")

        if(captionFirst != TRUE) 
            cat(paste("\\includegraphics[width=", width, "]{", dir, "/", figname, "}\n", sep=""))
            
        cat(paste("\\caption{", caption, "}\n", sep=""))
        cat(paste("\\label{", label, "}\n", sep=""))

        if(captionFirst == TRUE) 
            cat(paste("\\includegraphics[width=", width, "]{", dir, "/", figname, "}\n", sep=""))

        cat(paste("\\end{", figureEnv, "}\n", sep=""))
    }
}
