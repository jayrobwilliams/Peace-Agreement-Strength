all:	PA.RDS Paper.pdf SI.pdf clean

PA.RDS: Directories.tar.gz
	Rscript --verbose 00\ Execution.R 2>&1 | tee -a log.txt

Knitr\ Input/knitr\ input.RData: PA.RDS
	Rscript --verbose 04\ Analysis.R 2>&1 | tee -a log.txt

Paper.pdf: Knitr\ Input/knitr\ input.RData
	Rscript -e "require ('knitr'); knit ('Paper.Rnw')" 2>&1 | tee -a log.txt
	pdflatex -interaction=nonstopmode Paper
	bibtex Paper
	pdflatex -interaction=nonstopmode Paper
	pdflatex -interaction=nonstopmode Paper

SI.pdf: Knitr\ Input/int_dur.RData
	Rscript -e "require ('knitr'); knit ('SI.Rnw')" 2>&1| tee -a log.txt
	pdflatex -interaction=nonstopmode SI
	bibtex SI
	pdflatex -interaction=nonstopmode SI
	pdflatex -interaction=nonstopmode SI

clean:
	rm -vf *.aux *.log *.out *.bbl *.blg *.toc *.synctex.gz