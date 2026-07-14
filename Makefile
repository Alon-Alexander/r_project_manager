.PHONY: document pkgdown cmdcheck

document:
	Rscript -e "devtools::document()"

pkgdown:
	Rscript -e "pkgdown::build_site()"

cmdcheck:
	Rscript -e "rcmdcheck::rcmdcheck(args = c('--no-manual', '--compact-vignettes=gs+qpdf'), error_on = 'warning')"
