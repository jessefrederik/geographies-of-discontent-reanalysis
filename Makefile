.PHONY: all clean paper with-appendix appendix

all: size_gradient_report.pdf size_gradient_report_appendix.pdf size_gradient_report_with_appendix.pdf

# Convenience aliases
paper:        size_gradient_report.pdf
appendix:     size_gradient_report_appendix.pdf
with-appendix: size_gradient_report_with_appendix.pdf

# --- 2022 Italian election extension (downloads ~50MB on first run) ----------
data_processed/italy/electoral_panel_extended.csv: code/italy/03_download_2022.R code/italy/04_build_2022.R code/italy/05_extend_panel.R
	Rscript code/italy/03_download_2022.R
	Rscript code/italy/04_build_2022.R
	Rscript code/italy/05_extend_panel.R

# --- Italy analysis: Tables 1, 2, 3, 6, 7, 10; placebo scatter --------------
output/tables/italy/tab_rd.tex output/figures/italy/placebo_scatter.pdf: code/italy/01_analysis_italy.R data_processed/italy/electoral_panel_extended.csv
	Rscript code/italy/01_analysis_italy.R

# --- Italy gradient (Figure 1) ----------------------------------------------
output/figures/italy/fig_logpop_facet.pdf: code/italy/02_figure_italy_gradient.R
	Rscript code/italy/02_figure_italy_gradient.R

# --- Bandwidth sweep (Figure 2, attenuation visualization) ------------------
output/figures/italy/fig_bandwidth_sweep.pdf: code/italy/06_figure_bandwidth_sweep.R
	Rscript code/italy/06_figure_bandwidth_sweep.R

# --- Placebo threshold sweep across TWFE/MTWFE/SDID -------------------------
output/figures/italy/fig_placebo_sweep.pdf: code/italy/07_figure_placebo_sweep.R
	Rscript code/italy/07_figure_placebo_sweep.R

# --- France analysis: Tables 4, 5, 9; Figures 2 & 6 (~200MB download) -------
output/tables/france/tab_narrowband_fr.tex output/figures/france/placebo_scatter_fr.pdf: code/france/run_all.R code/france/00_download.R code/france/01_parse_elections.R code/france/02_population.R code/france/03_harmonize_panel.R code/france/04_analysis.R code/france/05_figures.R
	Rscript code/france/run_all.R

# --- France bandwidth & placebo sweep figures -------------------------------
output/figures/france/fig_bandwidth_sweep_fr.pdf: code/france/06_figure_bandwidth_sweep_fr.R data_processed/france/final/panel_commune.csv
	Rscript code/france/06_figure_bandwidth_sweep_fr.R

output/figures/france/fig_placebo_sweep_fr.pdf: code/france/07_figure_placebo_sweep_fr.R data_processed/france/final/panel_commune.csv
	Rscript code/france/07_figure_placebo_sweep_fr.R

# --- Mechanism analysis: Table 8, Figures 3, 4, 5 ---------------------------
# Builds the post-2010 unione indicator and OpenCivitas crosswalk from the
# raw Ministry registry and OpenCivitas FC questionnaires in data_raw/italy/.
output/tables/italy/tab_compliance_three.tex output/figures/italy/fig_union_formation_did.pdf output/figures/italy/fig_compliance_gradient_no_rd.pdf output/figures/italy/fig_service_diff_delta_no_rd.pdf: code/italy_mechanism/run_all.R data_processed/italy/electoral_panel_extended.csv
	Rscript code/italy_mechanism/run_all.R

# --- Compile paper (two passes for cross-references) -------------------------
size_gradient_report.pdf: size_gradient_report.tex references.bib \
		output/tables/italy/tab_rd.tex \
		output/tables/italy/tab_compliance_three.tex \
		output/figures/italy/fig_logpop_facet.pdf \
		output/figures/italy/fig_bandwidth_sweep.pdf \
		output/figures/italy/fig_placebo_sweep.pdf \
		output/figures/italy/fig_union_formation_did.pdf \
		output/figures/italy/fig_compliance_gradient_no_rd.pdf \
		output/figures/italy/fig_service_diff_delta_no_rd.pdf \
		output/tables/france/tab_narrowband_fr.tex \
		output/figures/france/placebo_scatter_fr.pdf \
		output/figures/france/fig_logpop_facet_fr.pdf \
		output/figures/france/fig_bandwidth_sweep_fr.pdf \
		output/figures/france/fig_placebo_sweep_fr.pdf
	pdflatex size_gradient_report.tex
	bibtex size_gradient_report
	pdflatex size_gradient_report.tex
	pdflatex size_gradient_report.tex

# --- Standalone appendix (Online Supporting Information edition) -------------
size_gradient_report_appendix.pdf: size_gradient_report_appendix.tex references.bib \
		output/tables/france/tab_temporal_fr.tex \
		output/figures/france/placebo_scatter_fr.pdf \
		output/tables/italy/tab_compliance_rd.tex \
		output/tables/italy/tab_covariate_mediation.tex
	pdflatex size_gradient_report_appendix.tex
	bibtex size_gradient_report_appendix
	pdflatex size_gradient_report_appendix.tex
	pdflatex size_gradient_report_appendix.tex

# --- Combined edition (main paper + appendix as a single document) ----------
size_gradient_report_with_appendix.pdf: size_gradient_report_with_appendix.tex references.bib \
		output/tables/italy/tab_rd.tex \
		output/tables/italy/tab_compliance_three.tex \
		output/figures/italy/fig_logpop_facet.pdf \
		output/figures/italy/fig_bandwidth_sweep.pdf \
		output/figures/italy/fig_placebo_sweep.pdf \
		output/figures/italy/fig_union_formation_did.pdf \
		output/figures/italy/fig_compliance_gradient_no_rd.pdf \
		output/figures/italy/fig_service_diff_delta_no_rd.pdf \
		output/tables/france/tab_narrowband_fr.tex \
		output/tables/france/tab_temporal_fr.tex \
		output/figures/france/placebo_scatter_fr.pdf \
		output/figures/france/fig_logpop_facet_fr.pdf \
		output/figures/france/fig_bandwidth_sweep_fr.pdf \
		output/figures/france/fig_placebo_sweep_fr.pdf \
		output/tables/italy/tab_compliance_rd.tex \
		output/tables/italy/tab_covariate_mediation.tex
	pdflatex size_gradient_report_with_appendix.tex
	bibtex size_gradient_report_with_appendix
	pdflatex size_gradient_report_with_appendix.tex
	pdflatex size_gradient_report_with_appendix.tex

clean:
	rm -f size_gradient_report.{pdf,aux,log,out,synctex.gz,bbl,blg,fls,fdb_latexmk,toc}
	rm -f size_gradient_report_appendix.{pdf,aux,log,out,synctex.gz,bbl,blg,fls,fdb_latexmk,toc}
	rm -f size_gradient_report_with_appendix.{pdf,aux,log,out,synctex.gz,bbl,blg,fls,fdb_latexmk,toc}
	rm -f output/tables/italy/*.tex output/figures/italy/*.{pdf,png}
	rm -f output/tables/france/*.tex output/figures/france/*.{pdf,png}
	rm -f output/csvs/italy/*.csv output/csvs/france/*.csv
