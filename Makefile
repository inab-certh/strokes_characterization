.PHONY:
prepare_data :
	code/run_cohort_generation.R && code/run_covariate_data_extraction.R && Rscript code/create_custom_age_groups.R
covariate_data :
	Rscript code/run_covariate_data_extraction.R
characterizations : 
	Rscript code/run_characterization.R short_term &&\
	Rscript code/run_characterization.R medium_term &&\
	Rscript code/run_characterization.R any_time_prior &&\
	Rscript code/run_characterization.R prior_stroke
prepare_shiny :
	rm -rf shiny/data/* && code/move_results_to_shiny.R
clean_shiny : 
	rm -rf shiny/data/*
clean_results :
	rm -rf results/*
clean_data :
	rm -rf shiny/data/* && rm -rf results/* && rm -rf data/*
all :
	code/run_cohort_generation.R &&	code/run_covariate_data_extraction.R &&	make characterizations && make prepare_shiny

