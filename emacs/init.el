(org-babel-load-file
 (expand-file-name
  "config.org"
  user-emacs-directory))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default))
 '(display-line-numbers-type 'relative)
 '(doc-view-continuous t)
 '(math-preview-mathjax-tags "all")
 '(org-agenda-files
   '("/home/tate/RoamNotes/20240101142739-functional_programming.org" "/home/tate/RoamNotes/20240101143744-referential_transparency.org" "/home/tate/RoamNotes/20240101144150-haskell.org" "/home/tate/RoamNotes/20240101144411-scala.org" "/home/tate/RoamNotes/20240101144847-category.org" "/home/tate/RoamNotes/20240101145034-category_theory.org" "/home/tate/RoamNotes/20240101145231-monoids.org" "/home/tate/RoamNotes/20240101145541-kleiski_catagories.org" "/home/tate/RoamNotes/20240101145733-monad.org" "/home/tate/RoamNotes/20240101150117-terminal_object.org" "/home/tate/RoamNotes/20240101161225-paper_variational_inference_a_review_for_statisticians.org" "/home/tate/RoamNotes/20240101161520-approximate_bayesian_computation.org" "/home/tate/RoamNotes/20240101161811-variational_inference.org" "/home/tate/RoamNotes/20240101161951-bayesian_statistics.org" "/home/tate/RoamNotes/20240101162053-markov_chain_monte_carlo.org" "/home/tate/RoamNotes/20240101162150-markov_chain.org" "/home/tate/RoamNotes/20240101162257-stationary_distribution.org" "/home/tate/RoamNotes/20240101163351-book_category_theory_for_programmers.org" "/home/tate/RoamNotes/20240101165410-apts.org" "/home/tate/RoamNotes/20240101165542-apts_statistical_inference.org" "/home/tate/RoamNotes/20240101165644-apts_statistical_computation.org" "/home/tate/RoamNotes/20240101170001-book_handbook_of_markov_chain_monte_carlo.org" "/home/tate/RoamNotes/20240101171833-apts_statistical_computation_pre_course_material.org" "/home/tate/RoamNotes/20240101172036-apts_statistical_computation_matrix_computation.org" "/home/tate/RoamNotes/20240101172303-apts_statistical_computation_optimisation.org" "/home/tate/RoamNotes/20240101172430-apts_statistical_computation_calculus_by_computer.org" "/home/tate/RoamNotes/20240101172629-apts_statistical_computation_random_number_generation.org" "/home/tate/RoamNotes/20240101172900-apts_statistical_computation_practical_1.org" "/home/tate/RoamNotes/20240101172935-apts_statistical_computation_practical_2.org" "/home/tate/RoamNotes/20240101173113-apts_statistical_inference_ideas_of_inference.org" "/home/tate/RoamNotes/20240101173234-apts_statistical_inference_the_likelihood_principle.org" "/home/tate/RoamNotes/20240101173352-apts_statistical_inference_statistical_decision_theory.org" "/home/tate/RoamNotes/20240101173507-apts_statistical_inference_likelihood_and_related_estimators.org" "/home/tate/RoamNotes/20240101173646-apts_statistical_inference_perspectives_on_inference.org" "/home/tate/RoamNotes/20240101174844-paper_surprising_convergence_properties_of_some_simple_gibbs_samplers_under_various_scans.org" "/home/tate/RoamNotes/20240101175338-the_gibbs_sampler.org" "/home/tate/RoamNotes/20240102122901-kullback_leibler_divergence.org" "/home/tate/RoamNotes/20240102123406-almost_everywhere.org" "/home/tate/RoamNotes/20240102125402-bayes_theorem.org" "/home/tate/RoamNotes/20240102130931-monte_carlo_methods.org" "/home/tate/RoamNotes/20240102131059-weighted_resampling.org" "/home/tate/RoamNotes/20240102132919-statistical_inference.org" "/home/tate/RoamNotes/20240102133054-frequentist_statistics.org" "/home/tate/RoamNotes/20240102134038-epistemic_uncertainty.org" "/home/tate/RoamNotes/20240102165252-reversibility.org" "/home/tate/RoamNotes/20240102165722-functionals.org" "/home/tate/RoamNotes/20240102171126-efficient_bayesian_modelling_of_infectious_diseases_in_wildlife_an_application_to_bovine_tuberculosis_in_badgers.org" "/home/tate/RoamNotes/20240102171259-hidden_markov_models.org" "/home/tate/RoamNotes/20240102172643-statistical_computation.org" "/home/tate/RoamNotes/20240104100939-latent_variables.org" "/home/tate/RoamNotes/20240106121412-coordinate_ascent_variational_inference.org" "/home/tate/RoamNotes/20240106135358-mrth.org" "/home/tate/RoamNotes/20240106142209-mrth_theorem.org" "/home/tate/RoamNotes/20240107112423-burn_in.org" "/home/tate/RoamNotes/20240109093757-lazy_lists.org" "/home/tate/RoamNotes/20240109094102-lazy.org" "/home/tate/RoamNotes/20240109095057-filter.org" "/home/tate/RoamNotes/20240109113208-thunks.org" "/home/tate/RoamNotes/20240109123626-bottom.org" "/home/tate/.config/emacs/config.org" "/home/tate/orgfiles/Supervisor_meetings/"))
 '(org-agenda-prefix-format
   '((agenda . " %i")
     (todo . " %i")
     (tags . " %i")
     (search . " %i")))
 '(org-babel-js-cmd "node")
 '(org-export-backends '(html latex md))
 '(org-latex-hyperref-template nil)
 '(org-latex-images-centered nil)
 '(org-modern-hide-stars "")
 '(org-modern-star '("*" "*" "*" "*" "*"))
 '(org-startup-with-latex-preview t)
 '(package-selected-packages '(edit-indirect)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
