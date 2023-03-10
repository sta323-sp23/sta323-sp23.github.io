#############################################
## install ghclass with artifact functions ##
#############################################
# remotes::install_github("rundel/ghclass@artifacts")

##############################
### to grab the html files ###
##############################
repos_of_interest = ghclass::org_repos("sta323-sp23", filter="lab-0")
ghclass::action_artifact_download(repos_of_interest, dir = "~/Desktop/sta323-test")

##############################
### to clean up artifacts ###
##############################
repos_of_interest = ghclass::org_repos("sta323-sp23", filter="lab-4")
ghclass::action_artifact_delete(repos_of_interest, ids=action_artifacts(repos, which="all"))