# File containing directories to be used in project (should be loaded at
# beginning of all scripts)


base_path = repo_path

code_path = file.path(base_path,
                      'code')
lake_path = file.path(base_path,
                      'lake')

objects_path = file.path(code_path,
                         'objects')
util_path = file.path(code_path,
                      'utilities')

data_base = file.path(substr(getwd(),0,gregexpr(pattern = project_name ,
                                                getwd())[[1]][1]-2),"sports_data_storage/sports/mlb")
data_in = file.path(data_base,'pregame_lines')

hook_path = file.path(data_base,
                      'webhooks')

source(file.path(util_path,'general_utilities.r'))
