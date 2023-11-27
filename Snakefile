import sys
import re
import os

file_exts = ['jpg','jpeg']
CC=[f for f in os.listdir('data/canopy-cover-pictures/') if f.lower().split('.')[-1] in file_exts]
print(CC)

wildcard_constraints:
    file=".*(JPG|JPEG|jpg|jpeg){1}",

rule all:
    input:
        "data/temp.rds",
        "data/cc.rds",
        # "data/growth.rds",

rule make_temp_data:
    input:
        "code/make_temp_data.R",
    output:
        "data/temp.rds",
        "data/wmean.rds",
    conda:
        "~/miniconda3/envs/nestbox",
    resources:
        runtime="20m"
    shell:
        "Rscript code/make_temp_data.R"

rule canopy_cover:
    input:
        expand("data/canopy-cover-pictures/{file}",file=CC),
        "code/canopy_cover.R",
    output:
        "data/canopy-cover-pictures/{file}.rds",
    conda:
        "~/miniconda3/envs/nestbox",
    resources:
        runtime="20m",
    shell:
        "Rscript code/canopy_cover.R {wildcards.file}"

rule cat_canopy_cover:
    input:
        expand("data/canopy-cover-pictures/{file}.rds", file=CC),
        "code/cat_canopy_cover.R",
    output:
        "data/cc.rds",
    conda:
        "~/miniconda3/envs/nestbox",
    resources:
        runtime="20m",
    shell:
        "Rscript code/cat_canopy_cover.R"

# rule make_growth_data:
#     input:
#         "data/temp.rds",
#         "data/cc.rds",
#         "code/make_growth_data.R",
#     output:
#         "data/growth.rds",
#     conda:
        "~/miniconda3/envs/nestbox",
#     resources:
#         runtime="2h",
#     shell:
#         "Rscript code/make_growth_data.R"
