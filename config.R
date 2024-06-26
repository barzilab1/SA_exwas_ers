

############# file paths #############

#path to abcd Restricted Access folder
root = "~"
box_path = "Library/CloudStorage/Box-Box"
abcd_box_path = "2. Barzi Lab - Restricted Access/2-ABCD"
abcd_data_path = "ABCD data"
abcd_version = "4.0"

abcd_data = file.path(root, box_path, abcd_box_path, abcd_data_path )
abcd_files_path = file.path(abcd_data, abcd_version)
abcd_genetics_path = file.path(abcd_data, "genetics")
abcd_partition_path = file.path(abcd_data, "partition")

abcd_additional_files_path = file.path(root, box_path, abcd_box_path, "Additional files" )

project_path = file.path(root, box_path, "3. Barzi Lab - Projects:Grants:Datasets etc./1-Projects/*multi datasets projects/SA_ExWAS/preprocessing of data")



