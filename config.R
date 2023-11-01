

############# file paths #############

#path to abcd Restricted Access folder
root = "~"
box_path = "Box Sync"
abcd_box_path = "2. Barzi Lab - Restricted Access/2-ABCD"
abcd_data_path = "ABCD data"
abcd_version = "4.0"

abcd_data = file.path(root, box_path, abcd_box_path, abcd_data_path )
abcd_files_path = file.path(abcd_data, abcd_version)
abcd_genetics_path = file.path(abcd_data, "genetics")
abcd_partition_path = file.path(abcd_data, "partition")


project_path = file.path(root, box_path, "3. Barzi Lab - Projects:Grants:Datasets etc./1-Projects/a-ABCD #844634/9-Projects/SA_ExWAS/preprocessing of data")
