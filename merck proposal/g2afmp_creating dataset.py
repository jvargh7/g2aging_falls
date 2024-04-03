
import openpyxl

# Load the workbook
path_merck_proposal = 'C:/Cloud/OneDrive - Emory University/Proposals/Merck Atherosclerosis and Diabetes'

def load_cosmos_excel(file_name):
    #wb = openpyxl.load_workbook(path_merck_proposal + '/working/' + file_name + '.xlsx', 
    #                            data_only=True,Password = 'Cosmos@2023')

    wb = openpyxl.load_workbook(path_merck_proposal + '/working/' + file_name + '.xlsx')
    sheet = wb.active
    return sheet

load_cosmos_excel('Test')
#load_cosmos_excel('Alabama Bottom 67_Cosmos@2023')