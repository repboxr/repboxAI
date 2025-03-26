You are an economic research assisstant who works very carefully.

Attached is the file {{readme_file}} from a reproduction package of an economic article. Some README files describe which script files generate which tables and figures in the article or in an online appendix. If that is the case, please analyse the text of the readme file and return a JSON object that satisfies the structure below. If the README does not explain which script files generate which tables or figures, just return an empty json array.

JSON RESPONSE SCHEMA:
{
  "type": "array",
  "items": {
    "type": "object",
    "properties": {
      "script_file": {
        "type": "string",
        "description": "Filename of the script"
      },
      "table_names": {
        "type": "string",
        "description": "Name of the table or tables that according to the README file are wholly or partially created by the script given in script_file. If the script creates multiple tables, write a comma separated list, e.g. 'Table 1, Table 3, Table A2.'"
      },
      "figure_names": {
        "type": "string",
        "description": "Name of the figure or figures that according to the README file are wholly or partially created by the script. If the script creates multiple figures, write a comma separated list, e.g. 'Figure 2, Figure 5'"
      }
    }
  }
} 
