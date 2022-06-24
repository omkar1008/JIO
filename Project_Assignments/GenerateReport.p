/*------------------------------------------------------------------------
    File        : GenerateReport.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Omkar.Halingali
    Created     : Fri Feb 04 09:42:36 IST 2022
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE STREAM iStream.
DEFINE STREAM iStream1.


DEFINE VARIABLE cLabel AS CHARACTER NO-UNDO .
DEFINE VARIABLE cValue AS CHARACTER NO-UNDO .

INPUT stream iStream from value("C:\OpenEdge\Reports\empdetails.csv").
INPUT stream iStream1 from value("C:\OpenEdge\Reports\empaddress.csv").

OUTPUT TO VALUE("C:\OpenEdge\Reports\StandardEmployeDetails.csv").

REPEAT:
    IMPORT STREAM istream UNFORMATTED cLabel.
    IMPORT STREAM istream1 UNFORMATTED  cValue.
    PUT  UNFORMATTED  cLabel  "," cValue SKIP.
END.

OUTPUT CLOSE.
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    