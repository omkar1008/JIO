
/*------------------------------------------------------------------------
    File        : ReadNestedJson.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Omkar.Halingali
    Created     : Thu Jan 20 17:21:27 IST 2022
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

USING Progress.Json.ObjectModel.ObjectModelParser FROM PROPATH.
USING Progress.Json.ObjectModel.JsonConstruct FROM PROPATH.
USING Progress.Json.ObjectModel.JsonObject FROM PROPATH.
USING Progress.Json.ObjectModel.JsonArray FROM PROPATH.
USING OpenEdge.Core.String FROM PROPATH.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */
DEFINE STREAM istream.
DEFINE TEMP-TABLE tt-employee NO-UNDO
    FIELD FirstName    AS CHARACTER LABEL "FirstName"
    FIELD LastName     AS CHARACTER LABEL "LastName"
    FIELD Age          AS CHARACTER LABEL "Age"
    FIELD EmailID      AS CHARACTER LABEL "EmailId"
    FIELD AddressLine1 AS CHARACTER LABEL "AddressLine1"
    FIELD AddressLine2 AS CHARACTER LABEL "AddressLine2"
    FIELD AddressLine3 AS CHARACTER LABEL "AddressLine3"
    FIELD City         AS CHARACTER LABEL "City"
    FIELD Zip          AS CHARACTER LABEL "ZipCode"
    FIELD Country      AS CHARACTER LABEL "Country".
    
RUN ParseJsonObject.

PROCEDURE ParseJsonObject:
    /*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE tthandle         AS HANDLE            NO-UNDO.
    DEFINE VARIABLE cInputFile       AS CHARACTER         NO-UNDO.
    DEFINE VARIABLE cOutputFile      AS CHARACTER         NO-UNDO.
    DEFINE VARIABLE cDir             AS CHARACTER         NO-UNDO.
    DEFINE VARIABLE oParser          AS ObjectModelParser NO-UNDO.       
    DEFINE VARIABLE oJsonObject      AS JsonObject        NO-UNDO.   
    DEFINE VARIABLE oJsonNamesArray  AS JsonArray         NO-UNDO.
    DEFINE VARIABLE oJsonAddressobj  AS JsonObject        NO-UNDO.
    DEFINE VARIABLE oJsonName        AS JsonObject        NO-UNDO.
    DEFINE VARIABLE cLabel           AS CHARACTER         NO-UNDO. 
    DEFINE VARIABLE cSecLabel        AS CHARACTER         NO-UNDO. 
    DEFINE VARIABLE cFieldNames      AS CHARACTER         EXTENT NO-UNDO.
    DEFINE VARIABLE cSecFieldNames   AS CHARACTER         EXTENT NO-UNDO.
    DEFINE VARIABLE cfieldValue      AS CHARACTER         NO-UNDO.
    DEFINE VARIABLE cSecfieldValue   AS CHARACTER         NO-UNDO.
    DEFINE VARIABLE iCnt             AS INTEGER           NO-UNDO.
    DEFINE VARIABLE iOuterCnt        AS INTEGER           NO-UNDO.
    DEFINE VARIABLE cfinalFieldvalue AS CHARACTER         NO-UNDO.
    DEFINE VARIABLE lcjsonparsed     AS LONGCHAR          NO-UNDO.

    tthandle  =  TEMP-TABLE tt-employee:HANDLE.
    ASSIGN 
        cDir        = SESSION:TEMP-DIRECTORY
        cInputfile  = cDir + "JsonNestedInput.json"
        cOutputFile = cDir + "lcJsonNested.csv".
    oParser = NEW ObjectModelParser().
        
    oJsonObject = CAST(oParser:ParseFile(cInputFile), JsonObject).
    oJsonNamesArray  = oJsonObject:GetJsonArray("tt-employee").

    OUTPUT TO VALUE (cOutputFile).        
    DO iOuterCnt = 1 TO oJsonNamesArray:Length :            

        oJsonName = oJsonNamesArray:GetJsonObject(iOuterCnt).
         
        cFieldNames = oJsonName:GetNames(). 
        DO iCnt = 1 TO EXTENT(cFieldNames):  
            IF iOuterCnt = 1 AND iCnt < extent(cFieldNames) THEN  
              
                ASSIGN 

                    cLabel = IF cLabel = "" THEN  cFieldNames[iCnt]   ELSE  (SUBSTITUTE ("&1,&2",cLabel,cFieldNames[iCnt]) ).      
  
        END.
        IF iOuterCnt = 1 THEN
        DO iCnt = 1 TO 6: 
            IF iOuterCnt = 1 THEN
            DO:
                oJsonAddressobj = oJsonName:GetJsonObject("Address").
                cSecFieldNames = oJsonAddressobj:GetNames(). 
                ASSIGN    
                    cSecLabel = IF cSecLabel = "" THEN  cSecFieldNames[iCnt]   ELSE  (SUBSTITUTE ("&1,&2",cSecLabel,cSecFieldNames[iCnt]) ).      
            END.
        END.
        IF iOuterCnt = 1 THEN
        DO:
            ASSIGN  
                cLabel = cLabel + (IF cLabel  = "" THEN "" ELSE ",") + cSecLabel.
            PUT UNFORMATTED cLabel SKIP.    
        END.
        RUN ParseJsonAddressObject(INPUT oJsonName , oJsonAddressobj , OUTPUT cfieldValue).
    
        cfinalFieldvalue = cfinalFieldvalue + (IF cfinalFieldvalue = "" THEN  "" ELSE ",") + cfieldValue.
    
        PUT UNFORMATTED  cfieldValue SKIP.

    END.
    OUTPUT CLOSE.

    CATCH e AS Progress.Lang.Error:
        DEFINE VARIABLE stest AS String NO-UNDO.
        stest = NEW string("Error in ParseJsonObject ").
        stest:append(" ") .
        stest:append(e:GetMessage(1)).
        MESSAGE stest:ToString().
    END CATCH.
    
    FINALLY:
    
        IF VALID-OBJECT(oJsonName)     THEN DELETE OBJECT oJsonName.
        IF VALID-OBJECT( oJsonAddressobj) THEN DELETE OBJECT oJsonAddressobj.
        IF VALID-OBJECT(oParser)      THEN DELETE OBJECT oParser.
        IF VALID-OBJECT(oJsonObject)   THEN DELETE OBJECT oJsonObject.
        IF VALID-OBJECT(oJsonAddressobj)   THEN DELETE OBJECT oJsonAddressobj.
        IF VALID-OBJECT(oJsonNamesArray)   THEN DELETE OBJECT oJsonNamesArray.
    END FINALLY. 
    
END PROCEDURE. 

RUN ReadToTempTable.     
            
PROCEDURE ParseJsonAddressObject:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER oJsonName AS JsonObject NO-UNDO.
    DEFINE INPUT PARAMETER oJsonAddress AS JsonObject NO-UNDO.
    DEFINE OUTPUT PARAMETER cfieldValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFieldNames    AS CHARACTER NO-UNDO EXTENT.
    DEFINE VARIABLE cSecFieldNames AS CHARACTER NO-UNDO EXTENT.
    DEFINE VARIABLE iCnt           AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cSecfieldValue AS CHARACTER NO-UNDO.


    cFieldNames = oJsonName:GetNames().
    
    cSecFieldNames = oJsonAddress:GetNames().
               
    DO iCnt = 1 TO EXTENT(cFieldNames):
        IF iCnt < 5 THEN  
               
            ASSIGN
                cfieldValue = IF cfieldValue = "" THEN  SUBSTITUTE("&1",oJsonName:GetJsonText(cFieldNames[iCnt]))   ELSE (SUBSTITUTE("&1,&2",cfieldvalue,oJsonName:GetJsonText(cFieldNames[iCnt]))).
    END.
   
    DO iCnt = 1 TO EXTENT(cSecFieldNames):
     
        ASSIGN    
            cSecfieldValue = IF cSecfieldValue = "" THEN  SUBSTITUTE("&1",oJsonAddress:GetJsonText(cSecFieldNames[iCnt]))   ELSE (SUBSTITUTE("&1,&2",cSecfieldvalue,oJsonAddress:GetJsonText(cSecFieldNames[iCnt]))).
    END.
    cfieldValue = cfieldValue + cSecfieldValue.

  
    CATCH e AS Progress.Lang.Error:
        DEFINE VARIABLE stest AS String NO-UNDO.
        stest = NEW string("Error in ParseJsonAddressObject ").
        stest:append(" ") .
        stest:append(e:GetMessage(1)).
        MESSAGE stest:ToString().
    END CATCH.
    

END PROCEDURE.  
            
            
/*------------------------------------------------------------------------------------------------------------------------*/




PROCEDURE ReadToTempTable:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    DEFINE VARIABLE cInputfile AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDir       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCount     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cLabel     AS CHARACTER NO-UNDO INIT 1.


    ASSIGN 
        cDir       = SESSION:TEMP-DIRECTORY
  
        cInputfile = cDir + "lcJsonNested.csv".

    INPUT stream iStream from value( cInputFile).
    IMPORT STREAM istream DELIMITER "," cLabel.

    REPEAT: 

        CREATE tt-employee.
        IMPORT STREAM istream DELIMITER "," tt-employee.   
             
    END.

    FOR EACH tt-employee:
        DISP tt-employee.FirstName.
    
    END.


    CATCH e AS Progress.Lang.Error:
        DEFINE VARIABLE stest AS String NO-UNDO.
        stest = NEW string("Error in ReadToTempTable ").
        stest:append(" ") .
        stest:append(e:GetMessage(1)).
        MESSAGE stest:ToString().
    END CATCH.
    
END PROCEDURE.








/*------------------------------------------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------------------------------------------*/
/*------------------------------By using data Set-------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------------------------------------------*/



DEFINE TEMP-TABLE tt-customer NO-UNDO
    FIELD Cust-Num    AS INTEGER   LABEL "Cust-Num"
    FIELD Name        AS CHARACTER LABEL "Name"
    FIELD Phone       AS CHARACTER LABEL "Phone"
    FIELD Address     AS CHARACTER LABEL "Address"
    FIELD Address2    AS CHARACTER LABEL "Address2"
    FIELD City        AS CHARACTER LABEL "City"
    FIELD State       AS CHARACTER LABEL "State"
    FIELD Country     AS CHARACTER LABEL "Country"
    FIELD Postal-Code AS CHARACTER LABEL "Postal-Code"
    FIELD Sales-Rep   AS CHARACTER LABEL "Sales-Rep".
  

DEFINE TEMP-TABLE tt-order NO-UNDO
    FIELD Carrier      AS CHARACTER LABEL "Carrier"
    FIELD Cust-Num     AS INTEGER   LABEL "Cust-Num"
    FIELD Order-num    AS INTEGER   LABEL "Order-Num"
    FIELD Order-Date   AS DATE      LABEL "Order-Date"
    FIELD Promise-Date AS DATE      LABEL "Promise-Date"
    FIELD Sales-Rep    AS CHARACTER LABEL "Sales-Rep"
    FIELD Ship-Date    AS DATE      LABEL "Ship-Date".
    
    

DEFINE TEMP-TABLE tt-orderline NO-UNDO
    FIELD Order-num      AS INTEGER LABEL "Order-Num"
    FIELD Item-num       AS INTEGER LABEL "Item-Num"
    FIELD Line-num       AS INTEGER LABEL "Line-num"
    FIELD Qty            AS INTEGER LABEL "Qty"
    FIELD Price          AS DECIMAL LABEL "Price"
    FIELD Extended-Price AS DECIMAL LABEL "Extended-Price".
    
DEFINE DATASET dsCustOrderline FOR tt-customer, tt-order, tt-orderline
    DATA-RELATION drcustorder FOR tt-customer, tt-order
    RELATION-FIELDS (cust-num,cust-num) NESTED
    DATA-RELATION drorderOrderline FOR tt-order,tt-orderline
    RELATION-FIELDS (order-num,order-num) NESTED.
       
DEFINE VARIABLE cDir       AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cInputfile AS CHARACTER NO-UNDO.
DEFINE VARIABLE lgreadjson AS LOGICAL   NO-UNDO.
DEFINE VARIABLE hDataset   AS HANDLE    NO-UNDO.
        
ASSIGN 
    cDir       = SESSION:TEMP-DIRECTORY
    cInputfile = cDir + "ttcustomerorderorderline.csv".
hDataset = DATASET dsCustOrderline:HANDLE.
ASSIGN  
    lgreadjson = hDataset:READ-JSON("file", cInputfile, "EMPTY").
MESSAGE lgreadjson
    VIEW-AS ALERT-BOX.
            
            
            
            
            
            
            
            