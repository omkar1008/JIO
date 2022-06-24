
/*------------------------------------------------------------------------
    File        : JsonFileOperation.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Omkar.Halingali
    Created     : Thu Jan 20 10:03:28 IST 2022
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

USING Progress.Json.ObjectModel.JsonObject FROM PROPATH.
USING Progress.Json.ObjectModel.JsonArray FROM PROPATH.

/* ********************  Preprocessor Definitions  ******************** */
/* ***************************  Main Block  *************************** */
/* **********************  Internal Procedures  *********************** */

DEFINE STREAM istream.
DEFINE TEMP-TABLE tt-employee NO-UNDO
    FIELD Id           AS CHARACTER LABEL "EmpID" INITIAL ""
    FIELD FirstName    AS CHARACTER LABEL "FirstName" INITIAL "Omkar"
    FIELD LastName     AS CHARACTER LABEL "LastName" INITIAL "Halingali"
    FIELD Age          AS CHARACTER LABEL "Age" INITIAL "22"
    FIELD EmailID      AS CHARACTER LABEL "EmailId" INITIAL "om@gmail.com"
    FIELD AddressLine1 AS CHARACTER LABEL "AddressLine1" INITIAL "addrs1"
    FIELD AddressLine2 AS CHARACTER LABEL "AddressLine2" INITIAL "addrs2"
    FIELD AddressLine3 AS CHARACTER LABEL "AddressLine3" INITIAL ""
    FIELD City         AS CHARACTER LABEL "City" INITIAL "athani"
    FIELD Zip          AS CHARACTER LABEL "ZipCode" INITIAL "591230"
    FIELD Country      AS CHARACTER LABEL "Country" INITIAL "India"
    INDEX IndId Id.

DEFINE VARIABLE cInputfile AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDir       AS CHARACTER NO-UNDO.
DEFINE VARIABLE iCount     AS INTEGER   NO-UNDO.
DEFINE VARIABLE cLabel     AS CHARACTER NO-UNDO INIT 1.
DEFINE VARIABLE lgvalue AS LOGICAL NO-UNDO.


ASSIGN 
    cDir       = SESSION:TEMP-DIRECTORY
  
    cInputfile = cDir + "csv_input_tt.csv".

INPUT stream iStream from value( cInputFile).
IMPORT STREAM istream DELIMITER "," cLabel.

REPEAT: 

    CREATE tt-employee.
    IMPORT STREAM istream DELIMITER "," tt-employee.   
             
END.
DEFINE VARIABLE htt-employee AS HANDLE NO-UNDO.
htt-employee = BUFFER tt-employee:HANDLE.
   
FOR EACH tt-employee:
    DISP tt-employee.Id.       
END.
   
 lgvalue = htt-employee:WRITE-JSON( "file", "C:\OpenEdge\tt_to_Json.json",TRUE,?,TRUE,FALSE).
   
   MESSAGE lgvalue
   VIEW-AS ALERT-BOX.
   
   
/*RUN tt_to_Json.*/
/*RUN tt_To_Xml. */
    
    
PROCEDURE tt_to_Json:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE TableHandle            AS HANDLE     NO-UNDO.
    DEFINE VARIABLE cOutputFile            AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE hHandle                AS HANDLE     NO-UNDO.
    DEFINE VARIABLE objWriteToJsonArray    AS JsonArray  NO-UNDO.
    DEFINE VARIABLE objWriteToinnerJsonObj AS JsonObject NO-UNDO.
    DEFINE VARIABLE objWriteToOuterJsonObj AS JsonObject NO-UNDO.
    DEFINE VARIABLE Cinfo                  AS CHARACTER  NO-UNDO.
    
    cOutputFile = cDir + "tt_To_json.json".
    
    objWriteToJsonArray = NEW JsonArray().
    FOR EACH tt-employee:
        hHandle = BUFFER tt-employee:HANDLE.
       
        objWriteToinnerJsonObj  = NEW JsonObject().
        DO iCount = 1 TO hHandle:NUM-FIELDS :
            IF hHandle:BUFFER-FIELD(iCount):buffer-Value() <> "" THEN
            DO:
                Cinfo  = "yes". 
                objWriteToinnerJsonObj:ADD( hHandle:BUFFER-FIELD(iCount):Name, hHandle:BUFFER-FIELD(iCount):buffer-Value()).
            END.
        END. 
        IF    Cinfo  = "yes" THEN 
        DO:
            objWriteToJsonArray:Add(objWriteToinnerJsonObj).
            Cinfo  = "No".
        END.

    END.
    objWriteToOuterJsonObj =  NEW JsonObject().
    objWriteToOuterJsonObj:add("tt-employee",objWriteToJsonArray).
    objWriteToOuterJsonObj:WriteFile(cOutputFile, TRUE,"UTF-8").
   
    CATCH e AS Progress.Lang.Error:
        MESSAGE  e:GetMessage(1)    
            VIEW-AS ALERT-BOX.
    END CATCH.


    FINALLY:

    END FINALLY.

END PROCEDURE.

PROCEDURE tt_To_Xml:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE TableHandle AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cOutputFile AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hHandle     AS HANDLE    NO-UNDO.
    DEFINE VARIABLE Cinfo       AS CHARACTER NO-UNDO.
    
    cOutputFile = cDir + "tt_To_Xml.xml".
    
    
    DEFINE VARIABLE hDoc   AS HANDLE.
    DEFINE VARIABLE hRoot  AS HANDLE.
    DEFINE VARIABLE hRow   AS HANDLE.
    DEFINE VARIABLE hField AS HANDLE.
    DEFINE VARIABLE hText  AS HANDLE.
    DEFINE VARIABLE hBuf   AS HANDLE.
    DEFINE VARIABLE hDBFld AS HANDLE.
   
 
    CREATE X-DOCUMENT   hDoc.     
    CREATE X-NODEREF    hRoot.     
    CREATE X-NODEREF    hRow.      
    CREATE X-NODEREF    hField.  
    CREATE X-NODEREF    hText.
        
    hDoc:CREATE-NODE(hRoot,"tt-employee","ELEMENT").
    hDoc:APPEND-CHILD(hRoot).

    
    FOR EACH tt-employee :
        hHandle = BUFFER tt-employee:HANDLE.
        DO iCount = 1 TO  hHandle:NUM-FIELDS:
            IF hHandle:BUFFER-FIELD(iCount):buffer-Value() <> "" THEN
            DO:
                Cinfo = "yes".
            END.
        END.
        IF Cinfo = "yes" THEN
        DO:
            hDoc:CREATE-NODE(hRow,"Employee","ELEMENT").
            hRoot:APPEND-CHILD(hRow).  
            DO iCount = 1 TO  hHandle:NUM-FIELDS:
                IF hHandle:BUFFER-FIELD(iCount):buffer-Value() <> "" THEN
                DO:
                    hDoc:CREATE-NODE(hField,hHandle:BUFFER-FIELD(iCount):Name, "ELEMENT").
                    hRow:APPEND-CHILD(hField).
                    hDoc:CREATE-NODE(hText, "", "TEXT").
                    hText:NODE-VALUE = STRING( hHandle:BUFFER-FIELD(iCount):buffer-Value()).
                    hField:APPEND-CHILD(hText).
                END.
            END.
            Cinfo = "NO".
        END.
    END.
    hDoc:SAVE("file", cOutputFile).
        
    CATCH e AS Progress.Lang.Error:
        MESSAGE  e:GetMessage(1)    
            VIEW-AS ALERT-BOX.
    END CATCH.
    FINALLY:
        
    END FINALLY.
END PROCEDURE.    
    
    

