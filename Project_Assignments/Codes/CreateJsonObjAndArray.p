
/*------------------------------------------------------------------------
    File        : CreateJsonObjAndArray.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Omkar.Halingali
    Created     : Fri Jan 21 13:44:15 IST 2022
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

USING Progress.Json.ObjectModel.JsonObject FROM PROPATH.
USING OpenEdge.Core.String FROM PROPATH.
USING Progress.Json.ObjectModel.JsonArray FROM PROPATH.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

DEFINE STREAM iStream .

RUN singlejsonobj.


/* **********************  Internal Procedures  *********************** */


PROCEDURE singlejsonobj:
    /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cJsonData         AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE iCnt              AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iLineCnt          AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cData             AS CHARACTER  NO-UNDO EXTENT 11.      
    DEFINE VARIABLE cLabel            AS CHARACTER  NO-UNDO EXTENT 11.
    DEFINE VARIABLE objWriteToJsonObj AS JsonObject NO-UNDO.
    DEFINE VARIABLE cDir              AS CHARACTER  NO-UNDO. 
    DEFINE VARIABLE cInputfile        AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cOutputFile       AS CHARACTER  NO-UNDO.
        
    ASSIGN 
        cDir        = SESSION:TEMP-DIRECTORY
        cInputfile  = cDir + "csv_input_tt.csv"
        cOutputFile = cDir + "csv_converted_jsonobj.json".
   

    IF   cInputFile = "" OR cInputFile = ?  THEN
    DO:
        UNDO, THROW NEW Progress.Lang.AppError("Input file has not been provided", 1).
    END.

    INPUT STREAM iStream FROM VALUE(cInputFile).

    REPEAT:

        IMPORT STREAM iStream UNFORMATTED cJsonData. 
    
        iLineCnt = iLineCnt + 1.
    
        IF iLineCnt > 1 THEN
            objWriteToJsonObj = NEW JsonObject().              
                
        DO iCnt = 1 TO NUM-ENTRIES(cJsonData,","):

            IF iLineCnt = 1 THEN                                   
                cLabel[iCnt] = ENTRY(iCnt,cJsonData,",").
            ELSE
            DO:
                cData[iCnt] = ENTRY(icnt,cJsonData,",").
                objWriteToJsonObj:Add(cLabel[iCnt],cData[iCnt]). 
            END.
        END.
    END. 
    objWriteToJsonObj:WriteFile(cOutputFile, TRUE,"UTF-8"). 
    INPUT STREAM iStream CLOSE.

    CATCH e AS Progress.Lang.Error:
        DEFINE VARIABLE stest AS String NO-UNDO.
        stest = NEW string("Error in singlejsonobj ").
        stest:append(" ") .
        stest:append(e:GetMessage(1)).
        MESSAGE stest:ToString().
    END CATCH.
    
    FINALLY:
        IF VALID-OBJECT(objWriteToJsonObj)     THEN DELETE OBJECT objWriteToJsonObj.
    END FINALLY. 
END PROCEDURE.




RUN JsonObjArray.

PROCEDURE JsonObjArray:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cJsonData        AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE iCnt             AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iLineCnt         AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cData            AS CHARACTER  NO-UNDO EXTENT 11.      
    DEFINE VARIABLE cLabel           AS CHARACTER  NO-UNDO EXTENT 11.
    DEFINE VARIABLE iCount           AS INTEGER    NO-UNDO.
    DEFINE VARIABLE outerObj         AS JsonObject NO-UNDO.
    DEFINE VARIABLE oNameObject      AS JsonObject NO-UNDO.
    DEFINE VARIABLE oJsonArrayObject AS JsonArray  NO-UNDO.
    DEFINE VARIABLE cDir             AS CHARACTER  NO-UNDO. 
    DEFINE VARIABLE cInputfile       AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cOutputFile      AS CHARACTER  NO-UNDO.   

    ASSIGN 
        cDir        = SESSION:TEMP-DIRECTORY
        cInputfile  = cDir + "csv_input_tt.csv"
        cOutputFile = cDir + "csv_converted_jsonArrayobj.json".
        
    oJsonArrayObject = NEW JsonArray ().
        
    IF   cInputFile = "" OR cInputFile = ?  THEN
    DO:
        UNDO, THROW NEW Progress.Lang.AppError("Input file has not been provided", 1).
    END.
        
    INPUT STREAM iStream FROM VALUE(cInputFile).       
        
    REPEAT:
        IMPORT STREAM iStream UNFORMATTED cJsonData.       

        iLineCnt = iLineCnt + 1. 
            
        IF iLineCnt > 1 THEN
            oNameObject = NEW JsonObject(). 
                              
        DO iCnt = 1 TO NUM-ENTRIES(cJsonData,","):                             
            IF iLineCnt = 1 THEN  
                cLabel[iCnt] = ENTRY(iCnt,cJsonData,",").
            ELSE
            DO:  
                cData[iCnt] = ENTRY(icnt,cJsonData,",").                                   
                oNameObject:add(cLabel[iCnt],cData[iCnt]).                    
            END.
        END.
            
        IF iLineCnt > 1 THEN
            oJsonArrayObject:ADD (oNameObject).
    END.

    INPUT STREAM iStream CLOSE.
        
    outerObj = NEW JsonObject ().
    outerObj:add("tt-Employee",OJsonArrayObject).
        
    OuterObj:WriteFile(cOutputFile, TRUE,"UTF-8").

    CATCH e AS Progress.Lang.Error:
        DEFINE VARIABLE stest AS String NO-UNDO.
        stest = NEW string("Error in JsonObjArray ").
        stest:append(" ") .
        stest:append(e:GetMessage(1)).
        MESSAGE stest:ToString().
    END CATCH.
    
    FINALLY:
        IF VALID-OBJECT(outerObj)     THEN DELETE OBJECT outerObj.
        IF VALID-OBJECT(oNameObject)     THEN DELETE OBJECT oNameObject.
        IF VALID-OBJECT(oJsonArrayObject)     THEN DELETE OBJECT oJsonArrayObject.
    END FINALLY. 

END PROCEDURE.

