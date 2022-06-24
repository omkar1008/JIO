
/*------------------------------------------------------------------------
    File        : custom.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Omkar.Halingali
    Created     : Mon Jan 31 13:32:15 IST 2022
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE STREAM iStream.
DEFINE STREAM iStream1.


/*DEFINE VARIABLE cLabel AS CHARACTER NO-UNDO .                          */
/*DEFINE VARIABLE cValue AS CHARACTER NO-UNDO .                          */
/*                                                                       */
/*INPUT stream iStream from value("C:\OpenEdge\Reports\empdetails.csv"). */
/*INPUT stream iStream1 from value("C:\OpenEdge\Reports\empaddress.csv").*/
/*                                                                       */
/*OUTPUT TO VALUE("C:\OpenEdge\Reports\StandardEmployeDetails.csv").     */
/*                                                                       */
/*REPEAT:                                                                */
/*    IMPORT STREAM istream UNFORMATTED cLabel.                          */
/*    IMPORT STREAM istream1 UNFORMATTED  cValue.                        */
/*    PUT  UNFORMATTED  cLabel  "," cValue SKIP.                         */
/*END.                                                                   */
/*                                                                       */
/*OUTPUT CLOSE.                                                          */


/*DEFINE VARIABLE cLabel AS CHARACTER NO-UNDO .                                        */
/*DEFINE VARIABLE cValue AS CHARACTER NO-UNDO .                                        */
/*                                                                                     */
/*INPUT stream iStream from value("C:\OpenEdge\Reports1\StandardEmployeDetails.csv").  */
/*INPUT stream iStream1 from value("C:\OpenEdge\Reports1\StandardEmployeDetails1.csv").*/
/*                                                                                     */
/*OUTPUT to  value ("C:\OpenEdge\Reports1\StandardEmployeDetailsop.csv").              */
/*IMPORT STREAM istream UNFORMATTED cLabel.                                            */
/*IMPORT STREAM istream1 UNFORMATTED  cValue.                                          */
/*PUT  UNFORMATTED  cLabel SKIP.                                                       */
/*REPEAT:                                                                              */
/*    IMPORT STREAM istream UNFORMATTED cLabel.                                        */
/*                                                                                     */
/*    PUT  UNFORMATTED  cLabel SKIP .                                                  */
/*END.                                                                                 */
/*                                                                                     */
/*REPEAT:                                                                              */
/*    IMPORT STREAM istream1 UNFORMATTED  cValue.                                      */
/*    PUT  UNFORMATTED  cValue SKIP.                                                   */
/*END.                                                                                 */
/*                                                                                     */
/*OUTPUT CLOSE.                                                                        */
   
   

DEFINE VARIABLE cLabel AS CHARACTER NO-UNDO .
DEFINE VARIABLE cValue AS CHARACTER NO-UNDO .

INPUT stream iStream from value("C:\OpenEdge\Report2\empdetails.csv").
INPUT stream iStream1 from value("C:\OpenEdge\Report2\StandardEmployeDetails.csv").

OUTPUT to  value ("C:\OpenEdge\Report2\StandardEmployeDetailsop.csv").
IMPORT STREAM istream1 UNFORMATTED  cValue.
IMPORT STREAM istream UNFORMATTED cLabel.
PUT  UNFORMATTED  cValue SKIP.
REPEAT:
    IMPORT STREAM istream UNFORMATTED cLabel.

    PUT  UNFORMATTED  cLabel SKIP .
END.

REPEAT:
    IMPORT STREAM istream1 UNFORMATTED  cValue.
    PUT  UNFORMATTED  cValue SKIP.
END.

OUTPUT CLOSE.
    