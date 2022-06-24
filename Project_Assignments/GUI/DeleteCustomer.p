
/*------------------------------------------------------------------------
    File        : DeleteCustomer.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Omkar.Halingali
    Created     : Sat Feb 19 09:25:13 IST 2022
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

DEFINE INPUT  PARAMETER iCust-num AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER cStatus AS CHARACTER NO-UNDO.

deletecustomer:
DO TRANSACTION ON ERROR  UNDO deletecustomer , LEAVE deletecustomer:    
    
    FIND FIRST customer WHERE Customer.Cust-Num = iCust-num EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE customer THEN
    DO:
        DELETE customer.
        cStatus = "Record Deleted Successfully..".
    END. 
    ELSE
    cStatus = "Record is not available or already deleted..".
    
END.
