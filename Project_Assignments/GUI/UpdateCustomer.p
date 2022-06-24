
/*------------------------------------------------------------------------
    File        : UpdateCustomer.p
    Purpose     : 
    Syntax      :
    Description : 
    Author(s)   : Omkar.Halingali
    Created     : Fri Feb 18 17:26:14 IST 2022
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
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
    FIELD Sales-Rep   AS CHARACTER LABEL "Sales-Rep"
    INDEX indcust-num Cust-Num.
DEFINE OUTPUT  PARAMETER cStatus AS CHARACTER NO-UNDO.    
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR tt-Customer.




UpdateCustomer:
DO TRANSACTION ON ERROR  UNDO UpdateCustomer , LEAVE UpdateCustomer:    
    
    FIND tt-customer NO-ERROR.
    FIND FIRST customer WHERE Customer.Cust-Num = tt-customer.Cust-Num EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE customer THEN
    DO:
   
        ASSIGN
            Customer.Name        = tt-customer.Name 
            Customer.Phone       = tt-customer.Phone  
            Customer.Address     = tt-customer.Address 
            Customer.Address2    = tt-customer.Address2   
            Customer.City        = tt-customer.City  
            Customer.State       = tt-customer.State 
            Customer.Country     = tt-customer.Country
            Customer.Postal-Code = tt-customer.Postal-Code 
            Customer.Sales-Rep   = tt-customer.Sales-Rep 
            cStatus              = "Updated Successfully".
    END. 
    ELSE
        cStatus = "Update Failure".
    

END.















