
/*------------------------------------------------------------------------
    File        : AddCustomer.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Omkar.Halingali
    Created     : Fri Feb 18 18:07:30 IST 2022
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
DEFINE VARIABLE iNextCustNum AS INTEGER NO-UNDO.
    
DEFINE OUTPUT  PARAMETER cStatus AS CHARACTER NO-UNDO.    
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR tt-Customer.




AddCustomer:
DO TRANSACTION ON ERROR  UNDO AddCustomer , LEAVE AddCustomer:    
 
    FIND tt-customer.
    IF AVAILABLE tt-Customer THEN
    DO:
       
        CREATE customer.
        ASSIGN
            iNextCustNum         = CURRENT-VALUE(Next-Cust-Num)
            Customer.Cust-Num    = iNextCustNum
            Customer.Name        = tt-customer.Name 
            Customer.Phone       = tt-customer.Phone  
            Customer.Address     = tt-customer.Address 
            Customer.Address2    = tt-customer.Address2   
            Customer.City        = tt-customer.City  
            Customer.State       = tt-customer.State 
            Customer.Country     = tt-customer.Country
            Customer.Postal-Code = tt-customer.Postal-Code 
            Customer.Sales-Rep   = tt-customer.Sales-Rep 
            cStatus              = "Record Adedd Successfully".
            
            MESSAGE iNextCustNum
            VIEW-AS ALERT-BOX.
        FIND FIRST customer WHERE customer.cust-num = iNextCustNum NO-LOCK.
        IF AVAILABLE customer THEN
        DO:
            ASSIGN
                tt-customer.Cust-Num    = Customer.Cust-Num
                tt-customer.Name        = Customer.Name
                tt-customer.Phone       = Customer.Phone
                tt-customer.Address     = Customer.Address
                tt-customer.Address2    = Customer.Address2
                tt-customer.City        = Customer.City
                tt-customer.State       = Customer.State
                tt-customer.Country     = Customer.Country
                tt-customer.Postal-Code = Customer.Postal-Code
                tt-customer.Sales-Rep   = Customer.Sales-Rep.
   
        END.
      
    END.
    ELSE
        cStatus  = "Adding Record Failed".
END.

























