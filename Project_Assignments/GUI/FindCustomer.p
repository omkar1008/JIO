
/*------------------------------------------------------------------------
    File        : FindCustomer.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Omkar.Halingali
    Created     : Fri Feb 18 16:04:12 IST 2022
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
    
DEFINE INPUT  PARAMETER iCustnum AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR tt-Customer.


findcustomer:
DO TRANSACTION ON ERROR  UNDO findcustomer , LEAVE findcustomer:    
    
    FIND FIRST customer WHERE Customer.Cust-Num = iCustnum NO-LOCK NO-ERROR.
    IF AVAILABLE customer THEN
    DO:
        CREATE tt-Customer.
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

