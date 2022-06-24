
/*------------------------------------------------------------------------
    File        : AddUpdateDelete.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Omkar.Halingali
    Created     : Mon Feb 21 17:20:32 IST 2022
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
    FIELD cDel        AS LOGICAL   LABEL "cDel"
    INDEX indcust-num Cust-Num.
    


DEFINE INPUT  PARAMETER iCustnum AS INTEGER NO-UNDO.
DEFINE  INPUT-OUTPUT PARAMETER TABLE FOR tt-Customer.
DEFINE INPUT-OUTPUT  PARAMETER cInfo AS CHARACTER NO-UNDO.
DEFINE VARIABLE lgStatus AS LOGICAL NO-UNDO.
 
DEFINE BUFFER custbuffer FOR customer.
DEFINE BUFFER ttbuffer   FOR tt-customer.
 
IF cInfo = "Add Customer" THEN
DO:
    
    DEFINE VARIABLE iNextCustNum AS INTEGER NO-UNDO.

    FIND tt-customer.
    IF AVAILABLE tt-Customer THEN
    DO:
        AddCustomer:
        DO TRANSACTION ON ERROR  UNDO AddCustomer , LEAVE AddCustomer:  
            DO FOR customer: 
                CREATE custbuffer.
                ASSIGN
                    iNextCustNum           = CURRENT-VALUE(Next-Cust-Num)
                    custbuffer.Cust-Num    = iNextCustNum
                    custbuffer.Name        = tt-customer.Name 
                    custbuffer.Phone       = tt-customer.Phone  
                    custbuffer.Address     = tt-customer.Address 
                    custbuffer.Address2    = tt-customer.Address2   
                    custbuffer.City        = tt-customer.City  
                    custbuffer.State       = tt-customer.State 
                    custbuffer.Country     = tt-customer.Country
                    custbuffer.Postal-Code = tt-customer.Postal-Code 
                    custbuffer.Sales-Rep   = tt-customer.Sales-Rep 
                    cInfo                  = "Record Adedd Successfully".
                TEMP-TABLE tt-customer:EMPTY-TEMP-TABLE ().
                FIND FIRST custbuffer WHERE custbuffer.cust-num = iNextCustNum NO-LOCK.
                IF AVAILABLE custbuffer THEN
                DO:
                    CREATE ttbuffer.
                    BUFFER-COPY custbuffer  TO ttbuffer. 
                    RELEASE custbuffer.
                END.
                ELSE
                    cInfo  = "Adding Record Failed".
            END.
        END.
    END.
END .

IF  cInfo = "Update Customer" THEN  
DO:
    
    FIND FIRST ttbuffer WHERE ttbuffer.Cust-Num = iCustnum NO-LOCK NO-ERROR.
    IF NOT AVAILABLE ttbuffer THEN 
    DO:
        MESSAGE iCustnum
        VIEW-AS ALERT-BOX.
        cInfo = "Empty  TT".
    END.
    ELSE
    DO:

        UpdateCustomer:
        DO TRANSACTION ON ERROR  UNDO UpdateCustomer , LEAVE UpdateCustomer: 
            DO FOR customer:
                FIND FIRST custbuffer  WHERE custbuffer.Cust-Num = ttbuffer.Cust-Num EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE custbuffer THEN
                DO:
                    lgStatus =  BUFFER ttbuffer:BUFFER-COMPARE(BUFFER custbuffer:HANDLE)  NO-ERROR.
                    IF  lgStatus = logical ("No") THEN 
                    DO:
                        BUFFER-COPY ttbuffer  TO custbuffer. 
                        ASSIGN
                            cInfo = "Updated Successfully".
                        RELEASE custbuffer.
                        RELEASE ttbuffer.
                    END.
                    ELSE 
                        cInfo = "No Value Updated".
                END.
                ELSE
                    cInfo = "Update Failure".
            END.
        END.
    END .
END.
IF  cInfo = "Delete Customer" THEN
DO:
    deletecustomer:
    DO TRANSACTION ON ERROR  UNDO deletecustomer , LEAVE deletecustomer:   
        DO FOR customer:
            FIND FIRST custbuffer WHERE custbuffer.Cust-Num = iCustnum EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE custbuffer THEN
            DO:
                custbuffer.cDel = YES.
                cInfo = "Record Deleted Successfully..".
                RELEASE custbuffer.
       
            END.
            ELSE
                cInfo = "Record is not available or already deleted..".
        END.
    END.
END.

IF  cInfo = "Un-Delete Customer" THEN
DO:
   
    Un-deletecustomer:
    DO TRANSACTION ON ERROR  UNDO Un-deletecustomer , LEAVE Un-deletecustomer: 
        DO FOR customer:  
            FIND FIRST custbuffer WHERE custbuffer.Cust-Num = iCustnum EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE custbuffer THEN
            DO:
                custbuffer.cDel = NO.
                cInfo = "Record Un-Deleted Successfully..".
                RELEASE custbuffer.
       
            END.
            ELSE
                cInfo = "Record is not available or already Un-deleted..".
        END.
    END.
END.










































/*
add-----
1 define transaction scope for buffer
2.check the availability in database ( use buffer) no-lock
3. if record is available then return
4. create a record create buffer then buffer copy from tt to db buffer
5.release the buffer



update & delete
1.-----
2.------ (exclusive -lock no-wait) 
3. check the record is locked 
4. check the availability
5.if cinfo is update then update the all the field the except cust-num
6.if cinfo is delete then update the delete column
7.release the buffer


*/



















