
/*------------------------------------------------------------------------
    File        : custom.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Omkar.Halingali
    Created     : Thu Apr 14 10:12:22 IST 2022
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

/*DEFINE TEMP-TABLE tt-customer NO-UNDO                             */
/*    FIELD Cust-Num    AS INTEGER   LABEL "Cust-Num"               */
/*    FIELD Name        AS CHARACTER LABEL "Name"                   */
/*    FIELD Phone       AS CHARACTER LABEL "Phone"                  */
/*    FIELD Address     AS CHARACTER LABEL "Address"                */
/*    FIELD Address2    AS CHARACTER LABEL "Address2"               */
/*    FIELD City        AS CHARACTER LABEL "City"                   */
/*    FIELD State       AS CHARACTER LABEL "State"                  */
/*    FIELD Country     AS CHARACTER LABEL "Country"                */
/*    FIELD Postal-Code AS CHARACTER LABEL "Postal-Code"            */
/*    FIELD Sales-Rep   AS CHARACTER LABEL "Sales-Rep"              */
/*    FIELD cDel        AS LOGICAL   LABEL "cDel"                   */
/*    INDEX indcust-num Cust-Num.                                   */
/*//DEFINE BUFFER ttbuffer FOR tt-customer.                         */
/*                                                                  */
/*//DEFINE INPUT  PARAMETER  BU   ttbuffer  BUFFER  FOR tt-customer.*/
/* DEFINE   PARAMETER BUFFER ttbuffer FOR tt-customer.              */
MESSAGE "omkar"
VIEW-AS ALERT-BOX WARNING.

















