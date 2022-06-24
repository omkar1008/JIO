&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME wWin
{adecomm/appserv.i}
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  INPUT-OUTPUT Parameters:
      <none>

  History: New V9 Version - January 15, 1998
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AB.              */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */
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
DEFINE BUFFER ttbuffer FOR tt-customer.

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */



/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{src/adm2/widgetprto.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain
&Scoped-define BROWSE-NAME BROWSE-customer

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-customer

/* Definitions for BROWSE BROWSE-customer                               */
&Scoped-define FIELDS-IN-QUERY-BROWSE-customer tt-customer.Cust-Num tt-customer.Name tt-customer.Phone tt-customer.Address tt-customer.Address2 tt-customer.City tt-customer.State tt-customer.Country tt-customer.Postal-Code tt-customer.Sales-Rep tt-customer.cDel   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-customer ALL   
&Scoped-define SELF-NAME BROWSE-customer
&Scoped-define QUERY-STRING-BROWSE-customer FOR EACH tt-customer
&Scoped-define OPEN-QUERY-BROWSE-customer OPEN QUERY {&SELF-NAME} FOR EACH tt-customer.
&Scoped-define TABLES-IN-QUERY-BROWSE-customer tt-customer
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-customer tt-customer


/* Definitions for FRAME fMain                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-fMain ~
    ~{&OPEN-QUERY-BROWSE-customer}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 FILL-IN-custnum ~
TOGGLE-Inactive BUTTON-Search BUTTON-cancel FILL-IN-Name FILL-IN-Phone ~
FILL-IN-Sales-Rep FILL-IN-Address1 FILL-IN-Address2 FILL-IN-City ~
FILL-IN-State FILL-IN-Country FILL-IN-Postal-Code BUTTON-add BUTTON-update ~
BUTTON-DELETE TOGGLE-update-tag BROWSE-customer 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-custnum TOGGLE-Inactive ~
FILL-IN-Name FILL-IN-Phone FILL-IN-Sales-Rep FILL-IN-Address1 ~
FILL-IN-Address2 FILL-IN-City FILL-IN-State FILL-IN-Country ~
FILL-IN-Postal-Code TOGGLE-update-tag 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-add 
    LABEL "ADD" 
    SIZE 12 BY 1.

DEFINE BUTTON BUTTON-cancel 
    LABEL "Cancel" 
    SIZE 12 BY 1.

DEFINE BUTTON BUTTON-DELETE 
    LABEL "DELETE" 
    SIZE 12 BY 1.

DEFINE BUTTON BUTTON-Search 
    LABEL "Search" 
    SIZE 12 BY 1.

DEFINE BUTTON BUTTON-update 
    LABEL "UPDATE" 
    SIZE 12 BY 1.

DEFINE VARIABLE FILL-IN-Address1    AS CHARACTER FORMAT "X(256)":U 
    LABEL "Address1" 
    VIEW-AS FILL-IN 
    SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-Address2    AS CHARACTER FORMAT "X(256)":U 
    LABEL "Address2" 
    VIEW-AS FILL-IN 
    SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-City        AS CHARACTER FORMAT "X(256)":U 
    LABEL "City" 
    VIEW-AS FILL-IN 
    SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-Country     AS CHARACTER FORMAT "X(256)":U 
    LABEL "Country" 
    VIEW-AS FILL-IN 
    SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-custnum     AS CHARACTER FORMAT "X(256)":U 
    LABEL "CustomerNo" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-Name        AS CHARACTER FORMAT "X(256)":U 
    LABEL "Name" 
    VIEW-AS FILL-IN 
    SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-Phone       AS CHARACTER FORMAT "X(256)":U 
    LABEL "Phone" 
    VIEW-AS FILL-IN 
    SIZE 14 BY .95 NO-UNDO.

DEFINE VARIABLE FILL-IN-Postal-Code AS CHARACTER FORMAT "X(256)":U 
    LABEL "Postal-Code" 
    VIEW-AS FILL-IN 
    SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-Sales-Rep   AS CHARACTER FORMAT "X(256)":U 
    LABEL "Sales-Rep" 
    VIEW-AS FILL-IN 
    SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-State       AS CHARACTER FORMAT "X(256)":U 
    LABEL "State" 
    VIEW-AS FILL-IN 
    SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 87 BY 14.52.

DEFINE RECTANGLE RECT-2
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 87 BY 6.19.

DEFINE VARIABLE TOGGLE-Inactive   AS LOGICAL INITIAL NO 
    LABEL "InActive" 
    VIEW-AS TOGGLE-BOX
    SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-update-tag AS LOGICAL INITIAL NO 
    LABEL "Update-Mode" 
    VIEW-AS TOGGLE-BOX
    SIZE 18 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-customer FOR 
    tt-customer SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-customer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-customer wWin _FREEFORM
    QUERY BROWSE-customer DISPLAY
    tt-customer.Cust-Num
    tt-customer.Name       
    tt-customer.Phone       
    tt-customer.Address     
    tt-customer.Address2    
    tt-customer.City      VIEW-AS COMBO-BOX 
    tt-customer.State       
    tt-customer.Country     
    tt-customer.Postal-Code 
    tt-customer.Sales-Rep
    tt-customer.cDel VIEW-AS TOGGLE-BOX
  ENABLE ALL
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 83 BY 5 ROW-HEIGHT-CHARS .81 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
    FILL-IN-custnum AT ROW 1.95 COL 15 COLON-ALIGNED WIDGET-ID 2
    TOGGLE-Inactive AT ROW 1.95 COL 37 WIDGET-ID 40
    BUTTON-Search AT ROW 1.95 COL 54 WIDGET-ID 24
    BUTTON-cancel AT ROW 1.95 COL 68 WIDGET-ID 26
    FILL-IN-Name AT ROW 4.33 COL 14 COLON-ALIGNED WIDGET-ID 6
    FILL-IN-Phone AT ROW 4.33 COL 41 COLON-ALIGNED WIDGET-ID 8
    FILL-IN-Sales-Rep AT ROW 4.33 COL 70 COLON-ALIGNED WIDGET-ID 20
    FILL-IN-Address1 AT ROW 5.52 COL 14 COLON-ALIGNED WIDGET-ID 10
    FILL-IN-Address2 AT ROW 5.52 COL 41 COLON-ALIGNED WIDGET-ID 12
    FILL-IN-City AT ROW 5.52 COL 70 COLON-ALIGNED WIDGET-ID 18
    FILL-IN-State AT ROW 6.71 COL 14 COLON-ALIGNED WIDGET-ID 14
    FILL-IN-Country AT ROW 6.71 COL 41 COLON-ALIGNED WIDGET-ID 16
    FILL-IN-Postal-Code AT ROW 6.71 COL 70 COLON-ALIGNED WIDGET-ID 22
    BUTTON-add AT ROW 8.14 COL 27 WIDGET-ID 28
    BUTTON-update AT ROW 8.14 COL 41 WIDGET-ID 30
    BUTTON-DELETE AT ROW 8.14 COL 55 WIDGET-ID 32
    TOGGLE-update-tag AT ROW 9.57 COL 5 WIDGET-ID 42
    BROWSE-customer AT ROW 10.29 COL 5 WIDGET-ID 200
    RECT-1 AT ROW 1.48 COL 3 WIDGET-ID 34
    RECT-2 AT ROW 3.38 COL 3 WIDGET-ID 36
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 89 BY 15 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Other Settings: APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
    CREATE WINDOW wWin ASSIGN
        HIDDEN             = YES
        TITLE              = "<insert SmartWindow title>"
        HEIGHT             = 15
        WIDTH              = 89
        MAX-HEIGHT         = 28.81
        MAX-WIDTH          = 177.8
        VIRTUAL-HEIGHT     = 28.81
        VIRTUAL-WIDTH      = 177.8
        RESIZE             = NO
        SCROLL-BARS        = NO
        STATUS-AREA        = NO
        BGCOLOR            = ?
        FGCOLOR            = ?
        THREE-D            = YES
        MESSAGE-AREA       = NO
        SENSITIVE          = YES.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWin 
/* ************************* Included-Libraries *********************** */

    {src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMain
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-customer TOGGLE-update-tag fMain */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
    THEN wWin:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-customer
/* Query rebuild information for BROWSE BROWSE-customer
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-customer.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-customer */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* <insert SmartWindow title> */
    OR ENDKEY OF {&WINDOW-NAME} ANYWHERE 
    DO:
        /* This case occurs when the user presses the "Esc" key.
           In a persistently run window, just ignore this.  If we did not, the
           application would exit. */
        IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* <insert SmartWindow title> */
    DO:
        /* This ADM code must be left here in order for the SmartWindow
           and its descendents to terminate properly on exit. */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-customer
&Scoped-define SELF-NAME BROWSE-customer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-customer wWin
ON MOUSE-SELECT-DBLCLICK OF BROWSE-customer IN FRAME fMain
    DO:
 
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-customer wWin
ON ROW-DISPLAY OF BROWSE-customer IN FRAME fMain
    DO:
        IF AVAILABLE tt-customer AND tt-customer.cDel = TRUE THEN 
            ASSIGN   
                tt-customer.Cust-Num:fgcolor IN BROWSE BROWSE-customer    = 12                                                   
                tt-customer.Name:fgcolor IN BROWSE BROWSE-customer        = 12
                tt-customer.Phone:fgcolor IN BROWSE BROWSE-customer       = 12
                tt-customer.Address:fgcolor IN BROWSE BROWSE-customer     = 12
                tt-customer.Address2:fgcolor IN BROWSE BROWSE-customer    = 12
                tt-customer.City:fgcolor IN BROWSE BROWSE-customer        = 12
                tt-customer.State:fgcolor IN BROWSE BROWSE-customer       = 12
                tt-customer.Country:fgcolor IN BROWSE BROWSE-customer     = 12
                tt-customer.Postal-Code:fgcolor IN BROWSE BROWSE-customer = 12
                tt-customer.Sales-Rep:fgcolor IN BROWSE BROWSE-customer   = 12.
    END. 

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-customer wWin
ON ROW-LEAVE OF BROWSE-customer IN FRAME fMain
    DO:
        DEFINE VARIABLE brws-col-hdl   AS HANDLE    NO-UNDO.
        DEFINE VARIABLE buff-field-hdl AS HANDLE    NO-UNDO.
        DEFINE VARIABLE iCount         AS INTEGER   NO-UNDO.  
        DEFINE VARIABLE cInfo          AS CHARACTER NO-UNDO.
        DEFINE VARIABLE icustnum       AS INTEGER   NO-UNDO.
        icustnum = tt-customer.Cust-Num.
        cInfo = "Update Customer". 
        IF BROWSE-customer:CURRENT-ROW-MODIFIED THEN 
        DO:
            REPEAT iCount = 1 TO BROWSE-customer:NUM-COLUMNS:
                brws-col-hdl = BROWSE-customer:GET-BROWSE-COLUMN(iCount).
                IF brws-col-hdl:MODIFIED THEN
                DO:
                    buff-field-hdl = brws-col-hdl:BUFFER-FIELD.
                    IF VALID-HANDLE (buff-field-hdl) THEN buff-field-hdl:BUFFER-VALUE = brws-col-hdl:SCREEN-VALUE.
                END.
            END.
            RUN GUI/AddUpdateDelete.p(INPUT icustnum,  INPUT-OUTPUT TABLE tt-customer,INPUT-OUTPUT cInfo).
            MESSAGE cInfo
                VIEW-AS ALERT-BOX.
            OPEN QUERY BROWSE-customer FOR EACH tt-customer  NO-LOCK WHERE tt-customer.cDel = logical (TOGGLE-Inactive:SCREEN-VALUE).
        END.
    END.
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-customer wWin
ON VALUE-CHANGED OF BROWSE-customer IN FRAME fMain
    DO:
        RUN   AssignToFillins ( TABLE tt-customer).
        IF tt-customer.cDel = logical ("YES") THEN
            BUTTON-DELETE:LABEL = "Un-delete".
        ELSE 
            BUTTON-DELETE:LABEL = "DELETE".
        ASSIGN
            BUTTON-update:SENSITIVE = TRUE
            BUTTON-DELETE:SENSITIVE = TRUE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-add wWin
ON CHOOSE OF BUTTON-add IN FRAME fMain /* ADD */
    DO:

        DEFINE VARIABLE cInfo AS CHARACTER NO-UNDO.

        cInfo = "Add Customer".
        IF SELF:LABEL = "Save" THEN
        DO:
            TEMP-TABLE tt-customer:EMPTY-TEMP-TABLE ().
            CREATE tt-customer.
            ASSIGN 
                tt-customer.Name        = FILL-IN-Name:SCREEN-VALUE 
                tt-customer.Phone       = FILL-IN-Phone:SCREEN-VALUE 
                tt-customer.Address     = FILL-IN-Address1:SCREEN-VALUE
                tt-customer.Address2    = FILL-IN-Address2:SCREEN-VALUE
                tt-customer.City        = FILL-IN-City:SCREEN-VALUE 
                tt-customer.State       = FILL-IN-State:SCREEN-VALUE
                tt-customer.Country     = FILL-IN-Country:SCREEN-VALUE
                tt-customer.Postal-Code = FILL-IN-Postal-Code:SCREEN-VALUE
                tt-customer.Sales-Rep   = FILL-IN-Sales-Rep:SCREEN-VALUE . 
         
            RUN GUI/AddUpdateDelete.p( INPUT 0 , INPUT-OUTPUT TABLE tt-customer, INPUT-OUTPUT cInfo).
        
            FIND FIRST tt-customer NO-LOCK NO-ERROR.
            IF AVAILABLE tt-customer THEN 
            DO:
                RUN   AssignToFillins (table tt-customer).           
                             

                ASSIGN
                    BUTTON-Search:SENSITIVE   = TRUE
                    BUTTON-update:SENSITIVE   = TRUE
                    BUTTON-DELETE:SENSITIVE   = TRUE
                    FILL-IN-custnum:READ-ONLY = FALSE.
                BUTTON-add:LABEL  = "ADD".
                RUN DisableFillins.
                BROWSE-customer:REFRESH ().
                MESSAGE cInfo
                    VIEW-AS ALERT-BOX.
            END. 
        END.
        ELSE  IF SELF:LABEL = "ADD" THEN
            DO:
                PAUSE 1 NO-MESSAGE.
                RUN EnableFillinns.
                ASSIGN
                    FILL-IN-custnum:READ-ONLY = TRUE
                    BUTTON-Search:SENSITIVE   = FALSE.
                BUTTON-update:SENSITIVE   = FALSE.
                BUTTON-DELETE:SENSITIVE   = FALSE.
                BUTTON-add:LABEL = "Save".
                CLEAR FRAME fmain.
                
              
            END.
   //  MESSAGE BUTTON-add:LABEL "After Clear Frame" VIEW-AS ALERT-BOX.
        CATCH e AS Progress.Lang.Error :
            BUTTON-add:LABEL = "ADD".
            MESSAGE  e:GetMessage(1)
                VIEW-AS ALERT-BOX.   
        END CATCH.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-cancel wWin
ON CHOOSE OF BUTTON-cancel IN FRAME fMain /* Cancel */
    DO:
        IF   BUTTON-add:LABEL = "Save" OR BUTTON-update:LABEL = "Save" THEN
        DO:
            ASSIGN
                BUTTON-add:LABEL          = "ADD"
                BUTTON-update:LABEL       = "UPDATE"
                BUTTON-add:SENSITIVE      = TRUE
                BUTTON-Search:SENSITIVE   = TRUE
                BUTTON-update:SENSITIVE   = TRUE
                BUTTON-DELETE:SENSITIVE   = TRUE
                FILL-IN-custnum:READ-ONLY = FALSE.
        END.
        ELSE
        DO:
            RUN   disable_UI.
        END.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-DELETE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-DELETE wWin
ON CHOOSE OF BUTTON-DELETE IN FRAME fMain /* DELETE */
    DO:
 
        DEFINE VARIABLE icustnum AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cInfo    AS CHARACTER NO-UNDO.
        icustnum = int (FILL-IN-custnum:SCREEN-VALUE).
        IF  SELF:LABEL = "Un-Delete" THEN
        DO:
            cInfo = "Un-Delete Customer".
        END.
        ELSE
        DO:
            cInfo = "Delete Customer".
            RUN DisableFillins.
            CLEAR FRAME fmain.
        END.
        RUN GUI/AddUpdateDelete.p(INPUT icustnum,  INPUT-OUTPUT TABLE tt-customer ,INPUT-OUTPUT cInfo).

        TEMP-TABLE tt-customer:EMPTY-TEMP-TABLE (). 
        APPLY "VALUE-CHANGED" TO TOGGLE-Inactive.
        MESSAGE cInfo
            VIEW-AS ALERT-BOX.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Search
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Search wWin
ON CHOOSE OF BUTTON-Search IN FRAME fMain /* Search */
    DO:
    
        DEFINE VARIABLE icustnum AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cInfo    AS CHARACTER NO-UNDO.
        cInfo = "Find".
        icustnum = int (FILL-IN-custnum:SCREEN-VALUE).
      
        TEMP-TABLE tt-customer:EMPTY-TEMP-TABLE ().
        IF AVAILABLE tt-customer THEN
            DELETE tt-customer.
        RUN GUI/FetchCustomer.p(INPUT icustnum,  INPUT-OUTPUT TABLE tt-customer,INPUT-OUTPUT cInfo ).

        FIND  tt-customer WHERE tt-customer.cDel = logical (TOGGLE-Inactive:SCREEN-VALUE) NO-ERROR .  
        IF AVAILABLE tt-customer THEN 
        DO:
            RUN   AssignToFillins (table tt-customer).
            OPEN QUERY BROWSE-customer FOR EACH tt-customer.
            ASSIGN
                BUTTON-update:SENSITIVE = TRUE
                BUTTON-DELETE:SENSITIVE = TRUE.
        END.
        ELSE
        DO:
            TEMP-TABLE tt-customer:EMPTY-TEMP-TABLE ().
            CLEAR FRAME fmain.
            ASSIGN
                BUTTON-update:SENSITIVE = FALSE
                BUTTON-DELETE:SENSITIVE = FALSE.
            UNDO, THROW NEW Progress.Lang.AppError( SUBSTITUTE ("Customer No &1 Not Found", STRING (icustnum)) ,1).
        END.
        CATCH e AS Progress.Lang.Error :
         
            MESSAGE  e:GetMessage(1)
                VIEW-AS ALERT-BOX.   
        
        END CATCH.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-update
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-update wWin
ON CHOOSE OF BUTTON-update IN FRAME fMain /* UPDATE */
    DO:
 
        DEFINE VARIABLE cInfo    AS CHARACTER NO-UNDO.
        DEFINE VARIABLE iCustnum AS INTEGER   NO-UNDO.
        iCustnum = tt-customer.Cust-Num.
        cInfo = "Update Customer".
        
        IF SELF:label = "Save" THEN
        DO:
            ASSIGN 
                tt-customer.Cust-Num    = int (FILL-IN-custnum:SCREEN-VALUE)
                tt-customer.Name        = FILL-IN-Name:SCREEN-VALUE 
                tt-customer.Phone       = FILL-IN-Phone:SCREEN-VALUE 
                tt-customer.Address     = FILL-IN-Address1:SCREEN-VALUE
                tt-customer.Address2    = FILL-IN-Address2:SCREEN-VALUE
                tt-customer.City        = FILL-IN-City:SCREEN-VALUE 
                tt-customer.State       = FILL-IN-State:SCREEN-VALUE
                tt-customer.Country     = FILL-IN-Country:SCREEN-VALUE
                tt-customer.Postal-Code = FILL-IN-Postal-Code:SCREEN-VALUE
                tt-customer.Sales-Rep   = FILL-IN-Sales-Rep:SCREEN-VALUE .
           
            RUN GUI/AddUpdateDelete.p(INPUT iCustnum ,  INPUT-OUTPUT TABLE tt-customer,INPUT-OUTPUT cInfo).

            FIND FIRST  tt-customer WHERE tt-customer.Cust-Num = iCustnum NO-LOCK NO-ERROR.
            IF AVAILABLE tt-customer THEN 
            DO:
                RUN   AssignToFillins (table tt-customer).
                MESSAGE cInfo
                    VIEW-AS ALERT-BOX.
                BUTTON-update:LABEL = "UPDATE".
                RUN DisableFillins.
                ASSIGN
                    BUTTON-add:SENSITIVE      = TRUE
                    BUTTON-Search:SENSITIVE   = TRUE
                    BUTTON-DELETE:SENSITIVE   = TRUE
                    FILL-IN-custnum:READ-ONLY = FALSE.
                OPEN QUERY BROWSE-customer FOR EACH tt-customer  NO-LOCK WHERE tt-customer.cDel = logical (TOGGLE-Inactive:SCREEN-VALUE).     
  
            END.
        END.
        ELSE  IF SELF:label = "UPDATE" THEN
            DO:
            
                SELF:label  = "Save".
                RUN EnableFillinns.
                ASSIGN
                    BUTTON-add:SENSITIVE      = FALSE
                    BUTTON-Search:SENSITIVE   = FALSE
                    BUTTON-DELETE:SENSITIVE   = FALSE
                    FILL-IN-custnum:READ-ONLY = TRUE.
            END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOGGLE-Inactive
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOGGLE-Inactive wWin
ON VALUE-CHANGED OF TOGGLE-Inactive IN FRAME fMain /* InActive */
    DO:
 
        CLEAR FRAME fMain.
        DEFINE VARIABLE cInfo AS CHARACTER NO-UNDO.
        IF AVAILABLE tt-customer THEN
            TEMP-TABLE tt-customer:EMPTY-TEMP-TABLE ().
        cInfo = "All".
        RUN GUI/FetchCustomer.p(INPUT 0,INPUT-OUTPUT table tt-customer,INPUT-OUTPUT cInfo).
        OPEN QUERY BROWSE-customer FOR EACH tt-customer  NO-LOCK WHERE tt-customer.cDel = logical (TOGGLE-Inactive:SCREEN-VALUE).
        BROWSE-customer:REFRESH ().
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOGGLE-update-tag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOGGLE-update-tag wWin
ON VALUE-CHANGED OF TOGGLE-update-tag IN FRAME fMain /* Update-Mode */
    DO:
        DEFINE VARIABLE hBrowse AS HANDLE  NO-UNDO.
        DEFINE VARIABLE hColumn AS HANDLE  NO-UNDO.
        DEFINE VARIABLE iCount  AS INTEGER NO-UNDO.
        ASSIGN
            hBrowse = BROWSE   BROWSE-customer:HANDLE.
            
        IF LOGICAL(TOGGLE-update-tag:SCREEN-VALUE) THEN 
        DO:
            DO iCount = 1 TO hBrowse:NUM-COLUMNS:
        
                hColumn = hBrowse:GET-BROWSE-COLUMN(iCount).
                IF hColumn:NAME <> "Cust-Num" THEN
                    hColumn:READ-ONLY = NO.
            END.
        END.
        ELSE 
        DO:
            DO iCount = 1 TO hBrowse:NUM-COLUMNS:
                hColumn = hBrowse:GET-BROWSE-COLUMN(iCount).
                hColumn:READ-ONLY = YES.
            END.   
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */

{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AssignToFillins wWin 
PROCEDURE AssignToFillins :
    /*------------------------------------------------------------------------------
                                 Purpose:
                                 Notes:
                                ------------------------------------------------------------------------------*/
    DEFINE  INPUT  PARAMETER TABLE FOR tt-Customer.
    DO WITH FRAME fmain:
        ASSIGN 
            FILL-IN-custnum:SCREEN-VALUE     = STRING (tt-customer.Cust-Num)
            FILL-IN-Address1:SCREEN-VALUE    = tt-customer.Address
            FILL-IN-Address2:SCREEN-VALUE    = tt-customer.Address2
            FILL-IN-City:SCREEN-VALUE        = tt-customer.City
            FILL-IN-Country:SCREEN-VALUE     = tt-customer.Country
            FILL-IN-Name:SCREEN-VALUE        = tt-customer.Name
            FILL-IN-Phone:SCREEN-VALUE       = tt-customer.Phone
            FILL-IN-Postal-Code:SCREEN-VALUE = tt-customer.Postal-Code
            FILL-IN-Sales-Rep:SCREEN-VALUE   = tt-customer.Sales-Rep
            FILL-IN-State:SCREEN-VALUE       = tt-customer.State.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisableFillins wWin 
PROCEDURE DisableFillins :
    /*------------------------------------------------------------------------------
                                                                     Purpose:
                                                                     Notes:
                                                                    ------------------------------------------------------------------------------*/
    DO WITH FRAME fmain:
        ASSIGN
            FILL-IN-Address1:READ-ONLY    = TRUE
            FILL-IN-Address2:READ-ONLY    = TRUE
            FILL-IN-City:READ-ONLY        = TRUE
            FILL-IN-Country:READ-ONLY     = TRUE
            FILL-IN-Name:READ-ONLY        = TRUE
            FILL-IN-Phone:READ-ONLY       = TRUE
            FILL-IN-Postal-Code:READ-ONLY = TRUE
            FILL-IN-Sales-Rep:READ-ONLY   = TRUE
            FILL-IN-State:READ-ONLY       = TRUE.
    END.

    CATCH e AS Progress.Lang.Error:
        MESSAGE  e:GetMessage(1)
            VIEW-AS ALERT-BOX.  
    END CATCH.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wWin  _DEFAULT-DISABLE
PROCEDURE disable_UI :
    /*------------------------------------------------------------------------------
      Purpose:     DISABLE the User Interface
      Parameters:  <none>
      Notes:       Here we clean-up the user-interface by deleting
                   dynamic widgets we have created and/or hide 
                   frames.  This procedure is usually called when
                   we are ready to "clean-up" after running.
    ------------------------------------------------------------------------------*/
    /* Delete the WINDOW we created */
    IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
        THEN DELETE WIDGET wWin.
    IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EnableFillinns wWin 
PROCEDURE EnableFillinns :
    /*------------------------------------------------------------------------------
                                                                     Purpose:
                                                                     Notes:
                                                                    ------------------------------------------------------------------------------*/
    
    DO WITH FRAME fmain:
        ASSIGN
            FILL-IN-custnum:READ-ONLY     = FALSE
            FILL-IN-Address1:READ-ONLY    = FALSE
            FILL-IN-Address2:READ-ONLY    = FALSE
            FILL-IN-City:READ-ONLY        = FALSE
            FILL-IN-Country:READ-ONLY     = FALSE
            FILL-IN-Name:READ-ONLY        = FALSE
            FILL-IN-Phone:READ-ONLY       = FALSE
            FILL-IN-Postal-Code:READ-ONLY = FALSE
            FILL-IN-Sales-Rep:READ-ONLY   = FALSE
            FILL-IN-State:READ-ONLY       = FALSE.
    END.
    CATCH e AS Progress.Lang.Error:
        MESSAGE  e:GetMessage(1)
            VIEW-AS ALERT-BOX.  
    END CATCH.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wWin  _DEFAULT-ENABLE
PROCEDURE enable_UI :
    /*------------------------------------------------------------------------------
      Purpose:     ENABLE the User Interface
      Parameters:  <none>
      Notes:       Here we display/view/enable the widgets in the
                   user-interface.  In addition, OPEN all queries
                   associated with each FRAME and BROWSE.
                   These statements here are based on the "Other 
                   Settings" section of the widget Property Sheets.
    ------------------------------------------------------------------------------*/
    DISPLAY FILL-IN-custnum TOGGLE-Inactive FILL-IN-Name FILL-IN-Phone 
        FILL-IN-Sales-Rep FILL-IN-Address1 FILL-IN-Address2 FILL-IN-City 
        FILL-IN-State FILL-IN-Country FILL-IN-Postal-Code TOGGLE-update-tag 
        WITH FRAME fMain IN WINDOW wWin.
    ENABLE RECT-1 RECT-2 FILL-IN-custnum TOGGLE-Inactive BUTTON-Search 
        BUTTON-cancel FILL-IN-Name FILL-IN-Phone FILL-IN-Sales-Rep 
        FILL-IN-Address1 FILL-IN-Address2 FILL-IN-City FILL-IN-State 
        FILL-IN-Country FILL-IN-Postal-Code BUTTON-add BUTTON-update 
        BUTTON-DELETE TOGGLE-update-tag BROWSE-customer 
        WITH FRAME fMain IN WINDOW wWin.
    {&OPEN-BROWSERS-IN-QUERY-fMain}
    VIEW wWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject wWin 
PROCEDURE exitObject :
    /*------------------------------------------------------------------------------
                                                                     Purpose:  Window-specific override of this procedure which destroys 
                                                                     its contents and itself.
                                                                     Notes:  
                                                                ------------------------------------------------------------------------------*/

    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
    /*------------------------------------------------------------------------------
                                                                      Purpose:
                                                                      Notes:
                                                                    ------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */

    RUN SUPER.

    /* Code placed here will execute AFTER standard behavior.    */

    DO WITH FRAME fmain:
        ASSIGN
            BUTTON-update:SENSITIVE = FALSE
            BUTTON-DELETE:SENSITIVE = FALSE.
    END.
    RUN DisableFillins.
    DEFINE VARIABLE cInfo AS CHARACTER NO-UNDO.
    cInfo = "All".
    RUN GUI/FetchCustomer.p(INPUT 0,INPUT-OUTPUT table tt-customer,INPUT-OUTPUT cInfo).
    APPLY "VALUE-CHANGED" TO TOGGLE-update-tag.
    OPEN QUERY BROWSE-customer FOR EACH tt-customer WHERE tt-customer.cDel = logical (TOGGLE-Inactive:SCREEN-VALUE). 

   
       
    
    CATCH e AS Progress.Lang.Error:
        MESSAGE e:GetMessage(1)
            VIEW-AS ALERT-BOX.
    END CATCH.

    FINALLY:

    END FINALLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

