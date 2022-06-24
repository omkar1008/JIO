&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
{adecomm/appserv.i}
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
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

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-13 FILL-IN-typeofchange ~
FILL-IN-priority FILL-IN-14 FILL-IN-15 FILL-IN-createdon FILL-IN-16 ~
FILL-IN-createdby FILL-IN-17 FILL-IN-5 FILL-IN-6 FILL-IN-18 FILL-IN-7 ~
FILL-IN-19 FILL-IN-8 FILL-IN-20 FILL-IN-9 FILL-IN-21 FILL-IN-10 FILL-IN-22 ~
FILL-IN-11 FILL-IN-23 FILL-IN-12 FILL-IN-24 FILL-IN-25 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-13 FILL-IN-typeofchange ~
FILL-IN-priority FILL-IN-14 FILL-IN-15 FILL-IN-createdon FILL-IN-16 ~
FILL-IN-createdby FILL-IN-17 FILL-IN-5 FILL-IN-6 FILL-IN-18 FILL-IN-7 ~
FILL-IN-19 FILL-IN-8 FILL-IN-20 FILL-IN-9 FILL-IN-21 FILL-IN-10 FILL-IN-22 ~
FILL-IN-11 FILL-IN-23 FILL-IN-12 FILL-IN-24 FILL-IN-25 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE FILL-IN-10 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fill 10" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-11 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fill 11" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-12 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fill 12" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-13 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fill 13" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-14 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fill 14" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-15 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fill 15" 
     VIEW-AS FILL-IN 
     SIZE 1 BY .48 NO-UNDO.

DEFINE VARIABLE FILL-IN-16 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fill 16" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-17 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fill 17" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-18 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fill 18" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-19 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fill 19" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-20 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fill 20" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-21 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fill 21" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-22 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fill 22" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-23 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fill 23" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-24 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fill 24" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-25 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fill 25" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-5 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fill 5" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-6 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fill 6" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-7 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fill 7" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-8 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fill 8" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-9 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fill 9" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-createdby AS CHARACTER FORMAT "X(256)":U 
     LABEL "Created by" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-createdon AS CHARACTER FORMAT "X(256)":U 
     LABEL "Created on" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-priority AS CHARACTER FORMAT "X(256)":U 
     LABEL "Priority" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-typeofchange AS CHARACTER FORMAT "X(256)":U 
     LABEL "Type-Of-Change" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     FILL-IN-13 AT ROW 1.24 COL 65 COLON-ALIGNED WIDGET-ID 26
     FILL-IN-typeofchange AT ROW 1.48 COL 34 COLON-ALIGNED WIDGET-ID 2 PASSWORD-FIELD 
     FILL-IN-priority AT ROW 2.91 COL 35 COLON-ALIGNED WIDGET-ID 4
     FILL-IN-14 AT ROW 3.38 COL 67 COLON-ALIGNED WIDGET-ID 28
     FILL-IN-15 AT ROW 4.1 COL 67 COLON-ALIGNED WIDGET-ID 30
     FILL-IN-createdon AT ROW 4.33 COL 35 COLON-ALIGNED WIDGET-ID 6
     FILL-IN-16 AT ROW 5.52 COL 67 COLON-ALIGNED WIDGET-ID 32
     FILL-IN-createdby AT ROW 6.24 COL 33 COLON-ALIGNED WIDGET-ID 8
     FILL-IN-17 AT ROW 7.67 COL 66 COLON-ALIGNED WIDGET-ID 34
     FILL-IN-5 AT ROW 8.14 COL 34 COLON-ALIGNED WIDGET-ID 10
     FILL-IN-6 AT ROW 9.81 COL 32 COLON-ALIGNED WIDGET-ID 12
     FILL-IN-18 AT ROW 11 COL 67 COLON-ALIGNED WIDGET-ID 36
     FILL-IN-7 AT ROW 11.95 COL 34 COLON-ALIGNED WIDGET-ID 14
     FILL-IN-19 AT ROW 13.14 COL 67 COLON-ALIGNED WIDGET-ID 38 PASSWORD-FIELD 
     FILL-IN-8 AT ROW 13.62 COL 34 COLON-ALIGNED WIDGET-ID 16
     FILL-IN-20 AT ROW 14.33 COL 67 COLON-ALIGNED WIDGET-ID 40
     FILL-IN-9 AT ROW 15.29 COL 33 COLON-ALIGNED WIDGET-ID 18
     FILL-IN-21 AT ROW 16.24 COL 68 COLON-ALIGNED WIDGET-ID 42
     FILL-IN-10 AT ROW 17.19 COL 32 COLON-ALIGNED WIDGET-ID 20
     FILL-IN-22 AT ROW 17.67 COL 68 COLON-ALIGNED WIDGET-ID 44
     FILL-IN-11 AT ROW 18.38 COL 32 COLON-ALIGNED WIDGET-ID 22
     FILL-IN-23 AT ROW 18.86 COL 68 COLON-ALIGNED WIDGET-ID 46
     FILL-IN-12 AT ROW 19.57 COL 33 COLON-ALIGNED WIDGET-ID 24
     FILL-IN-24 AT ROW 20.52 COL 67 COLON-ALIGNED WIDGET-ID 48
     FILL-IN-25 AT ROW 22.43 COL 65 COLON-ALIGNED WIDGET-ID 50
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 159.6 BY 23 WIDGET-ID 100.


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
         HEIGHT             = 23
         WIDTH              = 159.6
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 174
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 174
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
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
ASSIGN 
       FILL-IN-18:HIDDEN IN FRAME fMain           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* <insert SmartWindow title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
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
  DISPLAY FILL-IN-13 FILL-IN-typeofchange FILL-IN-priority FILL-IN-14 FILL-IN-15 
          FILL-IN-createdon FILL-IN-16 FILL-IN-createdby FILL-IN-17 FILL-IN-5 
          FILL-IN-6 FILL-IN-18 FILL-IN-7 FILL-IN-19 FILL-IN-8 FILL-IN-20 
          FILL-IN-9 FILL-IN-21 FILL-IN-10 FILL-IN-22 FILL-IN-11 FILL-IN-23 
          FILL-IN-12 FILL-IN-24 FILL-IN-25 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE FILL-IN-13 FILL-IN-typeofchange FILL-IN-priority FILL-IN-14 FILL-IN-15 
         FILL-IN-createdon FILL-IN-16 FILL-IN-createdby FILL-IN-17 FILL-IN-5 
         FILL-IN-6 FILL-IN-18 FILL-IN-7 FILL-IN-19 FILL-IN-8 FILL-IN-20 
         FILL-IN-9 FILL-IN-21 FILL-IN-10 FILL-IN-22 FILL-IN-11 FILL-IN-23 
         FILL-IN-12 FILL-IN-24 FILL-IN-25 
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

