&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS txtTenant btnFull btnCopilot btnInsights 
&Scoped-Define DISPLAYED-OBJECTS txtTenant 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCopilot 
     LABEL "Show Copilot" 
     SIZE 148 BY 1.14.

DEFINE BUTTON btnFull 
     LABEL "Show Full UI" 
     SIZE 148 BY 1.14.

DEFINE BUTTON btnInsights 
     LABEL "Show Insights" 
     SIZE 148 BY 1.14.

DEFINE VARIABLE txtTenant AS CHARACTER FORMAT "X(256)":U INITIAL "demo" 
     LABEL "Tenant" 
     VIEW-AS FILL-IN 
     SIZE 148 BY .95 NO-UNDO.

DEFINE BUTTON btnDashLink 
     LABEL "How do I find the ID of my dashbpoard?" 
     SIZE 148 BY 1.14.

DEFINE BUTTON btnShowDashboard 
     LABEL "Show Dashboard" 
     SIZE 148 BY 1.14.

DEFINE VARIABLE txtDashboardID AS CHARACTER FORMAT "X(256)":U INITIAL "a4019f2a-11f5-4e6c-8543-f4be336f365b" 
     LABEL "Dashboard ID" 
     VIEW-AS FILL-IN 
     SIZE 136 BY 1 NO-UNDO.

DEFINE BUTTON btnShowViz 
     LABEL "Show Visualisation" 
     SIZE 148 BY 1.14.

DEFINE BUTTON btnVizLink 
     LABEL "How do I find the ID of my visualisation?" 
     SIZE 148 BY 1.14.

DEFINE VARIABLE txtVisID AS CHARACTER FORMAT "X(256)":U INITIAL "29a70f82-1f9e-4d69-95bb-445957ab7990" 
     LABEL "Visualisation ID" 
     VIEW-AS FILL-IN 
     SIZE 136 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     txtTenant AT ROW 2.43 COL 4 WIDGET-ID 4
     btnFull AT ROW 4.1 COL 12 WIDGET-ID 8
     btnCopilot AT ROW 5.52 COL 12 WIDGET-ID 10
     btnInsights AT ROW 7.05 COL 12 WIDGET-ID 12
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 168 BY 22.76 WIDGET-ID 100.

DEFINE FRAME frmDashboard
     txtDashboardID AT ROW 1.71 COL 18 COLON-ALIGNED WIDGET-ID 2
     btnShowDashboard AT ROW 3.29 COL 9 WIDGET-ID 4
     btnDashLink AT ROW 5.05 COL 8 WIDGET-ID 8
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 4 ROW 16.48
         SIZE 161 BY 6.91
         TITLE "Dashboard" WIDGET-ID 300.

DEFINE FRAME frmViz
     txtVisID AT ROW 1.71 COL 18 COLON-ALIGNED WIDGET-ID 2
     btnShowViz AT ROW 3.29 COL 9 WIDGET-ID 4
     btnVizLink AT ROW 5.05 COL 8 WIDGET-ID 8
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 4 ROW 8.86
         SIZE 161 BY 6.91
         TITLE "Visualisation" WIDGET-ID 200.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "inmydata components example"
         HEIGHT             = 22.76
         WIDTH              = 168
         MAX-HEIGHT         = 22.76
         MAX-WIDTH          = 168
         VIRTUAL-HEIGHT     = 22.76
         VIRTUAL-WIDTH      = 168
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME frmDashboard:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME frmViz:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME frmViz:MOVE-AFTER-TAB-ITEM (btnInsights:HANDLE IN FRAME DEFAULT-FRAME)
       XXTABVALXX = FRAME frmViz:MOVE-BEFORE-TAB-ITEM (FRAME frmDashboard:HANDLE)
/* END-ASSIGN-TABS */.

/* SETTINGS FOR FILL-IN txtTenant IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
ASSIGN 
       txtTenant:AUTO-RESIZE IN FRAME DEFAULT-FRAME      = TRUE.

/* SETTINGS FOR FRAME frmDashboard
                                                                        */
/* SETTINGS FOR FRAME frmViz
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* inmydata components example */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* inmydata components example */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCopilot
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCopilot C-Win
ON CHOOSE OF btnCopilot IN FRAME DEFAULT-FRAME /* Show Copilot */
DO:
  RUN aichat.w (INPUT txtTenant:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmDashboard
&Scoped-define SELF-NAME btnDashLink
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDashLink C-Win
ON CHOOSE OF btnDashLink IN FRAME frmDashboard /* How do I find the ID of my dashbpoard? */
DO:
  OS-COMMAND SILENT START "https://developer.inmydata.com/a/solutions/articles/36000577000?portalId=36000061664".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME btnFull
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFull C-Win
ON CHOOSE OF btnFull IN FRAME DEFAULT-FRAME /* Show Full UI */
DO:
  RUN full.w (INPUT txtTenant:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnInsights
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnInsights C-Win
ON CHOOSE OF btnInsights IN FRAME DEFAULT-FRAME /* Show Insights */
DO:
  RUN insights.w (INPUT txtTenant:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmDashboard
&Scoped-define SELF-NAME btnShowDashboard
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnShowDashboard C-Win
ON CHOOSE OF btnShowDashboard IN FRAME frmDashboard /* Show Dashboard */
DO:
    RUN dashboard.w (
    INPUT txtTenant:SCREEN-VALUE IN FRAME DEFAULT-FRAME, 
    INPUT txtDashboardID:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmViz
&Scoped-define SELF-NAME btnShowViz
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnShowViz C-Win
ON CHOOSE OF btnShowViz IN FRAME frmViz /* Show Visualisation */
DO:
  RUN visualisation.w (
    INPUT txtTenant:SCREEN-VALUE IN FRAME DEFAULT-FRAME, 
    INPUT txtVisID:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnVizLink
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnVizLink C-Win
ON CHOOSE OF btnVizLink IN FRAME frmViz /* How do I find the ID of my visualisation? */
DO:
  OS-COMMAND SILENT START "https://developer.inmydata.com/a/solutions/articles/36000577000?portalId=36000061664".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  DISPLAY txtTenant 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE txtTenant btnFull btnCopilot btnInsights 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  DISPLAY txtVisID 
      WITH FRAME frmViz IN WINDOW C-Win.
  ENABLE txtVisID btnShowViz btnVizLink 
      WITH FRAME frmViz IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frmViz}
  DISPLAY txtDashboardID 
      WITH FRAME frmDashboard IN WINDOW C-Win.
  ENABLE txtDashboardID btnShowDashboard btnDashLink 
      WITH FRAME frmDashboard IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frmDashboard}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

