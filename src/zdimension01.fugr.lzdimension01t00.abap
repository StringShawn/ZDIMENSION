*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 11.06.2009 at 11:43:19 by user D00211
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZDIMBNO.........................................*
DATA:  BEGIN OF STATUS_ZDIMBNO                       .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZDIMBNO                       .
CONTROLS: TCTRL_ZDIMBNO
            TYPE TABLEVIEW USING SCREEN '9005'.
*...processing: ZDIMBTPL........................................*
DATA:  BEGIN OF STATUS_ZDIMBTPL                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZDIMBTPL                      .
CONTROLS: TCTRL_ZDIMBTPL
            TYPE TABLEVIEW USING SCREEN '9002'.
*...processing: ZDIMBTPLH.......................................*
DATA:  BEGIN OF STATUS_ZDIMBTPLH                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZDIMBTPLH                     .
CONTROLS: TCTRL_ZDIMBTPLH
            TYPE TABLEVIEW USING SCREEN '9003'.
*...processing: ZDIMBTPLI.......................................*
DATA:  BEGIN OF STATUS_ZDIMBTPLI                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZDIMBTPLI                     .
CONTROLS: TCTRL_ZDIMBTPLI
            TYPE TABLEVIEW USING SCREEN '9004'.
*...processing: ZDIMBUHK........................................*
DATA:  BEGIN OF STATUS_ZDIMBUHK                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZDIMBUHK                      .
CONTROLS: TCTRL_ZDIMBUHK
            TYPE TABLEVIEW USING SCREEN '9006'.
*...processing: ZDIMFIELDT......................................*
DATA:  BEGIN OF STATUS_ZDIMFIELDT                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZDIMFIELDT                    .
CONTROLS: TCTRL_ZDIMFIELDT
            TYPE TABLEVIEW USING SCREEN '9001'.
*...processing: ZDIMLDGRP.......................................*
DATA:  BEGIN OF STATUS_ZDIMLDGRP                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZDIMLDGRP                     .
CONTROLS: TCTRL_ZDIMLDGRP
            TYPE TABLEVIEW USING SCREEN '9007'.
*...processing: ZDIMSCNRO.......................................*
DATA:  BEGIN OF STATUS_ZDIMSCNRO                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZDIMSCNRO                     .
*.........table declarations:.................................*
TABLES: *ZDIMBNO                       .
TABLES: *ZDIMBTPL                      .
TABLES: *ZDIMBTPLH                     .
TABLES: *ZDIMBTPLI                     .
TABLES: *ZDIMBUHK                      .
TABLES: *ZDIMFIELDT                    .
TABLES: *ZDIMLDGRP                     .
TABLES: *ZDIMSCNRO                     .
TABLES: ZDIMBNO                        .
TABLES: ZDIMBTPL                       .
TABLES: ZDIMBTPLH                      .
TABLES: ZDIMBTPLI                      .
TABLES: ZDIMBUHK                       .
TABLES: ZDIMFIELDT                     .
TABLES: ZDIMLDGRP                      .
TABLES: ZDIMSCNRO                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
