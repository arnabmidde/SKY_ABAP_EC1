*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFTS_RSCODEINFO.................................*
DATA:  BEGIN OF STATUS_ZFTS_RSCODEINFO               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFTS_RSCODEINFO               .
CONTROLS: TCTRL_ZFTS_RSCODEINFO
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFTS_RSCODEINFO               .
TABLES: ZFTS_RSCODEINFO                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
