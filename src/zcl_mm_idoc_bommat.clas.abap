class ZCL_MM_IDOC_BOMMAT definition
  public
  final
  create public .

public section.

  data GT_STKO type TTY_STKO .
  data GT_STPO type TTY_STPO .
  data GWA_E1MASTM type E1MASTM .
  data GWA_E1STKOM type E1STKOM .
  data GWA_E1STPOM type E1STPOM .
  data GWA_E1STZUM type E1STZUM .
  data GT_MAST type PRD_T_MAST .

  methods GET_DATA .
  methods ADJUST_SEGMENTS
    importing
      !IS_IDOC_CONTROL type EDIDC
    changing
      !CT_IDOC_DATA type EDIDD_TT .
protected section.
private section.
ENDCLASS.



CLASS ZCL_MM_IDOC_BOMMAT IMPLEMENTATION.


METHOD adjust_segments.
*&------------------------------------------------------------------------------&*
*& Confidential Property of Stryker                                             &*
*& All Rights Reserved                                                          &*
*&******************************************************************************&*
*& Method  name : ADJUST_SEGMENTS                                              &*
*& Company      : Stryker Project Accelerate                                    &*
*& Author       : Bharathi Purum                                                &*
*& Date         : 26/08/2024                                                    &*
*& Title        : BOMs from SAP to WebOps                                       &*
*& FD #         : FTS.INT.224-01                                                &*
*&******************************************************************************&*
*& DESCRIPTION  : Need the ability to create SAP BOMs for prebuilt   EC2K905575 &*
*&                implant, instrumentation and combo kits and send   EC2K905577 &*
*&                (via interface) BOMs to WebOps.                               &*
*&******************************************************************************&*
*Variable and workarea declaration
  DATA : lwa_edidd    TYPE edidd,
         lwa_edidd_ai TYPE edidd,
         lwa_e1mastm  TYPE e1mastm,
         lwa_ze1stpom TYPE ze1stpom,
         lv_tabix     TYPE syst_tabix.
*Read the Master BoM data to get Bill of Material details
  READ TABLE ct_idoc_data INTO DATA(lwa_idoc_e1stzum) WITH KEY segnam = 'E1STZUM'.
  CHECK sy-subrc IS INITIAL.
  gwa_e1stzum = lwa_idoc_e1stzum-sdata.
  CHECK gwa_e1stzum-stlty IS NOT INITIAL AND gwa_e1stzum-stlnr IS NOT INITIAL.
*Get data from STKO and STPO tables
  me->get_data( ).
*Read the header data segment and update the all header filed data into the segment E1STKOM
  READ TABLE ct_idoc_data ASSIGNING FIELD-SYMBOL(<lfs_idoc_e1stkom>) WITH KEY segnam = 'E1STKOM'.
  IF sy-subrc IS INITIAL.
    gwa_e1stkom = <lfs_idoc_e1stkom>-sdata.
*Read STKO data based on BOM category and Bill of Material
    READ TABLE gt_stko INTO DATA(lwa_stko) WITH KEY stlty = gwa_e1stzum-stlty
                                                   stlnr = gwa_e1stzum-stlnr.
    IF sy-subrc IS INITIAL.
      gwa_e1stkom-aennr = lwa_stko-aennr.
      gwa_e1stkom-loekz = lwa_stko-loekz.
      gwa_e1stkom-bmeng = lwa_stko-bmeng.
      gwa_e1stkom-cadkz = lwa_stko-cadkz.
      gwa_e1stkom-labor = lwa_stko-labor.
      gwa_e1stkom-ltxsp = lwa_stko-ltxsp.
*Convert the UOM
      CALL FUNCTION 'UNIT_OF_MEASURE_ISO_TO_SAP'
        EXPORTING
          iso_code = gwa_e1stkom-bmein
        IMPORTING
          sap_code = gwa_e1stkom-bmein.
*Convert UOM
      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
        EXPORTING
          input  = gwa_e1stkom-bmein
        IMPORTING
          output = gwa_e1stkom-bmein.

      <lfs_idoc_e1stkom>-sdata = gwa_e1stkom.
    ENDIF.
  ENDIF.
  DELETE ct_idoc_data WHERE segnam = 'E1STPOM'.
*Read the Master BoM data to get Bill of Material details
  READ TABLE ct_idoc_data INTO DATA(lwa_idoc_e1mastm) WITH KEY segnam = 'E1MASTM'.
  IF sy-subrc IS INITIAL.
    lwa_e1mastm = lwa_idoc_e1mastm-sdata.
  ENDIF.
*Loop the STPO data and update all item components details
  LOOP AT gt_stpo INTO DATA(lwa_stpo).
    DATA(lv_lines) = lines( ct_idoc_data ).
    gwa_e1stpom = CORRESPONDING #( lwa_stpo ).
*Remove leading zeros for Component quantity
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = gwa_e1stpom-menge
      IMPORTING
        output = gwa_e1stpom-menge.
    gwa_e1stpom-menge_c = gwa_e1stpom-menge.
*Convert UOM
    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
      EXPORTING
        input  = gwa_e1stpom-meins
      IMPORTING
        output = gwa_e1stpom-meins.

    lwa_edidd-segnam = 'E1STPOM'.
    lwa_edidd-sdata = gwa_e1stpom.
    lv_tabix = lv_lines + 1.
    INSERT lwa_edidd INTO ct_idoc_data INDEX lv_tabix.
    CLEAR : lwa_edidd, gwa_e1stpom, lwa_stpo.
  ENDLOOP.
  CLEAR : lv_tabix.
  LOOP AT ct_idoc_data INTO DATA(lwa_idoc_data) WHERE segnam = 'E1STPOM'.
    lv_tabix = sy-tabix.
    gwa_e1stpom = lwa_idoc_data-sdata.
    READ TABLE gt_mast TRANSPORTING NO FIELDS WITH KEY matnr = gwa_e1stpom-idnrk
                                                       werks = lwa_e1mastm-werks.
    IF sy-subrc IS INITIAL.
      lwa_ze1stpom-stlkz = abap_true.
      lwa_edidd_ai-segnam = 'ZE1STPOM'.
      lwa_edidd_ai-sdata = lwa_ze1stpom.
      lv_tabix = lv_tabix + 1.
      INSERT lwa_edidd_ai INTO ct_idoc_data INDEX lv_tabix.
      CLEAR : lwa_edidd_ai, lwa_ze1stpom.
    ENDIF.
  ENDLOOP.

ENDMETHOD.


METHOD get_data.
*&------------------------------------------------------------------------------&*
*& Confidential Property of Stryker                                             &*
*& All Rights Reserved                                                          &*
*&******************************************************************************&*
*& Method  name : GET_DATA                                                      &*
*& Company      : Stryker Project Accelerate                                    &*
*& Author       : Bharathi Purum                                                &*
*& Date         : 26/08/2024                                                    &*
*& Title        : BOMs from SAP to WebOps                                       &*
*& FD #         : FTS.INT.224-01                                                &*
*&******************************************************************************&*
*& DESCRIPTION  : Need the ability to create SAP BOMs for prebuilt   EC2K905575 &*
*&                implant, instrumentation and combo kits and send   EC2K905577 &*
*&                (via interface) BOMs to WebOps.                               &*
*&******************************************************************************&*
*Get data from STKO table based on BOM category and Bill of Material
  SELECT * FROM stko
           INTO TABLE gt_stko
           WHERE stlty EQ gwa_e1stzum-stlty
           AND stlnr EQ gwa_e1stzum-stlnr.
  IF sy-subrc IS INITIAL.
    SORT gt_stko BY stlty stlnr.
  ENDIF.
  IF gt_stko IS NOT INITIAL.
*Get data from STPO table based on BOM category and Bill of Material
    SELECT * FROM stpo
             INTO TABLE gt_stpo
             FOR ALL ENTRIES IN gt_stko
             WHERE stlty EQ gt_stko-stlty
             AND stlnr EQ gt_stko-stlnr.
    IF sy-subrc IS INITIAL.
      SORT gt_stpo BY stlty stlnr stlkn.
    ENDIF.
  ENDIF.
  IF gt_stpo IS NOT INITIAL.
*Get the details from MAST table based on Material number
    SELECT * FROM mast
             INTO TABLE gt_mast
             FOR ALL ENTRIES IN gt_stpo
             WHERE matnr EQ gt_stpo-idnrk.
    IF sy-subrc IS INITIAL.
      SORT gt_mast BY matnr werks.
    ENDIF.
  ENDIF.

ENDMETHOD.
ENDCLASS.
