CLASS zcl_sd_core_doc_flow DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS create
      IMPORTING
        iv_doctype      TYPE zsd_vbtyp
        iv_number       TYPE vbeln
      RETURNING
        VALUE(r_result) TYPE REF TO zcl_sd_core_doc_flow.
    CLASS-METHODS class_constructor.
    DATA mt_flow TYPE STANDARD TABLE OF zsd_core_flow_s WITH DEFAULT KEY.
    METHODS: read_regular_flow,
      read_regular_flow_beu,
      read_ekbe IMPORTING iv_group TYPE string DEFAULT 'EKBE',
      read_rbkp,
      read
        RAISING
          zcx_bc_missing_value.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF mts_map,
             group  TYPE string,
             source TYPE string,
             target TYPE string,
           END OF mts_map.
    TYPES: mtt_mapping TYPE SORTED TABLE OF mts_map WITH UNIQUE  KEY group source.
    TYPES: mtt_vbeln_va TYPE STANDARD TABLE OF vbeln_va WITH DEFAULT KEY,
           BEGIN OF lts_row,
             vbeln TYPE vbap-vbeln,
             posnr TYPE vbap-posnr,
             matnr TYPE vbap-matnr,
             abgru TYPE vbap-abgru,
           END OF lts_row,
           mtt_flow       TYPE STANDARD TABLE OF zsd_core_flow_s,
           ty_lt_material TYPE STANDARD TABLE OF lts_row WITH DEFAULT KEY.
    METHODS read_additional_itm_info
      IMPORTING
        iv_vbeln TYPE vbeln_va
      CHANGING
        ct_vbfa  TYPE mtt_flow.
    CONSTANTS cv_inv_rcpt_noc TYPE string VALUE 'IRT-N' ##NO_TEXT.
    CONSTANTS cv_gds_rcpt_noc TYPE string VALUE 'GRT-N' ##NO_TEXT.
    CONSTANTS cv_purchase_oder_noc TYPE string VALUE 'P-NOC' ##NO_TEXT.
    CONSTANTS cv_sales_order_beu TYPE string VALUE 'O-BEU' ##NO_TEXT.
    CONSTANTS cv_invoice_beu TYPE zsd_core_flow_s-vbtyp_v VALUE 'I-BEU' ##NO_TEXT.
    DATA mv_vbeln_va TYPE vbeln_va.
    METHODS get_beu_salesorder_numbers
      RETURNING
        VALUE(rt_result) TYPE mtt_vbeln_va.
    METHODS fix_all_material.
    METHODS fix_material
      IMPORTING
        iv_level        TYPE i
        ir_row          TYPE REF TO zsd_core_flow_s
      RETURNING
        VALUE(rv_matnr) TYPE zsd_core_flow_s-matnr.
    CLASS-DATA: mt_mapping TYPE mtt_mapping.
    CLASS-DATA: BEGIN OF ms_dest,
                  noc TYPE rfcdest,
                  beu TYPE rfcdest,
                END OF ms_dest.
ENDCLASS.



CLASS zcl_sd_core_doc_flow IMPLEMENTATION.


  METHOD class_constructor.

    INSERT VALUE #( group = 'VBFA' source = 'C' target = 'O-NOC' ) INTO TABLE mt_mapping.
    INSERT VALUE #( group = 'VBFA' source = 'G' target = 'C-NOC' ) INTO TABLE mt_mapping.
    INSERT VALUE #( group = 'VBFA' source = 'J' target = 'D-NOC' ) INTO TABLE mt_mapping.
    INSERT VALUE #( group = 'VBFA' source = 'M' target = 'I-NOC' ) INTO TABLE mt_mapping.
    INSERT VALUE #( group = 'VBFA' source = 'Q' target = 'PIK-N' ) INTO TABLE mt_mapping.
    INSERT VALUE #( group = 'VBFA' source = 'R' target = 'MOV-N' ) INTO TABLE mt_mapping.
    INSERT VALUE #( group = 'VBFA' source = 'V' target = cv_purchase_oder_noc ) INTO TABLE mt_mapping.
    INSERT VALUE #( group = 'VBFA' source = '+' target = 'ACC-N' ) INTO TABLE mt_mapping.

    INSERT VALUE #( group = 'VBFA-BEU' source = 'C' target = cv_sales_order_beu ) INTO TABLE mt_mapping.
    INSERT VALUE #( group = 'VBFA-BEU' source = 'G' target = 'C-BEU' ) INTO TABLE mt_mapping.
    INSERT VALUE #( group = 'VBFA-BEU' source = 'J' target = 'D-BEU' ) INTO TABLE mt_mapping.
    INSERT VALUE #( group = 'VBFA-BEU' source = 'M' target = cv_invoice_beu  ) INTO TABLE mt_mapping.
    INSERT VALUE #( group = 'VBFA-BEU' source = 'Q' target = 'PIK-B' ) INTO TABLE mt_mapping.
    INSERT VALUE #( group = 'VBFA-BEU' source = 'R' target = 'MOV-B' ) INTO TABLE mt_mapping.
    INSERT VALUE #( group = 'VBFA-BEU' source = 'V' target = 'P-BEU' ) INTO TABLE mt_mapping.
    INSERT VALUE #( group = 'VBFA-BEU' source = '+' target = 'ACC-B' ) INTO TABLE mt_mapping.

    INSERT VALUE #( group = 'EKBE' source = 'Q' target = cv_inv_rcpt_noc ) INTO TABLE mt_mapping.
    INSERT VALUE #( group = 'EKBE' source = 'E' target = cv_gds_rcpt_noc ) INTO TABLE mt_mapping.

    ms_dest-noc = zcl_sd_core_remote=>for(
                    iv_org           = zcl_sd_core_remote=>mc_system_noc
                    iv_using_trusted = abap_false
                  ).
    ms_dest-beu = zcl_sd_core_remote=>for(
                    iv_org           = zcl_sd_core_remote=>mc_system_beu
                    iv_using_trusted = abap_false
                  ).

  ENDMETHOD.


  METHOD create.
    DATA: lt_vbeln_ex TYPE zbc_dopac_general_range_tt,
          lt_vbeln_im TYPE zbc_dopac_general_range_tt,
          lt_vbfa     TYPE STANDARD TABLE OF vbfa.

    CREATE OBJECT r_result.

    DATA(ls_row) = mt_mapping[ target = iv_doctype ].
    IF ls_row-group = 'VBFA'. " NOC Document

      IF ls_row-source = 'C'. " it's the salesorder, don't look further...
        r_result->mv_vbeln_va = iv_number.
      ELSE.
        CALL FUNCTION 'RV_ORDER_FLOW_INFORMATION'
          EXPORTING
            aufbereitung = '1'
            belegtyp     = ls_row-source
            comwa        = VALUE vbco6( vbeln = iv_number )
          TABLES
            vbfa_tab     = lt_vbfa
          EXCEPTIONS
            OTHERS       = 3.
        IF sy-subrc = 0.
          r_result->mv_vbeln_va = lt_vbfa[ vbtyp_v = 'C' ]-vbelv.
        ENDIF.
      ENDIF.

    ELSEIF ls_row-group = 'VBFA-BEU'. " BEU Document

      lt_vbeln_ex = VALUE #( ( sign = 'I' option = 'EQ' low = iv_number ) ).
      CALL FUNCTION 'Z_SD_CORE_C2_DATA_PRESEL_BEU'
        EXPORTING
          iv_vbtyp      = CONV char4( ls_row-source )
          it_vbeln      = lt_vbeln_ex
        IMPORTING
          et_sales_docs = lt_vbeln_im.
      IF lines( lt_vbeln_im ) > 0.
        r_result->mv_vbeln_va = CONV #( lt_vbeln_im[ 1 ]-low ).
      ENDIF.
    ELSE.

      CLEAR r_result.
    ENDIF.

  ENDMETHOD.


  METHOD fix_all_material.
    LOOP AT mt_flow REFERENCE INTO DATA(lr_row)
            WHERE vbelv IS INITIAL.
      fix_material( iv_level = 1
                    ir_row = lr_row ).
    ENDLOOP.
  ENDMETHOD.


  METHOD fix_material.
    IF ir_row->matnr CO ' X'.
      CLEAR ir_row->matnr.
    ENDIF.
    LOOP AT mt_flow
            REFERENCE INTO DATA(lr_next)
            WHERE vbelv = ir_row->vbeln AND posnv = ir_row->posnn.
      IF ir_row->matnr IS NOT INITIAL.
        lr_next->matnr = ir_row->matnr.
      ENDIF.
      fix_material( iv_level = 1 + iv_level
                    ir_row = lr_next ).
      IF ir_row->matnr IS INITIAL.
        ir_row->matnr = lr_next->matnr.
      ENDIF.
    ENDLOOP.

    IF ir_row->hlevel < iv_level.
      ir_row->hlevel = iv_level.
    ENDIF.
  ENDMETHOD.


  METHOD get_beu_salesorder_numbers.
    LOOP AT mt_flow INTO DATA(ls_flow)  WHERE vbtyp_n = cv_sales_order_beu.
      IF NOT line_exists( rt_result[ table_line = ls_flow-vbeln ] ).
        APPEND ls_flow-vbeln TO rt_result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD read.
    DATA lt_flow LIKE mt_flow.
    DATA: lt_vbkd TYPE STANDARD TABLE OF vbkd.
    IF mv_vbeln_va IS INITIAL.
      RAISE EXCEPTION TYPE zcx_bc_missing_value.
    ENDIF.

    CALL FUNCTION 'Z_SD_CORE_DOC_FLOW_GET_NOC'
      DESTINATION ms_dest-noc
      EXPORTING
        iv_doctype            = 'O-NOC'
        iv_vbeln              = mv_vbeln_va
      TABLES
        et_flow               = mt_flow
      EXCEPTIONS
        communication_failure = 97
        system_failure        = 98
        OTHERS                = 99.

    CALL FUNCTION 'Z_SD_CORE_DOC_FLOW_GET_BEU'
      DESTINATION ms_dest-beu
      EXPORTING
        iv_doctype            = 'O-NOC'
        iv_vbeln              = mv_vbeln_va
      TABLES
        et_flow               = lt_flow
      EXCEPTIONS
        communication_failure = 97
        system_failure        = 98
        OTHERS                = 99.
    APPEND LINES OF lt_flow TO mt_flow.

    LOOP AT get_beu_salesorder_numbers( ) INTO DATA(lv_vbeln).
      SELECT * FROM vbkd
              WHERE vbeln = @lv_vbeln
              APPENDING TABLE @lt_vbkd.
    ENDLOOP.

    LOOP AT mt_flow REFERENCE INTO DATA(lr_flow) WHERE vbtyp_n = cv_sales_order_beu.
      DATA(lr_vbkd) = REF #( lt_vbkd[   vbeln = lr_flow->vbeln
                                        posnr = lr_flow->posnn ] OPTIONAL ).
      IF lr_vbkd IS NOT BOUND.
        lr_vbkd = REF #(  lt_vbkd[  vbeln = lr_flow->vbeln
                                    posnr = '000000' ] OPTIONAL ).
      ENDIF.
      IF lr_vbkd IS BOUND
      AND lr_vbkd->bstkd_e IS NOT INITIAL
      AND lr_vbkd->posex_e IS NOT INITIAL.
        lr_flow->vbelv = lr_vbkd->bstkd_e.
        lr_flow->posnv = lr_vbkd->posex_e.
        lr_flow->vbtyp_v = cv_purchase_oder_noc.
      ENDIF.
    ENDLOOP.


    fix_all_material( ).


    SORT mt_flow BY matnr ASCENDING hlevel ASCENDING .

  ENDMETHOD.


  METHOD read_ekbe.
    LOOP AT mt_flow INTO DATA(ls_flow) WHERE vbtyp_n = cv_purchase_oder_noc.
      SELECT * FROM ekbe
                      WHERE ebeln = @ls_flow-vbeln
                      AND ebelp = @ls_flow-posnn
                      INTO TABLE @DATA(lt_ekbe).
      LOOP AT lt_ekbe INTO DATA(ls_ekbe).
        APPEND VALUE #(   BASE CORRESPONDING #( ls_ekbe )
                          vbelv = ls_ekbe-ebeln
                          posnv = ls_ekbe-ebelp
                          vbtyp_v = CONV #( cv_purchase_oder_noc )
                          vbeln = ls_ekbe-belnr
                          posnn = ls_ekbe-buzei
                          vbtyp_n = mt_mapping[ group = iv_group source = ls_ekbe-bewtp ]-target
                          hlevel = ls_flow-hlevel + 1
                          rfmng = ls_ekbe-menge
                          rfwrt = ls_ekbe-wrbtr
                          erdat = ls_ekbe-cpudt
                          erzet = ls_ekbe-cputm
                          ) TO mt_flow.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD read_additional_itm_info.

    DATA lt_material TYPE ty_lt_material.
    DATA lr_vbfa TYPE REF TO zsd_core_flow_s.

    SELECT vbeln,
            posnr,
            matnr,
            abgru
            FROM vbap
            WHERE vbeln = @iv_vbeln
            INTO TABLE @lt_material.

    LOOP AT lt_material INTO DATA(ls_material).
      LOOP AT ct_vbfa REFERENCE INTO lr_vbfa WHERE vbeln = ls_material-vbeln AND posnn = ls_material-posnr.
        lr_vbfa->matnr = ls_material-matnr.
        lr_vbfa->abgru = ls_material-abgru.
      ENDLOOP.
      LOOP AT ct_vbfa REFERENCE INTO lr_vbfa WHERE vbelv = ls_material-vbeln AND posnv = ls_material-posnr.
        lr_vbfa->matnr = ls_material-matnr.
        lr_vbfa->abgru = ls_material-abgru.
      ENDLOOP.
    ENDLOOP.


  ENDMETHOD.


  METHOD read_rbkp.
    " store new rows locally otherwise they will be interpreted again
    " resulting in an endless loop
    DATA lt_new_rows LIKE mt_flow.

    LOOP AT mt_flow INTO DATA(ls_flow) WHERE vbtyp_n = cv_inv_rcpt_noc.
      SELECT SINGLE * FROM rbkp
              WHERE belnr = @ls_flow-vbeln
              INTO @DATA(ls_rbkp).
      IF sy-subrc = 0.
        SELECT SINGLE * FROM rseg
                  WHERE belnr = @ls_flow-vbeln
                  AND   buzei = @ls_flow-posnn
                  INTO @DATA(ls_rseg).
      ENDIF.
      IF sy-subrc = 0.
        APPEND VALUE #(   BASE CORRESPONDING #( ls_rseg )
                          vbelv = ls_rbkp-xblnr
                          vbtyp_v = cv_invoice_beu
                          vbeln = ls_rbkp-belnr
                          posnn = ls_rseg-buzei
                          vbtyp_n = cv_inv_rcpt_noc
                          erdat = ls_rbkp-cpudt
                          erzet = ls_rbkp-cputm
                          ) TO lt_new_rows.
      ENDIF.
    ENDLOOP.
    APPEND LINES OF lt_new_rows TO mt_flow.
  ENDMETHOD.


  METHOD read_regular_flow.

    DATA: ls_comwa    TYPE vbco6,
          lt_vbfa     TYPE STANDARD TABLE OF vbfa,
          lt_material TYPE TABLE OF lts_row,
          lr_vbfa     TYPE REF TO vbfa.
    ASSERT mv_vbeln_va IS NOT INITIAL.

    ls_comwa-vbeln = mv_vbeln_va.

    CALL FUNCTION 'RV_ORDER_FLOW_INFORMATION'
      EXPORTING
*       aufbereitung  = '2'    " Type of flow editing
        belegtyp = 'C'    " Document Category: Order
        comwa    = ls_comwa    " Outgoing document for flow processing
*       nachfolger    = 'X'    " Successor display wanted
*       n_stufen = '50'    " Number of levels of the successors
*       vorgaenger    = 'X'    " Predecessor wanted
*       v_stufen = '50'    " Number of levels of the predecessors
*       no_acc_doc    = SPACE
*  IMPORTING
*       belegtyp_back =
      TABLES
        vbfa_tab = lt_vbfa    " Document flow information
*  EXCEPTIONS
*       no_vbfa  = 1
*       no_vbuk_found = 2
*       others   = 3
      .
    IF sy-subrc <> 0.
* MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.


    MOVE-CORRESPONDING lt_vbfa TO mt_flow.

    read_additional_itm_info(  EXPORTING iv_vbeln = mv_vbeln_va
                            CHANGING ct_vbfa = mt_flow ).

    LOOP AT mt_flow REFERENCE INTO DATA(lr_flow).
      IF lr_flow->vbtyp_n IS NOT INITIAL.
        lr_flow->vbtyp_n = VALUE #( mt_mapping[ group = 'VBFA' source = lr_flow->vbtyp_n ]-target
                                    DEFAULT |NOC-{ lr_flow->vbtyp_n }| ).

      ENDIF.
      IF lr_flow->vbtyp_v IS NOT INITIAL.
        lr_flow->vbtyp_v = VALUE #( mt_mapping[ group = 'VBFA' source = lr_flow->vbtyp_v ]-target
                                        DEFAULT |BEU-{ lr_flow->vbtyp_n }| ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD read_regular_flow_beu.
    DATA: ls_comwa TYPE vbco6,
          lt_vbfa  TYPE STANDARD TABLE OF vbfa.
    SELECT DISTINCT vbeln FROM vbkd
            WHERE bstkd = @mv_vbeln_va
            INTO TABLE @DATA(lt_beu_orders).

    LOOP AT lt_beu_orders INTO DATA(lv_beu_order_number).


      ls_comwa-vbeln = lv_beu_order_number-vbeln.

      CALL FUNCTION 'RV_ORDER_FLOW_INFORMATION'
        EXPORTING
          belegtyp = 'C'    " Document Category: Order
          comwa    = ls_comwa    " Outgoing document for flow processing
        TABLES
          vbfa_tab = lt_vbfa    " Document flow information
        EXCEPTIONS
          OTHERS   = 3.
      IF sy-subrc <> 0.
* MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.



      MOVE-CORRESPONDING lt_vbfa TO mt_flow.
      read_additional_itm_info(    EXPORTING   iv_vbeln = lv_beu_order_number-vbeln
                                CHANGING    ct_vbfa = mt_flow     ).
      LOOP AT mt_flow REFERENCE INTO DATA(lr_flow).
        IF lr_flow->vbtyp_n IS NOT INITIAL.
          lr_flow->vbtyp_n = VALUE #( mt_mapping[ group = 'VBFA-BEU' source = lr_flow->vbtyp_n ]-target
                                        DEFAULT |BEU-{ lr_flow->vbtyp_n }| ).
        ENDIF.
        IF lr_flow->vbtyp_v IS NOT INITIAL.
          lr_flow->vbtyp_v = VALUE #( mt_mapping[ group = 'VBFA-BEU' source = lr_flow->vbtyp_v ]-target
                                        DEFAULT |BEU-{ lr_flow->vbtyp_v }| ).
        ENDIF.
      ENDLOOP.

    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
