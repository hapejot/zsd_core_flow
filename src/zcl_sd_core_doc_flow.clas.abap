CLASS zcl_sd_core_doc_flow DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS create
      IMPORTING
        i_vbeln_va      TYPE vbeln_va
      RETURNING
        VALUE(r_result) TYPE REF TO zcl_sd_core_doc_flow.
    DATA mt_flow TYPE STANDARD TABLE OF zsd_core_flow_s WITH DEFAULT KEY.
    METHODS: read_regular_flow,
      read_ekbe,
      read_rbkp.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF mts_map,
             group  TYPE string,
             source TYPE string,
             target TYPE string,
           END OF mts_map.
    TYPES: mtt_mapping TYPE SORTED TABLE OF mts_map WITH UNIQUE  KEY group source.
    CONSTANTS cv_inv_rcpt_noc TYPE string VALUE 'IRT-N' ##NO_TEXT.
    CONSTANTS cv_gds_rcpt_noc TYPE string VALUE 'GRT-N' ##NO_TEXT.
    CONSTANTS cv_purchase_oder_noc TYPE string VALUE 'P-NOC' ##NO_TEXT.
    DATA mv_vbeln_va TYPE vbeln_va.
    CLASS-DATA mt_mapping TYPE mtt_mapping.

ENDCLASS.



CLASS zcl_sd_core_doc_flow IMPLEMENTATION.

  METHOD create.

    CREATE OBJECT r_result.

    r_result->mv_vbeln_va = i_vbeln_va.

    INSERT VALUE #( group = 'VBFA' source = 'C' target = 'O-NOC' ) INTO TABLE mt_mapping.
    INSERT VALUE #( group = 'VBFA' source = 'G' target = 'C-NOC' ) INTO TABLE mt_mapping.
    INSERT VALUE #( group = 'VBFA' source = 'J' target = 'D-NOC' ) INTO TABLE mt_mapping.
    INSERT VALUE #( group = 'VBFA' source = 'M' target = 'I-NOC' ) INTO TABLE mt_mapping.
    INSERT VALUE #( group = 'VBFA' source = 'Q' target = 'PIK-N' ) INTO TABLE mt_mapping.
    INSERT VALUE #( group = 'VBFA' source = 'R' target = 'MOV-N' ) INTO TABLE mt_mapping.
    INSERT VALUE #( group = 'VBFA' source = 'V' target = cv_purchase_oder_noc ) INTO TABLE mt_mapping.
    INSERT VALUE #( group = 'VBFA' source = '+' target = 'ACC-N' ) INTO TABLE mt_mapping.

    INSERT VALUE #( group = 'EKBE' source = 'Q' target = cv_inv_rcpt_noc ) INTO TABLE mt_mapping.
    INSERT VALUE #( group = 'EKBE' source = 'E' target = cv_gds_rcpt_noc ) INTO TABLE mt_mapping.

  ENDMETHOD.

  METHOD read_regular_flow.
    DATA: ls_comwa TYPE vbco6,
          lt_vbfa  TYPE STANDARD TABLE OF vbfa.
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
    LOOP AT mt_flow REFERENCE INTO DATA(lr_flow).
      IF lr_flow->vbtyp_n IS NOT INITIAL.
        lr_flow->vbtyp_n = mt_mapping[ group = 'VBFA' source = lr_flow->vbtyp_n ]-target.
      ENDIF.
      IF lr_flow->vbtyp_v IS NOT INITIAL.
        lr_flow->vbtyp_v = mt_mapping[ group = 'VBFA' source = lr_flow->vbtyp_v ]-target.
      ENDIF.
    ENDLOOP.
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
                          vbtyp_n = mt_mapping[ group = 'EKBE' source = ls_ekbe-bewtp ]-target
                          hlevel = ls_flow-hlevel + 1
                          rfmng = ls_ekbe-menge
                          rfwrt = ls_ekbe-wrbtr
                          erdat = ls_ekbe-cpudt
                          erzet = ls_ekbe-cputm
                          ) TO mt_flow.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD read_rbkp.
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
                          vbtyp_v = 'I-BEU'
                          vbeln = ls_rbkp-belnr
                          posnn = ls_rseg-buzei
                          vbtyp_n = cv_inv_rcpt_noc
                          erdat = ls_rbkp-cpudt
                          erzet = ls_rbkp-cputm
                          ) TO mt_flow.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
