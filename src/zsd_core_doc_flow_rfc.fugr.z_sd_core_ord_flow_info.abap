FUNCTION z_sd_core_ord_flow_info.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_AUFBEREITUNG) TYPE  CHAR1 DEFAULT '2'
*"     VALUE(IV_BELEGTYP) TYPE  VBUK-VBTYP DEFAULT ' '
*"     VALUE(IS_COMWA) TYPE  VBCO6
*"     VALUE(IV_NACHFOLGER) TYPE  CHAR1 DEFAULT 'X'
*"     VALUE(IV_N_STUFEN) TYPE  VBFA-STUFE DEFAULT '50'
*"     VALUE(IV_VORGAENGER) TYPE  CHAR1 DEFAULT 'X'
*"     VALUE(IV_V_STUFEN) TYPE  VBFA-STUFE DEFAULT '50'
*"     VALUE(IV_NO_ACC_DOC) TYPE  CHAR1 DEFAULT SPACE
*"  EXPORTING
*"     VALUE(EV_BELEGTYP_BACK) TYPE  VBUK-VBTYP
*"     VALUE(ES_RETURN) TYPE  BAPIRET2
*"  TABLES
*"      ET_VBFA_TAB STRUCTURE  ZSD_VBFA_ECC
*"----------------------------------------------------------------------
  DATA lt_vbfa TYPE STANDARD TABLE OF vbfa.

  CASE iv_belegtyp.
    WHEN 'V'. " special handling for purchase orders
      SELECT *
                  FROM vbfa
                  WHERE vbeln = @is_comwa-vbeln
                  INTO  CORRESPONDING FIELDS OF TABLE @et_vbfa_tab[].
      IF sy-subrc = 0.
        es_return = VALUE #( type = 'S' ).
      ELSE.
        es_return = VALUE #( type = 'E' message = 'no data found in VBFA' ).
      ENDIF.
    WHEN OTHERS.
      CALL FUNCTION 'RV_ORDER_FLOW_INFORMATION'
        EXPORTING
          aufbereitung  = iv_aufbereitung    " Type of flow editing
          belegtyp      = iv_belegtyp    " Document Category
          comwa         = is_comwa    " Outgoing document for flow processing
          nachfolger    = iv_nachfolger    " Successor display wanted
          n_stufen      = iv_n_stufen    " Number of levels of the successors
          vorgaenger    = iv_vorgaenger   " Predecessor wanted
          v_stufen      = iv_v_stufen    " Number of levels of the predecessors
          no_acc_doc    = iv_no_acc_doc
        IMPORTING
          belegtyp_back = ev_belegtyp_back
        TABLES
          vbfa_tab      = lt_vbfa    " Document flow information
        EXCEPTIONS
          no_vbfa       = 1
          no_vbuk_found = 2
          error_message = 3
          OTHERS        = 4.
      IF sy-subrc <> 0.
        es_return = VALUE bapiret2(
            type       = 'E'
            id         = sy-msgid
            number     = sy-msgno
            message    = 'error in RV_ORDER_FLOW_INFORMATION'
            message_v1 = sy-subrc
        ).
      ELSE.
        MOVE-CORRESPONDING lt_vbfa TO et_vbfa_tab[].
        es_return = VALUE #( type = 'S' ).
      ENDIF.
  ENDCASE.



ENDFUNCTION.
