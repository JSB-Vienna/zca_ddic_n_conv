"! <p class="shorttext synchronized" lang="en">Common object: Data conversion</p>
CLASS zcl_ca_conv DEFINITION PUBLIC
                             FINAL
                             CREATE PUBLIC.
* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   i n t e r f a c e s
    INTERFACES:
      if_xo_const_message,
      zif_ca_c_bool.

*   a l i a s e s
    ALIASES:
*     Boolean flags
      c_false              FOR  zif_ca_c_bool~c_false,
      c_true               FOR  zif_ca_c_bool~c_true.

*   c o n s t a n t s
    CONSTANTS:
      "! <p class="shorttext synchronized" lang="en">Pattern for dynamic execution of conversion exit for input</p>
      c_conv_exit_input   TYPE rs38l_fnam VALUE 'CONVERSION_EXIT_&_INPUT' ##no_text,
      "! <p class="shorttext synchronized" lang="en">Pattern for dynamic execution of conversion exit for output</p>
      c_conv_exit_output  TYPE rs38l_fnam VALUE 'CONVERSION_EXIT_&_OUTPUT' ##no_text,
      "! <p class="shorttext synchronized" lang="en">Conversion exit name: ALPHA</p>
      c_cvex_name_alpha   TYPE convexit   VALUE 'ALPHA' ##no_text,
      "! <p class="shorttext synchronized" lang="en">Decimal notation: Blank=4.567,89 / X=4,567.89 / Y=4 567,89</p>
      c_decimal_notations TYPE char3      VALUE ' XY' ##no_text.

*   s t a t i c   m e t h o d s
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Convert BAPI amount into SAP internal value</p>
      "!
      "! @parameter iv_bapi_amount | <p class="shorttext synchronized" lang="en">Amount in BAPI format</p>
      "! @parameter iv_currency    | <p class="shorttext synchronized" lang="en">Currency key</p>
      "! @parameter ev_int_amount  | <p class="shorttext synchronized" lang="en">Converted amount in SAP internal format</p>
      "! @raising   zcx_ca_conv    | <p class="shorttext synchronized" lang="en">Common exception: Conversion failed</p>
      bapi_amount_2_int
        IMPORTING
          iv_bapi_amount TYPE bapicurr_d
          iv_currency    TYPE waers      OPTIONAL
        EXPORTING
          ev_int_amount  TYPE p
        RAISING
          zcx_ca_conv,

      "! <p class="shorttext synchronized" lang="en">Convert 1 / 0 into X / space</p>
      "!
      "! @parameter iv_boolean | <p class="shorttext synchronized" lang="en">Boolean value =&gt; 1 or 0</p>
      "! @parameter rv_flag    | <p class="shorttext synchronized" lang="en">Flag =&gt; X or space</p>
      boolean_2_flag
        IMPORTING
          iv_boolean     TYPE dml_boolean
        RETURNING
          VALUE(rv_flag) TYPE abap_bool,

      "! <p class="shorttext synchronized" lang="en">CLASS_CONSTRUCTOR</p>
      class_constructor,

      "! <p class="shorttext synchronized" lang="en">Execute conversion exit for input or output</p>
      "!
      "! @parameter iv_for_output | <p class="shorttext synchronized" lang="en">1 = Convert for output; 0 = for input</p>
      "! @parameter iv_conv_exit  | <p class="shorttext synchronized" lang="en">Conversion exit name</p>
      "! @parameter io_elem_desc  | <p class="shorttext synchronized" lang="en">Element type description of target field</p>
      "! @parameter iv_input      | <p class="shorttext synchronized" lang="en">Passed value of elementary type</p>
      "! @parameter ev_output     | <p class="shorttext synchronized" lang="en">Converted value of elementary type</p>
      "! @raising   zcx_ca_conv   | <p class="shorttext synchronized" lang="en">Common exception: Conversion failed</p>
      conv_exit
        IMPORTING
          iv_for_output    TYPE dml_boolean              DEFAULT c_true
          iv_conv_exit     TYPE convexit                 DEFAULT c_cvex_name_alpha
          io_elem_desc     TYPE REF TO cl_abap_elemdescr OPTIONAL
          iv_input         TYPE simple
        EXPORTING
          VALUE(ev_output) TYPE simple
        RAISING
          zcx_ca_conv,

      "! <p class="shorttext synchronized" lang="en">Konvertiere eines externen Wertes in SAP internes format</p>
      "!
      "! @parameter iv_ext_value | <p class="shorttext synchronized" lang="en">External value in character format</p>
      "! @parameter iv_currency  | <p class="shorttext synchronized" lang="en">Currency key</p>
      "! @parameter iv_unit      | <p class="shorttext synchronized" lang="en">Unit of measure</p>
      "! @parameter ev_int_value | <p class="shorttext synchronized" lang="en">Value in SAP internal format depending on target field</p>
      "! @raising   zcx_ca_conv  | <p class="shorttext synchronized" lang="en">Common exception: Conversion failed</p>
      ext_2_int
        IMPORTING
          iv_ext_value TYPE csequence
          iv_currency  TYPE waers     OPTIONAL
          iv_unit      TYPE meins     OPTIONAL
        EXPORTING
          ev_int_value TYPE data
        RAISING
          zcx_ca_conv,

      "! <p class="shorttext synchronized" lang="en">Convert X / space into 1 / 0</p>
      "!
      "! @parameter iv_flag    | <p class="shorttext synchronized" lang="en">Flag =&gt; X or space</p>
      "! @parameter rv_boolean | <p class="shorttext synchronized" lang="en">Boolean value =&gt; 1 or 0</p>
      flag_2_boolean
        IMPORTING
          iv_flag           TYPE abap_bool
        RETURNING
          VALUE(rv_boolean) TYPE dml_boolean,

      "! <p class="shorttext synchronized" lang="en">Conversion of a SAP internal value into external format</p>
      "!
      "! @parameter iv_int_value    | <p class="shorttext synchronized" lang="en">SAP internal value of elementary type</p>
      "! @parameter iv_date_format  | <p class="shorttext synchronized" lang="en">Date format -&gt; see domain fixed values</p>
      "! @parameter iv_currency     | <p class="shorttext synchronized" lang="en">Currency</p>
      "! @parameter iv_unit         | <p class="shorttext synchronized" lang="en">Unit of measure</p>
      "! @parameter iv_no_seconds   | <p class="shorttext synchronized" lang="en">1 = Ausgabe der Zeit ohne Sekunden (hh:mm)</p>
      "! @parameter iv_is_for_idoc  | <p class="shorttext synchronized" lang="en">1 = Empfängerfeld ist ein IDoc-Feld (hat eigene Konv.regeln)</p>
      "! @parameter iv_is_bapi_amnt | <p class="shorttext synchronized" lang="en">1 = Wert ist ein BAPI-Betrag (andere Anzahl Dezimalstellen)</p>
      "! @parameter ev_ext_value    | <p class="shorttext synchronized" lang="en">Konvertierter Wert in externem Format, z.B. für BSP,BDC,IDoc</p>
      "! @raising   zcx_ca_conv     | <p class="shorttext synchronized" lang="en">Common exception: Conversion failed</p>
      int_2_ext
        IMPORTING
          iv_int_value    TYPE data
          iv_date_format  TYPE xudatfm      OPTIONAL
          iv_currency     TYPE waers        OPTIONAL
          iv_unit         TYPE meins        OPTIONAL
          iv_no_seconds   TYPE dml_boolean  DEFAULT c_false
          iv_is_for_idoc  TYPE dml_boolean  DEFAULT c_false
          iv_is_bapi_amnt TYPE dml_boolean  DEFAULT c_false
        EXPORTING
          ev_ext_value    TYPE csequence
        RAISING
          zcx_ca_conv,

      "! <p class="shorttext synchronized" lang="en">Convert SAP internal amount into external value</p>
      "!
      "! @parameter iv_int_value | <p class="shorttext synchronized" lang="en">SAP internal amount</p>
      "! @parameter iv_currency  | <p class="shorttext synchronized" lang="en">Currency key</p>
      "! @parameter ev_ext_value | <p class="shorttext synchronized" lang="en">Converted amount in external format</p>
      "! @raising   zcx_ca_conv  | <p class="shorttext synchronized" lang="en">Common exception: Conversion failed</p>
      int_amount_2_ext
        IMPORTING
          iv_int_value TYPE data
          iv_currency  TYPE waers OPTIONAL
        EXPORTING
          ev_ext_value TYPE csequence
        RAISING
          zcx_ca_conv.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.
*   a l i a s e s
    ALIASES:
*     Message types
      c_msgty_e            FOR  if_xo_const_message~error,
      c_msgty_i            FOR  if_xo_const_message~info,
      c_msgty_s            FOR  if_xo_const_message~success,
      c_msgty_w            FOR  if_xo_const_message~warning.

*   s t a t i c   a t t r i b u t e s
    CLASS-DATA:
      "! <p class="shorttext synchronized" lang="en">Default settings of user</p>
      gs_user_def TYPE usdefaults.
ENDCLASS.



CLASS zcl_ca_conv IMPLEMENTATION.

  METHOD bapi_amount_2_int.
    "-----------------------------------------------------------------*
    "   Conversion of a BAPI amount value into an internal (packed) value
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      ls_return     TYPE bapireturn.

    "Check if the passed value is an elementary type
    DATA(lo_elem_desc) =
             CAST cl_abap_elemdescr(
                            cl_abap_typedescr=>describe_by_data( ev_int_amount ) ).

    "Convert into an internal value AND ...
    CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_INTERNAL'
      EXPORTING
        currency             = iv_currency
        amount_external      = iv_bapi_amount
        max_number_of_digits = lo_elem_desc->output_length
      IMPORTING
        amount_internal      = ev_int_amount
        return               = ls_return.
    DATA(lx_error) =
           CAST zcx_ca_conv(
                  zcx_ca_error=>create_exception(
                      iv_excp_cls = zcx_ca_conv=>c_zcx_ca_conv
                      iv_function = 'BAPI_CURRENCY_CONV_TO_INTERNAL'  ##no_text
                      is_return   = VALUE #( type       = ls_return-type
                                             id         = ls_return-code(2)
                                             number     = ls_return-code+2(3)
                                             message_v1 = ls_return-message_v1
                                             message_v2 = ls_return-message_v2
                                             message_v3 = ls_return-message_v3
                                             message_v4 = ls_return-message_v4 ) ) ).
    IF lx_error IS BOUND.
      RAISE EXCEPTION lx_error.
    ENDIF.
  ENDMETHOD.                    "bapi_amount_2_int


  METHOD boolean_2_flag.
    "-----------------------------------------------------------------*
    "   Conversion of a boolean value (0/1) into a flag (blank/X)
    "-----------------------------------------------------------------*
    rv_flag = abap_false.
    IF iv_boolean EQ c_true.
      rv_flag = abap_true.
    ENDIF.
  ENDMETHOD.                    "boolean_2_flag


  METHOD class_constructor.
    "-----------------------------------------------------------------*
    "   Class constructor
    "-----------------------------------------------------------------*
    "Get current user and its defaults of user master
    DATA(lv_user) = cl_abap_syst=>get_user_name( ).
    CALL FUNCTION 'SUSR_USER_DEFAULTS_GET'
      EXPORTING
        user_name     = lv_user
      IMPORTING
        user_defaults = gs_user_def.
  ENDMETHOD.                    "class_constructor


  METHOD conv_exit.
    "-----------------------------------------------------------------*
    "   Dynamic execution of standard conversion exits
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lx_error        TYPE REF TO cx_root,
      lx_conv         TYPE REF TO zcx_ca_conv,
      lo_elem_desc    TYPE REF TO cl_abap_elemdescr,
      lv_fm_conv_exit TYPE rs38l_fnam,
      lv_input        TYPE c LENGTH 4000,
      lv_output_len   TYPE i.

    "Get description of output field
    TRY.
        IF io_elem_desc IS BOUND.
          lo_elem_desc = io_elem_desc.
        ELSE.
          lo_elem_desc ?= cl_abap_typedescr=>describe_by_data( ev_output ).
        ENDIF.

      CATCH cx_root INTO lx_error.
        lx_conv = CAST #( zcx_ca_error=>create_exception(
                                      iv_excp_cls = zcx_ca_conv=>c_zcx_ca_conv
                                      iv_class    = 'ZCL_CA_CONV'
                                      iv_method   = 'CONV_EXIT'
                                      ix_error    = lx_error ) ) ##no_text.
        IF lx_conv IS BOUND.
          RAISE EXCEPTION lx_conv.
        ENDIF.
    ENDTRY.

    "Is it for output or input
    CASE iv_for_output.
      WHEN c_false.
        lv_fm_conv_exit = c_conv_exit_input.
      WHEN OTHERS.
        lv_fm_conv_exit = c_conv_exit_output.
    ENDCASE.

    "Complete conversion exit name
    lv_fm_conv_exit = replace( val  = lv_fm_conv_exit
                               sub  = '&' ##no_text
                               with = iv_conv_exit
                               occ  = 1 ).

    "In case of strings the length is zero. Use then length of input value.
    IF lo_elem_desc->output_length EQ 0.
      lv_output_len = strlen( iv_input ).
    ELSE.
      lv_output_len = lo_elem_desc->output_length.
    ENDIF.

    "The ALPHA conversion exit raises a dump if the input value is LONGER than
    "output field. To avoid this the value is here shortened. Since the ALPHA
    "exit expect only character values (no strings!) the input length is
    "equal to output length (in opposite to a packed field). So we can compare
    "with the output length.
    IF iv_conv_exit       EQ c_cvex_name_alpha AND
       strlen( iv_input ) GT lv_output_len.
      lv_input = iv_input(lv_output_len).
    ELSE.
      lv_input = iv_input.
    ENDIF.

    TRY.
        CALL FUNCTION lv_fm_conv_exit
          EXPORTING
            input         = lv_input
          IMPORTING
            output        = ev_output
          EXCEPTIONS
            error_message = 1
            OTHERS        = 2.
        IF sy-subrc NE 0.
          lx_conv = CAST #( zcx_ca_error=>create_exception(
                                    iv_excp_cls = zcx_ca_conv=>c_zcx_ca_conv
                                    iv_function = lv_fm_conv_exit
                                    iv_subrc    = sy-subrc ) ) ##no_text.
          IF lx_conv IS BOUND.
            RAISE EXCEPTION lx_conv.
          ENDIF.
        ENDIF.

      CATCH cx_sy_dyn_call_illegal_func INTO lx_error.
        "Function module does not exist
        lx_conv = CAST #( zcx_ca_error=>create_exception(
                                     iv_excp_cls = zcx_ca_conv=>c_zcx_ca_conv
                                     iv_function = lv_fm_conv_exit
                                     ix_error    = lx_error ) ) ##no_text.
        IF lx_conv IS BOUND.
          RAISE EXCEPTION lx_conv.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "conv_exit


  METHOD ext_2_int.
    "-----------------------------------------------------------------*
    "   Conversion of an external value into an internal value
    "-----------------------------------------------------------------*
    "Local constant
    CONSTANTS:
      lc_rsdynss0_gl       TYPE char50 VALUE '(RSDYNSS0)GL'.

    "Local data definitions
    DATA:
      lx_error     TYPE REF TO cx_root,
      lx_conv      TYPE REF TO zcx_ca_conv,
      lo_type_desc TYPE REF TO cl_abap_typedescr,
      lr_ext_value TYPE REF TO data,
      ls_error     TYPE rsconverr,
      ls_convert   TYPE rsconvert,
      lv_date      TYPE sydatum,
      lv_subrc     TYPE sysubrc,
      lv_decimals  TYPE i,
*     Only to set SYST-MSGx variables for exception call
      lv_msg       TYPE bapi_msg.

    FIELD-SYMBOLS:
      <ls_rsdynss0_gl>  TYPE data,
      <lv_rsdynss0_fld> TYPE data,
      <lv_ext_value>    TYPE clike,
      <lv_dec_nota>     TYPE char1.

* for completion of this method use class CL_ABAP_DECFLOAT for type f values

    "If no external value is transferred clear result and leave
    CLEAR ev_int_value.
    IF iv_ext_value IS INITIAL.
      RETURN.
    ENDIF.

    "Check if the receiving field is an elementary type
    lo_type_desc = cl_abap_typedescr=>describe_by_data( ev_int_value ).
    IF lo_type_desc->kind NE cl_abap_typedescr=>kind_elem.
      "Parameter &1 is not of an elementary type
      RAISE EXCEPTION TYPE zcx_ca_conv
        EXPORTING
          textid   = zcx_ca_conv=>param_is_not_elem
          mv_msgty = c_msgty_e
          mv_msgv1 = 'EV_INT_VALUE' ##no_text.
    ENDIF.

    "Cast description to elemntary type to get more detailed informations
    DATA(lo_elem_desc) = CAST cl_abap_elemdescr( lo_type_desc ).

    "Check if the passed inbound field is an elementary type
    lo_type_desc = cl_abap_typedescr=>describe_by_data( iv_ext_value ).
    IF lo_type_desc->kind NE lo_type_desc->kind_elem.
      "Parameter &1 is not of an elementary type
      RAISE EXCEPTION TYPE zcx_ca_conv
        EXPORTING
          textid   = zcx_ca_conv=>param_is_not_elem
          mv_msgty = c_msgty_e
          mv_msgv1 = 'IV_EXT_VALUE' ##no_text.
    ENDIF.

    "If the inbound value is of type string create a character field in the
    "used length. Otherwise later used DESCRIBE statement dumps.
    IF lo_type_desc->type_kind NE lo_type_desc->typekind_string.
      ASSIGN iv_ext_value TO <lv_ext_value>.

    ELSE.
      TRY.
          DATA(lv_len) = strlen( iv_ext_value ).
          CREATE DATA lr_ext_value TYPE c LENGTH lv_len.
          ASSIGN lr_ext_value->* TO <lv_ext_value>.
          <lv_ext_value> = iv_ext_value.

        CATCH cx_sy_create_data_error INTO lx_error.
          lx_conv = CAST #( zcx_ca_error=>create_exception(
                                         iv_excp_cls = zcx_ca_conv=>c_zcx_ca_conv
                                         iv_class    = 'ZCL_CA_CONV'
                                         iv_method   = 'EXT_2_INT' ##no_text
                                         ix_error    = lx_error ) ).
          IF lx_conv IS BOUND.
            RAISE EXCEPTION lx_conv.
          ENDIF.
      ENDTRY.
    ENDIF.

    IF lo_elem_desc->is_ddic_type( ) EQ lo_elem_desc->true.
      "Call method not functional to be able to catch the exceptions
      CALL METHOD lo_elem_desc->get_ddic_field
        RECEIVING
          p_flddescr   = DATA(ls_dfies)
        EXCEPTIONS
          not_found    = 1
          no_ddic_type = 2
          OTHERS       = 3.
      IF sy-subrc NE 0.
        lx_conv = CAST #( zcx_ca_error=>create_exception(
                                       iv_excp_cls = zcx_ca_conv=>c_zcx_ca_conv
                                       iv_class    = 'CL_ABAP_ELEMDESCR'
                                       iv_method   = 'GET_DDIC_FIELD' ##no_text
                                       iv_subrc    = sy-subrc ) ).
        IF lx_conv IS BOUND.
          RAISE EXCEPTION lx_conv.
        ENDIF.
      ENDIF.

      ls_convert-decimals   = ls_dfies-decimals.
      ls_convert-dddecimals = ls_dfies-decimals.
      ls_convert-length     = ls_dfies-intlen.
      ls_convert-olength    = ls_dfies-outputlen.
      ls_convert-type       = ls_dfies-inttype.
      ls_convert-convexit   = ls_dfies-convexit.
      ls_convert-lower      = ls_dfies-lowercase.
      "Set "with sign" otherwise negative values will not be accepted
      ls_convert-sign       = ls_dfies-sign.

    ELSE.
      "Set values from element description
      ls_convert-type       = lo_elem_desc->type_kind.
      ls_convert-length     = lo_elem_desc->length.
      ls_convert-olength    = lo_elem_desc->output_length.
      ls_convert-decimals   = lo_elem_desc->decimals.
      "Set 'with sign' otherwise negative values will not be accepted
      ls_convert-sign       = abap_true.
    ENDIF.

    "Initialize target field
    CLEAR ev_int_value.

    "Convert some types in another way
    TRY.
        CASE ls_convert-type.
          WHEN lo_elem_desc->typekind_string.
            "Internal type is   s t r i n g
            ev_int_value = <lv_ext_value>.
            IF lo_elem_desc->is_ddic_type( ) EQ lo_elem_desc->true AND
               ls_convert-lower              EQ abap_false.
              TRANSLATE ev_int_value TO UPPER CASE.
            ENDIF.
            "Leave method after successful conversion
            RETURN.

          WHEN lo_elem_desc->typekind_date.
            "Internal type is   d a t e
            "Check at first, if it is already a valid date
            lv_date = <lv_ext_value>.
            CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
              EXPORTING
                date                      = lv_date
              EXCEPTIONS
                plausibility_check_failed = 1
                OTHERS                    = 2.
            "If yes, return external value
            IF sy-subrc EQ 0.
              ev_int_value = <lv_ext_value>.
              "Otherwise convert it
            ELSE.
              cl_abap_datfm=>conv_date_ext_to_int(
                                             EXPORTING
                                               im_datext = <lv_ext_value>
                                             IMPORTING
                                               ex_datint = ev_int_value ).
            ENDIF.
            "Leave method after successful conversion
            RETURN.

          WHEN lo_elem_desc->typekind_time.
            "Internal type is   t i m e
            cl_abap_timefm=>conv_time_ext_to_int(
                                            EXPORTING
                                              time_ext = <lv_ext_value>
                                            IMPORTING
                                              time_int = ev_int_value ).
            "Leave method after successful conversion
            RETURN.
        ENDCASE.

      CATCH cx_abap_datfm_no_date
            cx_abap_datfm_invalid_date
            cx_abap_datfm_format_unknown
            cx_abap_datfm_ambiguous
            cx_abap_timefm_invalid       INTO lx_error.
        lx_conv = CAST #( zcx_ca_error=>create_exception(
                                       iv_excp_cls = zcx_ca_conv=>c_zcx_ca_conv
                                       iv_class    = 'ZCL_CA_CONV'
                                       iv_method   = 'EXT_2_INT' ##no_text
                                       ix_error    = lx_error ) ).
        IF lx_conv IS BOUND.
          RAISE EXCEPTION lx_conv.
        ENDIF.
    ENDTRY.

    "Get decimals of currency
    IF iv_currency IS NOT INITIAL.
      SELECT SINGLE currdec INTO  lv_decimals
                            FROM  tcurx
                            WHERE currkey EQ iv_currency.
      IF sy-subrc EQ 0.
        ls_convert-decimals = lv_decimals.
      ELSE.
        ls_convert-decimals = 2.
      ENDIF.
    ENDIF.

    "Set decimals
    ls_convert-quan_unit = iv_unit.

    IF ls_convert-clength IS INITIAL.
      PERFORM ileng_2_cleng IN PROGRAM rsdynss0
                            USING    ls_convert-type
                                     ls_convert-length
                            CHANGING ls_convert-clength.
    ENDIF.

    "If conversion exit is defined, execute it by ourself,
    "because the ALPHA exit has special need.
    IF ls_convert-convexit IS NOT INITIAL.
      conv_exit(
            EXPORTING
              iv_for_output = c_false
              iv_conv_exit  = ls_convert-convexit
              io_elem_desc  = lo_elem_desc
              iv_input      = <lv_ext_value>
            IMPORTING
              ev_output     = ev_int_value ).

    ELSE.
      "Call standard subroutine for conversion
      PERFORM convert_ex_2_in IN PROGRAM rsdynss0
                              USING    ls_convert
                                       <lv_ext_value>
                              CHANGING lv_subrc
                                       ls_error
                                       ev_int_value.
      IF lv_subrc     NE 0        AND
         ev_int_value IS INITIAL.
        CASE ls_convert-type.
          WHEN cl_abap_typedescr=>typekind_packed.
            "P a c k e d   v a l u e s
            "SPACE = 1.234.567,89 (= e. g. DE);
            "X     = 1,234,567.89 (= e. g. US);
            "Y     = 1 234 567,89 (no country defined for it in T005X)
            "These three decimal notations are possible, but only the values
            "SPACE and X are maintained in T005X (see description of SET
            "COUNTRY). So Y is skipped during this handling.
            DO 3 TIMES.
              DATA(lv_off) = sy-index - 1.
              ASSIGN c_decimal_notations+lv_off(1) TO <lv_dec_nota>.
              IF <lv_dec_nota> EQ gs_user_def-dcpfm.
                "This case was already tried before and leads to an error
                CONTINUE.
              ENDIF.
              "Set another country to force the use of another decimal notation and ...
              CASE <lv_dec_nota>.
                WHEN space.
                  SET COUNTRY 'DE' ##no_text.
                WHEN 'X' ##no_text.
                  SET COUNTRY 'US' ##no_text.
                WHEN 'Y' ##no_text.
                  CONTINUE.                " no country known
              ENDCASE.
              "... and try again.
              "Therefore a value in the conversion program must be initialized.
              "Otherwise the masks of the first try will be used again.
              ASSIGN (lc_rsdynss0_gl) TO <ls_rsdynss0_gl>.
              IF sy-subrc EQ 0.
                DO 4 TIMES.
                  ASSIGN COMPONENT sy-index OF STRUCTURE <ls_rsdynss0_gl>
                                                      TO <lv_rsdynss0_fld>.
                  CLEAR <lv_rsdynss0_fld>.
                ENDDO.
              ENDIF.
              CLEAR: ls_error,
                     lv_subrc.
              PERFORM convert_ex_2_in IN PROGRAM rsdynss0
                                      USING    ls_convert
                                               <lv_ext_value>
                                      CHANGING lv_subrc
                                               ls_error
                                               ev_int_value.
              IF lv_subrc EQ 0.
                EXIT.                      " leave if the conversion was successful
              ENDIF.
            ENDDO.
            "Reset formatting as defined in user master
            SET COUNTRY space.
        ENDCASE.
      ENDIF.

      IF lv_subrc EQ 0.
        IF lo_elem_desc->is_ddic_type( ) EQ lo_elem_desc->true          AND
           ls_convert-type               EQ lo_elem_desc->typekind_char AND
           ls_convert-lower              EQ abap_false.
          TRANSLATE ev_int_value TO UPPER CASE.
        ENDIF.

        IF iv_currency           IS NOT INITIAL                   AND
           ls_convert-type       EQ lo_elem_desc->typekind_packed AND
           ls_convert-dddecimals NE 0                             AND
           ls_convert-decimals   EQ 0.
          ev_int_value = ev_int_value / ( 10 ** ls_convert-dddecimals ).
        ENDIF.
      ENDIF.

      IF lv_subrc NE 0.
        CASE lv_subrc.
          WHEN 1.
            "Entry is not numeric
            MESSAGE s738(db) WITH ls_error-ill_token INTO lv_msg.
          WHEN 2.
            "Too many decimal places (maximum &)
            MESSAGE s739(db) WITH ls_convert-decimals
                                  ls_error-decimals INTO lv_msg.
          WHEN 3.
            "Specify the sign either at the beginning or at the end
            MESSAGE s740(db) INTO lv_msg.
          WHEN 4.
            "Correct the distance (&1) between "&2" and "&2" or "&2" and "&3"
            MESSAGE s741(db) WITH ls_error-dist_1000
                                  ls_error-tdelimiter
                                  ls_error-ddelimiter INTO lv_msg.
          WHEN 5.
            "Entry is too long: Only & digits are allowed in the whole number part
            MESSAGE s744(db) WITH ls_error-digits_max
                                  ls_error-digits_inp INTO lv_msg.
          WHEN 6.
            "Signs are not allowed here
            MESSAGE s745(db) INTO lv_msg.
          WHEN 7.
            "Entered value is too large (maximum &2)
            MESSAGE s746(db) WITH ls_error-input
                                  ls_error-max INTO lv_msg.
          WHEN 8.
            "Entered value is too small (maximum &2)
            MESSAGE s747(db) WITH ls_error-input
                                  ls_error-min INTO lv_msg.
          WHEN 9.
            "Invalid date format. Please enter date in the format &1
            MESSAGE s748(db) WITH ls_error-date_mask
                                  ls_error-date_delim INTO lv_msg.
          WHEN 10.
            "Invalid date: Please enter date in the format &1
            MESSAGE s749(db) WITH ls_error-date_mask
                                  ls_error-date_delim INTO lv_msg.
          WHEN 11 ##number_ok.
            "Invalid time format. Please enter time in the format &1
            MESSAGE s751(db) WITH 'HH:MM:SS' ':' INTO lv_msg.
          WHEN 12 ##number_ok.
            "Invalid time: Please enter time in the format &1
            MESSAGE s753(db) WITH 'HH:MM:SS' ':' INTO lv_msg.
          WHEN 13 ##number_ok.
            "Invalid character "&" in hexadecimal field
            MESSAGE s754(db) WITH ls_error-ill_token INTO lv_msg.
          WHEN 14 ##number_ok.
            "You cannot begin the entry with the exponent
            MESSAGE s841(db) WITH ls_error-tdelimiter
                                  ls_error-ddelimiter INTO lv_msg.
          WHEN 15 ##number_ok.
            "Exponent empty
            MESSAGE s842(db) WITH ls_error-tdelimiter
                                  ls_error-ddelimiter INTO lv_msg.
          WHEN 16 ##number_ok.
            "'&1', but no decimal places
            MESSAGE s843(db) WITH ls_error-ddelimiter INTO lv_msg.
          WHEN 17 ##number_ok.
            "Invalid exponent
            MESSAGE s844(db) WITH ls_error-tdelimiter
                                  ls_error-ddelimiter INTO lv_msg.
          WHEN 18 ##number_ok.
            "'&2' after 'E'
            MESSAGE s845(db) WITH ls_error-tdelimiter
                                  ls_error-ddelimiter INTO lv_msg.
          WHEN 19 ##number_ok.
            "Exponent too large
            MESSAGE s846(db) WITH ls_error-expomax
                                  ls_error-ddelimiter INTO lv_msg.
          WHEN 20 ##number_ok.
            "Date is not valid -> send message of function module
            MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_msg.
          WHEN OTHERS.
            "Conversion error
            MESSAGE s755(db) INTO lv_msg.
        ENDCASE.

        lx_conv = CAST #( zcx_ca_error=>create_exception(
                                       iv_function   = 'RSDYNSS0'
                                       iv_subroutine = 'CONVERT_EX_2_IN' ##no_text
                                       iv_subrc      = lv_subrc ) ).
        IF lx_conv IS BOUND.
          RAISE EXCEPTION lx_conv.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "ext_2_int


  METHOD flag_2_boolean.
    "-----------------------------------------------------------------*
    "   Conversion of a flag (blank/X) into a boolean value (0/1).
    "   Input for TRUE can be: X, x, Y, J, j. Anything else is
    "   returned as FALSE.
    "-----------------------------------------------------------------*
    IF iv_flag CA 'XxYJj' ##no_text.
      rv_boolean = c_true.
    ELSE.
      rv_boolean = c_false.
    ENDIF.
  ENDMETHOD.                    "flag_2_boolean


  METHOD int_2_ext.
    "-----------------------------------------------------------------*
    "   Conversion of an internal value into an external appearance
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lx_error       TYPE REF TO cx_root,
      lx_conv        TYPE REF TO zcx_ca_conv,
      lv_convexit    TYPE rs38l_fnam,
      lv_date_format TYPE xudatfm,
      lv_offs_sign   TYPE syfdpos,
      lv_amount      TYPE wertv9,
      lv_subrc_write TYPE sysubrc,
      lv_char255     TYPE c LENGTH 255.

* for completion of this method use class CL_ABAP_DECFLOAT for type f values

    "Check if the passed value is an elementary type
    DATA(lo_type_desc) = cl_abap_typedescr=>describe_by_data( iv_int_value ).
    IF lo_type_desc->kind NE cl_abap_typedescr=>kind_elem.
      "Parameter &1 has not an elementary type
      RAISE EXCEPTION TYPE zcx_ca_conv
        EXPORTING
          textid   = zcx_ca_conv=>param_is_not_elem
          mv_msgty = c_msgty_e
          mv_msgv1 = 'IV_INT_VALUE' ##no_text.
    ENDIF.

    "Cast description to elemntary type to get more detailed informations
    DATA(lo_elem_desc) = CAST cl_abap_elemdescr( lo_type_desc ).
    IF lo_elem_desc->is_ddic_type( ) EQ lo_elem_desc->true.
      "Get DDIC description
      DATA(ls_dfies) = lo_elem_desc->get_ddic_field( ).
    ENDIF.

    "Initialize target field
    CLEAR ev_ext_value.

    "Use conversion exit if exist
    IF ls_dfies-convexit IS NOT INITIAL.
      conv_exit(
            EXPORTING
              iv_conv_exit = ls_dfies-convexit
              iv_input     = iv_int_value
            IMPORTING
              ev_output    = ev_ext_value ).

      "Use  e d i t   m a s k   if exist
    ELSEIF lo_elem_desc->edit_mask IS NOT INITIAL                 AND
           lo_elem_desc->type_kind NE lo_elem_desc->typekind_date AND
           lo_elem_desc->type_kind NE lo_elem_desc->typekind_time.
      WRITE iv_int_value USING EDIT MASK lo_elem_desc->edit_mask
                                      TO ev_ext_value.
      lv_subrc_write = sy-subrc.

    ELSE.
      "Prepare external value depending on the internal data type
      CASE lo_elem_desc->type_kind.
        WHEN lo_elem_desc->typekind_date.
          "internal type -  D a t e
          "IDocs use the SAP internal format as standard format - no need to convert
          IF iv_is_for_idoc EQ c_true.
            ev_ext_value = iv_int_value.

          ELSE.
            "Use user setting for date conversion if no format was passed
            IF iv_date_format IS INITIAL.
              lv_date_format = gs_user_def-datfm.
            ELSE.
              lv_date_format = iv_date_format.
            ENDIF.

            TRY.
                cl_abap_datfm=>conv_date_int_to_ext(
                                              EXPORTING
                                                im_datint    = iv_int_value
                                                im_datfmdes  = lv_date_format
                                              IMPORTING
                                                ex_datext    = ev_ext_value ).
                "There is a problem, didn't know what but the method before
                "returns no value. So we do this workaround.
                IF ev_ext_value IS INITIAL.
                  IF lv_date_format CA '23'.                "#EC DATFM
                    WRITE iv_int_value MM/DD/YYYY TO ev_ext_value. "#EC DATFM
                  ELSE.
                    WRITE iv_int_value DD/MM/YYYY TO ev_ext_value. "#EC DATFM
                  ENDIF.
                ENDIF.
                lv_subrc_write = sy-subrc.

              CATCH cx_abap_datfm_format_unknown INTO lx_error.
                lx_conv = CAST #( zcx_ca_error=>create_exception(
                                                iv_class    = 'CL_ABAP_DATFM'
                                                iv_method   = 'CONV_DATE_INT_TO_EXT'
                                                ix_error    = lx_error ) ) ##no_text.
                IF lx_conv IS BOUND.
                  RAISE EXCEPTION lx_conv.
                ENDIF.
            ENDTRY.
          ENDIF.

          "internal type -  T i m e
        WHEN lo_elem_desc->typekind_time.
          IF iv_is_for_idoc EQ c_true.
            ev_ext_value = iv_int_value.

          ELSE.
            DATA(lv_no_seconds) = boolean_2_flag( iv_no_seconds ).

            TRY.
                "Value ENVIRONMENT use the WRITE-statement with additon
                "environment TIME FORMAT.
                "format_according_to = cl_abap_timefm=>user  is defined, but not allowed
                cl_abap_timefm=>conv_time_int_to_ext(
                                EXPORTING
                                  time_int            = iv_int_value
                                  without_seconds     = lv_no_seconds
                                  format_according_to = cl_abap_timefm=>environment
                                IMPORTING
                                  time_ext            = DATA(lv_time_conv) ).
                ev_ext_value = lv_time_conv.

              CATCH cx_parameter_invalid_range INTO lx_error.
                lx_conv = CAST #( zcx_ca_error=>create_exception(
                                                 iv_class    = 'CL_ABAP_TIMEFM'
                                                 iv_method   = 'CONV_TIME_INT_TO_EXT'
                                                 ix_error    = lx_error ) ) ##no_text.
                IF lx_conv IS BOUND.
                  RAISE EXCEPTION lx_conv.
                ENDIF.
            ENDTRY.
          ENDIF.

        WHEN lo_elem_desc->typekind_num.
          "internal type -  n u m e r i c    c h a r a c t e r
          IF iv_is_for_idoc EQ c_true.
            ev_ext_value = iv_int_value.
          ELSE.
            conv_exit(
                  EXPORTING
                    iv_input  = iv_int_value
                  IMPORTING
                    ev_output = ev_ext_value ).
          ENDIF.

        WHEN lo_elem_desc->typekind_packed.
          "internal type -  p a c k e d   v a l u e
          CASE ls_dfies-datatype.
            WHEN 'CURR' ##no_text.
              "C u r r e n c y
              int_amount_2_ext(
                          EXPORTING
                            iv_int_value = iv_int_value
                            iv_currency  = iv_currency
                          IMPORTING
                            ev_ext_value = lv_char255 ).

            WHEN 'QUAN' ##no_text.
              "Q u a n t i t y
              IF iv_unit IS INITIAL.
                WRITE iv_int_value TO lv_char255.           "#EC *

              ELSE.
                "Check existence of the unit
                SELECT COUNT( * ) FROM  t006             "#EC CI_BYPASS
                                  WHERE msehi EQ iv_unit.
                IF sy-dbcnt NE 1.
                  "Parameter '&1' has invalid value '&2'
                  RAISE EXCEPTION TYPE zcx_ca_conv
                    EXPORTING
                      textid   = zcx_ca_conv=>param_invalid
                      mv_msgty = c_msgty_e
                      mv_msgv1 = 'IV_UNIT' ##no_text
                      mv_msgv2 = CONV #( iv_unit ).

                ELSE.
                  WRITE iv_int_value UNIT iv_unit
                                       TO lv_char255.
                ENDIF.
              ENDIF.
              lv_subrc_write = sy-subrc.

            WHEN OTHERS.
              CASE iv_is_bapi_amnt.
                WHEN c_false.
                  IF lo_elem_desc->decimals IS NOT INITIAL.
                    WRITE iv_int_value DECIMALS lo_elem_desc->decimals
                                             TO lv_char255.
                  ELSE.
                    WRITE iv_int_value TO lv_char255.       "#EC *
                  ENDIF.
                  lv_subrc_write = sy-subrc.

                WHEN c_true.
                  "A BAPI amount value (has 4 decimals) is also character value
                  "Convert BAPI amount into an internal value AND ...
                  bapi_amount_2_int(
                              EXPORTING
                                iv_bapi_amount = iv_int_value
                                iv_currency    = iv_currency
                              IMPORTING
                                ev_int_amount  = lv_amount ).

                  "... then reconvert into an "normal" external value
                  int_amount_2_ext(
                              EXPORTING
                                iv_int_value = lv_amount
                                iv_currency  = iv_currency
                              IMPORTING
                                ev_ext_value = lv_char255 ).
              ENDCASE.
          ENDCASE.

          SHIFT lv_char255 LEFT DELETING LEADING space.
          ev_ext_value = lv_char255.

        WHEN lo_elem_desc->typekind_float OR   " Floating point
             lo_elem_desc->typekind_int1  OR
             lo_elem_desc->typekind_int2  OR
             lo_elem_desc->typekind_int.
          "internal type -  f l o a t   a n d   i n t e g e r
          "WRITE these data types to string fields is not allowed, so
          "take the way via char-field
          WRITE iv_int_value TO lv_char255.                 "#EC *
          lv_subrc_write = sy-subrc.
          SHIFT lv_char255 LEFT DELETING LEADING space.
          ev_ext_value = lv_char255.

          "Not allowed types
        WHEN lo_elem_desc->typekind_hex      OR   " hexadecimal
             lo_elem_desc->typekind_xstring.      " hex string
          "Parameter '&1' has invalid value '&2'
          RAISE EXCEPTION TYPE zcx_ca_conv
            EXPORTING
              textid   = zcx_ca_conv=>param_invalid
              mv_msgty = c_msgty_e
              mv_msgv1 = 'IV_INT_VALUE' ##no_text
              mv_msgv2 = 'Types HEX and XSTRING are not allowed'(u01).

          "Other internal types - mostly character
        WHEN OTHERS.
          ev_ext_value = iv_int_value.
      ENDCASE.

      "Reconvert editing for IDoc values
      IF   iv_is_for_idoc          EQ c_true                        AND
         ( lo_elem_desc->type_kind EQ lo_elem_desc->typekind_packed  OR
           lo_elem_desc->type_kind EQ lo_elem_desc->typekind_int1    OR
           lo_elem_desc->type_kind EQ lo_elem_desc->typekind_int2    OR
           lo_elem_desc->type_kind EQ lo_elem_desc->typekind_int ).
        "The IDoc format for packed values is only a dot as decimal delimiter
        "and the negtive sign at the end
        CASE gs_user_def-dcpfm.               " see fixed values of domain
          WHEN space.            " = 1.234.567,89
            TRANSLATE ev_ext_value USING '. '.    " Convert dots into spaces
            CONDENSE ev_ext_value NO-GAPS.        " Delete spaces
            TRANSLATE ev_ext_value USING ',.'.    " Convert comma into dot

          WHEN 'X' ##no_text.    " = 1,234,567.89
            TRANSLATE ev_ext_value USING ', '.    " Convert commas into spaces
            CONDENSE ev_ext_value NO-GAPS.        " Delete spaces

          WHEN 'Y' ##no_text.    " = 1 234 567,89
            CONDENSE ev_ext_value NO-GAPS.        " Delete spaces
            TRANSLATE ev_ext_value USING ',.'.    " Convert comma into dot
        ENDCASE.

        "Set sign at the end, if it doesn't already
        IF iv_int_value LT 0.
          TRANSLATE ev_ext_value USING '- '.      " Delete existing sign in any case
          SHIFT ev_ext_value LEFT DELETING LEADING space.   " Set value far left
          lv_offs_sign = strlen( ev_ext_value ).
          ev_ext_value+lv_offs_sign = '-'.
        ENDIF.
      ENDIF.
    ENDIF.

    IF lv_subrc_write NE 0.
      "Conversion via WRITE statement failed
      RAISE EXCEPTION TYPE zcx_ca_conv
        EXPORTING
          textid   = zcx_ca_conv=>write_failed
          mv_msgty = c_msgty_e.
    ENDIF.

    "Convert to upper case if required
    IF lo_elem_desc->is_ddic_type( ) EQ lo_elem_desc->true AND
       ls_dfies-lowercase            IS INITIAL.
      TRANSLATE ev_ext_value TO UPPER CASE.
    ENDIF.

    "Delete leading spaces
    SHIFT ev_ext_value LEFT DELETING LEADING space.
  ENDMETHOD.                    "int_2_ext


  METHOD int_amount_2_ext.
    "-----------------------------------------------------------------*
    "   Conversion of an internal amount into an external appearance
    "-----------------------------------------------------------------*
    CLEAR ev_ext_value.
    IF iv_currency IS INITIAL.
      WRITE iv_int_value TO ev_ext_value.                   "#EC *

    ELSE.
      "Check existence of the currency
      SELECT COUNT( * ) FROM  tcurc                      "#EC CI_BYPASS
                        WHERE waers EQ iv_currency.
      IF sy-dbcnt NE 1.
        "Parameter '&1' has invalid value '&2'
        RAISE EXCEPTION TYPE zcx_ca_conv
          EXPORTING
            textid   = zcx_ca_conv=>param_invalid
            mv_msgty = c_msgty_e
            mv_msgv1 = 'IV_CURRENCY' ##no_text
            mv_msgv2 = CONV #( iv_currency ).

      ELSE.
        WRITE iv_int_value CURRENCY iv_currency
                                 TO ev_ext_value.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "int_amount_2_ext
ENDCLASS.

