"! <p class="shorttext synchronized" lang="en">Common exception: Conversion failed</p>
class ZCX_CA_CONV definition
  public
  inheriting from ZCX_CA_PARAM
  create public .

public section.

  constants:
    begin of ZCX_CA_CONV,
      msgid type symsgid value 'ZCA',
      msgno type symsgno value '030',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_CA_CONV .
  constants:
    begin of PARAM_IS_NOT_ELEM,
      msgid type symsgid value 'ZCA',
      msgno type symsgno value '031',
      attr1 type scx_attrname value 'MV_MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of PARAM_IS_NOT_ELEM .
  constants:
    begin of WRITE_FAILED,
      msgid type symsgid value 'ZCA',
      msgno type symsgno value '032',
      attr1 type scx_attrname value 'MV_MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of WRITE_FAILED .

  "! <p class="shorttext synchronized" lang="en">My own name</p>
  constants C_ZCX_CA_CONV type SEOCLSNAME value 'ZCX_CA_CONV' ##NO_TEXT.

  "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MT_RETURN type BAPIRET2_T optional
      !MV_SUBRC type SYST_SUBRC optional
      !MV_MSGTY type SYST_MSGTY optional
      !MV_MSGV1 type SYST_MSGV optional
      !MV_MSGV2 type SYST_MSGV optional
      !MV_MSGV3 type SYST_MSGV optional
      !MV_MSGV4 type SYST_MSGV optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_CA_CONV IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
MT_RETURN = MT_RETURN
MV_SUBRC = MV_SUBRC
MV_MSGTY = MV_MSGTY
MV_MSGV1 = MV_MSGV1
MV_MSGV2 = MV_MSGV2
MV_MSGV3 = MV_MSGV3
MV_MSGV4 = MV_MSGV4
.
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = ZCX_CA_CONV .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
