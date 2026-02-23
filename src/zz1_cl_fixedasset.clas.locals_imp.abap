*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS zz1_cl_fixedasset_lt DEFINITION INHERITING FROM  cl_abap_behavior_handler.
  PUBLIC SECTION.
    DATA: t_fixesasset TYPE STANDARD TABLE OF zz1_i_fixedasset.
    DATA w_flag TYPE c.
    DATA: BEGIN OF wa_fixedasset,
              CompanyCode       TYPE zz1_i_fixedasset-CompanyCode,
              MasterFixedAsset  TYPE zz1_i_fixedasset-MasterFixedAsset,
              FixedAsset        TYPE zz1_i_fixedasset-FixedAsset,
          END OF wa_fixedasset.

    DATA: wa_data TYPE zz1_i_fixedasset.

    METHODS: getEtag
      IMPORTING wa_fixedasset LIKE wa_fixedasset
      RETURNING VALUE(rv_result) TYPE string.

    METHODS: postWithRoom
      IMPORTING wa_data LIKE wa_data
                AssetLocation  TYPE zz1_i_fixedasset-AssetLocation
                CostCenter TYPE zz1_i_fixedasset-CostCenter
                Plant TYPE zz1_i_fixedasset-Plant
                Room TYPE zz1_i_fixedasset-Room
                ValidityStartDate TYPE zz1_i_fixedasset-ValidityStartDate
      EXPORTING message TYPE string
      RETURNING VALUE(rv_result) TYPE string.

    METHODS: postWithoutRoom
      IMPORTING wa_data2 LIKE wa_data
                AssetLocation  TYPE zz1_i_fixedasset-AssetLocation
                CostCenter TYPE zz1_i_fixedasset-CostCenter
                Plant TYPE zz1_i_fixedasset-Plant
                ValidityStartDate TYPE zz1_i_fixedasset-ValidityStartDate
      EXPORTING message TYPE string
      RETURNING VALUE(rv_result) TYPE string.

    DATA: w_user TYPE string VALUE 'YY1_FIXED_ASSET',
          w_pass TYPE string VALUE 'pV6@3a{UATJc\7+pPHTjJXZiyH9Edh3+}elp362[',
          w_clientd(3) VALUE 'XUR',"DEV
          w_clientt(3) VALUE 'N8X',"QAS
          w_clientp(3) VALUE 'PHK'."PRD

  PRIVATE SECTION.
    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR zz1_i_fixedasset RESULT result.

    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING REQUEST requested_authorizations FOR zz1_i_fixedasset RESULT result.

    METHODS UpdateData FOR MODIFY
      IMPORTING keys FOR ACTION zz1_i_fixedasset~UpdateData.

    METHODS Update_Data FOR MODIFY
      IMPORTING keys FOR ACTION zz1_i_fixedasset~Update_Data.

    METHODS lock FOR LOCK
      IMPORTING keys FOR LOCK zz1_i_fixedasset CHANGING reported TYPE data.

    METHODS read FOR READ
      IMPORTING keys FOR READ zz1_i_fixedasset RESULT result.
ENDCLASS.

CLASS zz1_cl_fixedasset_lt IMPLEMENTATION.
    METHOD get_instance_authorizations.
    ENDMETHOD.

    METHOD get_global_authorizations.
    ENDMETHOD.

    METHOD lock.
    ENDMETHOD.

    METHOD read.
        "這裡的 keys 是框架傳進來要讀取的主鍵列表
        SELECT * FROM zz1_i_fixedasset
          FOR ALL ENTRIES IN @keys
          WHERE CompanyCode = @keys-CompanyCode AND MasterFixedAsset = @keys-MasterFixedAsset
            AND FixedAsset = @keys-FixedAsset AND ValidityStartDate = @keys-ValidityStartDate
          INTO TABLE @DATA(lt_data).

        "把資料放回給框架
        result = VALUE #( FOR r IN lt_data ( %tky-CompanyCode = r-CompanyCode
                                             %tky-MasterFixedAsset = r-MasterFixedAsset
                                             %tky-FixedAsset = r-FixedAsset
                                             %tky-ValidityStartDate = r-ValidityStartDate
                                              CompanyCode = r-CompanyCode
                                              MasterFixedAsset = r-MasterFixedAsset
                                              FixedAsset = r-FixedAsset
                                              ValidityStartDate = r-ValidityStartDate
                                              CostCenter = r-CostCenter
                                              ResponsibleCostCenter = r-ResponsibleCostCenter
                                              Plant = r-Plant
                                              AssetLocation = r-AssetLocation
                                              Room = r-Room
                                              InternalOrder = r-InternalOrder
                                              PersonnelNumber = r-PersonnelNumber
                                              FunctionalArea = r-FunctionalArea
                                              IsShutDown = r-IsShutDown ) ).
    ENDMETHOD.

    METHOD UpdateData.
        DATA: l_url TYPE string.
        DATA: l_body TYPE string.
        DATA: lwa_header TYPE if_web_http_request=>name_value_pair,
              lt_header  TYPE if_web_http_request=>name_value_pairs.
        DATA: l_status TYPE if_web_http_response=>http_status .
        DATA: l_text TYPE string,
              l_text2 TYPE string,
              l_message TYPE string,
              l_code TYPE string.
        DATA: lr_http_destination TYPE REF TO if_http_destination.
        DATA: lr_web_http_client TYPE REF TO if_web_http_client.
        DATA: lr_request TYPE REF TO if_web_http_request.
        DATA: lr_response TYPE REF TO if_web_http_response.
        DATA: l_req_json TYPE string.
        DATA: l_severity TYPE if_abap_behv_message=>t_severity,
              lwa_mes TYPE STRUCTURE FOR REPORTED EARLY zz1_i_fixedasset.
        DATA: BEGIN OF lwa_AccountAssignment,
                 ValidityStartDate  TYPE sy-datum,
                 FixedAssetObjectActionCode TYPE string,
                 CostCenter         TYPE string,
                 Plant              TYPE string,
                 AssetLocation      TYPE string,
                 Room               TYPE string,
              END OF lwa_AccountAssignment,
              AccountAssignment LIKE STANDARD TABLE OF lwa_AccountAssignment.

         DATA: BEGIN OF lwa_req,
                 _AccountAssignment LIKE AccountAssignment,
              END OF lwa_req.

        DATA: BEGIN OF lwa_AccountAssignment2,
                 ValidityStartDate  TYPE sy-datum,
                 FixedAssetObjectActionCode TYPE string,
                 CostCenter         TYPE string,
                 Plant              TYPE string,
                 AssetLocation      TYPE string,
              END OF lwa_AccountAssignment2,
              AccountAssignment2 LIKE STANDARD TABLE OF lwa_AccountAssignment2.

         DATA: BEGIN OF lwa_req2,
                 _AccountAssignment LIKE AccountAssignment2,
              END OF lwa_req2.

        DATA: BEGIN OF lwa_res,
                 code    TYPE string,
                 message TYPE string,
              END OF lwa_res.

        DATA: BEGIN OF lwa_error,
                 error LIKE lwa_res,
              END OF lwa_error.

        DATA(l_AssetLocation)       = keys[ 1 ]-%param-AssetLocation.
        DATA(l_CostCenter)          = keys[ 1 ]-%param-CostCenter.
        DATA(l_Plant)               = keys[ 1 ]-%param-Plant.
        DATA(l_Room)                = keys[ 1 ]-%param-Room.
        DATA(l_ValidityStartDate)   = keys[ 1 ]-%param-ValidityStartDate.

        READ ENTITIES OF ZZ1_I_FIXEDASSET IN LOCAL MODE
          ENTITY ZZ1_I_FIXEDASSET
          ALL FIELDS WITH CORRESPONDING #( keys )
          RESULT DATA(lt_result).

        LOOP AT lt_result INTO DATA(lwa_result).

            CLEAR: wa_data, l_code,l_message.
            MOVE-CORRESPONDING lwa_result TO wa_data.

            l_code = postWithRoom( EXPORTING wa_data = wa_data
                                             AssetLocation  = l_AssetLocation
                                             CostCenter = l_CostCenter
                                             Plant = l_Plant
                                             Room = l_Room
                                             ValidityStartDate = l_ValidityStartDate
                                   IMPORTING message = l_message ).
            CLEAR: l_text2.
            IF l_code = 'SUCCESS'.
                l_severity = if_abap_behv_message=>severity-success.
                l_text2 = '處理成功'.
*                CONCATENATE lwa_result-CompanyCode lwa_result-MasterFixedAsset lwa_result-FixedAsset ': 處理成功' INTO l_text2 SEPARATED BY space.
            ELSE.
                l_severity = if_abap_behv_message=>severity-error.
                l_text2 = l_message.
*                CONCATENATE lwa_result-CompanyCode lwa_result-MasterFixedAsset lwa_result-FixedAsset ':' l_message INTO l_text2 SEPARATED BY space.
            ENDIF.

            CLEAR: lwa_mes.
            lwa_mes-CompanyCode = lwa_result-CompanyCode.
            lwa_mes-MasterFixedAsset = lwa_result-MasterFixedAsset.
            lwa_mes-FixedAsset = lwa_result-FixedAsset.
*            lwa_mes-%msg = new_message_with_text( severity = l_severity text = l_text2 ).
            lwa_mes-%msg = NEW_MESSAGE( ID = 'ZZ1_FI' NUMBER = 001 severity = l_severity V1 = lwa_result-CompanyCode
                                        V2 = lwa_result-MasterFixedAsset V3 = lwa_result-MasterFixedAsset V4 = l_text2 ).
            APPEND lwa_mes TO reported-zz1_i_fixedasset.

*            CLEAR wa_fixedasset.
*            wa_fixedasset-companycode = lwa_result-CompanyCode.
*            wa_fixedasset-masterfixedasset = lwa_result-MasterFixedAsset.
*            wa_fixedasset-fixedasset = lwa_result-FixedAsset.
*            DATA(l_etag) = getEtag( wa_fixedasset = wa_fixedasset ).
*
*            CLEAR: lt_header, l_body, l_url, l_severity.
*            IF l_etag IS INITIAL.
*                lt_header = "Headers參數
*                    VALUE #(
*                     ( name = 'Accept' value = 'application/json'  )
*                     ( name = 'Content-Type' value = 'application/json'  ) "Content-Type：內容格式(Body的格式)，application/json：JSON格式
*                     ( name = 'If-Match' value = '*'  ) ).
*            ELSE.
*                lt_header = "Headers參數
*                    VALUE #(
*                     ( name = 'Accept' value = 'application/json'  )
*                     ( name = 'Content-Type' value = 'application/json'  ) "Content-Type：內容格式(Body的格式)，application/json：JSON格式
*                     ( name = 'If-Match' value = l_etag  ) ).
*            ENDIF.
*
*            IF  sy-sysid = w_clientd.
*                l_url = `https://my405690-api.s4hana.cloud.sap/sap/opu/odata4/sap/api_fixedasset/srvd_a2x/sap/fixedasset/0001/FixedAsset/` && lwa_result-CompanyCode && `/`.
*                l_url = l_url && lwa_result-MasterFixedAsset && `/` && lwa_result-FixedAsset && `/` && `SAP__self.Change`.
*            ELSEIF sy-sysid = w_clientt.
*
*            ELSEIF sy-sysid = w_clientp.
*
*            ENDIF.
*            CLEAR: l_text, l_req_json, lwa_AccountAssignment, AccountAssignment, lwa_req, lwa_AccountAssignment2, lwa_req2, AccountAssignment2.
*            IF w_flag <> 'X'.
*                lwa_AccountAssignment-FixedAssetObjectActionCode = '01'.
*                lwa_AccountAssignment-validitystartdate = l_ValidityStartDate.
*                lwa_AccountAssignment-costcenter = l_CostCenter.
*                lwa_AccountAssignment-assetlocation = l_AssetLocation.
*                lwa_AccountAssignment-plant = l_Plant.
*                lwa_AccountAssignment-room = l_Room.
*                APPEND lwa_AccountAssignment TO AccountAssignment.
*                lwa_req-_accountassignment = AccountAssignment.
*                l_req_json = /ui2/cl_json=>serialize( data = lwa_req pretty_name = /ui2/cl_json=>pretty_mode-low_case ).
*                REPLACE ALL OCCURRENCES OF:
*                    '_accountassignment' IN l_req_json WITH '_AccountAssignment',
*                    'validitystartdate' IN l_req_json WITH 'ValidityStartDate',
*                    'fixedassetobjectactioncode' IN l_req_json WITH 'FixedAssetObjectActionCode',
*                    'costcenter' IN l_req_json WITH 'CostCenter',
*                    'plant' IN l_req_json WITH 'Plant',
*                    'assetlocation' IN l_req_json WITH 'AssetLocation',
*                    'room' IN l_req_json WITH 'Room'.
*            ELSE.
*                CLEAR w_flag.
*                lwa_AccountAssignment2-FixedAssetObjectActionCode = '01'.
*                lwa_AccountAssignment2-validitystartdate = l_ValidityStartDate.
*                lwa_AccountAssignment2-costcenter = l_CostCenter.
*                lwa_AccountAssignment2-assetlocation = l_AssetLocation.
*                lwa_AccountAssignment2-plant = l_Plant.
*                APPEND lwa_AccountAssignment2 TO AccountAssignment2.
*                lwa_req2-_accountassignment = AccountAssignment2.
*                l_req_json = /ui2/cl_json=>serialize( data = lwa_req pretty_name = /ui2/cl_json=>pretty_mode-low_case ).
*                REPLACE ALL OCCURRENCES OF:
*                    '_accountassignment' IN l_req_json WITH '_AccountAssignment',
*                    'validitystartdate' IN l_req_json WITH 'ValidityStartDate',
*                    'fixedassetobjectactioncode' IN l_req_json WITH 'FixedAssetObjectActionCode',
*                    'costcenter' IN l_req_json WITH 'CostCenter',
*                    'plant' IN l_req_json WITH 'Plant',
*                    'assetlocation' IN l_req_json WITH 'AssetLocation'.
*            ENDIF.
*
*
**        IF wa_manufacturingorder-YY1_MES_PO_ORD IS NOT INITIAL.
**            l_body = `{"d":{"YY1_MES_PO_ORD":"` && wa_manufacturingorder-YY1_MES_PO_ORD && `", "YY1_MES_STATUS_ORD":"` && wa_manufacturingorder-YY1_MES_STATUS_ORD && `"}}`. "Body參數 "YYYYYYYYY"
**        ELSE.
**            l_body = `{"d":{"YY1_MES_STATUS_ORD":"` && wa_manufacturingorder-YY1_MES_STATUS_ORD && `"}}`. "Body參數 "YYYYYYYYY"
**        ENDIF.
**
**        l_url = `https://my427098-api.s4hana.cloud.sap/sap/opu/odata/sap/API_PRODUCTION_ORDER_2_SRV/A_ProductionOrder_2('` &&  wa_manufacturingorder-manufacturingorder && `')`.
*
*            CLEAR: l_status, l_text.
*            FREE: lr_http_destination, lr_web_http_client, lr_request, lr_response.
*            TRY.
*                lr_http_destination = cl_http_destination_provider=>create_by_url( i_url = l_url ). "直接在程式碼中指定 URL 來呼叫 HTTP 或 SOAP 服務
*            CATCH cx_http_dest_provider_error INTO DATA(lr_data).
*            ENDTRY.
*            IF lr_data IS INITIAL.
*                TRY.
*                    lr_web_http_client = cl_web_http_client_manager=>create_by_http_destination( i_destination = lr_http_destination ).
*
*                    lr_request = lr_web_http_client->get_http_request( ).
*                    lr_request->set_authorization_basic( i_username = w_user i_password = w_pass ).
*                    lr_request->set_header_fields( i_fields = lt_header ).
*                    lr_request->set_text( i_text = l_req_json ).
*                    lr_web_http_client->set_csrf_token( ). "獲得TOKEN
*                    lr_response = lr_web_http_client->execute( i_method
*                    = if_web_http_client=>POST ).
*                    IF lr_response IS BOUND.
*                        l_status = lr_response->get_status( ). "獲得執行結果狀態
*                        l_text = lr_response->get_text( ). "獲得執行結果訊息
*                    ENDIF.
*                    DATA(lr_web_http_response) = lr_web_http_client->execute( if_web_http_client=>POST ).
*                    DATA(l_response) = lr_web_http_response->get_text( ).
*                    lr_web_http_client->close( ).
*                CATCH cx_web_http_client_error INTO DATA(lr_data2).
*                ENDTRY.
*
*            ENDIF.
*
*            CLEAR: l_message, l_text2.
*            IF ( l_status-code = 204 OR l_status-code = 200 ).
*                l_severity = if_abap_behv_message=>severity-success.
*                CONCATENATE lwa_result-CompanyCode lwa_result-MasterFixedAsset lwa_result-FixedAsset ': 處理成功' INTO l_text2 SEPARATED BY space.
*            ELSE.
*
*
*                CLEAR: lwa_error, l_message.
*                /ui2/cl_json=>deserialize( "將資料按照Json格式解譯放入ITAB
*                       EXPORTING
*                         json        =  l_text
*                         pretty_name = /ui2/cl_json=>pretty_mode-low_case
*                       CHANGING
*                         data        = lwa_error ).
*
*                IF lwa_error-error-code = 'FAA_MD/503'.
*                    w_flag = 'X'.
*                ENDIF.
*                l_message = lwa_error-error-message.
*
*                l_severity = if_abap_behv_message=>severity-error.
*                CONCATENATE lwa_result-CompanyCode lwa_result-MasterFixedAsset lwa_result-FixedAsset ':' l_message INTO l_text2 SEPARATED BY space.
*            ENDIF.
*            CLEAR: lwa_mes.
*            lwa_mes-CompanyCode = lwa_result-CompanyCode.
*            lwa_mes-MasterFixedAsset = lwa_result-MasterFixedAsset.
*            lwa_mes-FixedAsset = lwa_result-FixedAsset.
**            lwa_mes-%msg = new_message_with_text( severity = l_severity text = l_text2 ).
*            lwa_mes-%msg = NEW_MESSAGE( ID = 'ZZ1_FI' NUMBER = 001 severity = l_severity V1 = lwa_result-CompanyCode
*                                        V2 = lwa_result-MasterFixedAsset V3 = lwa_result-MasterFixedAsset V4 = l_message ).
*            APPEND lwa_mes TO reported-zz1_i_fixedasset.

        ENDLOOP.
    ENDMETHOD.

    METHOD Update_Data.
**        DATA: r_CompanyCode TYPE RANGE OF ZZ1_I_FIXEDASSET-CompanyCode,
**              r_MasterFixedAsset TYPE RANGE OF ZZ1_I_FIXEDASSET-MasterFixedAsset,
**              r_FixedAsset TYPE RANGE OF ZZ1_I_FIXEDASSET-FixedAsset,
**              r_ValidityStartDate TYPE RANGE OF ZZ1_I_FIXEDASSET-ValidityStartDate,
**              r_CostCenter TYPE RANGE OF ZZ1_I_FIXEDASSET-CostCenter,
**              r_ResponsibleCostCenter TYPE RANGE OF ZZ1_I_FIXEDASSET-ResponsibleCostCenter,
**              r_plant TYPE RANGE OF ZZ1_I_FIXEDASSET-plant,
**              r_AssetLocation TYPE RANGE OF ZZ1_I_FIXEDASSET-AssetLocation,
**              r_Room TYPE RANGE OF ZZ1_I_FIXEDASSET-Room.
*
**        READ TABLE keys WITH KEY %cid = '' INTO DATA(key_with_inital_cid).
**        ASSERT key_with_inital_cid IS INITIAL.
*
*        DATA(lv_new_status) = keys[ 1 ]-%param-AssetLocation.
*        DATA(lv_note)       = keys[ 1 ]-%param-CostCenter.
*
*        READ ENTITIES OF ZZ1_I_FIXEDASSET IN LOCAL MODE
*          ENTITY ZZ1_I_FIXEDASSET
*          ALL FIELDS WITH CORRESPONDING #( keys )
*          RESULT DATA(lt_result).
*
*
*
**         DATA(lresult) =  lt_result[ 1 ].
**
**        APPEND VALUE #(  %tky = lresult-%tky ) TO failed-zz1_i_fixedasset.
**
**        APPEND VALUE #( %tky        = lresult-%tky
**                        %msg        = new_message_with_text(
**
**                             severity = if_abap_behv_message=>severity-error
**                             text = 'No Data Found' )
**                              )
**                TO reported-zz1_i_fixedasset.
*
*
*        LOOP AT keys INTO DATA(ls_key).
**            READ ENTITIES OF ZZ1_I_FIXEDASSET IN LOCAL MODE
**              ENTITY ZZ1_I_FIXEDASSET
**              ALL FIELDS WITH CORRESPONDING #( keys )
**              RESULT DATA(lt_result).
*
**          IF key-%param-_filter[] IS NOT INITIAL.
**              LOOP AT key-%param-_filter[] INTO DATA(ls_filter).
**                  r_AssetLocation = VALUE #( FOR loc IN filter-AssetLocation (
**                                       sign = 'I'
**                                       option = 'EQ'
**                                       low = loc-AssetLocation ) ).
**              ENDLOOP.
**          ENDIF.
*
**          r_CompanyCode = VALUE #( FOR loc IN ls_key-%param-CompanyCode (
**                               sign = 'I'
**                               option = 'EQ'
**                               low = loc-CompanyCode ) ).
**            LOOP AT lt_result INTO DATA(ls_data).
**                " Do something with ls_data
**            ENDLOOP.
*
*        ENDLOOP.
*
**        LOOP AT keys INTO DATA(key).
**          plant = key-%param-plant.
**          r_location = VALUE #( FOR loc IN key-%param-_location (
**                               sign = 'I'
**                               option = 'EQ'
**                               low = loc-location ) ).
**          EXIT. "get the first record of the keys
**        ENDLOOP.
**
**        "select from cds view (not including draft data)
**        SELECT price,
**               stock
**          FROM zr_yasu_stock
**          WHERE plant =
**          AND location IN @r_location
**          INTO TABLE (stock_t).
**
**        " calculate stock amount
**        LOOP AT stock_t INTO DATA(stock).
**          amount = amount + stock-Price * stock-Stock.
**        ENDLOOP.
**
**        " return result
**        result = VALUE #( FOR key1 IN keys (
**                           %cid = key1-%cid
**                           %param =  VALUE #( amount = amount )
**                         ) ).
*
**        DATA: lt_time_record TYPE TABLE FOR CREATE ZZ1_I_TIME_RECORD.
**        READ TABLE keys WITH KEY %cid_ref = '' INTO DATA(key_with_inital_cid).
***        ASSERT key_with_inital_cid IS INITIAL.
**        READ ENTITIES OF ZZ1_I_TIME_RECORD IN LOCAL MODE
**          ENTITY ZZ1_I_TIME_RECORD
**          ALL FIELDS WITH CORRESPONDING #( keys )
**          RESULT DATA(lt_result).
**        IF key_with_inital_cid-%param-wbselementexternalid IS INITIAL.
**            key_with_inital_cid-%param-wbselementexternalid = 'X'.
**        ENDIF.
**        IF key_with_inital_cid-%param-ActivityType IS INITIAL.
**            key_with_inital_cid-%param-ActivityType = 'X'.
**        ENDIF.
**        IF key_with_inital_cid-%param-billingcontrolcategoryid IS INITIAL.
**            key_with_inital_cid-%param-billingcontrolcategoryid = 'X'.
**        ENDIF.
**        "create new BO instance
**        MODIFY ENTITIES OF ZZ1_I_TIME_RECORD IN LOCAL MODE
**          ENTITY ZZ1_I_TIME_RECORD
**          "  CREATE FIELDS ( TimeSheetDate RecordedHours wbselementexternalid )
**            CREATE FIELDS ( TimeSheetDate RecordedHours wbselementexternalid ActivityType billingcontrolcategoryid zz1_timesheettasktype )
**              WITH VALUE #( FOR key IN keys ( %cid = key-%cid
**                                                TimeSheetDate = key_with_inital_cid-%param-TimeSheetDate
**                                                RecordedHours = key_with_inital_cid-%param-RecordedHours
**                                                wbselementexternalid = key_with_inital_cid-%param-wbselementexternalid
**                                                ActivityType  = key_with_inital_cid-%param-ActivityType
**                                                billingcontrolcategoryid = key_with_inital_cid-%param-billingcontrolcategoryid
**                                                zz1_timesheettasktype = key_with_inital_cid-%param-zz1_timesheettasktype
**                                                    ) )
**          MAPPED DATA(mapped_create).
**          mapped-zz1_i_time_record = mapped_create-zz1_i_time_record.

    ENDMETHOD.

    METHOD getEtag.
        DATA: l_url TYPE string.
        DATA: l_etag  TYPE string.
        DATA: lwa_header TYPE if_web_http_request=>name_value_pair,
              lt_header  TYPE if_web_http_request=>name_value_pairs.
        DATA: l_status TYPE if_web_http_response=>http_status .
        DATA: l_text TYPE string.
        DATA: lr_http_destination TYPE REF TO if_http_destination.
        DATA: lr_web_http_client TYPE REF TO if_web_http_client.
        DATA: lr_request TYPE REF TO if_web_http_request.
        DATA: lr_response TYPE REF TO if_web_http_response.

        CLEAR: lt_header, l_url.
        lt_header = "Headers參數
            VALUE #(
             ( name = 'Accept' value = 'application/json'  )
             ( name = 'Content-Type' value = 'application/json'  ) "Content-Type：內容格式(Body的格式)，application/json：JSON格式
             ( name = 'If-Match' value = '*'  ) ).

        IF  sy-sysid = w_clientd.
            l_url = `https://my405690-api.s4hana.cloud.sap/sap/opu/odata4/sap/api_fixedasset/srvd_a2x/sap/fixedasset/0001/FixedAsset/` && wa_fixedasset-CompanyCode && `/`.
            l_url = l_url && wa_fixedasset-MasterFixedAsset && `/` && wa_fixedasset-FixedAsset && `/` && `SAP__self.Change`.
        ELSEIF sy-sysid = w_clientt.

        ELSEIF sy-sysid = w_clientp.
        ENDIF.

        CLEAR: l_status, l_text.
        FREE: lr_http_destination, lr_web_http_client, lr_request, lr_response.
        TRY.
            lr_http_destination = cl_http_destination_provider=>create_by_url( i_url = l_url ). "直接在程式碼中指定 URL 來呼叫 HTTP 或 SOAP 服務
        CATCH cx_http_dest_provider_error INTO DATA(lr_data).
        ENDTRY.
        IF lr_data IS INITIAL.
            TRY.
                lr_web_http_client = cl_web_http_client_manager=>create_by_http_destination( i_destination = lr_http_destination ).

                lr_request = lr_web_http_client->get_http_request( ).
                lr_request->set_authorization_basic( i_username = w_user i_password = w_pass ).
                lr_request->set_header_fields( i_fields = lt_header ).
                lr_web_http_client->set_csrf_token( ). "獲得TOKEN
                lr_response = lr_web_http_client->execute( i_method
                = if_web_http_client=>GET ).
                IF lr_response IS BOUND.
                    l_status = lr_response->get_status( ). "獲得執行結果狀態
                    l_text = lr_response->get_text( ). "獲得執行結果訊息
                ENDIF.
                DATA(lr_web_http_response) = lr_web_http_client->execute( if_web_http_client=>GET ).
                DATA(l_response) = lr_web_http_response->get_text( ).

                rv_result = lr_response->get_header_field( 'ETag' ).  " ← 拿到 ETag
                lr_web_http_client->close( ).
            CATCH cx_web_http_client_error INTO DATA(lr_data2).
            ENDTRY.
            IF ( l_status-code = 204 OR l_status-code = 200 ).

            ENDIF.
       ENDIF.

    ENDMETHOD.

    METHOD postWithRoom.
        DATA: l_url TYPE string.
        DATA: l_body TYPE string.
        DATA: lwa_header TYPE if_web_http_request=>name_value_pair,
              lt_header  TYPE if_web_http_request=>name_value_pairs.
        DATA: l_status TYPE if_web_http_response=>http_status .
        DATA: l_text TYPE string,
              l_text2 TYPE string,
              l_message TYPE string,
              l_code TYPE string.
        DATA: lr_http_destination TYPE REF TO if_http_destination.
        DATA: lr_web_http_client TYPE REF TO if_web_http_client.
        DATA: lr_request TYPE REF TO if_web_http_request.
        DATA: lr_response TYPE REF TO if_web_http_response.
        DATA: l_req_json TYPE string.
        DATA: l_severity TYPE if_abap_behv_message=>t_severity,
              lwa_mes TYPE STRUCTURE FOR REPORTED EARLY zz1_i_fixedasset.
        DATA: BEGIN OF lwa_AccountAssignment,
                 ValidityStartDate  TYPE sy-datum,
                 FixedAssetObjectActionCode TYPE string,
                 CostCenter         TYPE string,
                 Plant              TYPE string,
                 AssetLocation      TYPE string,
                 Room               TYPE string,
              END OF lwa_AccountAssignment,
              AccountAssignment LIKE STANDARD TABLE OF lwa_AccountAssignment.

         DATA: BEGIN OF lwa_req,
                 _AccountAssignment LIKE AccountAssignment,
              END OF lwa_req.

        DATA: BEGIN OF lwa_AccountAssignment2,
                 ValidityStartDate  TYPE sy-datum,
                 FixedAssetObjectActionCode TYPE string,
                 CostCenter         TYPE string,
                 Plant              TYPE string,
                 AssetLocation      TYPE string,
              END OF lwa_AccountAssignment2,
              AccountAssignment2 LIKE STANDARD TABLE OF lwa_AccountAssignment2.

         DATA: BEGIN OF lwa_req2,
                 _AccountAssignment LIKE AccountAssignment2,
              END OF lwa_req2.

        DATA: BEGIN OF lwa_res,
                 code    TYPE string,
                 message TYPE string,
              END OF lwa_res.

        DATA: BEGIN OF lwa_error,
                 error LIKE lwa_res,
              END OF lwa_error.
        DATA: l_langu(2).

            CLEAR wa_fixedasset.
            wa_fixedasset-companycode = wa_data-CompanyCode.
            wa_fixedasset-masterfixedasset = wa_data-MasterFixedAsset.
            wa_fixedasset-fixedasset = wa_data-FixedAsset.
            DATA(l_etag) = getEtag( wa_fixedasset = wa_fixedasset ).

            CLEAR: lt_header, l_body, l_url, l_severity, l_langu.
            CASE sy-langu.
                WHEN 'M'.
                    l_langu = 'ZF'.
                WHEN '1'.
                    l_langu = 'ZH'.
                WHEN 'E'.
                    l_langu = 'EN'.
            ENDCASE.

            IF l_langu IS INITIAL.
                l_langu = 'ZF'.
            ENDIF.
            IF l_etag IS INITIAL.
                lt_header = "Headers參數
                    VALUE #(
                     ( name = 'Accept' value = 'application/json'  )
                     ( name = 'Content-Type' value = 'application/json'  ) "Content-Type：內容格式(Body的格式)，application/json：JSON格式
                     ( name = 'Accept-Language' value = l_langu  )
                     ( name = 'If-Match' value = '*'  ) ).
            ELSE.
                lt_header = "Headers參數
                    VALUE #(
                     ( name = 'Accept' value = 'application/json'  )
                     ( name = 'Content-Type' value = 'application/json'  ) "Content-Type：內容格式(Body的格式)，application/json：JSON格式
                     ( name = 'Accept-Language' value = l_langu  )
                     ( name = 'If-Match' value = l_etag  ) ).
            ENDIF.

            IF  sy-sysid = w_clientd.
                l_url = `https://my405690-api.s4hana.cloud.sap/sap/opu/odata4/sap/api_fixedasset/srvd_a2x/sap/fixedasset/0001/FixedAsset/` && wa_data-CompanyCode && `/`.
                l_url = l_url && wa_data-MasterFixedAsset && `/` && wa_data-FixedAsset && `/` && `SAP__self.Change`.
            ELSEIF sy-sysid = w_clientt.

            ELSEIF sy-sysid = w_clientp.

            ENDIF.
            CLEAR: l_text, l_req_json, lwa_AccountAssignment, AccountAssignment, lwa_req, lwa_AccountAssignment2, lwa_req2, AccountAssignment2.
            lwa_AccountAssignment-FixedAssetObjectActionCode = '01'.
            lwa_AccountAssignment-validitystartdate = ValidityStartDate.
            lwa_AccountAssignment-costcenter = CostCenter.
            lwa_AccountAssignment-assetlocation = AssetLocation.
            lwa_AccountAssignment-plant = Plant.
            lwa_AccountAssignment-room = Room.
            APPEND lwa_AccountAssignment TO AccountAssignment.
            lwa_req-_accountassignment = AccountAssignment.
            l_req_json = /ui2/cl_json=>serialize( data = lwa_req pretty_name = /ui2/cl_json=>pretty_mode-low_case ).
            REPLACE ALL OCCURRENCES OF:
                '_accountassignment' IN l_req_json WITH '_AccountAssignment',
                'validitystartdate' IN l_req_json WITH 'ValidityStartDate',
                'fixedassetobjectactioncode' IN l_req_json WITH 'FixedAssetObjectActionCode',
                'costcenter' IN l_req_json WITH 'CostCenter',
                'plant' IN l_req_json WITH 'Plant',
                'assetlocation' IN l_req_json WITH 'AssetLocation',
                'room' IN l_req_json WITH 'Room'.

            CLEAR: l_status, l_text.
            FREE: lr_http_destination, lr_web_http_client, lr_request, lr_response.
            TRY.
                lr_http_destination = cl_http_destination_provider=>create_by_url( i_url = l_url ). "直接在程式碼中指定 URL 來呼叫 HTTP 或 SOAP 服務
            CATCH cx_http_dest_provider_error INTO DATA(lr_data).
            ENDTRY.
            IF lr_data IS INITIAL.
                TRY.
                    lr_web_http_client = cl_web_http_client_manager=>create_by_http_destination( i_destination = lr_http_destination ).

                    lr_request = lr_web_http_client->get_http_request( ).
                    lr_request->set_authorization_basic( i_username = w_user i_password = w_pass ).
                    lr_request->set_header_fields( i_fields = lt_header ).
                    lr_request->set_text( i_text = l_req_json ).
                    lr_web_http_client->set_csrf_token( ). "獲得TOKEN
                    lr_response = lr_web_http_client->execute( i_method
                    = if_web_http_client=>POST ).
                    IF lr_response IS BOUND.
                        l_status = lr_response->get_status( ). "獲得執行結果狀態
                        l_text = lr_response->get_text( ). "獲得執行結果訊息
                    ENDIF.
                    DATA(lr_web_http_response) = lr_web_http_client->execute( if_web_http_client=>POST ).
                    DATA(l_response) = lr_web_http_response->get_text( ).
                    lr_web_http_client->close( ).
                CATCH cx_web_http_client_error INTO DATA(lr_data2).
                ENDTRY.

            ENDIF.

            CLEAR: l_message, l_text2, l_code.
            IF ( l_status-code = 204 OR l_status-code = 200 ).
                rv_result = 'SUCCESS'.
            ELSE.

                CLEAR: lwa_error, message, l_message.
                /ui2/cl_json=>deserialize( "將資料按照Json格式解譯放入ITAB
                       EXPORTING
                         json        =  l_text
                         pretty_name = /ui2/cl_json=>pretty_mode-low_case
                       CHANGING
                         data        = lwa_error ).

                IF lwa_error-error-code = 'FAA_MD/503' AND Room IS INITIAL.
                    l_code = postWithoutRoom( EXPORTING wa_data2 = wa_data
                                                        AssetLocation  = AssetLocation
                                                        CostCenter = CostCenter
                                                        Plant = Plant
                                                        ValidityStartDate = ValidityStartDate
                                               IMPORTING message = l_message ).
                    IF l_code = 'ERROR'.
                        message = l_message.
                    ELSE.
                        rv_result = 'SUCCESS'.
                    ENDIF.
                ELSE.
                    rv_result = 'ERROR'.
                    message = lwa_error-error-message.
                ENDIF.

            ENDIF.
    ENDMETHOD.

    METHOD postWithoutRoom.
        DATA: l_url TYPE string.
        DATA: l_body TYPE string.
        DATA: lwa_header TYPE if_web_http_request=>name_value_pair,
              lt_header  TYPE if_web_http_request=>name_value_pairs.
        DATA: l_status TYPE if_web_http_response=>http_status .
        DATA: l_text TYPE string,
              l_text2 TYPE string,
              l_message TYPE string,
              l_code TYPE string.
        DATA: lr_http_destination TYPE REF TO if_http_destination.
        DATA: lr_web_http_client TYPE REF TO if_web_http_client.
        DATA: lr_request TYPE REF TO if_web_http_request.
        DATA: lr_response TYPE REF TO if_web_http_response.
        DATA: l_req_json TYPE string.
        DATA: l_severity TYPE if_abap_behv_message=>t_severity,
              lwa_mes TYPE STRUCTURE FOR REPORTED EARLY zz1_i_fixedasset.
        DATA: BEGIN OF lwa_AccountAssignment,
                 ValidityStartDate  TYPE sy-datum,
                 FixedAssetObjectActionCode TYPE string,
                 CostCenter         TYPE string,
                 Plant              TYPE string,
                 AssetLocation      TYPE string,
              END OF lwa_AccountAssignment,
              AccountAssignment LIKE STANDARD TABLE OF lwa_AccountAssignment.

         DATA: BEGIN OF lwa_req,
                 _AccountAssignment LIKE AccountAssignment,
              END OF lwa_req.

        DATA: BEGIN OF lwa_res,
                 code    TYPE string,
                 message TYPE string,
              END OF lwa_res.

        DATA: BEGIN OF lwa_error,
                 error LIKE lwa_res,
              END OF lwa_error.
        DATA: l_langu(2).

            CLEAR wa_fixedasset.
            wa_fixedasset-companycode = wa_data-CompanyCode.
            wa_fixedasset-masterfixedasset = wa_data-MasterFixedAsset.
            wa_fixedasset-fixedasset = wa_data-FixedAsset.
            DATA(l_etag) = getEtag( wa_fixedasset = wa_fixedasset ).

            CLEAR: lt_header, l_body, l_url, l_severity, l_langu.
            CASE sy-langu.
                WHEN 'M'.
                    l_langu = 'ZF'.
                WHEN '1'.
                    l_langu = 'ZH'.
                WHEN 'E'.
                    l_langu = 'EN'.
            ENDCASE.

            IF l_langu IS INITIAL.
                l_langu = 'ZF'.
            ENDIF.

            IF l_etag IS INITIAL.
                lt_header = "Headers參數
                    VALUE #(
                     ( name = 'Accept' value = 'application/json'  )
                     ( name = 'Content-Type' value = 'application/json'  ) "Content-Type：內容格式(Body的格式)，application/json：JSON格式
                     ( name = 'Accept-Language' value = l_langu  )
                     ( name = 'If-Match' value = '*'  ) ).
            ELSE.
                lt_header = "Headers參數
                    VALUE #(
                     ( name = 'Accept' value = 'application/json'  )
                     ( name = 'Content-Type' value = 'application/json'  ) "Content-Type：內容格式(Body的格式)，application/json：JSON格式
                     ( name = 'Accept-Language' value = l_langu  )
                     ( name = 'If-Match' value = l_etag  ) ).
            ENDIF.

            IF  sy-sysid = w_clientd.
                l_url = `https://my405690-api.s4hana.cloud.sap/sap/opu/odata4/sap/api_fixedasset/srvd_a2x/sap/fixedasset/0001/FixedAsset/` && wa_data-CompanyCode && `/`.
                l_url = l_url && wa_data-MasterFixedAsset && `/` && wa_data-FixedAsset && `/` && `SAP__self.Change`.
            ELSEIF sy-sysid = w_clientt.

            ELSEIF sy-sysid = w_clientp.

            ENDIF.
            CLEAR: l_text, l_req_json, lwa_AccountAssignment, AccountAssignment, lwa_req.
            lwa_AccountAssignment-FixedAssetObjectActionCode = '01'.
            lwa_AccountAssignment-validitystartdate = ValidityStartDate.
            lwa_AccountAssignment-costcenter = CostCenter.
            lwa_AccountAssignment-assetlocation = AssetLocation.
            lwa_AccountAssignment-plant = Plant.
            APPEND lwa_AccountAssignment TO AccountAssignment.
            lwa_req-_accountassignment = AccountAssignment.
            l_req_json = /ui2/cl_json=>serialize( data = lwa_req pretty_name = /ui2/cl_json=>pretty_mode-low_case ).
            REPLACE ALL OCCURRENCES OF:
                '_accountassignment' IN l_req_json WITH '_AccountAssignment',
                'validitystartdate' IN l_req_json WITH 'ValidityStartDate',
                'fixedassetobjectactioncode' IN l_req_json WITH 'FixedAssetObjectActionCode',
                'costcenter' IN l_req_json WITH 'CostCenter',
                'plant' IN l_req_json WITH 'Plant',
                'assetlocation' IN l_req_json WITH 'AssetLocation',
                'room' IN l_req_json WITH 'Room'.

            CLEAR: l_status, l_text.
            FREE: lr_http_destination, lr_web_http_client, lr_request, lr_response.
            TRY.
                lr_http_destination = cl_http_destination_provider=>create_by_url( i_url = l_url ). "直接在程式碼中指定 URL 來呼叫 HTTP 或 SOAP 服務
            CATCH cx_http_dest_provider_error INTO DATA(lr_data).
            ENDTRY.
            IF lr_data IS INITIAL.
                TRY.
                    lr_web_http_client = cl_web_http_client_manager=>create_by_http_destination( i_destination = lr_http_destination ).

                    lr_request = lr_web_http_client->get_http_request( ).
                    lr_request->set_authorization_basic( i_username = w_user i_password = w_pass ).
                    lr_request->set_header_fields( i_fields = lt_header ).
                    lr_request->set_text( i_text = l_req_json ).
                    lr_web_http_client->set_csrf_token( ). "獲得TOKEN
                    lr_response = lr_web_http_client->execute( i_method
                    = if_web_http_client=>POST ).
                    IF lr_response IS BOUND.
                        l_status = lr_response->get_status( ). "獲得執行結果狀態
                        l_text = lr_response->get_text( ). "獲得執行結果訊息
                    ENDIF.
                    DATA(lr_web_http_response) = lr_web_http_client->execute( if_web_http_client=>POST ).
                    DATA(l_response) = lr_web_http_response->get_text( ).
                    lr_web_http_client->close( ).
                CATCH cx_web_http_client_error INTO DATA(lr_data2).
                ENDTRY.

            ENDIF.

            CLEAR: l_message, l_text2, l_code.
            IF ( l_status-code = 204 OR l_status-code = 200 ).
                rv_result = 'SUCCESS'.
            ELSE.

                CLEAR: lwa_error, message.
                /ui2/cl_json=>deserialize( "將資料按照Json格式解譯放入ITAB
                       EXPORTING
                         json        =  l_text
                         pretty_name = /ui2/cl_json=>pretty_mode-low_case
                       CHANGING
                         data        = lwa_error ).

                rv_result = 'ERROR'.
                message = lwa_error-error-message.
            ENDIF.
    ENDMETHOD.
ENDCLASS.
