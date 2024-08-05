CLASS zcl_product_test DEFINITION PUBLIC FINAL CREATE PUBLIC. " * Starter Dev (Test)
  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .

  PROTECTED SECTION.

  PRIVATE SECTION.

    METHODS create_product IMPORTING out TYPE REF TO if_oo_adt_classrun_out.
    METHODS read_salestax IMPORTING out TYPE REF TO if_oo_adt_classrun_out.
    METHODS update_salestax IMPORTING out TYPE REF TO if_oo_adt_classrun_out.
    METHODS create_product_via_api IMPORTING out TYPE REF TO if_oo_adt_classrun_out.
    METHODS create_product_via_eml IMPORTING out TYPE REF TO if_oo_adt_classrun_out.

ENDCLASS. " zcl_product_test DEFINITION

CLASS ZCL_PRODUCT_TEST IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

*    create_product( out ).
*    read_salestax( out ).
*    update_salestax( out ).
     create_product_via_api( out ).
*     create_product_via_eml( out ).

  ENDMETHOD. " main

  METHOD create_product.

*    My example (not yet works)
*    MODIFY ENTITIES OF I_ProductTP_2
*        ENTITY Product
*        CREATE FIELDS (
*            Product
*            ProductType
*            BaseUnit
*            IndustrySector
*        )
*        WITH VALUE #( (
*            %cid            = 'Root'
*            Product         = '000000000000000017'
*            ProductType     = 'HAWA'
*            BaseUnit        = 'EA'
*            IndustrySector  = 'M'
*        ) )
*        MAPPED DATA(mapped)
*        FAILED DATA(failed)
*        REPORTED DATA(reported).

**   Example which works:
*    DATA create_product TYPE TABLE FOR CREATE I_ProductTP_2.
*
*    create_product = VALUE #( (
*        %cid = 'product1'
*        Product = '000000000000000017'
*        %control-Product = if_abap_behv=>mk-on
*        ProductType = 'HAWA'
*        %control-ProductType = if_abap_behv=>mk-on
*        BaseUnit = 'EA'
*        %control-BaseUnit = if_abap_behv=>mk-on
*        IndustrySector = 'M'
*        %control-IndustrySector = if_abap_behv=>mk-on
*    ) ).
*
*    MODIFY ENTITIES OF I_ProductTP_2
*        ENTITY Product
*        CREATE FROM
*            create_product
*        CREATE BY
*            \_ProductDescription
*        FIELDS (
*            Language
*            ProductDescription
*        )
*        WITH VALUE #( (
*            %cid_ref = 'product1'
*            Product = '000000000000000017'
*            %target = VALUE #(
*                (   %cid = 'desc1'
*                    Product = '000000000000000017'
*                    Language = 'E'
*                    ProductDescription = 'test2' )
*                (   %cid = 'desc2'
*                    Product = '000000000000000017'
*                    Language = 'D'
*                    ProductDescription = 'test2' )
*            )
*        ) )
*        MAPPED DATA(mapped)
*        REPORTED DATA(reported)
*        FAILED DATA(failed).

**   Simplified Example 1:
*    DATA create_product TYPE TABLE FOR CREATE I_ProductTP_2.
*
*    create_product = VALUE #( (
*        %cid = 'product1'
*        Product = '000231-048-B-020'
**        %control-Product = if_abap_behv=>mk-on
*        ProductType = 'MAT'
**        %control-ProductType = if_abap_behv=>mk-on
*        BaseUnit = 'EA'
**        %control-BaseUnit = if_abap_behv=>mk-on
*        IndustrySector = 'M'
**        %control-IndustrySector = if_abap_behv=>mk-on
*    ) ).
*
*    MODIFY ENTITIES OF I_ProductTP_2
*        ENTITY Product
*        CREATE FROM
*            create_product
*        CREATE BY
*            \_ProductDescription
*        FIELDS (
*            Language
*            ProductDescription
*        )
*        WITH VALUE #( (
*            %cid_ref = 'product1'
*            Product = '000231-048-B-020'
*            %target = VALUE #( (   %cid = 'desc1'
*                    Product = '000231-048-B-020'
*                    Language = 'E'
*                    ProductDescription = 'Descr for 000231-048-B-020' ) )
*        ) )
*        MAPPED DATA(mapped)
*        REPORTED DATA(reported)
*        FAILED DATA(failed).

**   Simplified Example 2:
*
*    DATA(product) = '0001056-003-0-034'.
*    CONCATENATE 'Descr for' product INTO DATA(productDescription) SEPARATED BY space.
*
*    MODIFY ENTITIES OF I_ProductTP_2
*        ENTITY Product
*        CREATE FIELDS (
*            Product
*            ProductType
*            BaseUnit
*            IndustrySector
*        )
*        WITH VALUE #( (
*            %cid = 'product1'
*            Product = product " '000231-048-B-030'
*            ProductType = 'MAT'
*            BaseUnit = 'EA'
*            IndustrySector = 'M'
*        ) )
*        CREATE BY
*            \_ProductDescription
*        FIELDS (
*            Language
*            ProductDescription
*        )
*        WITH VALUE #( (
*            %cid_ref = 'product1'
*            Product = product " '000231-048-B-030'
*            %target = VALUE #( (
*                %cid                = 'desc1'
*                Product             = product " '000231-048-B-030'
*                Language            = 'E'
*                ProductDescription  = productDescription " 'Descr for 000231-048-B-030'
*            ) )
*        ) )
*        MAPPED DATA(mapped)
*        REPORTED DATA(reported)
*        FAILED DATA(failed).
*
**   It is obligatory:
*    COMMIT ENTITIES
*        RESPONSE OF I_ProductTP_2
*        FAILED DATA(failed_commit)
*        REPORTED DATA(reported_commit).

    DATA it_product_create              TYPE TABLE FOR CREATE I_ProductTP_2\\Product.
    DATA it_productdescription_create   TYPE TABLE FOR CREATE I_ProductTP_2\\Product\_ProductDescription.
    DATA it_productplant_create         TYPE TABLE FOR CREATE I_ProductTP_2\\Product\_ProductPlant.
    DATA it_supplyplanning_create       TYPE TABLE FOR CREATE I_ProductTP_2\\ProductPlant\_ProductPlantSupplyPlanning.
    DATA cid                            TYPE abp_behv_cid.

    DATA(old_product) = 'TG000231-048-A-060'.
    DATA(new_product) = '0000301-047-B-110'.

*   Read Source Product (with nodes)
    READ ENTITIES OF I_ProductTP_2
        ENTITY Product
        ALL FIELDS WITH VALUE #( (
            Product = old_product " 'TG000231-048-A-060'
        ) )
        RESULT DATA(lt_product)
        ENTITY Product BY \_ProductDescription
        ALL FIELDS WITH VALUE #( (
            Product = old_product " 'TG000231-048-A-060'
        ) )
        RESULT DATA(lt_productdescription)
        ENTITY Product BY \_ProductPlant
        ALL FIELDS WITH VALUE #( (
            Product = old_product " 'TG000231-048-A-060'
        ) )
        RESULT DATA(lt_productplant)
        ENTITY ProductPlant BY \_ProductPlantSupplyPlanning
        ALL FIELDS WITH VALUE #( (
            Product = old_product " 'TG000231-048-A-060'
            Plant   = '1010'
        ) )
        RESULT DATA(lt_supplyplanning)
        FAILED DATA(failed1)
        REPORTED DATA(reported1).

    CLEAR it_product_create[].
    CLEAR it_productdescription_create[].
    CLEAR it_productplant_create[].
    CLEAR it_supplyplanning_create[].

*   Product
    LOOP AT lt_product INTO DATA(ls_product).
        ls_product-Product = new_product.
        cid = 'product'.
        APPEND VALUE #(
            %cid    = cid
            %data   = ls_product-%data
        )
        TO it_product_create[].
    ENDLOOP.
*   Product Description
    LOOP AT lt_productdescription INTO DATA(ls_productdescription).
        ls_productdescription-Product = new_product. " '000231-048-B-035'
        cid = 'descr' && CONV string( sy-tabix ).
        CONDENSE cid.
        IF ( ls_productdescription-Language = 'E' ).
            CONCATENATE 'Descr for' new_product INTO ls_productdescription-ProductDescription SEPARATED BY space. " 'Descr for 000231-048-B-035'
        ENDIF.
        APPEND VALUE #(
            %cid_ref    = 'product'
            Product     = new_product " '000231-048-B-035'
            %target = VALUE #( (
                %cid    = cid
                %data   = ls_productdescription-%data
            ) )
        )
        TO it_productdescription_create[].
    ENDLOOP.
*   Plant
    LOOP AT lt_productplant INTO DATA(ls_productplant).
        ls_productplant-Product = new_product. " '000231-048-B-035'
        cid = 'plant' && CONV string( sy-tabix ).
        CONDENSE cid.
        APPEND VALUE #(
            %cid_ref    = 'product'
            Product     = ls_productplant-Product " '000231-048-B-035'
            %target = VALUE #( (
                %cid    = cid
                %data   = ls_productplant-%data
            ) )
        )
        TO it_productplant_create[].
    ENDLOOP.
*   Supply Planning
    LOOP AT lt_supplyplanning INTO DATA(ls_supplyplanning).
        ls_supplyplanning-Product = new_product. " '000231-048-B-035'
        cid = 'suppl' && CONV string( sy-tabix ).
        CONDENSE cid.
        APPEND VALUE #(
            %cid_ref    = 'plant1'
            Product     = ls_supplyplanning-Product " '000231-048-B-035'
            Plant       = ls_supplyplanning-Plant   " '000231-048-B-035'
            %target = VALUE #( (
                %cid    = cid
                %data   = ls_supplyplanning-%data
            ) )
        )
        TO it_supplyplanning_create[].
    ENDLOOP.

    MODIFY ENTITIES OF I_ProductTP_2
*       Product
        ENTITY Product
*       Product - Product
        CREATE FIELDS (
            ANPCode
            ArticleCategory
            AuthorizationGroup
            BaseUnit
            BaseUnitSpecificProductHeight
            BaseUnitSpecificProductLength
            BaseUnitSpecificProductWidth
            BasicProduct
*            CreatedByUser
*            CreationDate
            CreationDateTime
*            CreationTime
            CrossPlantConfigurableProduct
            CrossPlantStatus
            CrossPlantStatusValidityDate
            DangerousGoodsIndProfile
            DiscountInKindEligibility
            Division
            DocumentIsCreatedByCAD
            ExternalProductGroup
            GrossWeight
            HandlingIndicator
            HandlingUnitType
            HasVariableTareWeight
            IndustrySector
            IndustryStandardName
            InternationalArticleNumberCat
            IsApprovedBatchRecordReqd
            IsBatchManagementRequired
            IsMarkedForDeletion
            IsPilferable
            IsRelevantForHzdsSubstances
            ItemCategoryGroup
            LaboratoryOrDesignOffice
*            LastChangeDate
*            LastChangeDateTime
*            LastChangeTime
*            LastChangedByUser
            MaximumCapacity
            MaximumPackagingHeight
            MaximumPackagingLength
            MaximumPackagingWidth
            NetWeight
            OvercapacityTolerance
            PackagingProductGroup
            PackingReferenceProduct
            ProdAllocDetnProcedure
            ProdChmlCmplncRelevanceCode
            ProdCompetitorCustomerNumber
            ProdEffctyParamValsAreAssigned
            ProdIsEnvironmentallyRelevant
            Product
            ProductDocumentChangeNumber
            ProductDocumentNumber
            ProductDocumentPageCount
            ProductDocumentPageFormat
            ProductDocumentPageNumber
            ProductDocumentType
            ProductDocumentVersion
            ProductGroup
            ProductHierarchy
            ProductIsConfigurable
            ProductIsHighlyViscous
            ProductMeasurementUnit
            ProductOldID
            ProductStandardID
            ProductType
            ProductVolume
            ProductionMemoPageFormat
            ProductionOrInspectionMemoTxt
            QualityInspectionGroup
            QuarantinePeriod
            SerialNoExplicitnessLevel
            SerialNumberProfile
            SizeOrDimensionText
            StandardHandlingUnitType
            TimeUnitForQuarantinePeriod
            TransportIsInBulk
            UnitForMaxPackagingDimensions
            VolumeUnit
            WarehouseProductGroup
            WarehouseStorageCondition
            WeightUnit
        )
        WITH it_product_create
*       Product - Description
        CREATE BY \_ProductDescription FIELDS (
            Language
            ProductDescription
        )
        WITH it_productdescription_create
*       Product - Plant
        CREATE BY \_ProductPlant FIELDS (
            BaseUnit
            ConfigurableProduct
            DistrCntrDistributionProfile
            FiscalYearVariant
            GoodsIssueUnit
            IsBatchManagementRequired
            IsMarkedForDeletion
            IsNegativeStockAllowed
            OriginalBatchReferenceProduct
            PeriodType
            Plant
            ProductCFOPCategory
            ProductControlTemperatureUnit
            ProductFreightGroup
            ProductIsCriticalPrt
            ProductIsExciseTaxRelevant
            ProductLogisticsHandlingGroup
            ProductMaxControlTemperature
            ProductMinControlTemperature
            ProfileCode
            ProfileValidityStartDate
            ProfitCenter
            SerialNumberProfile
            StockDeterminationGroup
        )
        WITH it_productplant_create
        ENTITY ProductPlant
*       Product Plant - Supply Planning
        CREATE BY \_ProductPlantSupplyPlanning FIELDS (
*            Product
*            Plant
            LotSizingProcedure
            MRPType
            MRPResponsible
            ReorderThresholdQuantity
            PlanningStrategyGroup
            TotalReplenishmentLeadTime
            ProcurementType
            AvailabilityCheckType
            Currency
            BaseUnit
        )
        WITH it_supplyplanning_create
        MAPPED DATA(mapped2)
        REPORTED DATA(reported2)
        FAILED DATA(failed2).

*   It is obligatory:
    COMMIT ENTITIES
        RESPONSE OF I_ProductTP_2
        FAILED DATA(failed_commit)
        REPORTED DATA(reported_commit).

  ENDMETHOD. " create_product

  METHOD read_salestax.

*    READ ENTITIES OF I_ProductTP_2
**       Product Sales Delivery - Sales Tax
*        ENTITY ProductSalesDelivery BY \_ProdSalesDeliverySalesTax
*        ALL FIELDS WITH VALUE #( (
*            %key-Product                 = '0000301-030-C-080'
*            %key-ProductSalesOrg         = '1000'
*            %key-ProductDistributionChnl = '10'
*        ) )
*        RESULT DATA(lt_salestax1) " 1 row
*        FAILED DATA(ls_failed1)
*        REPORTED DATA(ls_reported1).
*
*    Do not work
*    READ ENTITIES OF I_ProductTP_2
**       Product Sales Delivery Sales Tax
*        ENTITY ProductSalesDeliverySalesTax
*        ALL FIELDS WITH VALUE #( (
*            %key-Country                 = 'CH'
*            %key-Product                 = '0000301-030-C-080'
*            %key-ProductSalesOrg         = '1000'
*            %key-ProductDistributionChnl = '10'
*            %key-ProductSalesTaxCategory = 'TTX1'
*        ) )
*        RESULT DATA(lt_salestax2) " 0 rows
*        FAILED DATA(ls_failed2)
*        REPORTED DATA(ls_reported2).
*
*    Do not work
*    SELECT * FROM I_ProdSlsDeliverySalesTaxTP_2 WHERE ( Product = '0000301-030-C-080' ) INTO TABLE @DATA(it_salestax3).
*
*    SELECT * FROM I_ProductTaxClassification WHERE ( Product = '0000301-030-C-080' ) INTO TABLE @DATA(it_taxclassification3).
*
*    The use of element PRODUCT of CDS Entity I_PRODUCTSALESCOUNTRYWD is not permitted.
*    SELECT * FROM I_ProductSalesCountryWD WHERE ( Product = '0000301-030-C-080' ) INTO TABLE @DATA(it_productsalescountrywd).

    SELECT * FROM I_ProductSalesTax WHERE ( Product = '0000301-030-C-080' ) INTO TABLE @DATA(it_productsalestax).

    LOOP AT it_productsalestax INTO DATA(wa_productsalestax).
        out->write( wa_productsalestax ).
    ENDLOOP.

  ENDMETHOD. " read_salestax

  METHOD update_salestax.

*    Does not wowrk
*    MODIFY ENTITIES OF I_ProductTP_2
*        ENTITY ProductSalesDeliverySalesTax
*        UPDATE FIELDS ( ProductTaxClassification )
*        WITH VALUE #( ( %key-Product = '0000301-030-C-080'
*                        %key-Country = 'CH'
*                        %key-ProductSalesTaxCategory = 'TTX1'
*                        %key-ProductSalesOrg = '1000'
*                        %key-ProductDistributionChnl = '10'
*                        ProductTaxClassification = '2' ) )
*        REPORTED DATA(reported3)
*        FAILED DATA(failed3).

*   ProductSalesDelivery
    DATA it_salestax_create TYPE TABLE FOR CREATE I_ProductTP_2\\ProductSalesDelivery\_ProdSalesDeliverySalesTax.

*   Product - Sales Delivery - Sales Tax
*    LOOP AT it_salestax INTO DATA(wa_salestax) WHERE ( ProductSalesOrg          = ls_sourceproductsalesdelivery-ProductSalesOrg ) AND
*                                                     ( ProductDistributionChnl  = ls_sourceproductsalesdelivery-ProductDistributionChnl ).
        DATA(product) = '0080505-003-B-075'.
        DATA(cid2) = 'salestax1'.
        CONDENSE cid2.
        APPEND VALUE #(
            %cid_ref                        = 'salesdelivery1'
            %key-Product                    = product " '0000301-030-C-080'
            %key-ProductSalesOrg            = '1000'
            %key-ProductDistributionChnl    = '10'
            %target = VALUE #( (
                %cid    = 'salestax1'
                Product = product " '0000301-030-C-080'
                Country = 'CH'
                ProductSalesTaxCategory = 'TTX1'
                ProductSalesOrg = '1000'
                ProductDistributionChnl = '10'
                ProductTaxClassification = '2'
            ) )
        )
        TO it_salestax_create[].

*    ENDLOOP.

    MODIFY ENTITIES OF I_ProductTP_2
        ENTITY ProductSalesDelivery
        CREATE BY \_ProdSalesDeliverySalesTax FIELDS (
*            Product
*            ProductSalesOrg
*            ProductDistributionChnl
            Country
            ProductSalesTaxCategory
            ProductTaxClassification
        )
        WITH it_salestax_create
        MAPPED DATA(mapped)
        REPORTED DATA(reported)
        FAILED DATA(failed).

*   It is obligatory:
    COMMIT ENTITIES
        RESPONSE OF I_ProductTP_2
        FAILED DATA(failed_commit)
        REPORTED DATA(reported_commit).

    out->write( 'updated.' ).
*    RETURN.

  ENDMETHOD. " update_salestax

  METHOD create_product_via_api.

*    SELECT * FROM i_product INTO TABLE @DATA(it_product).

    TRY.

*       DATA(i_url) = 'https://my404930-api.s4hana.cloud.sap:443/sap/opu/odata/sap/API_PRODUCT_SRV/A_Product'.

        DATA i_url      TYPE string. " VALUE 'https://my404907-api.s4hana.cloud.sap/sap/opu/odata/sap//sap/opu/odata/sap/API_PRODUCT_SRV/A_Product'.
        DATA i_username TYPE string. " VALUE 'INBOUND_USER'.
        DATA i_password TYPE string. " VALUE 'rtrVDDgelabtTjUiybRX}tVD3JksqqfvPpBdJRaL'.

        DATA(system_url) = cl_abap_context_info=>get_system_url( ).

        IF ( system_url(8) = 'my404930' ). " dev dev
            i_url       = 'https://my404930-api.s4hana.cloud.sap/sap/opu/odata/sap/API_PRODUCT_SRV/A_Product'.
            i_username  = 'INBOUND_USER'.
            i_password  = 'rtrVDDgelabtTjUiybRX}tVD3JksqqfvPpBdJRaL'.
        ENDIF.
        IF ( system_url(8) = 'my404898' ). " dev cust
            i_url       = 'https://my404898-api.s4hana.cloud.sap/sap/opu/odata/sap/API_PRODUCT_SRV/A_Product'.
            i_username  = 'INBOUND_USER'.
            i_password  = 'rtrVDDgelabtTjUiybRX}tVD3JksqqfvPpBdJRaL'.
        ENDIF.
        IF ( system_url(8) = 'my404907' ). " test
            i_url       = 'https://my404907-api.s4hana.cloud.sap/sap/opu/odata/sap/API_PRODUCT_SRV/A_Product'.
            i_username  = 'INBOUND_USER'.
            i_password  = 'rtrVDDgelabtTjUiybRX}tVD3JksqqfvPpBdJRaL'.
        ENDIF.
        IF ( system_url(8) = 'my410080' ). " prod
            i_url       = 'https://my410080-api.s4hana.cloud.sap/sap/opu/odata/sap/API_PRODUCT_SRV/A_Product'.
            i_username  = 'INBOUND_USER'.
            i_password  = 'YKXMYdjNnGgqko&aEueVx5mHTFPRGcDGAVgQgnFh'.
        ENDIF.

        IF ( system_url(8) <> 'my404898' ).
            out->write( 'It should run in customizing tenant only!' ).
            RETURN.
        ENDIF.

        DATA(http_destination) = cl_http_destination_provider=>create_by_url( i_url ).

        DATA(lo_http_client) = cl_web_http_client_manager=>create_by_http_destination( http_destination ).

        lo_http_client->get_http_request( )->set_authorization_basic(
            i_username = i_username
            i_password = i_password
        ).

        DATA(lo_http_request) = lo_http_client->get_http_request( ).


*       Get Token:

        lo_http_request->set_header_field(
            i_name  = 'x-csrf-token'
            i_value = 'fetch'
        ).

        DATA(lo_http_response) = lo_http_client->execute(
            i_method   = if_web_http_client=>get
        ).

        DATA(text)                   = lo_http_response->get_text( ).
        DATA(status)                 = lo_http_response->get_status( ).
        DATA(response_header_fields) = lo_http_response->get_header_fields( ).

        READ TABLE response_header_fields WITH KEY name = 'x-csrf-token' INTO DATA(field).
        IF ( sy-subrc = 0 ).
            DATA(token) = field-value.
        ENDIF.

*       Update Code:

        DATA i_fields TYPE if_web_http_request=>name_value_pairs.
        APPEND VALUE #(
            name  = 'x-csrf-token'
            value = token " '5iGZK1qT45Vi4UfHYazbPQ=='
        )
        TO i_fields.
        APPEND VALUE #(
            name  = 'Content-Type'
            value = 'application/json'
        )
        TO i_fields.

        lo_http_request->set_header_fields(
          EXPORTING
            i_fields = i_fields
*          RECEIVING
*            r_value  =
        ).

        DATA i_text TYPE string.
        CONCATENATE
                '{'
                '   "Product":"1111111118",'
                '   "ProductType":"HAWA",'
                '   "BaseUnit":"EA",'
                '   "to_Description":{'
                '       "results":['
                '           {'
                '               "Product":"1111111118",'
                '               "Language":"E",'
                '               "ProductDescription":"TEST 8 Desciption"'
                '           }'
                '       ]'
                '   }'
                '}'
            INTO
                i_text.

        lo_http_request->set_text(
            i_text  = i_text
        ).

        lo_http_response = lo_http_client->execute(
*            i_method   = if_web_http_client=>put
            i_method   = if_web_http_client=>post " does not work
        ).

        text                      = lo_http_response->get_text( ).
        status                    = lo_http_response->get_status( ).
        response_header_fields    = lo_http_response->get_header_fields( ).

    CATCH cx_web_message_error.

    CATCH cx_abap_context_info_error INTO DATA(lx_abap_context_info_error).
      " Handle remote Exception
*      RAISE SHORTDUMP lx_abap_context_info_error.

    CATCH /iwbep/cx_cp_remote INTO DATA(lx_remote).
      " Handle remote Exception
*      RAISE SHORTDUMP lx_remote.

    CATCH /iwbep/cx_gateway INTO DATA(lx_gateway).
      " Handle Exception
*      RAISE SHORTDUMP lx_gateway.

    CATCH cx_web_http_client_error INTO DATA(lx_web_http_client_error).
      " Handle Exception
*      RAISE SHORTDUMP lx_web_http_client_error.

    CATCH cx_http_dest_provider_error INTO DATA(lx_http_dest_provider_error).
        "handle exception
*      RAISE SHORTDUMP lx_http_dest_provider_error.

    ENDTRY.

  ENDMETHOD. " create_product_via_api

  METHOD create_product_via_eml. " for dev-cust only - does not work

    TRY.
        DATA(system_url) = cl_abap_context_info=>get_system_url( ).
      CATCH cx_abap_context_info_error.
        "handle exception
    ENDTRY.
    IF ( system_url(8) <> 'my404898' ).
        out->write( 'It should run in customizing tenant only!' ).
        RETURN.
    ENDIF.

*    SELECT * FROM i_product INTO TABLE @DATA(it_product).

    DATA(product) = '1111111116'. " dev-cust
    CONCATENATE 'Descr for' product 'via EML' INTO DATA(productDescription) SEPARATED BY space.

    MODIFY ENTITIES OF I_ProductTP_2
        ENTITY Product
        CREATE FIELDS (
            Product
            ProductType
            BaseUnit
            IndustrySector
        )
        WITH VALUE #( (
            %cid = 'product1'
            Product = product " '1111111116'
            ProductType = 'HAWA'
            BaseUnit = 'EA'
            IndustrySector = 'M'
        ) )
        CREATE BY
            \_ProductDescription
        FIELDS (
            Language
            ProductDescription
        )
        WITH VALUE #( (
            %cid_ref = 'product1'
            Product = product " '1111111116'
            %target = VALUE #( (
                %cid                = 'desc1'
                Product             = product " '1111111116'
                Language            = 'E'
                ProductDescription  = productDescription " 'Descr for 1111111116 via EML'
            ) )
        ) )
        MAPPED DATA(mapped)
        REPORTED DATA(reported)
        FAILED DATA(failed).

*   It is obligatory:
    COMMIT ENTITIES
        RESPONSE OF I_ProductTP_2
        FAILED DATA(failed_commit)
        REPORTED DATA(reported_commit).

  ENDMETHOD.

ENDCLASS.
