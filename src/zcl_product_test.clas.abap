CLASS zcl_product_test DEFINITION PUBLIC FINAL CREATE PUBLIC. " * Starter Dev (Test)
  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_PRODUCT_TEST IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.

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

  ENDMETHOD.
ENDCLASS.
