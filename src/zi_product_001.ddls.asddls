@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'ZI_PRODUCT_001'
define root view entity ZI_PRODUCT_001 as select from zproduct001 as Product
composition [0..*] of ZI_SIZEHEAD_001 as _SizeHead
composition [0..*] of ZI_SIZE_001 as _Size
composition [0..*] of ZI_ITEM_001 as _Item
{
    key productuuid as ProductUUID,
    productid as ProductID,
    producttype as ProductType,
    model as Model,
    color as Color,
    matrixtypeid as MatrixTypeID,
    country as Country,
    description as Description,
    copying as Copying,
    sourceproduct as SourceProduct,
    sourceproducturl as SourceProductURL,

    hidden00 as Hidden00,
    hidden01 as Hidden01,
    hidden02 as Hidden02,
    hidden03 as Hidden03,
    hidden04 as Hidden04,
    hidden05 as Hidden05,
    hidden06 as Hidden06,
    hidden07 as Hidden07,
    hidden08 as Hidden08,
    hidden09 as Hidden09,
    hidden10 as Hidden10,
    hidden11 as Hidden11,
    hidden12 as Hidden12,
    hidden13 as Hidden13,
    hidden14 as Hidden14,
    hidden15 as Hidden15,
    hidden16 as Hidden16,
    hidden17 as Hidden17,
    hidden18 as Hidden18,
    hidden19 as Hidden19,
    hidden20 as Hidden20,
    hidden21 as Hidden21,
    hidden22 as Hidden22,

    createdby as CreatedBy,
    createdat as CreatedAt,
    lastchangedby as LastChangedBy,
    lastchangedat as LastChangedAt,
    locallastchangedat as LocalLastChangedAt,
    
    _SizeHead,
    _Size,
    _Item
}
