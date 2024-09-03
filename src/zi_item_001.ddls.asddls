@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'ZI_ITEM_001'
define view entity ZI_ITEM_001 as select from zitem001 as Item
association to parent ZI_PRODUCT_001 as _Product on $projection.ProductUUID = _Product.ProductUUID
{
    key itemuuid as ItemUUID,
    productuuid as ProductUUID,
    itemid as ItemID,
    model as Model,
    color as Color,
    cupsize as CupSize,
    backsize as BackSize,
    backsizefr as BackSizeFR,
    backsizeus as BackSizeUS,
    backsizegb as BackSizeGB,
    product as Product,
    status as Status,
    description as Description,
    checkbox01 as Checkbox01,
    criticality01 as Criticality01,
    producturl as ProductURL,
    createdby as CreatedBy,
    createdat as CreatedAt,
    lastchangedby as LastChangedBy,
    lastchangedat as LastChangedAt,
    locallastchangedat as LocalLastChangedAt,
    _Product // Make association public
}
