@EndUserText.label: 'ZC_ITEM_001'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
define view entity ZC_ITEM_001 as projection on ZI_ITEM_001 as Item
{
    key ItemUUID,
    ProductUUID,
    ItemID,
    Model,
    Color,
    CupSize,
    BackSize,
    Product,
    Status,
    Description,
    Checkbox01,
    Criticality01,
    ProductURL,
    CreatedBy,
    CreatedAt,
    LastChangedBy,
    LastChangedAt,
    LocalLastChangedAt,
    /* Associations */
    _Product : redirected to parent ZC_PRODUCT_001
}
