@EndUserText.label: 'ZC_SIZEHEAD_001'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
define view entity ZC_SIZEHEAD_001 as projection on ZI_SIZEHEAD_001
{
    key SizeHeadUUID,
    ProductUUID,
    SizeHeadID,
    Back,
    A,
    B,
    C,
    D,
    E,
    F,
    G,
    H,
    I,
    J,
    K,
    L,
    BackSizeID,
    CreatedBy,
    CreatedAt,
    LastChangedBy,
    LastChangedAt,
    LocalLastChangedAt,
    /* Associations */
    _Product : redirected to parent ZC_PRODUCT_001
}
