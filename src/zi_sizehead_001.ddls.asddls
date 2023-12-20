@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'ZI_SIZEHEAD_001'
define view entity ZI_SIZEHEAD_001 as select from zsizehead001
association to parent ZI_PRODUCT_001 as _Product on $projection.ProductUUID = _Product.ProductUUID
{
    key sizeheaduuid as SizeHeadUUID,
    productuuid as ProductUUID,
    sizeheadid as SizeHeadID,
    back as Back,
    a as A,
    b as B,
    c as C,
    d as D,
    e as E,
    f as F,
    g as G,
    h as H,
    i as I,
    j as J,
    k as K,
    l as L,
    backsizeid as BackSizeID,
    createdby as CreatedBy,
    createdat as CreatedAt,
    lastchangedby as LastChangedBy,
    lastchangedat as LastChangedAt,
    locallastchangedat as LocalLastChangedAt,
    _Product // Make association public
}
