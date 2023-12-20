@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'ZI_SIZE_001'
/*+[hideWarning] { "IDS" : [ "CARDINALITY_CHECK" ]  } */
define view entity ZI_SIZE_001 as select from zsize001
association to ZI_BOOL_001 as _A on $projection.A = _A.ID
association to ZI_BOOL_001 as _B on $projection.B = _B.ID
association to ZI_BOOL_001 as _C on $projection.C = _C.ID
association to ZI_BOOL_001 as _D on $projection.D = _D.ID
association to ZI_BOOL_001 as _E on $projection.E = _E.ID
association to ZI_BOOL_001 as _F on $projection.F = _F.ID
association to ZI_BOOL_001 as _G on $projection.G = _G.ID
association to ZI_BOOL_001 as _H on $projection.H = _H.ID
association to ZI_BOOL_001 as _I on $projection.I = _I.ID
association to ZI_BOOL_001 as _J on $projection.J = _J.ID
association to ZI_BOOL_001 as _K on $projection.K = _K.ID
association to ZI_BOOL_001 as _L on $projection.L = _L.ID
association to parent ZI_PRODUCT_001 as _Product on $projection.ProductUUID = _Product.ProductUUID
{
    key sizeuuid as SizeUUID,
    productuuid as ProductUUID,
    sizeid as SizeID,
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

    a02 as A02,
    b02 as B02,
    c02 as C02,
    d02 as D02,
    e02 as E02,
    f02 as F02,
    g02 as G02,
    h02 as H02,
    i02 as I02,
    j02 as J02,
    k02 as K02,
    l02 as L02,

    criticality01 as Criticality01,
    criticality02 as Criticality02,
    criticality03 as Criticality03,
    criticality04 as Criticality04,
    criticality05 as Criticality05,
    criticality06 as Criticality06,
    criticality07 as Criticality07,
    criticality08 as Criticality08,
    criticality09 as Criticality09,
    criticality10 as Criticality10,
    criticality11 as Criticality11,
    criticality12 as Criticality12,

    backsizeid as BackSizeID,
    createdby as CreatedBy,
    createdat as CreatedAt,
    lastchangedby as LastChangedBy,
    lastchangedat as LastChangedAt,
    locallastchangedat as LocalLastChangedAt,
    
    _A,
    _B,
    _C,
    _D,
    _E,
    _F,
    _G,
    _H,
    _I,
    _J,
    _K,
    _L,

    _Product // Make association public
}
