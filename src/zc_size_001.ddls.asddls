@EndUserText.label: 'ZC_SIZE_001'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
@ObjectModel.semanticKey: [ 'Back'/*, 'A','B','C','D','E','F','G','H','I','J','K','L'*/ ]
define view entity ZC_SIZE_001 as projection on ZI_SIZE_001
{
    key SizeUUID,

    ProductUUID,

    SizeID,

    Back,

    @ObjectModel.text.element: ['TextA']
    @UI.textArrangement: #TEXT_ONLY
    A,

    @ObjectModel.text.element: ['TextB']
    @UI.textArrangement: #TEXT_ONLY
    B,

    @ObjectModel.text.element: ['TextC']
    @UI.textArrangement: #TEXT_ONLY
    C,

    @ObjectModel.text.element: ['TextD']
    @UI.textArrangement: #TEXT_ONLY
    D,

    @ObjectModel.text.element: ['TextE']
    @UI.textArrangement: #TEXT_ONLY
    E,

    @ObjectModel.text.element: ['TextF']
    @UI.textArrangement: #TEXT_ONLY
    F,

    @ObjectModel.text.element: ['TextG']
    @UI.textArrangement: #TEXT_ONLY
    G,

    @ObjectModel.text.element: ['TextH']
    @UI.textArrangement: #TEXT_ONLY
    H,

    @ObjectModel.text.element: ['TextI']
    @UI.textArrangement: #TEXT_ONLY
    I,

    @ObjectModel.text.element: ['TextJ']
    @UI.textArrangement: #TEXT_ONLY
    J,

    @ObjectModel.text.element: ['TextK']
    @UI.textArrangement: #TEXT_ONLY
    K,

    @ObjectModel.text.element: ['TextL']
    @UI.textArrangement: #TEXT_ONLY
    L,
    
    A02,
    B02,
    C02,
    D02,
    E02,
    F02,
    G02,
    H02,
    I02,
    J02,
    K02,
    L02,

    Criticality01,
    Criticality02,
    Criticality03,
    Criticality04,
    Criticality05,
    Criticality06,
    Criticality07,
    Criticality08,
    Criticality09,
    Criticality10,
    Criticality11,
    Criticality12,

    BackSizeID,
    CreatedBy,
    CreatedAt,
    LastChangedBy,
    LastChangedAt,
    LocalLastChangedAt,

    /* Associations to Bool */
    @UI.hidden: true
    _A.Text as TextA,
    @UI.hidden: true
    _B.Text as TextB,
    @UI.hidden: true
    _C.Text as TextC,
    @UI.hidden: true
    _D.Text as TextD,
    @UI.hidden: true
    _E.Text as TextE,
    @UI.hidden: true
    _F.Text as TextF,
    @UI.hidden: true
    _G.Text as TextG,
    @UI.hidden: true
    _H.Text as TextH,
    @UI.hidden: true
    _I.Text as TextI,
    @UI.hidden: true
    _J.Text as TextJ,
    @UI.hidden: true
    _K.Text as TextK,
    @UI.hidden: true
    _L.Text as TextL,

    /* Association */
    _Product : redirected to parent ZC_PRODUCT_001

}
