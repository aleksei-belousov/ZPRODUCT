@EndUserText.label: 'ZC_PRODUCT_001'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
define root view entity ZC_PRODUCT_001 provider contract transactional_query as projection on ZI_PRODUCT_001 as Product
{
    key ProductUUID,
    ProductID,
    ProductType,

    @Consumption.valueHelpDefinition: [ { entity: { name: 'ZC_MODEL_005', element: 'ModelID' } } ]
    Model,

    @Consumption.valueHelpDefinition: [ { entity: { name: 'ZC_COLOR_005', element: 'ColorID' } } ]
    Color,

    MatrixTypeID,

    @Consumption.valueHelpDefinition: [ { entity: { name: 'ZC_COUNTRY_005', element: 'CountryID' } } ]
    Country,

    Description,
    Copying,

    @Consumption.valueHelpDefinition: [ { entity: { name: 'I_ProductTP_2', element: 'Product' } } ]
    SourceProduct,

    SourceProductURL,

    Hidden00,
    Hidden01,
    Hidden02,
    Hidden03,
    Hidden04,
    Hidden05,
    Hidden06,
    Hidden07,
    Hidden08,
    Hidden09,
    Hidden10,
    Hidden11,
    Hidden12,
    Hidden13,
    Hidden14,
    Hidden15,
    Hidden16,
    Hidden17,
    Hidden18,
    Hidden19,
    Hidden20,
    Hidden21,
    Hidden22,
    
    CreatedBy,
    CreatedAt,
    LastChangedBy,
    LastChangedAt,
    LocalLastChangedAt,
    
    /* Associations */
//    _Model: redirected to composition child ZC_MODEL_001,
//    _Color: redirected to composition child ZC_COLOR_001,
    _SizeHead: redirected to composition child ZC_SIZEHEAD_001,
    _Size: redirected to composition child ZC_SIZE_001,
    _Item: redirected to composition child ZC_ITEM_001
}
