@EndUserText.label: 'ZC_BOOL_001'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
define root view entity ZC_BOOL_001 provider contract transactional_query as projection on ZI_BOOL_001
{
    key UUID,
    ID,
    Text
}
